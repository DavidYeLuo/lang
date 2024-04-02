#include "parser.h"

#include <iostream>
#include <string>
#include <string_view>
#include <vector>

#include "ast.h"
#include "astbuilder.h"
#include "astdumper.h"
#include "llvm/Support/Casting.h"

namespace lang {

Parser::Parser(Lexer &lexer) : lexer_(lexer) {
  auto &decl =
      builder_.getDeclare(SourceLocation(), "write",
                          builder_.getWriteType(builder_.getGenericType()),
                          /*is_write=*/true, /*is_cdecl=*/false);
  module_.AddDeclaration("write", decl);
}

Result<Module *> Parser::Parse() {
  Result<Token> next = lexer_.Peek();
  if (!next)
    return next;

  while (!next->isa(Token::TK_EOF)) {
    Result<Node *> top_level_entity = [&]() -> Result<Node *> {
      if (next->isa(Token::TK_Def))
        return ParseDefine();
      else if (next->isa(Token::TK_Decl) || next->isa(Token::TK_CDecl))
        return ParseDeclare();
      else
        return getDiag(next->getStart())
               << "Unknown top level entity `" << next->getChars() << "`; "
               << "expected either `def` or `decl`";
    }();

    if (!top_level_entity)
      return top_level_entity;

    next = lexer_.Peek();
    if (!next)
      return next;
  }

  return &module_;
}

Result<Declare *> Parser::ParseDefineImpl() {
  auto peek = lexer_.Peek();
  SourceLocation def_loc = peek->getStart();
  Consume(Token::TK_Def);

  Result<Token> res = lexer_.Lex();
  if (!res)
    return res;

  if (res->getKind() != Token::TK_Identifier)
    return getDiag(res->getStart()) << "Expected an identifier; instead found `"
                                    << res->getChars() << "`";

  std::string name(res->getChars());

  res = lexer_.Lex();
  if (!res)
    return res;

  if (res->getKind() != Token::TK_Assign)
    return getDiag(res->getStart())
           << "Expected `=`; instead found `" << res->getChars() << "`";

  // All definitions must have a body.
  Result<Expr *> expr_res = ParseExpr();
  if (!expr_res)
    return expr_res;

  Expr &expr = **expr_res;
  auto decls = module_.getDeclares(name);
  Declare *decl;
  // This expr can correspond to more than one existing declaration. Find the
  // right one.
  auto found = std::find_if(
      decls.begin(), decls.end(),
      [&expr](const Declare *d) { return d->getType() == expr.getType(); });
  if (found == decls.end()) {
    // This is a new definition. Let's assert someone calling this might not
    // be ambiguous with calling any other function.
    auto ambiguous_callable = std::find_if(
        decls.begin(), decls.end(),
        [&](const Declare *d) { return expr.getType().Matches(d->getType()); });
    if (ambiguous_callable != decls.end()) {
      // TODO: We dont't want to dump the ast. We want to print the relevant
      // lines in the source.
      auto diag = std::move(getDiag(expr.getStart())
                            << "Callable `" << name << "` with type `"
                            << expr.getType().toString()
                            << "` is handled by another callable");
      std::stringstream ss;
      ss << diag.get() << "\n";
      ASTDumper(ss).Dump(**ambiguous_callable);
      return Result<Declare *>::Error(ss.str());
    }
    decl = &builder_.getDeclare(def_loc, name, expr, /*is_write=*/false,
                                /*is_cdecl=*/false);
    module_.AddDeclaration(name, *decl);
  } else {
    // Found an existing one.
    assert(found != decls.end());
    (*found)->setBody(expr);
    decl = *found;
  }

  return decl;
}

// <decl> ::= "decl" <identifier> "=" <type>
Result<Declare *> Parser::ParseDeclareImpl() {
  auto decl_tok = lexer_.Peek();
  SourceLocation decl_loc = decl_tok->getStart();

  assert(decl_tok->isa(Token::TK_Decl) || decl_tok->isa(Token::TK_CDecl));
  bool cdecl = decl_tok->isa(Token::TK_CDecl);
  lexer_.Lex();  // Consume

  Result<Token> tok = lexer_.Lex();
  if (!tok)
    return tok;

  if (!tok->isa(Token::TK_Identifier))
    return getDiag(tok->getStart()) << "Expected an identifier; instead found `"
                                    << tok->getChars() << "`";

  std::string name(tok->getChars());

  // Consume `=`.
  tok = lexer_.Lex();
  if (!tok)
    return tok;
  if (tok->getKind() != Token::TK_Assign)
    return getDiag(tok->getStart())
           << "Expected `=`; instead found `" << tok->getChars() << "`";

  Result<const Type *> type = ParseType();
  if (!type)
    return type;

  Declare &decl =
      builder_.getDeclare(decl_loc, name, **type, name == "write", cdecl);
  module_.AddDeclaration(name, decl);
  return &decl;
}

// <callable> ::= "\" <arg list> "->" <type> <expr>
Result<Callable *> Parser::ParseCallable(std::string_view *callable_name,
                                         const Type *hint) {
  Result<Token> res = lexer_.Lex();
  if (!res)
    return res;

  if (res->getKind() != Token::TK_Lambda)
    return getDiag(res->getStart())
           << "Expected lambda start `\\`; instead found `" << res->getChars()
           << "`";

  SourceLocation callable_loc = res->getStart();

  // Maybe parse arguments.
  res = lexer_.Peek();
  if (!res)
    return res;

  std::vector<std::string> arg_names;
  std::vector<const Type *> arg_types;
  std::vector<SourceLocation> arg_locs;
  while (res->getKind() != Token::TK_Arrow) {
    // Parse one argument (a type and an identifier).
    Result<const Type *> type_res = ParseType();
    if (!type_res)
      return type_res;

    const Type &type = *type_res.get();

    res = lexer_.Lex();
    if (!res)
      return res;
    if (res->getKind() != Token::TK_Identifier)
      return getDiag(res->getStart())
             << "Expected an argument name; instead found `" << res->getChars()
             << "`";

    std::string_view name(res->getChars());

    arg_names.emplace_back(name);
    arg_types.push_back(&type);
    arg_locs.push_back(res->getStart());

    // Finished processing one argument. Check for the ending -> or continue
    // parsing more arguments.
    res = lexer_.Peek();
    if (!res)
      return res;
  }

  Consume(Token::TK_Arrow);

  // Get a return type.
  SourceLocation lambda_loc = lexer_.getCurrentLoc();
  Result<const Type *> type_res = ParseType();
  if (!type_res)
    return type_res;
  const Type &ret_type = **type_res;

  if (callable_name) {
    const auto possible_callables =
        getPossibleCallables(*callable_name, &arg_types);
    if (!possible_callables.empty()) {
      // TODO: We dont't want to dump the ast. We want to print the relevant
      // lines in the source.
      auto diag =
          std::move(getDiag(callable_loc)
                    << "Callable `" << *callable_name << "` with arg types `");
      for (const Type *ty : arg_types) {
        diag << ty->toString() << " ";
      }
      diag << "` is handled by another callable";
      std::stringstream ss;
      ss << diag.get() << "\n";
      for (const Expr *expr : possible_callables) {
        ASTDumper(ss).Dump(*expr);
      }
      return Result<Callable *>::Error(ss.str());
    }
  }

  Callable &callable = builder_.getCallable(callable_loc, ret_type, arg_locs,
                                            arg_names, arg_types);

  // Register the variable names.
  for (size_t i = 0; i < callable.getNumArgs(); ++i)
    RegisterLocalVar(callable.getArgName(i), callable.getArg(i));

  // Parse the body which is just an expression.
  Result<Expr *> expr_res = ParseExpr(&ret_type);
  if (!expr_res)
    return expr_res;
  callable.setBody(**expr_res);

  // Check the return type.
  if (callable.getType().getReturnType() != callable.getBody().getType()) {
    return getDiag(lambda_loc)
           << "Mismatch between callable return type and body return type; "
              "expected "
           << callable.getType().getReturnType().toString()
           << " but instead found " << callable.getBody().getType().toString();
  }

  // TODO: Check against the hint.
  return &callable;
}

// <type>         ::= <namedtype> | <callabletype> | <compositetype>
//   <namedtype>              ::= <identifier>
//   <callabletype>           ::= "\" <type>* "->" <type>
//   <compositetype>          ::= "<" <type>+ ">"
//   <arraytype>              ::= "[" \d+ "x" <type> "]"
//   <generictype>            ::= "GENERIC"
//   <genericremainingtype>   ::= "GENERIC_REMAINING"
Result<const Type *> Parser::ParseType(bool parse_callable_names) {
  Result<Token> res = lexer_.Lex();
  if (!res)
    return res;

  if (res->isa(Token::TK_Identifier)) {
    std::string_view type_name(res->getChars());

    // TODO: Check custom types here eventually.
    if (!IsBuiltinType(type_name)) {
      return getDiag(res->getStart())
             << "Unknown builtin type `" << type_name << "`";
    }

    return &builder_.getNamedType(type_name);
  } else if (res->isa(Token::TK_Lambda)) {
    res = lexer_.Peek();
    if (!res)
      return res;

    std::vector<const Type *> arg_types;
    while (!res->isa(Token::TK_Arrow)) {
      Result<const Type *> arg_type = ParseType(parse_callable_names);
      if (!arg_type)
        return arg_type;
      arg_types.push_back(arg_type.get());

      if (parse_callable_names) {
        // Such as in a definition.
        Consume(Token::TK_Identifier);
      }

      res = lexer_.Peek();
      if (!res)
        return res;
    }

    Consume(Token::TK_Arrow);
    Result<const Type *> res_type = ParseType(parse_callable_names);
    if (!res_type)
      return res_type;

    return &builder_.getCallableType(*res_type.get(), arg_types);
  } else if (res->isa(Token::TK_LAngleBrack)) {
    std::vector<const Type *> types;
    do {
      Result<const Type *> type = ParseType(parse_callable_names);
      if (!type)
        return type;

      types.push_back(*type);

      res = lexer_.Peek();
      if (!res)
        return res;
    } while (!res->isa(Token::TK_RAngleBrack));
    Consume(Token::TK_RAngleBrack);
    return &builder_.getCompositeType(types);
  } else if (res->isa(Token::TK_LSqBrack)) {
    auto loc = lexer_.getCurrentLoc();
    Result<Int *> num = ParseInt();
    if (!num)
      return num;

    if ((*num)->getInt() <= 0) {
      return getDiag(loc)
             << "Expected a positive integral size; instead found `"
             << (*num)->getInt() << "`";
    }

    res = lexer_.Lex();
    if (!res)
      return res;
    if (!res->isa(Token::TK_Identifier) || res->getChars() != "x") {
      return getDiag(loc) << "Expected `x` in array type; instead found `"
                          << res->getChars() << "`";
    }

    Result<const Type *> type = ParseType(parse_callable_names);
    if (!type)
      return type;

    res = lexer_.Lex();
    if (!res)
      return res;
    if (!res->isa(Token::TK_RSqBrack))
      return getDiag(loc)
             << "Expected closing `]` for array type; instead found `"
             << res->getChars() << "`";

    return &builder_.getArrayType(**type,
                                  static_cast<size_t>((*num)->getInt()));
  } else if (res->isa(Token::TK_GENERIC)) {
    return &builder_.getGenericType();
  } else if (res->isa(Token::TK_GENERIC_REMAINING)) {
    return &builder_.getGenericRemainingType();
  }

  return getDiag(res->getStart())
         << "Expected a type; instead found `" << res->getChars() << "`";
}

Result<Expr *> Parser::ParseExpr(const Type *hint) {
  SourceLocation loc = lexer_.PeekLoc();
  auto p = lexer_.Peek();
  Result<Expr *> res = ParseExprImpl(hint);
  if (!res)
    return res;

  Expr *expr = *res;

  // Check for the new call syntax <expr> "(" ... ")".
  if (const auto *callable_ty =
          llvm::dyn_cast<CallableType>(&expr->getType())) {
    Result<Token> peek = lexer_.Peek();
    if (peek && peek->isa(Token::TK_LParen)) {
      Consume(Token::TK_LParen);
      auto args_res = ParseCallArguments(Token::TK_RParen, callable_ty);
      if (!args_res)
        return args_res;
      Consume(Token::TK_RParen);

      // FIXME: We should also leverage the deduction logic in ParseCall.
      expr =
          &builder_.getCall(expr->getStart(), *expr, *args_res, /*pure=*/true);
    }
  }

  if (hint && !hint->isGeneric()) {
    if (expr->getType().isGenericCallable() && llvm::isa<CallableType>(hint)) {
      if (!llvm::cast<CallableType>(expr->getType())
               .CallableTypesMatch(llvm::cast<CallableType>(*hint))) {
        return getDiag(loc) << "Expression type mismatch; found `"
                            << expr->getType().toString() << "` but expected `"
                            << hint->toString() << "`";
      }
    } else if (*hint != expr->getType()) {
      return getDiag(loc) << "Expression type mismatch; found `"
                          << expr->getType().toString() << "` but expected `"
                          << hint->toString() << "`";
    }
  }

  return expr;
}

Result<Expr *> Parser::ParseExprImpl(const Type *hint) {
  Result<Token> res = lexer_.Peek();
  if (!res)
    return res;

  // TODO: Check the types of these if a hint is provided.

  if (res->getKind() == Token::TK_Call ||
      res->getKind() == Token::TK_ImpureCall)
    return ParseCall(hint);
  if (res->getKind() == Token::TK_Zero)
    return ParseZero(hint);
  if (res->getKind() == Token::TK_Readc)
    return ParseReadc();
  if (res->getKind() == Token::TK_Str)
    return ParseStr();
  if (res->getKind() == Token::TK_Char)
    return ParseChar();
  if (res->getKind() == Token::TK_Identifier)
    return ParseIdentifier(hint);
  if (res->getKind() == Token::TK_Let)
    return ParseLet(hint);
  if (res->getKind() == Token::TK_Keep)
    return ParseKeep(hint);
  if (res->getKind() == Token::TK_Int)
    return ParseInt();
  if (res->getKind() == Token::TK_If)
    return ParseIf(hint);
  if (res->getKind() == Token::TK_CAST)
    return ParseCast();
  if (res->getKind() == Token::TK_GET)
    return ParseGet();
  if (res->getKind() == Token::TK_SET)
    return ParseSet();
  // if (res->getKind() == Token::TK_None) return ParseNone();
  if (res->getKind() == Token::TK_LAngleBrack)
    return ParseComposite();
  if (res->getKind() == Token::TK_Lambda)
    return ParseCallable(/*name=*/nullptr, hint);
  if (res->isBinOpKind())
    return ParseBinOp(hint);

  SourceLocation expr_loc = res->getStart();

  if (res->getKind() == Token::TK_True) {
    Consume(Token::TK_True);
    return &builder_.getBool(expr_loc, true);
  }
  if (res->getKind() == Token::TK_False) {
    Consume(Token::TK_False);
    return &builder_.getBool(expr_loc, false);
  }

  return getDiag(expr_loc) << "Unable to parse expression starting with `"
                           << res->getChars() << "`";
}

// Result<const None *> Parser::ParseNone() {
//   Consume(Token::TK_None);
//   return &builder_.getNone();
// }

Result<BinOp *> Parser::ParseBinOp(const Type *hint) {
  Result<Token> res = lexer_.Lex();
  assert(res);

  BinOp::OpKind kind;
  if (res->isa(Token::TK_ADD))
    kind = BinOp::OK_Add;
  else if (res->isa(Token::TK_SUB))
    kind = BinOp::OK_Sub;
  else if (res->isa(Token::TK_LT))
    kind = BinOp::OK_Lt;
  else if (res->isa(Token::TK_GE))
    kind = BinOp::OK_Ge;
  else if (res->isa(Token::TK_EQ))
    kind = BinOp::OK_Eq;
  else if (res->isa(Token::TK_OR))
    kind = BinOp::OK_Or;
  else if (res->isa(Token::TK_MOD))
    kind = BinOp::OK_Mod;
  else
    return getDiag(res->getStart())
           << "Unknown binary operation `" << res->getChars() << "`";

  Result<Expr *> lhs = ParseExpr();
  if (!lhs)
    return lhs;

  Result<Expr *> rhs = ParseExpr();
  if (!rhs)
    return rhs;

  if ((*lhs)->getType() != (*rhs)->getType()) {
    return getDiag(res->getStart())
           << "Operands of binary operator have differing types; "
           << (*lhs)->getType().toString() << " and "
           << (*rhs)->getType().toString();
  }

  return &builder_.getBinOp(*lhs.get(), *rhs.get(), kind);
}

// <if> ::= "if" <expr> <expr> "else" <expr>
Result<If *> Parser::ParseIf(const Type *hint) {
  Result<Token> if_tok = lexer_.Lex();
  assert(if_tok->isa(Token::TK_If));

  SourceLocation if_loc = if_tok->getStart();

  Result<Token> cond_expr_tok = lexer_.Peek();

  Result<Expr *> cond = ParseExpr();
  if (!cond)
    return cond;

  const Type &type = (*cond)->getType();
  if (!type.isNamedType("bool"))
    return getDiag(cond_expr_tok->getStart())
           << "Expected `bool` type for if condition expression; instead "
              "found `"
           << type.toString() << "`";

  Result<Expr *> if_body = ParseExpr(hint);
  if (!if_body)
    return if_body;

  Result<Token> res = lexer_.Lex();
  if (!res)
    return res;
  if (!res->isa(Token::TK_Else))
    return getDiag(res->getStart())
           << "Expected `else` for if starting at " << if_tok->getStart()
           << "; instead found `" << res->getChars() << "`";

  Result<Expr *> else_body = ParseExpr(hint);
  if (!else_body)
    return else_body;

  if ((*if_body)->getType() != (*else_body)->getType()) {
    return getDiag(if_tok->getStart())
           << "Mismatch type between if and else expressions; if expression "
              "has type "
           << (*if_body)->getType().toString()
           << " but else expression has type "
           << (*else_body)->getType().toString();
  }

  if (hint) {
    if ((*if_body)->getType() != *hint) {
      return getDiag(if_tok->getStart())
             << "Mismatch type between if body and expected type; if body has "
                "type "
             << (*if_body)->getType().toString() << " but expected type "
             << hint->toString();
    }
    if ((*else_body)->getType() != *hint) {
      return getDiag(if_tok->getStart())
             << "Mismatch type between else body and expected type; else body "
                "has type "
             << (*else_body)->getType().toString() << " but expected type "
             << hint->toString();
    }
  }

  return &builder_.getIf(if_loc, *cond.get(), *if_body.get(), *else_body.get());
}

// <keep> ::= "keep" <identifier> "=" <type> <expr> <body>
//
// This is similar to a Let except the body is retained in the node itself, so a
// Keep will always be retained in the AST.
Result<Keep *> Parser::ParseKeep(const Type *hint) {
  auto keep_tok = lexer_.Peek();
  SourceLocation keep_loc = keep_tok->getStart();
  Consume(Token::TK_Keep);

  Result<Token> res = lexer_.Lex();
  if (!res)
    return res;

  if (!res->isa(Token::TK_Identifier))
    return getDiag(res->getStart())
           << "Expected an identifier but instead found `" << res->getChars()
           << "`";

  std::string_view name(res->getChars());
  if (HasVar(name)) {
    return getDiag(res->getStart())
           << "Variable name `" << name << "` is already defined prior";
  }

  Consume(Token::TK_Assign);

  Result<const Type *> type_res = ParseType();
  if (!type_res)
    return type_res;

  Result<Expr *> expr_res = ParseExpr(type_res.get());
  if (!expr_res)
    return expr_res;

  RegisterLocalVar(name, **expr_res);

  Result<Expr *> body_res = ParseExpr(hint);
  if (!body_res)
    return body_res;

  return &builder_.getKeep(keep_loc, name, **expr_res, **body_res);
}

// <let> ::= "let" <identifier> "=" <type> <expr> <body>
Result<Expr *> Parser::ParseLet(const Type *hint) {
  auto let_tok = lexer_.Peek();
  SourceLocation let_loc = let_tok->getStart();
  Consume(Token::TK_Let);

  Result<Token> res = lexer_.Lex();
  if (!res)
    return res;

  if (!res->isa(Token::TK_Identifier))
    return getDiag(res->getStart())
           << "Expected an identifier but instead found `" << res->getChars()
           << "`";

  std::string_view name(res->getChars());
  if (HasVar(name)) {
    return getDiag(res->getStart())
           << "Variable name `" << name << "` is already defined prior";
  }

  Consume(Token::TK_Assign);

  Result<const Type *> type_res = ParseType();
  if (!type_res)
    return type_res;

  Result<Expr *> expr_res = ParseExpr(type_res.get());
  if (!expr_res)
    return expr_res;

  Let &let = builder_.getLet(let_loc, name, *expr_res.get());
  RegisterLocalVar(name, let);

  Result<Expr *> body_res = ParseExpr(hint);
  return body_res;
}

Result<Parser::CallableResults> Parser::ParseCallableFromArgs(
    const SourceLocation &call_loc, std::string_view name,
    Token::TokenKind end_tok, const Type *return_type_hint) {
  auto args = ParseCallArguments(end_tok);
  if (!args)
    return args;

  std::vector<const Type *> arg_types(args->size());
  std::transform(args->begin(), args->end(), arg_types.begin(),
                 [](const Expr *expr) { return &expr->getType(); });

  std::vector<Expr *> possible_callables =
      getPossibleCallables(name, &arg_types);

  if (return_type_hint) {
    std::vector<Expr *> tmp;
    std::copy_if(possible_callables.begin(), possible_callables.end(),
                 std::back_inserter(tmp), [return_type_hint](const Expr *e) {
                   return e->getType().getReturnType() == *return_type_hint;
                 });
    possible_callables = tmp;
  }

  if (possible_callables.empty()) {
    auto diag(getDiag(call_loc));
    diag << "Could not find callable `" << name << "` with arg types `";
    for (const Type *t : arg_types)
      diag << t->toString() << " ";
    diag << "`";
    return diag;
  }

  if (possible_callables.size() > 1) {
    if (AmbiguousCall::CanMake(possible_callables))
      return CallableResults{possible_callables, *args};

    return getAmbiguousCallDiag<CallableResults>(call_loc, name,
                                                 possible_callables);
  }

  return CallableResults{possible_callables, *args};
}

Result<Expr *> Parser::ParseIdentifier(const Type *hint) {
  Result<Token> res = lexer_.Lex();
  assert(res && res->getKind() == Token::TK_Identifier);

  std::string_view name(res->getChars());
  if (!HasVar(name))
    return getDiag(res->getStart()) << "Unknown variable `" << name << "`";

  if (const auto *callable_ty = llvm::dyn_cast_or_null<CallableType>(hint)) {
    if (HasVar(name, *callable_ty))
      return &LookupCallable(name, *callable_ty);
  }

  // Handle a potential call.
  const std::vector<const Type *> *maybe_args = nullptr;
  if (const auto *callable_ty = llvm::dyn_cast_or_null<CallableType>(hint))
    maybe_args = &callable_ty->getArgTypes();
  auto possible_callables = getPossibleCallables(name, maybe_args);
  if (possible_callables.size() > 1) {
    if (lexer_.Peek()->isa(Token::TK_LParen)) {
      // Handle the new call syntax.
      Consume(Token::TK_LParen);
      auto maybe_callable_and_args =
          ParseCallableFromArgs(res->getStart(), name, Token::TK_RParen, hint);
      if (!maybe_callable_and_args)
        return maybe_callable_and_args;
      Consume(Token::TK_RParen);

      auto &callables = maybe_callable_and_args->possible_callables;
      auto &args = maybe_callable_and_args->args;
      assert(!callables.empty());
      if (callables.size() == 1) {
        return getAndCheckCall(res->getStart(), *callables.front(), args,
                               /*pure=*/false, hint);
      } else {
        return &builder_.getAmbiguousCall(res->getStart(), callables, args);
      }
    } else {
      // Handle the old call syntax.
      return getAmbiguousCallDiag<Expr *>(res->getStart(), name,
                                          possible_callables);
    }
  }

  return &LookupSingleExpr(name);
}

// NOTE: Callers of this are in charge of consuming the ending tokens after a
// successful call to this.
Result<const std::vector<Expr *>> Parser::ParseCallArguments(
    Token::TokenKind end_tok, const CallableType *callable_type) {
  Result<Token> res = lexer_.Peek();
  if (!res)
    return res;

  SourceLocation current = lexer_.getCurrentLoc();
  size_t arg_no = 0;

  std::vector<Expr *> args;
  while (res->getKind() != end_tok) {
    if (callable_type && arg_no >= callable_type->getNumArgs()) {
      return getDiag(lexer_.getCurrentLoc())
             << "Call exceeds expected number of arguments; expected "
             << callable_type->getNumArgs() << " args for callable type "
             << callable_type->toString() << " but instead found `"
             << res->getChars() << "`";
    }

    // Parse an expression as an argument.
    Result<Expr *> arg =
        ParseExpr(callable_type ? &callable_type->getArgType(arg_no) : nullptr);
    if (!arg)
      return arg;
    args.push_back(arg.get());

    res = lexer_.Peek();
    if (!res)
      return res;

    arg_no++;
  }

  if (callable_type) {
    std::vector<const Type *> arg_types(args.size());
    std::transform(args.begin(), args.end(), arg_types.begin(),
                   [](const Expr *e) { return &e->getType(); });
    if (!callable_type->ArgumentTypesMatch(arg_types)) {
      Diagnostic diag(getDiag(current));
      diag << "Cannot call function of type `" << callable_type->toString()
           << "` with arg types `";
      for (const Type *t : arg_types)
        diag << t->toString() << " ";
      diag << "`";
      return diag;
    }
  }

  return args;
}

Result<Expr *> Parser::ParseCall(const Type *return_type_hint) {
  Result<Token> tok = lexer_.Lex();
  assert(tok && (tok->isa(Token::TK_Call) || tok->isa(Token::TK_ImpureCall)));

  SourceLocation call_loc = tok->getStart();
  bool pure = tok->isa(Token::TK_Call);

  Result<Token> res = lexer_.Peek();
  if (!res)
    return res;

  bool is_identifier = res->isa(Token::TK_Identifier);

  // Only continue to deduce the calling type if the number of possible
  // callables is 1 since we already know the only possible type it can be.
  bool should_deduce_call_from_args =
      is_identifier && getNumPossibleCallables(res->getChars()) > 1;

  Expr *callable;
  std::vector<Expr *> args;
  if (should_deduce_call_from_args) {
    // We have multiple possible function overloads we can call, so attempt to
    // find the right one from the arguments.
    std::string func_name(res->getChars());
    Consume(Token::TK_Identifier);

    auto maybe_callable = ParseCallableFromArgs(
        res->getStart(), func_name, Token::TK_End, return_type_hint);
    if (!maybe_callable)
      return maybe_callable;

    auto &callables = maybe_callable->possible_callables;
    args = maybe_callable->args;
    if (callables.size() > 1) {
      // TODO: Clean this up and consolidate with `getAndCheckCall`.
      AmbiguousCall &call =
          builder_.getAmbiguousCall(call_loc, callables, args);
      Consume(Token::TK_End);
      return &call;
    }

    callable = callables.front();
  } else {
    Result<Expr *> callable_res = ParseExpr();
    if (!callable_res)
      return callable_res;

    callable = *callable_res;
    const Type &type = callable->getType();
    if (!llvm::isa<CallableType>(type)) {
      return getDiag(callable->getStart())
             << "Expected callable expression to be a callable type; instead "
                "found "
             << type.toString();
    }

    auto args_res =
        ParseCallArguments(Token::TK_End, &llvm::cast<CallableType>(type));
    if (!args_res)
      return args_res;
    args = *args_res;
  }
  Consume(Token::TK_End);

  return getAndCheckCall(call_loc, *callable, args, pure, return_type_hint);
}

Result<Call *> Parser::getAndCheckCall(const SourceLocation &callable_loc,
                                       Expr &maybe_callable,
                                       const std::vector<Expr *> &args,
                                       bool pure,
                                       const Type *return_type_hint) {
  const auto *actual_ty =
      llvm::dyn_cast<CallableType>(&maybe_callable.getType());
  if (!actual_ty)
    return getExpectedCallableDiag(callable_loc, maybe_callable.getType());

  if (return_type_hint && actual_ty->getReturnType() != *return_type_hint) {
    return getDiag(callable_loc)
           << "Return type mismatch for call; found "
           << actual_ty->getReturnType().toString() << " but expected "
           << return_type_hint->toString();
  }

  if (actual_ty->getNumArgs() != args.size() &&
      !actual_ty->isGenericRemainingCallable()) {
    return getDiag(callable_loc)
           << "Mismatch between number of arguments; found " << args.size()
           << " but expected " << actual_ty->getNumArgs();
  } else if (actual_ty->isGenericRemainingCallable() &&
             args.size() < actual_ty->getNumArgs()) {
    return getDiag(callable_loc)
           << "Not enough arguments for callable; found " << args.size()
           << " but expected at least " << actual_ty->getNumArgs();
  }

  for (size_t i = 0; i < actual_ty->getNumArgs(); ++i) {
    if (!actual_ty->getArgType(i).isGeneric() &&
        actual_ty->getArgType(i) != args.at(i)->getType()) {
      return getDiag(callable_loc)
             << "Type mismatch for argument " << i << "; found "
             << args.at(i)->getType().toString() << " but expected "
             << actual_ty->getArgType(i).toString();
    }
  }

  return &builder_.getCall(callable_loc, maybe_callable, args, pure);
}

Result<Int *> Parser::ParseInt() {
  Result<Token> res = lexer_.Lex();
  assert(res && res->isa(Token::TK_Int));
  return &builder_.getInt(res->getStart(),
                          std::stoi(std::string(res->getChars())));
}

Result<Char *> Parser::ParseChar() {
  Result<Token> res = lexer_.Lex();
  assert(res && res->isa(Token::TK_Char));
  assert(res->getChars().size() == 3 && res->getChars().front() == '\'' &&
         res->getChars().back() == '\'');

  return &builder_.getChar(res->getStart(), res->getChars().at(1));
}

Result<Str *> Parser::ParseStr() {
  Result<Token> res = lexer_.Lex();
  assert(res && res->isa(Token::TK_Str));

  std::string_view chars(res->getChars());
  assert(chars.front() == '"' && chars.back() == '"');
  assert(chars.size() >= 2);

  return &builder_.getStr(res->getStart(), chars.substr(1, chars.size() - 2));
}

Result<Readc *> Parser::ParseReadc() {
  SourceLocation loc = lexer_.Peek()->getStart();
  Consume(Token::TK_Readc);
  return &builder_.getReadc(loc);
}

Result<Zero *> Parser::ParseZero(const Type *hint) {
  if (!hint) {
    return getDiag(lexer_.getCurrentLoc())
           << "Unable to determine type of `zero`";
  }
  SourceLocation loc = lexer_.Peek()->getStart();
  Consume(Token::TK_Zero);
  return &builder_.getZero(loc, *hint);
}

// <composite> ::= "<" <expr>+ ">"
Result<Composite *> Parser::ParseComposite() {
  SourceLocation loc = lexer_.Peek()->getStart();
  Consume(Token::TK_LAngleBrack);

  std::vector<Expr *> elems;
  Result<Token> next = lexer_.Peek();
  if (!next)
    return next;
  do {
    Result<Expr *> expr = ParseExpr();
    if (!expr)
      return expr;

    elems.push_back(*expr);

    next = lexer_.Peek();
    if (!next)
      return next;
  } while (!next->isa(Token::TK_RAngleBrack));

  Consume(Token::TK_RAngleBrack);

  return &builder_.getComposite(loc, elems);
}

// <set> ::= "SET" <expr> <expr> <expr>
//
// The first <expr> is the composite type being accessed. The second <expr> is
// the index. The third <expr> is the value being stored.
Result<Set *> Parser::ParseSet() {
  SourceLocation loc = lexer_.Peek()->getStart();
  Consume(Token::TK_SET);

  auto peek = lexer_.Peek();
  if (!peek)
    return peek;
  SourceLocation composite_loc = peek->getStart();
  Result<Expr *> composite = ParseExpr();
  if (!composite)
    return composite;

  const Type &type = (*composite)->getType();
  if (!(llvm::isa<CompositeType>(type) || llvm::isa<ArrayType>(type))) {
    return getDiag(composite_loc)
           << "Expression is not a composite or array type";
  }

  Result<Expr *> idx = ParseExpr();
  if (!idx)
    return idx;

  peek = lexer_.Peek();
  if (!peek)
    return peek;
  SourceLocation idx_loc = peek->getStart();
  if (!(*idx)->getType().isNamedType("int")) {
    return getDiag(idx_loc)
           << "Expression for index is not type `int`; instead is `"
           << (*idx)->getType().toString() << "`";
  }

  Result<Expr *> store_val = ParseExpr();
  if (!store_val)
    return store_val;

  return &builder_.getSet(loc, **composite, **idx, **store_val);
}

// <get> ::= "GET" <type> <expr> <expr>
//
// The <type> is the resulting type of the GET expression. The first <expr> is
// the composite type that is being accessed. The second <expr> is the index.
Result<Get *> Parser::ParseGet() {
  SourceLocation loc = lexer_.Peek()->getStart();
  Consume(Token::TK_GET);

  Result<Token> peek = lexer_.Peek();
  if (!peek)
    return peek;
  SourceLocation typeloc = peek->getStart();
  Result<const Type *> type_res = ParseType();
  if (!type_res)
    return type_res;

  const Type &type = **type_res;

  peek = lexer_.Peek();
  if (!peek)
    return peek;
  SourceLocation exprloc = peek->getStart();
  Result<Expr *> expr = ParseExpr();
  if (!expr)
    return expr;
  const Type &expr_type = (*expr)->getType();
  if (!(llvm::isa<CompositeType>(expr_type) ||
        llvm::isa<ArrayType>(expr_type))) {
    return getDiag(exprloc) << "Expression is not a composite or array type";
  }

  Result<Token> i = lexer_.Peek();
  if (!i)
    return i;

  if (i->isa(Token::TK_Int)) {
    const Type *result_type;
    size_t idx = std::stoi(std::string(i->getChars()));
    if (const auto *comp_ty = llvm::dyn_cast<CompositeType>(&expr_type)) {
      if (idx >= comp_ty->getNumTypes()) {
        return getDiag(i->getStart())
               << "Index " << idx << " exceeds size of composite type which is "
               << comp_ty->getNumTypes();
      }
      result_type = &comp_ty->getTypeAt(idx);
    } else if (const auto *arr_ty = llvm::dyn_cast<ArrayType>(&expr_type)) {
      if (idx >= arr_ty->getNumElems()) {
        return getDiag(i->getStart())
               << "Index " << idx << " exceeds size of array type which is "
               << arr_ty->getNumElems();
      }
      result_type = &arr_ty->getElemType();
    } else {
      __builtin_trap();
    }

    if (type != *result_type) {
      return getDiag(typeloc)
             << "GET type mismatch; expected `" << type.toString()
             << "` but found `" << result_type->toString() << "`";
    }
  }

  Result<Expr *> idx = ParseExpr();
  if (!idx)
    return idx;
  if (!(*idx)->getType().isNamedType("int")) {
    return getDiag(i->getStart())
           << "Expression for index is not type `int`; instead is `"
           << (*idx)->getType().toString() << "`";
  }

  return &builder_.getGet(loc, type, **expr, **idx);
}

// <cast> ::= "CAST" <type> <expr>
Result<Cast *> Parser::ParseCast() {
  SourceLocation loc = lexer_.Peek()->getStart();
  Consume(Token::TK_CAST);
  Result<const Type *> type = ParseType();
  if (!type)
    return type;

  Result<Expr *> expr = ParseExpr();
  if (!expr)
    return expr;

  // Do some type checking.
  if ((*type)->isCharArray() && (*expr)->getType().isCharArray()) {
    const auto &to_type = llvm::cast<ArrayType>(**type);
    const auto &from_type = llvm::cast<ArrayType>((*expr)->getType());
    if (to_type.getNumElems() < from_type.getNumElems()) {
      // TODO: Would be nice to have a formal warning system also that doesn't
      // involve making an error result.
      Diagnostic warn(lexer_.getInput(), loc);
      warn << "Casting from a longer " << from_type.getNumElems()
           << " length char array type to a shorter " << to_type.getNumElems()
           << " length char array type truncates result";
      std::cerr << warn.get() << std::endl;
    }
  }

  if (**type == (*expr)->getType()) {
    Diagnostic warn(lexer_.getInput(), loc);
    warn << "Unnecessary cast here since types are the same";
    std::cerr << warn.get() << std::endl;
  }

  return &builder_.getCast(loc, **type, **expr);
}

}  // namespace lang
