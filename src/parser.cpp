#include "parser.h"

#include <iostream>
#include <string>
#include <string_view>
#include <vector>

#include "ast.h"
#include "astbuilder.h"
#include "astdumper.h"
#include "lang.h"
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
        return getErrorDiag()
               << next->getStart() << ": Unknown top level entity `"
               << next->getChars() << "`; "
               << "expected either `def` or `decl`"
               << DumpLine{next->getStart()};
    }();

    if (!top_level_entity)
      return top_level_entity;

    next = lexer_.Peek();
    if (!next)
      return next;
  }

  return &module_;
}

//
// <def> ::= "def" <identifier> "=" <expr>
//
Result<Declare *> Parser::ParseDefineImpl() {
  SourceLocation def_loc = lexer_.PeekLoc();
  Consume(Token::TK_Def);

  Result<Token> res = lexer_.Lex();
  if (!res)
    return res;

  if (res->getKind() != Token::TK_Identifier)
    return getErrorDiag() << res->getStart()
                          << "Expected an identifier; instead found `"
                          << res->getChars() << "`"
                          << DumpLine{res->getStart()};

  std::string name(res->getChars());

  res = lexer_.Lex();
  if (!res)
    return res;

  if (res->getKind() != Token::TK_Assign)
    return getErrorDiag() << res->getStart() << "Expected `=`; instead found `"
                          << res->getChars() << "`"
                          << DumpLine{res->getStart()};

  if (!lexer_.Peek()) {
    return getErrorDiag() << res->getStart()
                          << "Missing definition body after `=`"
                          << DumpLine{res->getStart()};
  }

  bool parsing_callable = lexer_.Peek()->isa(Token::TK_Lambda);

  Expr *body;
  if (parsing_callable) {
    // This definition is a lambda. We can attempt to get the type early by just
    // parsing the callable head. This allows us to build a decl which can be
    // used for handling callables which reference themselves.
    Result<Callable *> callable_head_res = ParseCallableHead();
    if (!callable_head_res)
      return callable_head_res;

    body = *callable_head_res;
  } else {
    // This is not a new callable. It may be a non-callable or a reference to
    // another expression which may be an identifier. Either way, we don't know
    // the type yet so we'll need to parse the expression before actually making
    // a decl.
    Result<Expr *> expr_res = ParseExpr();
    if (!expr_res)
      return expr_res;

    body = *expr_res;
  }

  auto decls = module_.getDeclares(name);
  Declare *decl;
  // This expr can correspond to more than one existing declaration. Find the
  // right one.
  auto found = std::find_if(
      decls.begin(), decls.end(),
      [&body](const Declare *d) { return d->getType() == body->getType(); });
  if (found == decls.end()) {
    // This is a new definition. Let's assert someone calling this might not
    // be ambiguous with calling any other function.
    auto ambiguous_callable =
        std::find_if(decls.begin(), decls.end(), [&](const Declare *d) {
          return body->getType().Matches(d->getType());
        });
    if (ambiguous_callable != decls.end()) {
      Diagnostic diag(getErrorDiag());
      diag << body->getStart() << ": Callable `" << name << "` with type `"
           << body->getType().toString() << "` is handled by another callable"
           << DumpLine{body->getStart()} << "\n"
           << (**ambiguous_callable).getStart() << ": note: Declared here"
           << DumpLine{(**ambiguous_callable).getStart()};
      return diag;
    }
    decl = &builder_.getDeclare(def_loc, name, *body, /*is_write=*/false,
                                /*is_cdecl=*/false);
    module_.AddDeclaration(name, *decl);
  } else {
    // Found an existing one.
    assert(found != decls.end());
    (*found)->setBody(*body);
    decl = *found;
  }

  if (parsing_callable) {
    // Finish the rest of the body.
    Callable &callable = llvm::cast<Callable>(*body);
    Result<Callable *> callable_res = ParseCallableBody(callable);
    if (!callable_res)
      return callable_res;
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
    return getErrorDiag() << tok->getStart()
                          << ": Expected an identifier; instead found `"
                          << tok->getChars() << "`"
                          << DumpLine{tok->getStart()};

  std::string name(tok->getChars());

  // Consume `=`.
  tok = lexer_.Lex();
  if (!tok)
    return tok;
  if (tok->getKind() != Token::TK_Assign)
    return getErrorDiag() << tok->getStart()
                          << ": Expected `=`; instead found `"
                          << tok->getChars() << "`"
                          << DumpLine{tok->getStart()};

  Result<const Type *> type = ParseType();
  if (!type)
    return type;

  Declare &decl =
      builder_.getDeclare(decl_loc, name, **type, name == "write", cdecl);
  module_.AddDeclaration(name, decl);
  return &decl;
}

Result<Callable *> Parser::ParseCallableHead() {
  SourceLocation callable_loc = lexer_.PeekLoc();

  Consume(Token::TK_Lambda);

  // Maybe parse arguments.
  Result<Token> res = lexer_.Peek();
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
      return getErrorDiag()
             << res->getStart() << "Expected an argument name; instead found `"
             << res->getChars() << "`" << DumpLine{res->getStart()};

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
  Result<const Type *> type_res = ParseType();
  if (!type_res)
    return type_res;
  const Type &ret_type = **type_res;

  Callable &callable = builder_.getCallable(callable_loc, ret_type, arg_locs,
                                            arg_names, arg_types);
  return &callable;
}

Result<Callable *> Parser::ParseCallableBody(Callable &callable) {
  // Register the variable names.
  for (size_t i = 0; i < callable.getNumArgs(); ++i)
    RegisterLocalVar(callable.getArgName(i), callable.getArg(i));

  // Parse the body which is just an expression.
  Result<Expr *> expr_res = ParseExpr(&callable.getType().getReturnType());
  if (!expr_res)
    return expr_res;
  callable.setBody(**expr_res);

  // Check the return type.
  if (!callable.getType().getReturnType().Matches(
          callable.getBody().getType())) {
    return getErrorDiag()
           << callable.getStart()
           << ": Mismatch between callable return type and body return type; "
              "expected `"
           << callable.getType().getReturnType().toString()
           << "` but instead found `" << callable.getBody().getType().toString()
           << "`" << DumpLine{callable.getStart()};
  }

  return &callable;
}

// <callable> ::= "\" <arg list> "->" <type> <expr>
Result<Callable *> Parser::ParseCallable() {
  Result<Callable *> callable_head = ParseCallableHead();
  if (!callable_head)
    return callable_head;

  return ParseCallableBody(**callable_head);
}

// <type> ::= ( "mut" )? (<namedtype> | <callabletype> | <compositetype> |
//                        <arraytype> | <generictype> | <genericremainingtype>)
//   <namedtype>              ::= <identifier>
//   <callabletype>           ::= "\" <type>* "->" <type>
//   <compositetype>          ::= "<" <type>+ ">"
//   <arraytype>              ::= "[" \d+ "x" <type> "]"
//   <generictype>            ::= "GENERIC"
//   <genericremainingtype>   ::= "GENERIC_REMAINING"
Result<const Type *> Parser::ParseType() {
  Result<Token> res = lexer_.Lex();
  if (!res)
    return res;

  bool mut = res->isa(Token::TK_Mut);
  if (mut) {
    res = lexer_.Lex();
    if (!res)
      return res;
  }

  if (res->isa(Token::TK_Identifier)) {
    std::string_view type_name(res->getChars());

    // TODO: Check custom types here eventually.
    if (!IsBuiltinType(type_name)) {
      return getErrorDiag() << res->getStart() << ": Unknown builtin type `"
                            << type_name << "`" << DumpLine{res->getStart()};
    }

    return &builder_.getNamedType(type_name);
  } else if (res->isa(Token::TK_Lambda)) {
    res = lexer_.Peek();
    if (!res)
      return res;

    std::vector<const Type *> arg_types;
    while (!res->isa(Token::TK_Arrow)) {
      Result<const Type *> arg_type = ParseType();
      if (!arg_type)
        return arg_type;
      arg_types.push_back(arg_type.get());

      res = lexer_.Peek();
      if (!res)
        return res;
    }

    Consume(Token::TK_Arrow);
    Result<const Type *> res_type = ParseType();
    if (!res_type)
      return res_type;

    return &builder_.getCallableType(*res_type.get(), arg_types);
  } else if (res->isa(Token::TK_LAngleBrack)) {
    std::vector<const Type *> types;
    do {
      Result<const Type *> type = ParseType();
      if (!type)
        return type;

      types.push_back(*type);

      res = lexer_.Peek();
      if (!res)
        return res;
    } while (!res->isa(Token::TK_RAngleBrack));
    Consume(Token::TK_RAngleBrack);
    return &builder_.getCompositeType(types, mut);
  } else if (res->isa(Token::TK_LSqBrack)) {
    auto loc = lexer_.getCurrentLoc();
    Result<Int *> num = ParseInt();
    if (!num)
      return num;

    if ((*num)->getInt() <= 0) {
      return getErrorDiag()
             << loc << ": Expected a positive integral size; instead found `"
             << (*num)->getInt() << "`" << DumpLine{loc};
    }

    res = lexer_.Lex();
    if (!res)
      return res;
    if (!res->isa(Token::TK_Identifier) || res->getChars() != "x") {
      return getErrorDiag()
             << loc << ": Expected `x` in array type; instead found `"
             << res->getChars() << "`" << DumpLine{loc};
    }

    Result<const Type *> type = ParseType();
    if (!type)
      return type;

    res = lexer_.Lex();
    if (!res)
      return res;
    if (!res->isa(Token::TK_RSqBrack))
      return getErrorDiag()
             << loc << ": Expected closing `]` for array type; instead found `"
             << res->getChars() << "`" << DumpLine{loc};

    return &builder_.getArrayType(**type, static_cast<size_t>((*num)->getInt()),
                                  mut);
  } else if (res->isa(Token::TK_GENERIC)) {
    return &builder_.getGenericType(mut);
  } else if (res->isa(Token::TK_GENERIC_REMAINING)) {
    return &builder_.getGenericRemainingType();
  }

  return getErrorDiag() << res->getStart()
                        << ": Expected a type; instead found `"
                        << res->getChars() << "`" << DumpLine{res->getStart()};
}

Result<Expr *> Parser::ParseAltSyntaxes(Expr &expr) {
  Result<Token> peek = lexer_.Peek();
  if (!peek)
    return &expr;

  const Type &type = expr.getType();

  // Check for the new call syntax <expr> "(" ... ")".
  if (const auto *callable_ty = llvm::dyn_cast<CallableType>(&type)) {
    if (peek->isa(Token::TK_LParen)) {
      Consume(Token::TK_LParen);
      auto args_res = ParseCallArguments(Token::TK_RParen, callable_ty);
      if (!args_res)
        return args_res;
      Consume(Token::TK_RParen);

      // FIXME: We should also leverage the deduction logic in ParseCall.
      Expr &altexpr =
          builder_.getCall(expr.getStart(), expr, *args_res, /*pure=*/true);
      return ParseAltSyntaxes(altexpr);
    }
  }

  // Check for the alt GET/SET syntax <expr> "[" ... "]".
  if (type.isAggregateType() && peek->isa(Token::TK_LSqBrack)) {
    Consume(Token::TK_LSqBrack);
    Result<Expr *> idx_res = ParseAggregateIndexExpr(type, expr.getStart());
    if (!idx_res)
      return idx_res;
    Expr &idx = **idx_res;
    Consume(Token::TK_RSqBrack);

    // Check for a following ":" for the SET syntax.
    peek = lexer_.Peek();
    if (!peek)
      return peek;

    if (peek->isa(Token::TK_Colon)) {
      // Handle a SET.
      Consume(Token::TK_Colon);

      Result<Expr *> store_val = ParseExpr();
      if (!store_val)
        return store_val;

      // TODO: If the idx is a constant integer we can determine now, let's
      // maybe check that the corresponding type at the idx matches the type of
      // the store value.

      Set &set = builder_.getSet(expr.getStart(), expr, idx, **store_val);
      return ParseAltSyntaxes(set);
    } else {
      // Handle a GET.
      const Type &result_type = getGetResultType(type, idx);
      Get &get = builder_.getGet(expr.getStart(), result_type, expr, idx);
      return ParseAltSyntaxes(get);
    }
  }

  // Check for the method syntax <expr> "." <identifier> "(" ... ")".
  //
  // TODO: Once we support structs with named members, we should update this to
  // not always expect calls.
  if (peek->isa(Token::TK_Dot)) {
    // Handle the method syntax.
    Consume(Token::TK_Dot);

    // Parse the method name.
    Result<Token> res = lexer_.Lex();
    if (!res)
      return res;
    if (!res->isa(Token::TK_Identifier)) {
      return getErrorDiag()
             << res->getStart()
             << ": Expected an identifier for the method name; instead found `"
             << res->getChars() << "`" << DumpLine{res->getStart()};
    }

    // Get the first identifier as an expression.
    std::vector<Expr *> parsed_args;
    parsed_args.push_back(&expr);

    // Falthrough to the alt call syntax.
    std::string name(res->getChars());

    res = lexer_.Lex();
    if (!res)
      return res;
    if (!res->isa(Token::TK_LParen)) {
      return getErrorDiag()
             << res->getStart()
             << ": Expected '(' to indicate the start of a call; instead found "
                "`"
             << res->getChars() << "`" << DumpLine{res->getStart()};
    }

    // Handle the new call syntax.
    Result<Expr *> call =
        ParseCallFromArgs(expr.getStart(), name, parsed_args, Token::TK_RParen);
    if (!call)
      return call;

    return ParseAltSyntaxes(**call);
  }

  return &expr;
}

Result<Expr *> Parser::ParseExpr(const Type *hint) {
  SourceLocation loc = lexer_.PeekLoc();
  auto p = lexer_.Peek();
  Result<Expr *> res = ParseExprImpl(hint);
  if (!res)
    return res;

  Result<Expr *> maybe_alt_expr = ParseAltSyntaxes(**res);
  if (!maybe_alt_expr)
    return maybe_alt_expr;
  Expr &expr = **maybe_alt_expr;

  if (hint && !hint->isGeneric() && !hint->CanConvertFrom(expr)) {
    return getErrorDiag() << loc << ": Expression type mismatch; found `"
                          << expr.getType().toString() << "` but expected `"
                          << hint->toString() << "`" << DumpLine{loc};
  }

  return &expr;
}

Result<Expr *> Parser::ParseExprImpl(const Type *hint) {
  Result<Token> res = lexer_.Peek();
  if (!res)
    return res;

  if (res->getKind() == Token::TK_Call ||
      res->getKind() == Token::TK_ImpureCall)
    return ParseCall(hint);
  if (res->getKind() == Token::TK_Zero)
    return ParseZero();
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
  if (res->getKind() == Token::TK_LAngleBrack)
    return ParseComposite();
  if (res->getKind() == Token::TK_Lambda)
    return ParseCallable();
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

  return getErrorDiag() << expr_loc
                        << ": Unable to parse expression starting with `"
                        << res->getChars() << "`" << DumpLine{expr_loc};
}

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
    return getErrorDiag() << res->getStart() << "Unknown binary operation `"
                          << res->getChars() << "`";

  Result<Expr *> lhs = ParseExpr();
  if (!lhs)
    return lhs;

  Result<Expr *> rhs = ParseExpr();
  if (!rhs)
    return rhs;

  if ((*lhs)->getType() != (*rhs)->getType()) {
    return getErrorDiag()
           << res->getStart()
           << ": Operands of binary operator have differing types; "
           << (*lhs)->getType().toString() << " and "
           << (*rhs)->getType().toString() << DumpLine{res->getStart()};
  }

  return &builder_.getBinOp(*lhs.get(), *rhs.get(), kind);
}

//
// <if> ::= "if" <expr> <expr> "else" <expr>
//
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
    return getErrorDiag()
           << cond_expr_tok->getStart()
           << ": Expected `bool` type for if condition expression; instead "
              "found `"
           << type.toString() << "`" << DumpLine{cond_expr_tok->getStart()};

  Result<Expr *> if_body = ParseExpr(hint);
  if (!if_body)
    return if_body;
  const Type &if_type = (*if_body)->getType();

  Result<Token> res = lexer_.Lex();
  if (!res)
    return res;
  if (!res->isa(Token::TK_Else))
    return getErrorDiag() << res->getStart()
                          << ": Expected `else` for if starting at "
                          << if_tok->getStart() << "; instead found `"
                          << res->getChars() << "`"
                          << DumpLine{res->getStart()};

  Result<Expr *> else_body = ParseExpr(hint);
  if (!else_body)
    return else_body;
  const Type &else_type = (*else_body)->getType();

  // Ignore becuase even if only one of the branches is immutable, then the
  // return type can be immutable. Additionally, if the return type of whatever
  // function this is in is immutable, then we'll check against that.
  if (!if_type.Equals(else_type, Type::QualifierCmp::Ignore)) {
    return getErrorDiag()
           << if_tok->getStart()
           << ": Mismatch type between if and else expressions; if expression "
              "has type "
           << if_type.toString() << DumpLine{(**if_body).getStart()} << "\n"
           << "but else expression has type " << else_type.toString()
           << DumpLine{(**else_body).getStart()};
  }

  if (hint) {
    if (!hint->CanConvertFrom(if_type)) {
      return getErrorDiag()
             << if_tok->getStart()
             << ": Mismatch type between if body and expected "
                "type; if body has "
                "type "
             << if_type.toString() << " but expected type " << hint->toString();
    }
    if (!hint->CanConvertFrom(else_type)) {
      return getErrorDiag() << if_tok->getStart()
                            << ": Mismatch type between else body and expected "
                               "type; else body "
                               "has type "
                            << else_type.toString() << " but expected type "
                            << hint->toString();
    }
  }

  // Get the immutable type if there is one.
  //
  // Note that if the common type is a composite or array (such as
  // `<[128xchar]>` vs `<mut [128xchar]>`) then we can't just compare outer
  // mutablity. So let's take the one that can't be converted to the other.
  const Type &res_ty = [&if_type, &else_type]() -> const Type & {
    // This check only fails if `else_type` has some nested immutability.
    if (if_type.CanConvertFrom(else_type))
      return if_type;
    return else_type;
  }();

  return &builder_.getIf(if_loc, res_ty, *cond.get(), **if_body, **else_body);
}

//
// <keep> ::= "keep" <identifier> "=" <expr> <body>
//
// This is similar to a Let except the body is retained in the node itself, so a
// Keep will always be retained in the AST.
//
Result<Keep *> Parser::ParseKeep(const Type *hint) {
  auto keep_tok = lexer_.Peek();
  SourceLocation keep_loc = keep_tok->getStart();
  Consume(Token::TK_Keep);

  Result<Token> res = lexer_.Lex();
  if (!res)
    return res;

  if (!res->isa(Token::TK_Identifier))
    return getErrorDiag() << res->getStart()
                          << ": Expected an identifier but instead found `"
                          << res->getChars() << "`"
                          << DumpLine{res->getStart()};

  std::string_view name(res->getChars());
  if (HasVar(name)) {
    return getErrorDiag() << res->getStart() << ": Variable name `" << name
                          << "` is already defined prior"
                          << DumpLine{res->getStart()};
  }

  Consume(Token::TK_Assign);

  Result<Expr *> expr_res = ParseExpr();
  if (!expr_res)
    return expr_res;

  RegisterLocalVar(name, **expr_res);

  Result<Expr *> body_res = ParseExpr(hint);
  if (!body_res)
    return body_res;

  return &builder_.getKeep(keep_loc, name, **expr_res, **body_res);
}

//
// <let> ::= "let" <identifier> "=" <expr> <body>
//
Result<Expr *> Parser::ParseLet(const Type *hint) {
  SourceLocation let_loc = lexer_.PeekLoc();
  Consume(Token::TK_Let);

  Result<Token> res = lexer_.Lex();
  if (!res)
    return res;

  if (!res->isa(Token::TK_Identifier))
    return getExpectedIdentifierDiag(*res);

  std::string_view name(res->getChars());
  if (HasVar(name)) {
    return getErrorDiag() << res->getStart() << ": Variable name `" << name
                          << "` is already defined prior"
                          << DumpLine{res->getStart()};
  }

  Result<Token> peek = lexer_.Peek();
  if (!peek)
    return peek;

  bool comp_unpack = peek->isa(Token::TK_Comma);
  std::vector<std::string> unpack_ids;
  std::vector<SourceLocation> id_locs;
  if (comp_unpack) {
    unpack_ids.emplace_back(name);
    id_locs.push_back(res->getStart());
    Consume(Token::TK_Comma);
    peek = lexer_.Peek();

    while (peek && !peek->isa(Token::TK_Assign)) {
      // Expect an identifier.
      if (!peek->isa(Token::TK_Identifier))
        return getExpectedIdentifierDiag(*peek);
      unpack_ids.emplace_back(peek->getChars());
      id_locs.push_back(peek->getStart());
      Consume(Token::TK_Identifier);

      peek = lexer_.Peek();
      if (!peek)
        return peek;
      if (peek->isa(Token::TK_Comma)) {
        Consume(Token::TK_Comma);
        peek = lexer_.Peek();
      }
    }

    if (!peek)
      return peek;
  } else if (!peek->isa(Token::TK_Assign)) {
    return getErrorDiag() << peek->getStart()
                          << ": Expected an assignment `=`; instead found `"
                          << peek->getChars() << "`";
  }

  Consume(Token::TK_Assign);

  Result<Expr *> expr_res = ParseExpr();
  if (!expr_res)
    return expr_res;
  Expr &expr = **expr_res;

  if (comp_unpack) {
    // TODO: It would probably be useful to have this for arrays also.
    const auto *comp_ty = llvm::dyn_cast<CompositeType>(&expr.getType());
    if (!comp_ty) {
      return getErrorDiag()
             << expr.getStart()
             << ": Expected a composite type for unpacking; instead found `"
             << expr.getType().toString() << "`" << DumpLine{expr.getStart()};
    }

    if (comp_ty->getNumTypes() != unpack_ids.size()) {
      return getErrorDiag()
             << let_loc << ": Composite type has " << comp_ty->getNumTypes()
             << " to unpack; instead found " << unpack_ids.size()
             << " identifiers" << DumpLine{let_loc};
    }

    for (size_t i = 0; i < unpack_ids.size(); ++i) {
      const Type &type = comp_ty->getTypeAt(i);
      Int &idx = builder_.getInt(expr.getStart(), static_cast<int>(i));
      Get &get = builder_.getGet(expr.getStart(), type, expr, idx);
      Let &let = builder_.getLet(id_locs.at(i), unpack_ids.at(i), get);
      RegisterLocalVar(unpack_ids.at(i), let);
    }
  } else {
    Let &let = builder_.getLet(let_loc, name, expr);
    RegisterLocalVar(name, let);
  }

  Result<Expr *> body_res = ParseExpr(hint);
  return body_res;
}

Result<Expr *> Parser::ParseCallFromArgs(const SourceLocation &call_loc,
                                         std::string_view name,
                                         const std::vector<Expr *> &parsed_args,
                                         Token::TokenKind end_tok,
                                         const Type *return_type_hint) {
  auto maybe_callable_and_args = ParseCallableFromArgs(
      call_loc, name, parsed_args, end_tok, return_type_hint);
  if (!maybe_callable_and_args)
    return maybe_callable_and_args;
  Consume(end_tok);

  auto &callables = maybe_callable_and_args->possible_callables;
  auto &args = maybe_callable_and_args->args;
  assert(!callables.empty());
  if (callables.size() == 1) {
    return getAndCheckCall(call_loc, *callables.front(), args,
                           /*pure=*/false, return_type_hint);
  }

  return &builder_.getAmbiguousCall(call_loc, callables, args);
}

Result<Parser::CallableResults> Parser::ParseCallableFromArgs(
    const SourceLocation &call_loc, std::string_view name,
    const std::vector<Expr *> &parsed_args, Token::TokenKind end_tok,
    const Type *return_type_hint) {
  auto args_res = ParseCallArguments(end_tok);
  if (!args_res)
    return args_res;
  std::vector<Expr *> args;
  args.insert(args.end(), parsed_args.begin(), parsed_args.end());
  args.insert(args.end(), args_res->begin(), args_res->end());

  std::vector<const Type *> arg_types(args.size());
  std::transform(args.begin(), args.end(), arg_types.begin(),
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
    auto diag(getErrorDiag());
    diag << call_loc << ": Could not find callable `" << name
         << "` with arg types `";
    for (const Type *t : arg_types)
      diag << t->toString() << " ";
    diag << "`" << DumpLine{call_loc};
    return diag;
  }

  if (possible_callables.size() > 1) {
    if (AmbiguousCall::CanMake(possible_callables))
      return CallableResults{possible_callables, args};

    return getAmbiguousCallDiag(call_loc, name, possible_callables, &arg_types);
  }

  return CallableResults{possible_callables, args};
}

Result<Expr *> Parser::ParseIdentifier(const Type *hint) {
  Result<Token> res = lexer_.Lex();
  assert(res && res->getKind() == Token::TK_Identifier);
  SourceLocation loc = res->getStart();

  std::string name(res->getChars());
  if (!HasVar(name))
    return getErrorDiag() << loc << "Unknown variable `" << name << "`"
                          << DumpLine{loc};

  const auto *callable_ty = llvm::dyn_cast_or_null<CallableType>(hint);
  if (callable_ty && HasVar(name, *callable_ty))
    return &LookupCallable(name, *callable_ty);

  Result<Token> peek = lexer_.Peek();
  if (!peek)
    return peek;

  std::vector<Expr *> parsed_args;
  if (peek->isa(Token::TK_Dot)) {
    // Handle the method syntax.
    Consume(Token::TK_Dot);

    // Parse the method name.
    res = lexer_.Lex();
    if (!res)
      return res;
    if (!res->isa(Token::TK_Identifier)) {
      return getErrorDiag()
             << res->getStart()
             << ": Expected an identifier for the method name; instead found `"
             << res->getChars() << "`" << DumpLine{res->getStart()};
    }

    // Get the first identifier as an expression.
    Expr &expr = LookupSingleExpr(name);
    parsed_args.push_back(&expr);

    // Falthrough to the alt call syntax.
    name = res->getChars();
    peek = lexer_.Peek();
    if (!peek)
      return peek;
  }

  // Handle a potential call.
  //
  // TODO: See if we can consolidate this with ParseAltSyntaxes.
  if (peek->isa(Token::TK_LParen)) {
    // Handle the new call syntax.
    Consume(Token::TK_LParen);
    return ParseCallFromArgs(loc, name, parsed_args, Token::TK_RParen, hint);
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
      return getErrorDiag()
             << current
             << ": Call exceeds expected number of arguments; expected "
             << callable_type->getNumArgs() << " args for callable type "
             << callable_type->toString() << " but instead found `"
             << res->getChars() << "`" << DumpLine{current};
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
    if (!callable_type->CanApplyArgs(arg_types)) {
      Diagnostic diag(getErrorDiag());
      diag << current << ": Cannot call function of type `"
           << callable_type->toString() << "` with arg types `";
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
  if (should_deduce_call_from_args) {
    // We have multiple possible function overloads we can call, so attempt to
    // find the right one from the arguments.
    std::string func_name(res->getChars());
    Consume(Token::TK_Identifier);
    return ParseCallFromArgs(res->getStart(), func_name, /*parsed_args=*/{},
                             Token::TK_End, return_type_hint);
  }

  Result<Expr *> callable_res = ParseExpr();
  if (!callable_res)
    return callable_res;

  callable = *callable_res;
  const Type &type = callable->getType();
  if (!llvm::isa<CallableType>(type)) {
    return getErrorDiag()
           << callable->getStart()
           << ": Expected callable expression to be a callable type; instead "
              "found "
           << type.toString() << DumpLine{callable->getStart()};
  }

  auto args_res =
      ParseCallArguments(Token::TK_End, &llvm::cast<CallableType>(type));
  if (!args_res)
    return args_res;
  Consume(Token::TK_End);

  return getAndCheckCall(call_loc, *callable, *args_res, pure,
                         return_type_hint);
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

  if (return_type_hint &&
      !return_type_hint->CanConvertFrom(actual_ty->getReturnType())) {
    return getErrorDiag() << callable_loc
                          << ": Return type mismatch for call; found "
                          << actual_ty->getReturnType().toString()
                          << " but expected " << return_type_hint->toString()
                          << DumpLine{callable_loc};
  }

  if (actual_ty->getNumArgs() != args.size() &&
      !actual_ty->isGenericRemainingCallable()) {
    return getErrorDiag() << callable_loc
                          << ": Mismatch between number of arguments; found "
                          << args.size() << " but expected "
                          << actual_ty->getNumArgs();
  } else if (actual_ty->isGenericRemainingCallable() &&
             args.size() < actual_ty->getNumArgs()) {
    return getErrorDiag() << callable_loc
                          << ": Not enough arguments for callable; found "
                          << args.size() << " but expected at least "
                          << actual_ty->getNumArgs();
  }

  for (size_t i = 0; i < actual_ty->getNumArgs(); ++i) {
    if (!actual_ty->getArgType(i).CanConvertFrom(*args.at(i))) {
      return getErrorDiag()
             << callable_loc << ": Cannot convert argument " << i
             << " which is `" << args.at(i)->getType().toString()
             << "` to parameter type `" << actual_ty->getArgType(i).toString()
             << "`" << DumpLine{callable_loc};
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

//
// <zero> = "zero" "as" <type>
//
// Zero's will always require a type hint so we can determine its type.
//
Result<Zero *> Parser::ParseZero() {
  SourceLocation loc = lexer_.Peek()->getStart();
  Consume(Token::TK_Zero);

  Result<Token> as = lexer_.Lex();
  if (!as)
    return as;

  if (!as->isa(Token::TK_As)) {
    return getErrorDiag() << loc << ": `zero` requires a type hint `as <type>`"
                          << DumpLine{loc};
  }

  Result<const Type *> type_res = ParseType();
  if (!type_res)
    return type_res;

  return &builder_.getZero(loc, **type_res);
}

//
// <composite> ::= "<" <expr>+ ">"
//
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

//
// <set> ::= "SET" <expr> <expr> <expr>
//
// The first <expr> is the aggregate type being accessed. The second <expr> is
// the index. The third <expr> is the value being stored.
//
Result<Set *> Parser::ParseSet() {
  SourceLocation loc = lexer_.Peek()->getStart();
  Consume(Token::TK_SET);

  Result<Expr *> composite_res = ParseExpr();
  if (!composite_res)
    return composite_res;

  Expr &aggregate = **composite_res;

  const Type &type = aggregate.getType();
  if (!type.isAggregateType()) {
    return getErrorDiag() << aggregate.getStart()
                          << ": Expression is not a composite or array type"
                          << DumpLine{aggregate.getStart()};
  }

  if (!type.isMutable()) {
    return getErrorDiag()
           << loc << ": Attempting to SET type that is not marked as mutable"
           << DumpLine{loc} << "\n"
           << aggregate.getStart() << ": Expression to SET declared here"
           << DumpLine{aggregate.getStart()};
  }

  Result<Expr *> idx_res = ParseAggregateIndexExpr(type, loc);
  if (!idx_res)
    return idx_res;
  Expr &idx = **idx_res;

  Result<Expr *> store_val = ParseExpr();
  if (!store_val)
    return store_val;

  // TODO: If the idx is a constant integer we can determine now, let's maybe
  // check that the corresponding type at the idx matches the type of the store
  // value.

  Set &set = builder_.getSet(loc, aggregate, idx, **store_val);
  return &set;
}

Result<Expr *> Parser::ParseAggregateIndexExpr(const Type &agg_ty,
                                               const SourceLocation &agg_loc) {
  assert(agg_ty.isAggregateType());

  Result<Token> idx_peek = lexer_.Peek();
  if (!idx_peek)
    return idx_peek;

  if (idx_peek->isa(Token::TK_Int)) {
    size_t idx = std::stoi(std::string(idx_peek->getChars()));
    const auto &idx_loc = idx_peek->getStart();

    if (const auto *comp_ty = llvm::dyn_cast<CompositeType>(&agg_ty)) {
      if (idx >= comp_ty->getNumTypes()) {
        return getErrorDiag() << idx_loc << ": Index " << idx
                              << " exceeds size of composite type which is "
                              << comp_ty->getNumTypes() << DumpLine{idx_loc};
      }
    } else if (const auto *arr_ty = llvm::dyn_cast<ArrayType>(&agg_ty)) {
      if (idx >= arr_ty->getNumElems()) {
        return getErrorDiag() << idx_loc << ": Index " << idx
                              << " exceeds size of array type which is "
                              << arr_ty->getNumElems() << DumpLine{idx_loc};
      }
    } else {
      UNREACHABLE(
          "We should've checked if this was either a composite or array type "
          "above.");
    }
  }

  Result<Expr *> idxres = ParseExpr();
  if (!idxres)
    return idxres;
  Expr &idx = **idxres;

  if (!idx.getType().isNamedType("int")) {
    return getErrorDiag()
           << idx.getStart()
           << ": Expression for index is not type `int`; instead is `"
           << idx.getType().toString() << "`" << DumpLine{idx.getStart()};
  }

  if (llvm::isa<CompositeType>(agg_ty) && !llvm::isa<Int>(idx)) {
    return getErrorDiag() << agg_loc
                          << ": Indexing a composite type requires a "
                             "compile-time constant integer"
                          << DumpLine{agg_loc} << "\n"
                          << idx.getStart() << ": Index declared here"
                          << DumpLine{idx.getStart()};
  }

  return &idx;
}

const Type &Parser::getGetResultType(const Type &agg_ty, const Expr &idx) {
  if (const auto *arr_ty = llvm::dyn_cast<ArrayType>(&agg_ty))
    return arr_ty->getElemType();
  const auto &comp_ty = llvm::cast<CompositeType>(agg_ty);
  const Int &idx_node = llvm::cast<Int>(idx);
  return comp_ty.getTypeAt(idx_node.getInt());
}

//
// <get> ::= "GET" <expr> <expr>
//
// The <type> is the resulting type of the GET expression. The first <expr> is
// the composite type that is being accessed. The second <expr> is the index.
//
Result<Get *> Parser::ParseGet() {
  SourceLocation loc = lexer_.PeekLoc();
  Consume(Token::TK_GET);

  Result<Expr *> expr = ParseExpr();
  if (!expr)
    return expr;

  Expr &aggregate = **expr;
  const Type &agg_type = aggregate.getType();

  if (!agg_type.isAggregateType()) {
    return getErrorDiag() << aggregate.getStart()
                          << ": Expression is not a composite or array type"
                          << DumpLine{aggregate.getStart()};
  }

  Result<Expr *> idx_res = ParseAggregateIndexExpr(agg_type, loc);
  if (!idx_res)
    return idx_res;
  Expr &idx = **idx_res;

  const Type &result_type = getGetResultType(agg_type, idx);
  return &builder_.getGet(loc, result_type, aggregate, idx);
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
      Diagnostic warn(lexer_.getInput());
      warn << loc << ": Casting from a longer " << from_type.getNumElems()
           << " length char array type to a shorter " << to_type.getNumElems()
           << " length char array type truncates result";
      std::cerr << warn.get() << std::endl;
    }
  }

  if (**type == (*expr)->getType()) {
    Diagnostic warn(lexer_.getInput());
    warn << loc << ": Unnecessary cast here since types are the same"
         << DumpLine{loc};
    std::cerr << warn.get() << std::endl;
  }

  return &builder_.getCast(loc, **type, **expr);
}

}  // namespace lang
