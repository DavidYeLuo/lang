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

const Type &Type::getReturnType() const {
  return llvm::cast<CallableType>(*this).getReturnType();
}

bool Type::isNamedType(std::string_view name) const {
  if (const auto *named_ty = llvm::dyn_cast<NamedType>(this))
    return named_ty->getName() == name;
  return false;
}

bool Type::isCharArray() const {
  if (const auto *array_ty = llvm::dyn_cast<ArrayType>(this))
    return array_ty->getElemType().isNamedType("char");
  return false;
}

bool Type::isCompositeOrArrayType() const {
  return llvm::isa<CompositeType>(this) || llvm::isa<ArrayType>(this);
}

// decl readc = \IO -> <IO int>
bool Readc::CheckType(const Type &type) {
  if (const auto *callable_ty = llvm::dyn_cast<CallableType>(&type)) {
    if (const auto *comp_ty =
            llvm::dyn_cast<CompositeType>(&callable_ty->getReturnType())) {
      if (comp_ty->getNumTypes() != 2)
        return false;

      if (!comp_ty->getTypeAt(0).isNamedType("IO"))
        return false;

      if (!comp_ty->getTypeAt(1).isNamedType("int"))
        return false;

      if (callable_ty->getArgTypes().size() != 1)
        return false;

      return callable_ty->getArgType(0).isNamedType("IO");
    }
  }
  return false;
}

bool IsBuiltinType(std::string_view name) {
  return name == builtins::kIntTypeName || name == builtins::kCharTypeName ||
         name == builtins::kIOTypeName || name == builtins::kBoolTypeName ||
         name == builtins::kCPtrTypeName || name == builtins::kNoneTypeName;
}

bool Type::isBuiltinType() const {
  if (const auto *named_ty = llvm::dyn_cast<NamedType>(this))
    return IsBuiltinType(named_ty->getName());
  return false;
}

bool Type::isGenericCallable() const {
  if (const auto *ty = llvm::dyn_cast<CallableType>(this))
    return ty->isGeneric();
  return false;
}

const NamedType &ASTBuilder::getNamedType(std::string_view name) {
  auto found = named_types_.find(name);
  if (found != named_types_.end())
    return *found->second;

  const NamedType *ptr = new NamedType(name);
  types_.emplace_back(ptr);
  named_types_[std::string(name)] = ptr;
  return *ptr;
}

bool ASTBuilder::Equals(const Type &lhs, const Type &rhs) const {
  switch (lhs.getKind()) {
#define TYPE(name)      \
  case Type::TK_##name: \
    return Equals(llvm::cast<name>(lhs), rhs);
#include "types.def"
  }
}

bool ASTBuilder::Equals(const GenericType &lhs, const Type &rhs) const {
  return llvm::isa<GenericType>(rhs);
}

bool ASTBuilder::Equals(const ArrayType &lhs, const Type &rhs) const {
  if (const auto *array_rhs = llvm::dyn_cast<ArrayType>(&rhs)) {
    if (lhs.getNumElems() != array_rhs->getNumElems())
      return false;

    return Equals(lhs.getElemType(), array_rhs->getElemType());
  }
  return false;
}

bool ASTBuilder::Equals(const CompositeType &lhs, const Type &rhs) const {
  if (const auto *composite_rhs = llvm::dyn_cast<CompositeType>(&rhs)) {
    if (lhs.getTypes().size() != composite_rhs->getTypes().size())
      return false;

    for (size_t i = 0; i < lhs.getTypes().size(); ++i) {
      if (!Equals(lhs.getTypeAt(i), composite_rhs->getTypeAt(i)))
        return false;
    }

    return true;
  }
  return false;
}

bool ASTBuilder::Equals(const CallableType &lhs, const Type &rhs) const {
  if (const auto *callable_rhs = llvm::dyn_cast<CallableType>(&rhs)) {
    if (lhs.getArgTypes().size() != callable_rhs->getArgTypes().size())
      return false;

    for (size_t i = 0; i < lhs.getArgTypes().size(); ++i) {
      if (!Equals(*lhs.getArgTypes().at(i), *callable_rhs->getArgTypes().at(i)))
        return false;
    }

    return Equals(lhs.getReturnType(), callable_rhs->getReturnType());
  }
  return false;
}

bool ASTBuilder::ArgumentTypesMatch(
    const std::vector<const Type *> &args1,
    const std::vector<const Type *> &args2) const {
  if (args1.size() != args2.size())
    return false;

  for (size_t i = 0; i < args1.size(); ++i) {
    if (args1.at(i)->isGeneric() || args2.at(i)->isGeneric())
      continue;

    if (!Equals(*args1.at(i), *args2.at(i)))
      return false;
  }

  return true;
}

bool ASTBuilder::CallableTypesMatch(const CallableType &lhs,
                                    const CallableType &rhs) const {
  if (!lhs.isGeneric() && !rhs.isGeneric())
    return Equals(lhs, rhs);
  if (!Equals(lhs.getReturnType(), rhs.getReturnType()))
    return false;
  return ArgumentTypesMatch(lhs.getArgTypes(), rhs.getArgTypes());
}

bool ASTBuilder::Equals(const NamedType &lhs, const Type &rhs) const {
  if (const auto *named_rhs = llvm::dyn_cast<NamedType>(&rhs))
    return lhs.getName() == named_rhs->getName();
  return false;
}

Parser::Parser(Lexer &lexer) : lexer_(lexer) {
  const Declare &write_decl =
      builder_.getDeclare(SourceLocation(), "write",
                          builder_.getWriteType(builder_.getGenericType()));
  RegisterGlobalVar("write", write_decl);
}

Result<const Parser::AST> Parser::Parse() {
  std::vector<const Node *> ast;
  Result<Token> next = lexer_.Peek();
  if (!next)
    return next;
  while (!next->isa(Token::TK_EOF)) {
    Result<const Node *> top_level_entity = [&]() -> Result<const Node *> {
      if (next->isa(Token::TK_Def))
        return ParseDefine();
      else if (next->isa(Token::TK_Decl))
        return ParseDeclare();
      else
        return getDiag(next->getStart())
               << "Unknown top level entity `" << next->getChars() << "`; "
               << "expected either `def` or `decl`";
    }();

    if (!top_level_entity)
      return top_level_entity;
    // ASTDumper(**top_level_entity, std::cerr).Dump();
    ast.push_back(*top_level_entity);

    next = lexer_.Peek();
    if (!next)
      return next;
  }
  return ast;
}

Result<const Define *> Parser::ParseDefineImpl() {
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

  // Parse the body of the define.
  // TODO: Handle stuff other than lambdas here.
  std::string_view name_ref(name);
  Result<const Callable *> callable_res =
      ParseCallable(&name_ref, /*hint=*/nullptr);
  if (!callable_res)
    return callable_res;

  const Define &define = builder_.getDefine(def_loc, name, **callable_res);
  return &define;
}

// <decl> ::= "decl" <identifier> "=" <type>
Result<const Declare *> Parser::ParseDeclareImpl() {
  auto decl_tok = lexer_.Peek();
  SourceLocation decl_loc = decl_tok->getStart();

  Consume(Token::TK_Decl);
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

  const Declare &decl = builder_.getDeclare(decl_loc, name, **type);
  RegisterGlobalVar(name, decl);
  return &decl;
}

// <callable> ::= "\" <arg list> "->" <type> <expr>
Result<const Callable *> Parser::ParseCallable(std::string_view *callable_name,
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

  // FIXME: This feels awkward.
  bool has_error = false;
  Result<const Callable *> callable_res;

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
        ASTDumper(*expr, ss).Dump();
      }
      return Result<const Callable *>::Error(ss.str());
    }
  }

  const Callable &callable = builder_.getCallable(
      callable_loc, ret_type, arg_locs, arg_names, arg_types,
      // NOTE: This lambda needs to explicitly set the return type as a
      // reference, otherwise the deduced argument could be a non-reference copy
      // and result in an eventual stack-use-after-return.
      [&](const CallableBase &this_func,
          const std::vector<const lang::Arg *> &args) -> const Expr & {
        if (callable_name) {
          RegisterGlobalCallableVar(*callable_name,
                                    llvm::cast<Callable>(this_func));
        }

        // Register the variable names.
        assert(arg_names.size() == args.size());
        for (size_t i = 0; i < args.size(); ++i) {
          RegisterLocalVar(arg_names.at(i), *args.at(i));
        }

        // Parse the body which is just an expression.
        Result<const Expr *> expr_res = ParseExpr(&ret_type);
        if (!expr_res) {
          has_error = true;
          callable_res = expr_res;  // Propagate the result back up.
          return this_func;         // This acts as a dummy return value.
        }

        return *expr_res.get();
      });

  if (has_error)
    return callable_res;

  // Check the return type.
  if (!builder_.Equals(callable.getType().getReturnType(),
                       callable.getBody().getType())) {
    return getDiag(lambda_loc)
           << "Mismatch between callable return type and body return type; "
              "expected "
           << callable.getType().getReturnType().toString()
           << " but instead found " << callable.getBody().getType().toString();
  }

  // TODO: Check against the hint.
  callable_res = &callable;
  return callable_res;
}

// <type>         ::= <namedtype> | <callabletype> | <compositetype>
//   <namedtype>      ::= <identifier>
//   <callabletype>   ::= "\" <type>* "->" <type>
//   <compositetype>  ::= "<" <type>+ ">"
//   <arraytype>      ::= "[" \d+ "x" <type> "]"
Result<const Type *> Parser::ParseType() {
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
    return &builder_.getCompositeType(types);
  } else if (res->isa(Token::TK_LSqBrack)) {
    auto loc = lexer_.getCurrentLoc();
    Result<const Int *> num = ParseInt();
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

    Result<const Type *> type = ParseType();
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
  }

  return getDiag(res->getStart())
         << "Expected a type; instead found `" << res->getChars() << "`";
}

Result<const Expr *> Parser::ParseExpr(const Type *hint) {
  SourceLocation loc = lexer_.PeekLoc();
  auto p = lexer_.Peek();
  Result<const Expr *> res = ParseExprImpl(hint);
  if (!res)
    return res;

  const Expr *expr = *res;

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
      if (!builder_.CallableTypesMatch(
              llvm::cast<CallableType>(expr->getType()),
              llvm::cast<CallableType>(*hint))) {
        return getDiag(loc) << "Expression type mismatch; found `"
                            << expr->getType().toString() << "` but expected `"
                            << hint->toString() << "`";
      }
    } else if (!builder_.Equals(*hint, expr->getType())) {
      return getDiag(loc) << "Expression type mismatch; found `"
                          << expr->getType().toString() << "` but expected `"
                          << hint->toString() << "`";
    }
  }

  return expr;
}

Result<const Expr *> Parser::ParseExprImpl(const Type *hint) {
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

Result<const BinOp *> Parser::ParseBinOp(const Type *hint) {
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

  Result<const Expr *> lhs = ParseExpr();
  if (!lhs)
    return lhs;

  Result<const Expr *> rhs = ParseExpr();
  if (!rhs)
    return rhs;

  if (!builder_.Equals((*lhs)->getType(), (*rhs)->getType())) {
    return getDiag(res->getStart())
           << "Operands of binary operator have differing types; "
           << (*lhs)->getType().toString() << " and "
           << (*rhs)->getType().toString();
  }

  return &builder_.getBinOp(*lhs.get(), *rhs.get(), kind);
}

// <if> ::= "if" <expr> <expr> "else" <expr>
Result<const If *> Parser::ParseIf(const Type *hint) {
  Result<Token> if_tok = lexer_.Lex();
  assert(if_tok->isa(Token::TK_If));

  SourceLocation if_loc = if_tok->getStart();

  Result<Token> cond_expr_tok = lexer_.Peek();

  Result<const Expr *> cond = ParseExpr();
  if (!cond)
    return cond;

  const Type &type = (*cond)->getType();
  if (!type.isNamedType("bool"))
    return getDiag(cond_expr_tok->getStart())
           << "Expected `bool` type for if condition expression; instead "
              "found `"
           << type.toString() << "`";

  Result<const Expr *> if_body = ParseExpr(hint);
  if (!if_body)
    return if_body;

  Result<Token> res = lexer_.Lex();
  if (!res)
    return res;
  if (!res->isa(Token::TK_Else))
    return getDiag(res->getStart())
           << "Expected `else` for if starting at " << if_tok->getStart()
           << "; instead found `" << res->getChars() << "`";

  Result<const Expr *> else_body = ParseExpr(hint);
  if (!else_body)
    return else_body;

  if (!builder_.Equals((*if_body)->getType(), (*else_body)->getType())) {
    return getDiag(if_tok->getStart())
           << "Mismatch type between if and else expressions; if expression "
              "has type "
           << (*if_body)->getType().toString()
           << " but else expression has type "
           << (*else_body)->getType().toString();
  }

  if (hint) {
    if (!builder_.Equals((*if_body)->getType(), *hint)) {
      return getDiag(if_tok->getStart())
             << "Mismatch type between if body and expected type; if body has "
                "type "
             << (*if_body)->getType().toString() << " but expected type "
             << hint->toString();
    }
    if (!builder_.Equals((*else_body)->getType(), *hint)) {
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
Result<const Keep *> Parser::ParseKeep(const Type *hint) {
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

  Result<const Expr *> expr_res = ParseExpr(type_res.get());
  if (!expr_res)
    return expr_res;

  RegisterLocalVar(name, **expr_res);

  Result<const Expr *> body_res = ParseExpr(hint);
  if (!body_res)
    return body_res;

  return &builder_.getKeep(keep_loc, name, **expr_res, **body_res);
}

// <let> ::= "let" <identifier> "=" <type> <expr> <body>
Result<const Expr *> Parser::ParseLet(const Type *hint) {
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

  Result<const Expr *> expr_res = ParseExpr(type_res.get());
  if (!expr_res)
    return expr_res;

  const Let &let = builder_.getLet(let_loc, name, *expr_res.get());
  RegisterLocalVar(name, let);

  Result<const Expr *> body_res = ParseExpr(hint);
  return body_res;
}

Result<std::pair<const Expr *, std::vector<const Expr *>>>
Parser::ParseCallableFromArgs(const SourceLocation &call_loc,
                              std::string_view name, Token::TokenKind end_tok,
                              const Type &return_type_hint) {
  Result<std::vector<const Expr *>> args = ParseCallArguments(end_tok);
  if (!args)
    return args;

  std::vector<const Type *> arg_types(args->size());
  std::transform(args->begin(), args->end(), arg_types.begin(),
                 [](const Expr *expr) { return &expr->getType(); });
  const CallableType &callable_ty =
      builder_.getCallableType(return_type_hint, arg_types);

  if (!HasVar(name, callable_ty)) {
    return getDiag(call_loc) << "Could not find callable `" << name
                             << "` with type " << callable_ty.toString();
  }
  return std::make_pair(&LookupCallable(name, callable_ty), *args);
}

Result<const Expr *> Parser::ParseIdentifier(const Type *hint) {
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
      Consume(Token::TK_LParen);
      auto maybe_callable_and_args =
          ParseCallableFromArgs(res->getStart(), name, Token::TK_RParen, *hint);
      if (!maybe_callable_and_args)
        return maybe_callable_and_args;
      Consume(Token::TK_RParen);
      return getAndCheckCall(res->getStart(),
                             *maybe_callable_and_args.get().first,
                             maybe_callable_and_args.get().second,
                             /*pure=*/false, hint);
    } else {
      // TODO: We dont't want to dump the ast. We want to print the relevant
      // lines in the source.
      auto diag = std::move(getDiag(res->getStart())
                            << "Ambiguous call to `" << name << "`");
      std::stringstream ss;
      ss << diag.get() << "\n";
      for (const Expr *expr : possible_callables) {
        ASTDumper(*expr, ss).Dump();
      }
      return Result<const Expr *>::Error(ss.str());
    }
  }

  return &LookupSingleExpr(name);
}

// NOTE: Callers of this are in charge of consuming the ending tokens after a
// successful call to this.
Result<std::vector<const Expr *>> Parser::ParseCallArguments(
    Token::TokenKind end_tok, const CallableType *callable_type) {
  Result<Token> res = lexer_.Peek();
  if (!res)
    return res;

  SourceLocation current = lexer_.getCurrentLoc();
  size_t arg_no = 0;

  std::vector<const Expr *> args;
  while (res->getKind() != end_tok) {
    if (callable_type && arg_no >= callable_type->getNumArgs()) {
      return getDiag(lexer_.getCurrentLoc())
             << "Call exceeds expected number of arguments; expected "
             << callable_type->getNumArgs() << " args for callable type "
             << callable_type->toString() << " but instead found `"
             << res->getChars() << "`";
    }

    // Parse an expression as an argument.
    Result<const Expr *> arg =
        ParseExpr(callable_type ? &callable_type->getArgType(arg_no) : nullptr);
    if (!arg)
      return arg;
    args.push_back(arg.get());

    res = lexer_.Peek();
    if (!res)
      return res;

    arg_no++;
  }

  if (callable_type && args.size() != callable_type->getNumArgs()) {
    return getDiag(current)
           << "Expected " << callable_type->getNumArgs()
           << " arguments; instead found " << args.size()
           << " args for callbale type " << callable_type->toString();
  }

  return args;
}

Result<const Call *> Parser::ParseCall(const Type *return_type_hint) {
  return ParseCallImpl(/*is_new_call_syntax=*/false, return_type_hint);
}

Result<const Call *> Parser::ParseCallImpl(bool is_new_call_syntax,
                                           const Type *return_type_hint) {
  Result<Token> tok = lexer_.Lex();
  assert(tok && (tok->isa(Token::TK_Call) || tok->isa(Token::TK_ImpureCall)));

  SourceLocation call_loc = tok->getStart();
  bool pure = tok->isa(Token::TK_Call);

  Result<Token> res = lexer_.Peek();
  if (!res)
    return res;

  // SourceLocation callable_loc = res->getStart();

  bool is_identifier = res->isa(Token::TK_Identifier);
  bool should_deduce_call_from_args = is_identifier && return_type_hint;
  if (should_deduce_call_from_args) {
    // Only continue to deduce the calling type if the number of possible
    // callables is 1 since we already know the only possible type it can be.
    should_deduce_call_from_args = getNumPossibleCallables(res->getChars()) > 1;
  }

  const Expr *callable;
  std::vector<const Expr *> args;
  if (should_deduce_call_from_args) {
    // We have multiple possible function overloads we can call, so attempt to
    // find the right one from the arguments.
    std::string func_name(res->getChars());
    Consume(Token::TK_Identifier);

    Result<std::pair<const Expr *, std::vector<const Expr *>>> maybe_callable =
        ParseCallableFromArgs(res->getStart(), func_name, Token::TK_End,
                              *return_type_hint);
    if (!maybe_callable)
      return maybe_callable;

    callable = (*maybe_callable).first;
    args = (*maybe_callable).second;
  } else {
    Result<const Expr *> callable_res = ParseExpr();
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

Result<const Call *> Parser::getAndCheckCall(
    const SourceLocation &callable_loc, const Expr &maybe_callable,
    const std::vector<const Expr *> &args, bool pure,
    const Type *return_type_hint) {
  const auto *actual_ty =
      llvm::dyn_cast<CallableType>(&maybe_callable.getType());
  if (!actual_ty)
    return getExpectedCallableDiag(callable_loc, maybe_callable.getType());

  if (return_type_hint &&
      !builder_.Equals(actual_ty->getReturnType(), *return_type_hint)) {
    return getDiag(callable_loc)
           << "Return type mismatch for call; found "
           << actual_ty->getReturnType().toString() << " but expected "
           << return_type_hint->toString();
  }

  if (actual_ty->getNumArgs() != args.size()) {
    return getDiag(callable_loc)
           << "Mismatch between number of arguments; found " << args.size()
           << " but expected " << actual_ty->getNumArgs();
  }

  for (size_t i = 0; i < args.size(); ++i) {
    if (!actual_ty->getArgType(i).isGeneric() &&
        !builder_.Equals(actual_ty->getArgType(i), args.at(i)->getType())) {
      return getDiag(callable_loc)
             << "Type mismatch for argument " << i << "; found "
             << args.at(i)->getType().toString() << " but expected "
             << actual_ty->getArgType(i).toString();
    }
  }

  return &builder_.getCall(callable_loc, maybe_callable, args, pure);
}

Result<const Int *> Parser::ParseInt() {
  Result<Token> res = lexer_.Lex();
  if (!res)
    return res;

  assert(res->isa(Token::TK_Int));

  return &builder_.getInt(res->getStart(),
                          std::stoi(std::string(res->getChars())));
}

Result<const Char *> Parser::ParseChar() {
  Result<Token> res = lexer_.Lex();
  assert(res && res->isa(Token::TK_Char));
  assert(res->getChars().size() == 3 && res->getChars().front() == '\'' &&
         res->getChars().back() == '\'');

  return &builder_.getChar(res->getStart(), res->getChars().at(1));
}

Result<const Str *> Parser::ParseStr() {
  Result<Token> res = lexer_.Lex();
  if (!res)
    return res;

  if (res->getKind() != Token::TK_Str)
    return getDiag(res->getStart())
           << "Expected a string; instead found `" << res->getChars() << "`";

  std::string_view chars(res->getChars());
  assert(chars.front() == '"' && chars.back() == '"');
  assert(chars.size() >= 2);

  return &builder_.getStr(res->getStart(), chars.substr(1, chars.size() - 2));
}

Result<const Readc *> Parser::ParseReadc() {
  SourceLocation loc = lexer_.Peek()->getStart();
  Consume(Token::TK_Readc);
  return &builder_.getReadc(loc);
}

Result<const Zero *> Parser::ParseZero(const Type *hint) {
  if (!hint) {
    return getDiag(lexer_.getCurrentLoc())
           << "Unable to determine type of `zero`";
  }
  SourceLocation loc = lexer_.Peek()->getStart();
  Consume(Token::TK_Zero);
  return &builder_.getZero(loc, *hint);
}

// <composite> ::= "<" <expr>+ ">"
Result<const Composite *> Parser::ParseComposite() {
  SourceLocation loc = lexer_.Peek()->getStart();
  Consume(Token::TK_LAngleBrack);

  std::vector<const Expr *> elems;
  Result<Token> next = lexer_.Peek();
  if (!next)
    return next;
  do {
    Result<const Expr *> expr = ParseExpr();
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
Result<const Set *> Parser::ParseSet() {
  SourceLocation loc = lexer_.Peek()->getStart();
  Consume(Token::TK_SET);

  auto peek = lexer_.Peek();
  if (!peek)
    return peek;
  SourceLocation composite_loc = peek->getStart();
  Result<const Expr *> composite = ParseExpr();
  if (!composite)
    return composite;

  const Type &type = (*composite)->getType();
  if (!(llvm::isa<CompositeType>(type) || llvm::isa<ArrayType>(type))) {
    return getDiag(composite_loc)
           << "Expression is not a composite or array type";
  }

  Result<const Expr *> idx = ParseExpr();
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

  Result<const Expr *> store_val = ParseExpr();
  if (!store_val)
    return store_val;

  return &builder_.getSet(loc, **composite, **idx, **store_val);
}

// <get> ::= "GET" <type> <expr> <expr>
//
// The <type> is the resulting type of the GET expression. The first <expr> is
// the composite type that is being accessed. The second <expr> is the index.
Result<const Get *> Parser::ParseGet() {
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
  Result<const Expr *> expr = ParseExpr();
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

    if (!builder_.Equals(type, *result_type)) {
      return getDiag(typeloc)
             << "GET type mismatch; expected `" << type.toString()
             << "` but found `" << result_type->toString() << "`";
    }
  }

  Result<const Expr *> idx = ParseExpr();
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
Result<const Cast *> Parser::ParseCast() {
  SourceLocation loc = lexer_.Peek()->getStart();
  Consume(Token::TK_CAST);
  Result<const Type *> type = ParseType();
  if (!type)
    return type;

  Result<const Expr *> expr = ParseExpr();
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

  if (builder_.Equals(**type, (*expr)->getType())) {
    Diagnostic warn(lexer_.getInput(), loc);
    warn << "Unnecessary cast here since types are the same";
    std::cerr << warn.get() << std::endl;
  }

  return &builder_.getCast(loc, **type, **expr);
}

}  // namespace lang
