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

bool Write::CheckType(const Type &type) {
  if (const auto *callable_ty = llvm::dyn_cast<CallableType>(&type)) {
    return callable_ty->getReturnType().isNamedType("IO") &&
           callable_ty->getArgTypes().size() == 2 &&
           callable_ty->getArgTypes().at(0)->isNamedType("IO");
  }
  return false;
}

// decl readc = \IO -> <IO int>
bool Readc::CheckType(const Type &type) {
  if (const auto *callable_ty = llvm::dyn_cast<CallableType>(&type)) {
    if (const auto *comp_ty =
            llvm::dyn_cast<CompositeType>(&callable_ty->getReturnType())) {
      if (comp_ty->getNumTypes() != 2) return false;

      if (!comp_ty->getTypeAt(0).isNamedType("IO")) return false;

      if (!comp_ty->getTypeAt(1).isNamedType("int")) return false;

      if (callable_ty->getArgTypes().size() != 1) return false;

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

const NamedType &ASTBuilder::getNamedType(std::string_view name) {
  auto found = named_types_.find(name);
  if (found != named_types_.end()) return *found->second;

  const NamedType *ptr = new NamedType(name);
  nodes_.emplace_back(ptr);
  named_types_[std::string(name)] = ptr;
  return *ptr;
}

bool ASTBuilder::Equals(const Type &lhs, const Type &rhs) const {
  switch (lhs.getKind()) {
    case Node::NK_NamedType:
      return Equals(llvm::cast<NamedType>(lhs), rhs);
    case Node::NK_CallableType:
      return Equals(llvm::cast<CallableType>(lhs), rhs);
    case Node::NK_CompositeType:
      return Equals(llvm::cast<CompositeType>(lhs), rhs);
    case Node::NK_ArrayType:
      return Equals(llvm::cast<ArrayType>(lhs), rhs);
#define NODE(name) case Node::NK_##name:
#define TYPE(name)
#include "nodes.def"
      __builtin_trap();
  }
}

bool ASTBuilder::Equals(const ArrayType &lhs, const Type &rhs) const {
  if (const auto *array_rhs = llvm::dyn_cast<ArrayType>(&rhs)) {
    if (lhs.getNumElems() != array_rhs->getNumElems()) return false;

    return Equals(lhs.getElemType(), array_rhs->getElemType());
  }
  return false;
}

bool ASTBuilder::Equals(const CompositeType &lhs, const Type &rhs) const {
  if (const auto *composite_rhs = llvm::dyn_cast<CompositeType>(&rhs)) {
    if (lhs.getTypes().size() != composite_rhs->getTypes().size()) return false;

    for (size_t i = 0; i < lhs.getTypes().size(); ++i) {
      if (!Equals(lhs.getTypeAt(i), composite_rhs->getTypeAt(i))) return false;
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

bool ASTBuilder::Equals(const NamedType &lhs, const Type &rhs) const {
  if (const auto *named_rhs = llvm::dyn_cast<NamedType>(&rhs))
    return lhs.getName() == named_rhs->getName();
  return false;
}

Result<const Parser::AST> Parser::Parse() {
  std::vector<const Node *> ast;
  Result<Token> next = lexer_.Peek();
  if (!next) return next;
  while (!next->isa(Token::TK_EOF)) {
    Result<const Node *> top_level_entity;
    if (next->isa(Token::TK_Def))
      top_level_entity = ParseDefine();
    else if (next->isa(Token::TK_Decl))
      top_level_entity = ParseDeclare();
    else
      return Result<const Parser::AST>::BuildError()
             << next->getStart() << ": Unknown top level entity `"
             << next->getChars() << "`; " << "expected either `def` or `decl`";

    if (!top_level_entity) return top_level_entity;
    ast.push_back(top_level_entity.get());

    next = lexer_.Peek();
    if (!next) return next;
  }
  return ast;
}

Result<const Define *> Parser::ParseDefineImpl() {
  Consume(Token::TK_Def);

  Result<Token> res = lexer_.Lex();
  if (!res) return res;

  if (res->getKind() != Token::TK_Identifier)
    return getDiag<const Define *>(res->getStart())
           << "Expected an identifier; instead found `" << res->getChars()
           << "`";

  std::string name(res->getChars());

  res = lexer_.Lex();
  if (!res) return res;

  if (res->getKind() != Token::TK_Assign)
    return getDiag<const Define *>(res->getStart())
           << "Expected `=`; instead found `" << res->getChars() << "`";

  // Parse the body of the define.
  // TODO: Handle stuff other than lambdas here.
  std::string_view name_ref(name);
  Result<const Callable *> callable_res =
      ParseCallable(/*hint=*/nullptr, &name_ref);
  if (!callable_res) return callable_res;

  const Define &define = builder_.getDefine(name, *callable_res.get());
  return &define;
}

// <decl> ::= "decl" <identifier> "=" <type>
Result<const Declare *> Parser::ParseDeclareImpl() {
  Consume(Token::TK_Decl);
  Result<Token> tok = lexer_.Lex();
  if (!tok) return tok;

  if (!tok->isa(Token::TK_Identifier))
    return getDiag<const Declare *>(tok->getStart())
           << "Expected an identifier; instead found `" << tok->getChars()
           << "`";

  std::string name(tok->getChars());

  // Consume `=`.
  tok = lexer_.Lex();
  if (!tok) return tok;
  if (tok->getKind() != Token::TK_Assign)
    return getDiag<const Declare *>(tok->getStart())
           << "Expected `=`; instead found `" << tok->getChars() << "`";

  Result<const Type *> type = ParseType();
  if (!type) return type;

  const Declare &decl = builder_.getDeclare(name, *type.get());
  RegisterGlobalVar(name, decl);
  return &decl;
}

Result<const Callable *> Parser::ParseCallable(
    const Type *hint, std::string_view *callable_name) {
  Result<Token> res = lexer_.Lex();
  if (!res) return res;

  if (res->getKind() != Token::TK_Lambda)
    return Result<const Callable *>::BuildError()
           << res->getStart() << ": Expected lambda start `\\`; instead found `"
           << res->getChars() << "`";

  // Maybe parse arguments.
  res = lexer_.Peek();
  if (!res) return res;

  std::vector<std::string> arg_names;
  std::vector<const Type *> arg_types;
  while (res->getKind() != Token::TK_Arrow) {
    // Parse one argument (a type and an identifier).
    Result<const Type *> type_res = ParseType();
    if (!type_res) return type_res;

    const Type &type = *type_res.get();

    res = lexer_.Lex();
    if (!res) return res;
    if (res->getKind() != Token::TK_Identifier)
      return Result<const Callable *>::BuildError()
             << res->getStart()
             << ": Expected an argument name; instead found `"
             << res->getChars() << "`";

    std::string_view name(res->getChars());

    arg_names.emplace_back(name);
    arg_types.push_back(&type);

    // Finished processing one argument. Check for the ending -> or continue
    // parsing more arguments.
    res = lexer_.Peek();
    if (!res) return res;
  }

  assert(res->getKind() == Token::TK_Arrow);
  lexer_.Lex();  // Consume '->'.

  // Get a return type.
  SourceLocation lambda_loc = lexer_.getCurrentLoc();
  Result<const Type *> type_res = ParseType();
  if (!type_res) return type_res;

  // FIXME: This feels awkward.
  bool has_error = false;
  Result<const Callable *> callable_res;

  const Callable &callable = builder_.getCallable(
      *type_res.get(), arg_names, arg_types,
      // NOTE: This lambda needs to explicitly set the return type as a
      // reference, otherwise the deduced argument could be a non-reference copy
      // and result in an eventual stack-use-after-return.
      [&](const CallableBase &this_func,
          const std::vector<const lang::Arg *> &args) -> const Expr & {
        if (callable_name) {
          RegisterGlobalVar(*callable_name, this_func);
        }

        // Register the variable names.
        assert(arg_names.size() == args.size());
        for (size_t i = 0; i < args.size(); ++i) {
          RegisterLocalVar(arg_names.at(i), *args.at(i));
        }

        // Parse the body which is just an expression.
        Result<const Expr *> expr_res = ParseExpr();
        if (!expr_res) {
          has_error = true;
          callable_res = expr_res;  // Propagate the result back up.
          return this_func;         // This acts as a dummy return value.
        }

        return *expr_res.get();
      });

  if (has_error) return callable_res;

  // Check the return type.
  if (!builder_.Equals(callable.getType().getReturnType(),
                       callable.getBody().getType())) {
    return getDiag<const Callable *>(lambda_loc)
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
  if (!res) return res;

  if (res->isa(Token::TK_Identifier)) {
    std::string_view type_name(res->getChars());

    // TODO: Check custom types here eventually.
    if (!IsBuiltinType(type_name)) {
      return getDiag<const Type *>(res->getStart())
             << "Unknown builtin type `" << type_name << "`";
    }

    return &builder_.getNamedType(type_name);
  } else if (res->isa(Token::TK_Lambda)) {
    res = lexer_.Peek();
    if (!res) return res;

    std::vector<const Type *> arg_types;
    while (!res->isa(Token::TK_Arrow)) {
      Result<const Type *> arg_type = ParseType();
      if (!arg_type) return arg_type;
      arg_types.push_back(arg_type.get());

      res = lexer_.Peek();
      if (!res) return res;
    }

    Consume(Token::TK_Arrow);
    Result<const Type *> res_type = ParseType();
    if (!res_type) return res_type;

    return &builder_.getCallableType(*res_type.get(), arg_types);
  } else if (res->isa(Token::TK_LAngleBrack)) {
    std::vector<const Type *> types;
    do {
      Result<const Type *> type = ParseType();
      if (!type) return type;

      types.push_back(*type);

      res = lexer_.Peek();
      if (!res) return res;
    } while (!res->isa(Token::TK_RAngleBrack));
    Consume(Token::TK_RAngleBrack);
    return &builder_.getCompositeType(types);
  } else if (res->isa(Token::TK_LSqBrack)) {
    auto loc = lexer_.getCurrentLoc();
    Result<const Int *> num = ParseInt();
    if (!num) return num;

    if ((*num)->getInt() <= 0) {
      return getDiag<const Type *>(loc)
             << "Expected a positive integral size; instead found `"
             << (*num)->getInt() << "`";
    }

    res = lexer_.Lex();
    if (!res) return res;
    if (!res->isa(Token::TK_Identifier) || res->getChars() != "x") {
      return getDiag<const Type *>(loc)
             << "Expected `x` in array type; instead found `" << res->getChars()
             << "`";
    }

    Result<const Type *> type = ParseType();
    if (!type) return type;

    res = lexer_.Lex();
    if (!res) return res;
    if (!res->isa(Token::TK_RSqBrack))
      return getDiag<const Type *>(loc)
             << "Expected closing `]` for array type; instead found `"
             << res->getChars() << "`";

    return &builder_.getArrayType(**type,
                                  static_cast<size_t>((*num)->getInt()));
  }

  return getDiag<const Type *>(res->getStart())
         << "Expected a type; instead found `" << res->getChars() << "`";
}

Result<const Expr *> Parser::ParseExpr(const Type *hint) {
  SourceLocation loc = lexer_.getCurrentLoc();
  Result<const Expr *> res = ParseExprImpl(hint);

  if (hint && res && !builder_.Equals(*hint, (*res)->getType())) {
    return Result<const Expr *>::Diagnostic(lexer_.getInput(), loc)
           << "Expression type mismatch; found " << (*res)->getType().toString()
           << " but expected " << hint->toString();
  }

  return res;
}

Result<const Expr *> Parser::ParseExprImpl(const Type *hint) {
  Result<Token> res = lexer_.Peek();
  if (!res) return res;

  // TODO: Check the types of these if a hint is provided.

  if (res->getKind() == Token::TK_Call ||
      res->getKind() == Token::TK_ImpureCall)
    return ParseCall(hint);
  if (res->getKind() == Token::TK_Zero) return ParseZero(hint);
  if (res->getKind() == Token::TK_Write) return ParseWrite(hint);
  if (res->getKind() == Token::TK_Readc) return ParseReadc();
  if (res->getKind() == Token::TK_Str) return ParseStr();
  if (res->getKind() == Token::TK_Char) return ParseChar();
  if (res->getKind() == Token::TK_Identifier) return ParseIdentifier(hint);
  if (res->getKind() == Token::TK_Let) return ParseLet(hint);
  if (res->getKind() == Token::TK_Keep) return ParseKeep(hint);
  if (res->getKind() == Token::TK_Int) return ParseInt();
  if (res->getKind() == Token::TK_If) return ParseIf(hint);
  if (res->getKind() == Token::TK_CAST) return ParseCast();
  if (res->getKind() == Token::TK_GET) return ParseGet();
  if (res->getKind() == Token::TK_SET) return ParseSet();
  // if (res->getKind() == Token::TK_None) return ParseNone();
  if (res->getKind() == Token::TK_LAngleBrack) return ParseComposite();

  if (res->isBinOpKind()) return ParseBinOp(hint);

  if (res->getKind() == Token::TK_True) {
    Consume(Token::TK_True);
    return &builder_.getBool(true);
  }
  if (res->getKind() == Token::TK_False) {
    Consume(Token::TK_False);
    return &builder_.getBool(false);
  }

  return getDiag<const Expr *>(res->getStart())
         << "Unable to parse expression starting with `" << res->getChars()
         << "`";
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
    return getDiag<const BinOp *>(res->getStart())
           << "Unknown binary operation `" << res->getChars() << "`";

  Result<const Expr *> lhs = ParseExpr();
  if (!lhs) return lhs;

  Result<const Expr *> rhs = ParseExpr();
  if (!rhs) return rhs;

  if (!builder_.Equals((*lhs)->getType(), (*rhs)->getType())) {
    return getDiag<const BinOp *>(res->getStart())
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

  Result<Token> cond_expr_tok = lexer_.Peek();

  Result<const Expr *> cond = ParseExpr();
  if (!cond) return cond;

  const Type &type = (*cond)->getType();
  if (!type.isNamedType("bool"))
    return Result<const If *>::BuildError()
           << cond_expr_tok->getStart()
           << ": Expected `bool` type for if condition expression; instead "
              "found `"
           << type.toString() << "`";

  Result<const Expr *> if_body = ParseExpr();
  if (!if_body) return if_body;

  Result<Token> res = lexer_.Lex();
  if (!res) return res;
  if (!res->isa(Token::TK_Else))
    return Result<const If *>::BuildError()
           << res->getStart() << ": Expected `else` for if starting at "
           << if_tok->getStart() << "; instead found `" << res->getChars()
           << "`";

  Result<const Expr *> else_body = ParseExpr();
  if (!else_body) return else_body;

  // TODO: Check against the type hint here.

  return &builder_.getIf(*cond.get(), *if_body.get(), *else_body.get());
}

// <keep> ::= "keep" <identifier> "=" <type> <expr> <body>
//
// This is similar to a Let except the body is retained in the node itself, so a
// Keep will always be retained in the AST.
Result<const Keep *> Parser::ParseKeep(const Type *hint) {
  Consume(Token::TK_Keep);

  Result<Token> res = lexer_.Lex();
  if (!res) return res;

  if (!res->isa(Token::TK_Identifier))
    return getDiag<const Keep *>(res->getStart())
           << "Expected an identifier but instead found `" << res->getChars()
           << "`";

  std::string_view name(res->getChars());

  Consume(Token::TK_Assign);

  Result<const Type *> type_res = ParseType();
  if (!type_res) return type_res;

  Result<const Expr *> expr_res = ParseExpr(type_res.get());
  if (!expr_res) return expr_res;

  RegisterLocalVar(name, **expr_res);

  Result<const Expr *> body_res = ParseExpr(hint);
  if (!body_res) return body_res;

  return &builder_.getKeep(name, **expr_res, **body_res);
}

// <let> ::= "let" <identifier> "=" <type> <expr> <body>
Result<const Expr *> Parser::ParseLet(const Type *hint) {
  Consume(Token::TK_Let);

  Result<Token> res = lexer_.Lex();
  if (!res) return res;

  if (!res->isa(Token::TK_Identifier))
    return getDiag<const Expr *>(res->getStart())
           << "Expected an identifier but instead found `" << res->getChars()
           << "`";

  std::string_view name(res->getChars());

  Consume(Token::TK_Assign);

  Result<const Type *> type_res = ParseType();
  if (!type_res) return type_res;

  Result<const Expr *> expr_res = ParseExpr(type_res.get());
  if (!expr_res) return expr_res;

  const Let &let = builder_.getLet(name, *expr_res.get());
  RegisterLocalVar(name, let);

  Result<const Expr *> body_res = ParseExpr(hint);
  return body_res;
}

Result<const Expr *> Parser::ParseIdentifier(const Type *hint) {
  Result<Token> res = lexer_.Lex();
  if (!res) return res;

  assert(res->getKind() == Token::TK_Identifier);

  std::string_view name(res->getChars());
  if (!HasVar(name))
    return Result<const Expr *>::Diagnostic(lexer_.getInput(), res->getStart())
           << "Unknown variable `" << name << "`";

  // TODO: Compare the type against the hint if provided.

  return &LookupExpr(name);
}

Result<std::vector<const Expr *>> Parser::ParseCallArguments(
    const CallableType *callable_type) {
  Result<Token> res = lexer_.Peek();
  if (!res) return res;

  SourceLocation current = lexer_.getCurrentLoc();
  size_t arg_no = 0;

  std::vector<const Expr *> args;
  while (res->getKind() != Token::TK_End) {
    if (callable_type && arg_no >= callable_type->getNumArgs()) {
      return getDiag<std::vector<const Expr *>>(lexer_.getCurrentLoc())
             << "Call exceeds expected number of arguments; expected "
             << callable_type->getNumArgs() << " args for callable type "
             << callable_type->toString() << " but instead found `"
             << res->getChars() << "`";
    }

    // Parse an expression as an argument.
    Result<const Expr *> arg =
        ParseExpr(callable_type ? &callable_type->getArgType(arg_no) : nullptr);
    if (!arg) return arg;
    args.push_back(arg.get());

    res = lexer_.Peek();
    if (!res) return res;

    arg_no++;
  }

  if (callable_type && args.size() != callable_type->getNumArgs()) {
    return getDiag<std::vector<const Expr *>>(current)
           << "Expected " << callable_type->getNumArgs()
           << " arguments; instead found " << args.size()
           << " args for callbale type " << callable_type->toString();
  }

  return args;
}

Result<const Call *> Parser::ParseCall(const Type *hint) {
  Result<Token> tok = lexer_.Lex();
  assert(tok && (tok->isa(Token::TK_Call) || tok->isa(Token::TK_ImpureCall)));
  bool pure = tok->isa(Token::TK_Call);

  Result<Token> res = lexer_.Peek();
  if (!res) return res;

  if (res->getKind() == Token::TK_Write) {
    // Before we actually create the callee expression, attempt to deduce the
    // type of the callee by first getting the argument types.
    lexer_.Lex();  // Consume the identifier.

    auto args = ParseCallArguments();
    if (!args) return args;

    // Consume `end`.
    assert(lexer_.Peek()->getKind() == Token::TK_End);
    lexer_.Lex();

    if (args->size() != 2)
      return Result<const Call *>::BuildError()
             << res->getStart()
             << ": Expected `write` to have 2 arguments; instead found "
             << args->size();

    // TODO: Check the `write` argument types.
    // TODO: Check against the type hint here.

    const Write &write = builder_.getWrite(args->at(1)->getType());
    return &builder_.getCall(write, args.get(), pure);
  } else {
    SourceLocation callable_loc = res->getStart();

    Result<const Expr *> callable = ParseExpr();
    if (!callable) return callable;

    const Type &type = (*callable)->getType();
    if (!llvm::isa<CallableType>(type)) {
      return getDiag<const Call *>(callable_loc)
             << "Expected callable expression to be a callable type; instead "
                "found "
             << type.toString();
    }

    auto args = ParseCallArguments(&llvm::cast<CallableType>(type));
    if (!args) return args;

    // Consume `end`.
    assert(lexer_.Peek()->getKind() == Token::TK_End);
    lexer_.Lex();

    const auto *actual_ty =
        llvm::dyn_cast<CallableType>(&(*callable)->getType());
    if (!actual_ty) {
      return getDiag<const Call *>(callable_loc)
             << "Expected a callable type here; instead found "
             << (*callable)->getType().toString();
    }

    if (hint && !builder_.Equals(actual_ty->getReturnType(), *hint)) {
      return getDiag<const Call *>(callable_loc)
             << "Return type mismatch for call; found "
             << actual_ty->getReturnType().toString() << " but expected "
             << hint->toString();
    }

    if (actual_ty->getNumArgs() != args->size()) {
      return getDiag<const Call *>(callable_loc)
             << "Mismatch between number of arguments; found " << args->size()
             << " but expected " << actual_ty->getNumArgs();
    }

    for (size_t i = 0; i < args->size(); ++i) {
      if (!builder_.Equals(actual_ty->getArgType(i), args->at(i)->getType())) {
        return getDiag<const Call *>(callable_loc)
               << "Type mismatch for argument " << i << "; found "
               << args->at(i)->getType().toString() << " but expected "
               << actual_ty->getArgType(i).toString();
      }
    }

    return &builder_.getCall(*callable.get(), args.get(), pure);
  }
}

Result<const Int *> Parser::ParseInt() {
  Result<Token> res = lexer_.Lex();
  if (!res) return res;

  assert(res->isa(Token::TK_Int));

  return &builder_.getInt(std::stoi(std::string(res->getChars())));
}

Result<const Char *> Parser::ParseChar() {
  Result<Token> res = lexer_.Lex();
  assert(res && res->isa(Token::TK_Char));
  assert(res->getChars().size() == 3 && res->getChars().front() == '\'' &&
         res->getChars().back() == '\'');

  return &builder_.getChar(res->getChars().at(1));
}

Result<const Str *> Parser::ParseStr() {
  Result<Token> res = lexer_.Lex();
  if (!res) return res;

  if (res->getKind() != Token::TK_Str)
    return Result<const Str *>::BuildError()
           << res->getStart() << ": Expected a string; instead found `"
           << res->getChars() << "`";

  std::string_view chars(res->getChars());
  assert(chars.front() == '"' && chars.back() == '"');
  assert(chars.size() >= 2);

  return &builder_.getStr(chars.substr(1, chars.size() - 2));
}

Result<const Readc *> Parser::ParseReadc() {
  Consume(Token::TK_Readc);
  return &builder_.getReadc();
}

Result<const Zero *> Parser::ParseZero(const Type *hint) {
  if (!hint) {
    return getDiag<const Zero *>(lexer_.getCurrentLoc())
           << "Unable to determine type of `zero`";
  }
  Consume(Token::TK_Zero);
  return &builder_.getZero(*hint);
}

Result<const Write *> Parser::ParseWrite(const Type *hint) {
  Result<Token> res = lexer_.Lex();
  if (!res) return res;

  assert(res->isa(Token::TK_Write));

  if (!hint) {
    return Result<const Write *>::BuildError()
           << res->getStart()
           << ": Unable to deduce the second argument for `write`";
  }

  auto check_io_type = [](const CallableType *callable_ty) {
    if (!callable_ty->getReturnType().isNamedType("IO")) return false;

    if (callable_ty->getArgTypes().size() != 2) return false;

    if (!callable_ty->getArgType(0).isNamedType("IO")) return false;

    return true;
  };

  const auto *callable_ty = llvm::dyn_cast<CallableType>(hint);
  if (!callable_ty || !check_io_type(callable_ty)) {
    return Result<const Write *>::BuildError()
           << res->getStart() << ": Expected a callable type; instead found `"
           << hint->toString() << "`";
  }

  return &builder_.getWrite(callable_ty->getArgType(1));
}

// <composite> ::= "<" <expr>+ ">"
Result<const Composite *> Parser::ParseComposite() {
  Consume(Token::TK_LAngleBrack);

  std::vector<const Expr *> elems;
  Result<Token> next = lexer_.Peek();
  if (!next) return next;
  do {
    Result<const Expr *> expr = ParseExpr();
    if (!expr) return expr;

    elems.push_back(*expr);

    next = lexer_.Peek();
    if (!next) return next;
  } while (!next->isa(Token::TK_RAngleBrack));

  Consume(Token::TK_RAngleBrack);

  return &builder_.getComposite(elems);
}

// <set> ::= "SET" <expr> <expr> <expr>
//
// The first <expr> is the composite type being accessed. The second <expr> is
// the index. The third <expr> is the value being stored.
Result<const Set *> Parser::ParseSet() {
  Consume(Token::TK_SET);

  auto peek = lexer_.Peek();
  if (!peek) return peek;
  SourceLocation composite_loc = peek->getStart();
  Result<const Expr *> composite = ParseExpr();
  if (!composite) return composite;

  const Type &type = (*composite)->getType();
  if (!(llvm::isa<CompositeType>(type) || llvm::isa<ArrayType>(type))) {
    return getDiag<const Set *>(composite_loc)
           << "Expression is not a composite or array type";
  }

  Result<const Expr *> idx = ParseExpr();
  if (!idx) return idx;

  peek = lexer_.Peek();
  if (!peek) return peek;
  SourceLocation idx_loc = peek->getStart();
  if (!(*idx)->getType().isNamedType("int")) {
    return getDiag<const Set *>(idx_loc)
           << "Expression for index is not type `int`; instead is `"
           << (*idx)->getType().toString() << "`";
  }

  Result<const Expr *> store_val = ParseExpr();
  if (!store_val) return store_val;

  return &builder_.getSet(**composite, **idx, **store_val);
}

// <get> ::= "GET" <type> <expr> <expr>
//
// The <type> is the resulting type of the GET expression. The first <expr> is
// the composite type that is being accessed. The second <expr> is the index.
Result<const Get *> Parser::ParseGet() {
  Consume(Token::TK_GET);

  Result<Token> peek = lexer_.Peek();
  if (!peek) return peek;
  SourceLocation typeloc = peek->getStart();
  Result<const Type *> type_res = ParseType();
  if (!type_res) return type_res;

  const Type &type = **type_res;

  peek = lexer_.Peek();
  if (!peek) return peek;
  SourceLocation exprloc = peek->getStart();
  Result<const Expr *> expr = ParseExpr();
  if (!expr) return expr;
  const Type &expr_type = (*expr)->getType();
  if (!(llvm::isa<CompositeType>(expr_type) ||
        llvm::isa<ArrayType>(expr_type))) {
    return getDiag<const Get *>(exprloc)
           << "Expression is not a composite or array type";
  }

  Result<Token> i = lexer_.Peek();
  if (!i) return i;

  if (i->isa(Token::TK_Int)) {
    const Type *result_type;
    size_t idx = std::stoi(std::string(i->getChars()));
    if (const auto *comp_ty = llvm::dyn_cast<CompositeType>(&expr_type)) {
      if (idx >= comp_ty->getNumTypes()) {
        return getDiag<const Get *>(i->getStart())
               << "Index " << idx << " exceeds size of composite type which is "
               << comp_ty->getNumTypes();
      }
      result_type = &comp_ty->getTypeAt(idx);
    } else if (const auto *arr_ty = llvm::dyn_cast<ArrayType>(&expr_type)) {
      if (idx >= arr_ty->getNumElems()) {
        return getDiag<const Get *>(i->getStart())
               << "Index " << idx << " exceeds size of array type which is "
               << arr_ty->getNumElems();
      }
      result_type = &arr_ty->getElemType();
    } else {
      __builtin_trap();
    }

    if (!builder_.Equals(type, *result_type)) {
      return getDiag<const Get *>(typeloc)
             << "GET type mismatch; expected `" << type.toString()
             << "` but found `" << result_type->toString() << "`";
    }
  }

  Result<const Expr *> idx = ParseExpr();
  if (!idx) return idx;
  if (!(*idx)->getType().isNamedType("int")) {
    return getDiag<const Get *>(i->getStart())
           << "Expression for index is not type `int`; instead is `"
           << (*idx)->getType().toString() << "`";
  }

  return &builder_.getGet(type, **expr, **idx);
}

// <cast> ::= "CAST" <type> <expr>
Result<const Cast *> Parser::ParseCast() {
  Consume(Token::TK_CAST);
  Result<const Type *> type = ParseType();
  if (!type) return type;

  SourceLocation loc = lexer_.getCurrentLoc();

  Result<const Expr *> expr = ParseExpr();
  if (!expr) return expr;

  // Do some type checking.
  if ((*type)->isCharArray() && (*expr)->getType().isCharArray()) {
    const auto &to_type = llvm::cast<ArrayType>(**type);
    const auto &from_type = llvm::cast<ArrayType>((*expr)->getType());
    if (to_type.getNumElems() < from_type.getNumElems()) {
      // TODO: Would be nice to have a formal warning system also that doesn't
      // involve making an error result.
      Result<const Cast *>::Diagnostic warn(lexer_.getInput(), loc);
      warn << "Casting from a longer " << from_type.getNumElems()
           << " length char array type to a shorter " << to_type.getNumElems()
           << " length char array type truncates result";
      std::cerr << warn.get() << std::endl;
    }
  }

  if (builder_.Equals(**type, (*expr)->getType())) {
    Result<const Cast *>::Diagnostic warn(lexer_.getInput(), loc);
    warn << "Unnecessary cast here since types are the same";
    std::cerr << warn.get() << std::endl;
  }

  return &builder_.getCast(**type, **expr);
}

}  // namespace lang
