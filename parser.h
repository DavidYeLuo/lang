#ifndef PARSER_H_
#define PARSER_H_

#include <cassert>
#include <map>
#include <vector>

#include "ast.h"
#include "astbuilder.h"
#include "lang.h"
#include "lexer.h"

namespace lang {

class Parser {
 public:
  Parser(Lexer &lexer) : lexer_(lexer) {}

  using AST = std::vector<const Node *>;
  Result<const AST> Parse();
  Result<const Declare *> ParseDeclare() {
    auto res = ParseDeclareImpl();
    local_scope_.clear();
    return res;
  }
  Result<const Define *> ParseDefine() {
    auto res = ParseDefineImpl();
    local_scope_.clear();
    return res;
  }

  // Each of the ParseExpr functions takes an optional type hint that can be
  // used for checking or inferring types.
  Result<const Expr *> ParseExpr(const Type *hint = nullptr);
  Result<const Expr *> ParseExprImpl(const Type *hint);
  // ParseCallable takes an optional name that indicates this is part of a high
  // level `define`.
  Result<const Callable *> ParseCallable(const Type *hint = nullptr,
                                         std::string_view *name = nullptr);
  Result<const Call *> ParseCall(const Type *hint = nullptr);
  Result<const Zero *> ParseZero(const Type *hint);
  Result<const Write *> ParseWrite(const Type *hint = nullptr);
  Result<const Readc *> ParseReadc();
  Result<const Str *> ParseStr();
  Result<const Char *> ParseChar();
  Result<const Int *> ParseInt();
  // Result<const None *> ParseNone();
  Result<const Cast *> ParseCast();
  Result<const Get *> ParseGet();
  Result<const Set *> ParseSet();
  Result<const Composite *> ParseComposite();
  Result<const Expr *> ParseLet(const Type *hint = nullptr);
  Result<const Keep *> ParseKeep(const Type *hint = nullptr);
  Result<const If *> ParseIf(const Type *hint = nullptr);
  Result<const BinOp *> ParseBinOp(const Type *hint = nullptr);
  Result<const Expr *> ParseIdentifier(const Type *hint = nullptr);

  Result<const Type *> ParseType();

 private:
  Result<const Define *> ParseDefineImpl();
  Result<const Declare *> ParseDeclareImpl();
  Result<std::vector<const Expr *>> ParseCallArguments(
      const CallableType *callable_type = nullptr);

  void Consume(Token::TokenKind kind) {
    [[maybe_unused]] auto res = lexer_.Lex();
    assert(res && res->isa(kind));
  }

  template <typename T>
  Result<T>::Diagnostic getDiag(const SourceLocation &loc) const {
    typename Result<T>::Diagnostic diag(lexer_.getInput(), loc);
    return diag;
  }

  void Warn(const SourceLocation &loc) const;

  void RegisterLocalVar(std::string_view var, const Expr &expr) {
    assert(!HasVar(var));
    local_scope_.try_emplace(std::string(var), &expr);
  }

  void RegisterGlobalVar(std::string_view var, const Expr &expr) {
    assert(!HasVar(var));
    global_scope_.try_emplace(std::string(var), &expr);
  }

  const Expr &LookupExpr(std::string_view name) const {
    auto local = local_scope_.find(name);
    if (local != local_scope_.end()) return *local->second;
    return *global_scope_.find(name)->second;
  }

  bool HasVar(std::string_view name) const {
    return local_scope_.find(name) != local_scope_.end() ||
           global_scope_.find(name) != global_scope_.end();
  }

  Lexer &lexer_;
  lang::ASTBuilder builder_;
  std::map<std::string, const Expr *, std::less<>> local_scope_;
  std::map<std::string, const Expr *, std::less<>> global_scope_;
};

}  // namespace lang

#endif  // PARSER_H_
