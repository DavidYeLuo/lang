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
  Parser(Lexer &lexer);

  using AST = std::vector<const Node *>;
  Result<const AST> Parse();
  Result<const Declare *> ParseDeclare() {
    auto res = ParseDeclareImpl();
    local_scope_.clear();
    local_callable_scope_.clear();
    return res;
  }
  Result<const Define *> ParseDefine() {
    auto res = ParseDefineImpl();
    local_scope_.clear();
    local_callable_scope_.clear();
    return res;
  }

  // Each of the ParseExpr functions takes an optional type hint that can be
  // used for checking or inferring types.
  Result<const Expr *> ParseExpr(const Type *hint = nullptr);
  Result<const Expr *> ParseExprImpl(const Type *hint);
  // ParseCallable takes an optional name that indicates this is part of a high
  // level `define`.
  Result<const Callable *> ParseCallable(std::string_view *name = nullptr,
                                         const Type *hint = nullptr);
  Result<const Call *> ParseCall(const Type *return_type_hint = nullptr);
  Result<const Zero *> ParseZero(const Type *hint);
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
      Token::TokenKind end_tok, const CallableType *callable_type = nullptr);
  Result<const Call *> ParseCallImpl(/*const Expr &callee,*/
                                     bool is_new_call_syntax,
                                     const Type *return_type_hint = nullptr);
  Result<std::pair<const Expr *, std::vector<const Expr *>>>
  ParseCallableFromArgs(const SourceLocation &call_loc, std::string_view name,
                        Token::TokenKind end_tok, const Type &return_type_hint);
  Result<const Call *> getAndCheckCall(const SourceLocation &call_loc,
                                       const Expr &maybe_callable,
                                       const std::vector<const Expr *> &args,
                                       bool pure,
                                       const Type *return_type_hint = nullptr);

  void Consume(Token::TokenKind kind) {
    [[maybe_unused]] auto res = lexer_.Lex();
    assert(res && res->isa(kind));
  }

  // Simple helper for creating an error result. Since this is an error, we
  // don't really need to provide a type, so let's just use nullptr_t.
  Diagnostic getDiag(const SourceLocation &loc) const {
    return Diagnostic(lexer_.getInput(), loc);
  }

  Diagnostic getExpectedCallableDiag(const SourceLocation &loc,
                                     const Type &found) const {
    return std::move(getDiag(loc) << "Expected a callable type; instead found `"
                                  << found.toString() << "`");
  }

  void Warn(const SourceLocation &loc) const;

  void RegisterLocalVar(std::string_view var, const Expr &expr) {
    if (const auto *callable_ty =
            llvm::dyn_cast<CallableType>(&expr.getType())) {
      assert(!HasVar(var, *callable_ty));
      local_callable_scope_[std::string(var)].push_back(
          std::make_pair(&expr, callable_ty));
    } else {
      assert(!HasVar(var));
      local_scope_.try_emplace(std::string(var), &expr);
    }
  }

  void RegisterGlobalVar(std::string_view var, const Expr &expr) {
    assert(!HasVar(var));
    global_scope_.try_emplace(std::string(var), &expr);
  }

  void RegisterGlobalCallableVar(std::string_view var,
                                 const Callable &callable) {
    const auto &ty = llvm::cast<CallableType>(callable.getType());
    assert(!HasVar(var, ty));
    global_callable_scope_[std::string(var)].push_back(
        std::make_pair(&callable, &ty));
  }

  const Expr &LookupCallable(std::string_view name,
                             const CallableType &type) const {
    auto found_glob = global_callable_scope_.find(name);
    if (found_glob != global_callable_scope_.end()) {
      for (const auto &p : found_glob->second) {
        if (builder_.CallableTypesMatch(type, *p.second))
          return *p.first;
      }
    }
    auto found_loc = local_callable_scope_.find(name);
    if (found_loc != local_callable_scope_.end()) {
      for (const auto &p : found_loc->second) {
        if (builder_.CallableTypesMatch(type, *p.second))
          return *p.first;
      }
    }
    UNREACHABLE("Couldn't lookup function `%s`", name.data());
  }

  std::vector<const Expr *> getPossibleCallables(
      std::string_view name,
      const std::vector<const Type *> *arg_types = nullptr) const {
    std::vector<const Expr *> exprs;
    auto found_glob = global_callable_scope_.find(name);
    if (found_glob != global_callable_scope_.end()) {
      for (const auto &p : found_glob->second) {
        if (!arg_types ||
            builder_.ArgumentTypesMatch(p.second->getArgTypes(), *arg_types))
          exprs.push_back(p.first);
      }
    }
    auto found_loc = local_callable_scope_.find(name);
    if (found_loc != local_callable_scope_.end()) {
      for (const auto &p : found_loc->second) {
        if (!arg_types ||
            builder_.ArgumentTypesMatch(p.second->getArgTypes(), *arg_types))
          exprs.push_back(p.first);
      }
    }
    return exprs;
  }

  size_t getNumPossibleCallables(
      std::string_view name,
      const std::vector<const Type *> *arg_types = nullptr) const {
    return getPossibleCallables(name, arg_types).size();
  }

  const Expr &LookupSingleExpr(std::string_view name) const {
    auto local = local_scope_.find(name);
    if (local != local_scope_.end())
      return *local->second;
    auto global = global_scope_.find(name);
    if (global != global_scope_.end())
      return *global->second;

    // Return a callable expression if it has one type associated with it.
    auto found_glob = global_callable_scope_.find(name);
    if (found_glob != global_callable_scope_.end() &&
        found_glob->second.size() == 1)
      return *found_glob->second.front().first;
    auto found_loc = local_callable_scope_.find(name);
    if (found_loc != local_callable_scope_.end() &&
        found_loc->second.size() == 1)
      return *found_loc->second.front().first;

    UNREACHABLE("Couldn't lookup expression `%s`", name.data());
  }

  bool HasVar(std::string_view name, const CallableType &type) {
    auto found_glob = global_callable_scope_.find(name);
    if (found_glob != global_callable_scope_.end()) {
      return std::any_of(found_glob->second.begin(), found_glob->second.end(),
                         [&](const auto &p) {
                           return builder_.CallableTypesMatch(type, *p.second);
                         });
    }

    auto found_loc = local_callable_scope_.find(name);
    if (found_loc != local_callable_scope_.end()) {
      return std::any_of(found_loc->second.begin(), found_loc->second.end(),
                         [&](const auto &p) {
                           return builder_.CallableTypesMatch(type, *p.second);
                         });
    }

    return false;
  }

  bool HasVar(std::string_view name) const {
    return local_scope_.find(name) != local_scope_.end() ||
           global_scope_.find(name) != global_scope_.end() ||
           global_callable_scope_.find(name) != global_callable_scope_.end() ||
           local_callable_scope_.find(name) != local_callable_scope_.end();
  }

  Lexer &lexer_;
  lang::ASTBuilder builder_;
  std::map<std::string, const Expr *, std::less<>> local_scope_;
  std::map<std::string, const Expr *, std::less<>> global_scope_;
  std::map<std::string,
           std::vector<std::pair<const Callable *, const CallableType *>>,
           std::less<>>
      global_callable_scope_;
  std::map<std::string,
           std::vector<std::pair<const Expr *, const CallableType *>>,
           std::less<>>
      local_callable_scope_;
};

}  // namespace lang

#endif  // PARSER_H_
