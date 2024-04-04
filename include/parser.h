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

  Result<lang::Module *> Parse();
  Result<Declare *> ParseDeclare() {
    auto res = ParseDeclareImpl();
    local_scope_.clear();
    local_callable_scope_.clear();
    return res;
  }
  Result<Declare *> ParseDefine() {
    auto res = ParseDefineImpl();
    if (res) {
      assert((*res)->isDefinition());
    }
    local_scope_.clear();
    local_callable_scope_.clear();
    return res;
  }

  // Each of the ParseExpr functions takes an optional type hint that can be
  // used for checking or inferring types.
  Result<Expr *> ParseExpr(const Type *hint = nullptr);
  Result<Expr *> ParseExprImpl(const Type *hint);
  Result<Callable *> ParseCallable();
  // TODO: This can return either a `Call` or `AmbiguousCall`. We should
  // probably consolidate them.
  Result<Expr *> ParseCall(const Type *return_type_hint = nullptr);
  Result<Zero *> ParseZero(const Type *hint);
  Result<Readc *> ParseReadc();
  Result<Str *> ParseStr();
  Result<Char *> ParseChar();
  Result<Int *> ParseInt();
  // Result<const None *> ParseNone();
  Result<Cast *> ParseCast();
  Result<Get *> ParseGet();
  Result<Set *> ParseSet();
  Result<Composite *> ParseComposite();
  Result<Expr *> ParseLet(const Type *hint = nullptr);
  Result<Keep *> ParseKeep(const Type *hint = nullptr);
  Result<If *> ParseIf(const Type *hint = nullptr);
  Result<BinOp *> ParseBinOp(const Type *hint = nullptr);
  Result<Expr *> ParseIdentifier(const Type *hint = nullptr);
  Result<const Type *> ParseType(bool parse_callable_names = false);

 private:
  Result<Declare *> ParseDefineImpl();
  Result<Declare *> ParseDeclareImpl();
  Result<const std::vector<Expr *>> ParseCallArguments(
      Token::TokenKind end_tok, const CallableType *callable_type = nullptr);

  struct CallableResults {
    std::vector<Expr *> possible_callables;
    std::vector<Expr *> args;
  };
  Result<CallableResults> ParseCallableFromArgs(
      const SourceLocation &call_loc, std::string_view name,
      Token::TokenKind end_tok, const Type *return_type_hint = nullptr);
  Result<Call *> getAndCheckCall(const SourceLocation &call_loc,
                                 Expr &maybe_callable,
                                 const std::vector<Expr *> &args, bool pure,
                                 const Type *return_type_hint = nullptr);

  Result<Callable *> ParseCallableHead();
  Result<Callable *> ParseCallableBody(Callable &callable_head);

  void Consume(Token::TokenKind kind) {
    [[maybe_unused]] auto res = lexer_.Lex();
    assert(res && res->isa(kind));
  }

  Diagnostic getErrorDiag() const { return Diagnostic(lexer_.getInput()); }

  Diagnostic getExpectedCallableDiag(const SourceLocation &loc,
                                     const Type &found) const {
    Diagnostic diag(getErrorDiag());
    diag << loc << ": Expected a callable type; instead found `"
         << found.toString() << "`";
    return diag;
  }

  Diagnostic getAmbiguousCallDiag(
      const SourceLocation &loc, std::string_view name,
      const std::vector<Expr *> &possible_callables,
      const std::vector<const Type *> *maybe_argtypes) const {
    Diagnostic diag(getErrorDiag());
    diag << loc << ": Ambiguous call to `" << name << "`";
    if (maybe_argtypes) {
      diag << " with arg types `";
      for (const Type *type : *maybe_argtypes)
        diag << type->toString() << " ";
      diag << "`";
    }
    for (const Expr *expr : possible_callables)
      diag << DumpLine{expr->getStart()} << "\n";
    return diag;
  }

  void Warn(const SourceLocation &loc) const;

  void RegisterLocalVar(std::string_view var, Expr &expr) {
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

  Expr &LookupCallable(std::string_view name,
                       const std::vector<const Type *> &argtypes) const {
    auto callables = getPossibleCallables(name, &argtypes);
    assert(callables.size() == 1);
    return *callables.front();
  }

  Expr &LookupCallable(std::string_view name, const CallableType &type) const {
    auto callables = getPossibleCallables(name, &type.getArgTypes());
    assert(callables.size() == 1);
    assert(type.getReturnType() ==
           callables.front()->getType().getReturnType());
    return *callables.front();
  }

  std::vector<Expr *> getPossibleCallables(
      std::string_view name,
      const std::vector<const Type *> *arg_types = nullptr) const {
    std::vector<Expr *> exprs;
    auto found_loc = local_callable_scope_.find(name);
    if (found_loc != local_callable_scope_.end()) {
      for (const auto &p : found_loc->second) {
        if (!arg_types ||
            ArgumentTypesMatch(p.second->getArgTypes(), *arg_types))
          exprs.push_back(p.first);
      }
    }

    auto decls = module_.getDeclares(name);
    for (Declare *decl : decls) {
      if (const auto *ty = llvm::dyn_cast<CallableType>(&decl->getType())) {
        if (!arg_types || ArgumentTypesMatch(ty->getArgTypes(), *arg_types))
          exprs.push_back(decl);
      }
    }

    return exprs;
  }

  size_t getNumPossibleCallables(
      std::string_view name,
      const std::vector<const Type *> *arg_types = nullptr) const {
    return getPossibleCallables(name, arg_types).size();
  }

  Expr &LookupSingleExpr(std::string_view name) const {
    auto local = local_scope_.find(name);
    if (local != local_scope_.end())
      return *local->second;

    // Return a callable expression if it has one type associated with it.
    auto callables = getPossibleCallables(name);
    if (callables.size() == 1)
      return *callables.front();

    UNREACHABLE("Couldn't lookup single expression for `%s`", name.data());
  }

  bool HasVar(std::string_view name, const CallableType &type) {
    return getNumPossibleCallables(name, &type.getArgTypes());
  }

  bool HasVar(std::string_view name) const {
    return local_scope_.find(name) != local_scope_.end() ||
           getNumPossibleCallables(name) > 0;
  }

  Lexer &lexer_;
  lang::Module module_;
  lang::ASTBuilder builder_;
  std::map<std::string, Expr *, std::less<>> local_scope_;
  std::map<std::string, std::vector<std::pair<Expr *, const CallableType *>>,
           std::less<>>
      local_callable_scope_;
};

}  // namespace lang

#endif  // PARSER_H_
