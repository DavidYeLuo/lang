#ifndef GENERICRESOLVER_H_
#define GENERICRESOLVER_H_

#include <algorithm>
#include <functional>
#include <map>
#include <set>
#include <span>

#include "ast.h"
#include "astbuilder.h"
#include "astvisitor.h"

namespace lang {

class GenericResolver : public NonConstASTVisitor<Node &> {
 public:
  GenericResolver(Module &mod, ASTBuilder &builder, Declare &generic_decl,
                  const std::vector<const Type *> &arg_types)
      : mod_(mod),
        builder_(builder),
        generic_decl_(generic_decl),
        arg_types_(arg_types) {
    assert(generic_decl_.getType().isGenericCallable());
    assert(std::none_of(arg_types.begin(), arg_types.end(),
                        [](const Type *const t) { return t->isGeneric(); }));
  }

  Declare &Resolve();

#define NODE(name) Node &Visit(name &) override;
#include "nodes.def"

 private:
  Node &Visit(Node &node) override { return NonConstASTVisitor::Visit(node); }

  Expr &VisitExpr(Expr &expr) {
    auto found = cached_exprs_.find(&expr);
    if (found == cached_exprs_.end()) {
      Expr &newexpr = llvm::cast<Expr>(Visit(expr));
      cached_exprs_[&expr] = &newexpr;
      return newexpr;
    }
    return *found->second;
  }

  std::vector<Expr *> CloneExprVector(const std::vector<Expr *> &container) {
    std::vector<Expr *> clones(container.size());
    std::transform(container.begin(), container.end(), clones.begin(),
                   [&](Expr *e) { return &VisitExpr(*e); });
    return clones;
  }

  std::vector<Expr *> GetResolvedCallArgs(const std::vector<Expr *> &callargs);

  Call &VisitWithEvaluatedArgs(const SourceLocation &loc, Expr &func,
                               const std::vector<Expr *> &evaluated_args,
                               bool pure);

  Callable &getNewCallable() const {
    return llvm::cast<Callable>(newdecl_->getBody());
  }

  Callable &getOldCallable() const {
    return llvm::cast<Callable>(generic_decl_.getBody());
  }

  Module &mod_;
  ASTBuilder &builder_;
  Declare &generic_decl_;
  const std::vector<const Type *> &arg_types_;
  Declare *newdecl_ = nullptr;
  // TODO: Rather than having to do caching for each ast traversal, perhaps the
  // astbuilder should be in charge of caching. A reference to the exact same
  // expression can be returned for the exact same input arguments.
  std::map<Expr *, Expr *> cached_exprs_;
};

inline Declare *ExtractDeclare(Expr &expr) {
  if (auto *decl = llvm::dyn_cast<Declare>(&expr))
    return decl;
  else if (auto *let = llvm::dyn_cast<Let>(&expr))
    return ExtractDeclare(let->getExpr());
  // Cannot get a declare from this.
  return nullptr;
}

}  // namespace lang

#endif  // GENERICRESOLVER_H_
