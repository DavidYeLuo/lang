#ifndef ASTCLONER_H_
#define ASTCLONER_H_

#include <algorithm>
#include <functional>
#include <map>
#include <set>

#include "ast.h"
#include "astbuilder.h"
#include "astvisitor.h"

namespace lang {

class ASTCloner : public ConstASTVisitor<Node &> {
 public:
  ASTCloner(ASTBuilder &builder) : builder_(builder) {}

  Node &Clone(const Node &node) {
    seen_callables_.clear();
    return Visit(node);
  }

  void AddCallableTypeReplacement(const Callable &callable,
                                  const std::vector<const Type *> &argtypes) {
    assert(!callable_type_replacement_.contains(&callable));
    callable_type_replacement_[&callable] = argtypes;
  }

#define NODE(name) Node &Visit(const name &) override;
#include "nodes.def"

 private:
  Node &Visit(const Node &node) override {
    if (const auto *callable = llvm::dyn_cast<Callable>(&node)) {
      auto found_callable = seen_callables_.find(&node);
      if (found_callable != seen_callables_.end())
        return *found_callable->second;
    }

    return ConstASTVisitor::Visit(node);
  }

  Expr &VisitExpr(const Expr &expr) { return llvm::cast<Expr>(Visit(expr)); }

  std::vector<Expr *> CloneExprVector(const std::vector<Expr *> &container) {
    std::vector<Expr *> clones(container.size());
    std::transform(container.begin(), container.end(), clones.begin(),
                   [&](const Expr *e) { return &VisitExpr(*e); });
    return clones;
  }

  std::vector<Expr *> GetResolvedCallArgs(const std::vector<Expr *> &callargs) {
    std::vector<Expr *> args;
    for (const Expr *e : callargs) {
      if (const auto *arg = llvm::dyn_cast<Arg>(e)) {
        if (remaining_arg_replacements_.contains(arg)) {
          for (Arg *a : remaining_arg_replacements_.at(arg))
            args.push_back(a);
          continue;
        }
      }
      args.push_back(&VisitExpr(*e));
    }
    return args;
  }

  ASTBuilder &builder_;
  std::map<const Arg *, Arg *> new_args_;
  std::map<const Callable *, std::vector<const Type *>>
      callable_type_replacement_;
  std::map<const Arg *, std::vector<Arg *>> remaining_arg_replacements_;

  // Used to prevent infinite recursion when cloning self-referential callables.
  std::map<const Callable *, Callable *, std::less<>> seen_callables_;
};

}  // namespace lang

#endif  // ASTCLONER_H_
