#include "astcloner.h"

#include <format>
#include <iostream>
#include <utility>

#include "astdumper.h"

namespace lang {

Node &ASTCloner::Visit(const Declare &decl) {
  if (decl.isDefinition())
    return builder_.getDeclare(decl.getStart(), decl.getName(),
                               VisitExpr(decl.getBody()), decl.isBuiltinWrite(),
                               decl.isCDecl());
  else
    return builder_.getDeclare(decl.getStart(), decl.getName(), decl.getType(),
                               decl.isBuiltinWrite(), decl.isCDecl());
}

Node &ASTCloner::Visit(const Composite &comp) {
  return builder_.getComposite(comp.getStart(),
                               CloneExprVector(comp.getElems()));
}

Node &ASTCloner::Visit(const Get &get) {
  return builder_.getGet(get.getStart(), get.getType(),
                         VisitExpr(get.getExpr()), VisitExpr(get.getIdx()));
}

Node &ASTCloner::Visit(const Set &set) {
  return builder_.getSet(set.getStart(), VisitExpr(set.getExpr()),
                         VisitExpr(set.getIdx()), VisitExpr(set.getStore()));
}

Node &ASTCloner::Visit(const AmbiguousCall &call) {
  std::vector<Expr *> args = GetResolvedCallArgs(call.getArgs());
  auto found = std::find_if(
      call.getFuncs().begin(), call.getFuncs().end(), [&args](const Expr *e) {
        const auto &callable_ty = llvm::cast<CallableType>(e->getType());
        return callable_ty.ArgumentTypesMatch(args);
      });
  assert(found != call.getFuncs().end() &&
         "Could not find a callable where the arguments can apply.");
  assert(std::none_of(found + 1, call.getFuncs().end(),
                      [&args](const Expr *func) {
                        const auto &callable_ty =
                            llvm::cast<CallableType>(func->getType());
                        return callable_ty.ArgumentTypesMatch(args);
                      }) &&
         "Cannot resolve which function to call from arguments");

  // TODO: Do we always want to return a different call type for the cloner?
  return builder_.getCall(call.getStart(), **found, args);
}

Node &ASTCloner::Visit(const Call &call) {
  Expr &func = VisitExpr(call.getFunc());
  std::vector<Expr *> args = GetResolvedCallArgs(call.getArgs());
  return builder_.getCall(call.getStart(), func, args, call.isPure());
}

Node &ASTCloner::Visit(const Keep &keep) {
  return builder_.getKeep(keep.getStart(), keep.getName(),
                          VisitExpr(keep.getExpr()), VisitExpr(keep.getBody()));
}

Node &ASTCloner::Visit(const Callable &callable) {
  std::vector<const Type *> arg_types =
      callable_type_replacement_.contains(&callable)
          ? callable_type_replacement_.at(&callable)
          : callable.getType().getArgTypes();

  std::vector<SourceLocation> arglocs(callable.getArgLocs());
  if (arglocs.size() < arg_types.size())
    arglocs.resize(arg_types.size());

  std::vector<std::string> argnames(callable.getArgNames());
  for (size_t i = argnames.size(); i < arg_types.size(); ++i) {
    argnames.push_back(std::format("arg{}", i));
  }

  Callable &newcallable = builder_.getCallable(
      callable.getStart(), callable.getType().getReturnType(), arglocs,
      argnames, arg_types);

  seen_callables_[&callable] = &newcallable;

  for (size_t i = 0; i < callable.getNumArgs(); ++i)
    new_args_[&callable.getArg(i)] = &newcallable.getArg(i);

  if (callable_type_replacement_.contains(&callable) &&
      callable.getType().isGenericRemainingCallable()) {
    assert(newcallable.getNumArgs() >= callable.getNumArgs());
    size_t start = callable.getNumArgs() - 1;
    remaining_arg_replacements_.try_emplace(
        &callable.getLastArg(), newcallable.getArgs().begin() + start,
        newcallable.getArgs().end());
  }

  newcallable.setBody(VisitExpr(callable.getBody()));
  return newcallable;
}

Node &ASTCloner::Visit(const Arg &arg) { return *new_args_.at(&arg); }

Node &ASTCloner::Visit(const Let &let) {
  return builder_.getLet(let.getStart(), let.getName(),
                         VisitExpr(let.getExpr()));
}

Node &ASTCloner::Visit(const Readc &readc) {
  return builder_.getReadc(readc.getStart());
}

Node &ASTCloner::Visit(const Str &str) {
  return builder_.getStr(str.getStart(), str.get());
}

Node &ASTCloner::Visit(const Bool &b) {
  return builder_.getBool(b.getStart(), b.get());
}

Node &ASTCloner::Visit(const Char &c) {
  return builder_.getChar(c.getStart(), c.getChar());
}

Node &ASTCloner::Visit(const If &if_expr) {
  return builder_.getIf(
      if_expr.getStart(), if_expr.getType(), VisitExpr(if_expr.getCond()),
      VisitExpr(if_expr.getIf()), VisitExpr(if_expr.getElse()));
}

Node &ASTCloner::Visit(const BinOp &binop) {
  return builder_.getBinOp(VisitExpr(binop.getLHS()), VisitExpr(binop.getRHS()),
                           binop.getType(), binop.getOp());
}

Node &ASTCloner::Visit(const Int &i) {
  return builder_.getInt(i.getStart(), i.getInt());
}

Node &ASTCloner::Visit(const Zero &zero) {
  return builder_.getZero(zero.getStart(), zero.getType());
}

Node &ASTCloner::Visit(const Cast &cast) {
  return builder_.getCast(cast.getStart(), cast.getType(),
                          VisitExpr(cast.getExpr()));
}

}  // namespace lang
