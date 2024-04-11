#include "genericresolver.h"

#include <iostream>
#include <sstream>
#include <utility>

#include "astbuilder.h"
#include "astdumper.h"
#include "lang.h"

namespace lang {

Declare &GenericResolver::Resolve() {
  const CallableType &callable_ty =
      llvm::cast<CallableType>(generic_decl_.getType());
  const CallableType &newdecl_ty =
      builder_.getCallableType(callable_ty.getReturnType(), arg_types_);
  assert(!newdecl_ty.isGeneric());

  // First see if we can find an already existing non-generic decl that fits
  // this type.
  auto existing_decls = mod_.getDeclares(generic_decl_.getName());
  auto found = std::find_if(
      existing_decls.begin(), existing_decls.end(), [&](const Declare *d) {
        return !d->getType().isGeneric() && d->getType() == newdecl_ty;
      });
  if (found != existing_decls.end())
    return **found;

  assert(!newdecl_ &&
         "This must be the first time we create the new declaration");
  newdecl_ = &builder_.getDeclare(
      generic_decl_.getStart(), generic_decl_.getName(), newdecl_ty,
      generic_decl_.isBuiltinWrite(), generic_decl_.isCDecl());
  mod_.AddDeclaration(generic_decl_.getName(), *newdecl_);

  if (generic_decl_.isDefinition()) {
    std::vector<SourceLocation> arglocs(getOldCallable().getArgLocs());
    if (arglocs.size() < arg_types_.size())
      arglocs.resize(arg_types_.size());

    std::vector<std::string> argnames(getOldCallable().getArgNames());
    for (size_t i = argnames.size(); i < arg_types_.size(); ++i)
      argnames.push_back(Concat("arg", i));

    Callable &newcallable = builder_.getCallable(getOldCallable().getStart(),
                                                 callable_ty.getReturnType(),
                                                 arglocs, argnames, arg_types_);

    newdecl_->setBody(newcallable);
    cached_exprs_[&getOldCallable()] = &newcallable;

    newcallable.setBody(VisitExpr(getOldCallable().getBody()));
  }

  return *newdecl_;
}

Node &GenericResolver::Visit(Declare &decl) {
  // This is a reference to an existing decl. If we got here through calls, the
  // calls themselves should habdle this.
  assert(!decl.getType().isGeneric() &&
         "Generic declarations should've been handled by the call visitors.");
  return decl;
}

Node &GenericResolver::Visit(Composite &comp) {
  return builder_.getComposite(comp.getStart(),
                               CloneExprVector(comp.getElems()));
}

Node &GenericResolver::Visit(Get &get) {
  return builder_.getGet(get.getStart(), get.getType(),
                         VisitExpr(get.getExpr()), VisitExpr(get.getIdx()));
}

Node &GenericResolver::Visit(Set &set) {
  Expr &e = VisitExpr(set.getExpr());
  return builder_.getSet(set.getStart(), e, VisitExpr(set.getIdx()),
                         VisitExpr(set.getStore()));
}

Node &GenericResolver::Visit(AmbiguousCall &call) {
  std::vector<Expr *> args = GetResolvedCallArgs(call.getArgs());
  auto found = std::find_if(
      call.getFuncs().begin(), call.getFuncs().end(), [&args](Expr *e) {
        const auto &callable_ty = llvm::cast<CallableType>(e->getType());
        return callable_ty.CanApplyArgs(args);
      });
  assert(found != call.getFuncs().end() &&
         "Could not find a callable where the arguments can apply.");
  [[maybe_unused]] bool found_other =
      std::none_of(found + 1, call.getFuncs().end(), [&args](Expr *func) {
        const auto &callable_ty = llvm::cast<CallableType>(func->getType());
        return callable_ty.CanApplyArgs(args);
      });
  assert(found_other &&
         "Cannot resolve which function to call from arguments. Found another "
         "function that can match the call arguments.");

  // Note this might result in another call to a generic function, so we need to
  // Visit this again.
  return VisitWithEvaluatedArgs(call.getStart(), **found, args,
                                /*is_pure=*/false);
}

// This accounts for generic remaining types.
std::vector<Expr *> GenericResolver::GetResolvedCallArgs(
    const std::vector<Expr *> &callargs) {
  std::vector<Expr *> args;
  for (Expr *e : callargs) {
    if (auto *arg = llvm::dyn_cast<Arg>(e)) {
      if (llvm::isa<GenericRemainingType>(arg->getType())) {
        assert(arg == &getOldCallable().getLastArg() &&
               "We expect the GENERIC_REMAINING argument to be last in the "
               "callable.");
        for (size_t i = arg->getArgNo(); i < getNewCallable().getNumArgs();
             ++i) {
          Arg &newarg = getNewCallable().getArg(i);
          args.push_back(&newarg);
        }
        continue;
      }
    }

    // If this arg is a regular generic type, the Visit(Arg &) method will
    // handle it.
    args.push_back(&VisitExpr(*e));
  }

  assert(std::none_of(args.begin(), args.end(),
                      [](Expr *e) { return e->getType().isGeneric(); }));

  return args;
}

Call &GenericResolver::VisitWithEvaluatedArgs(
    const SourceLocation &loc, Expr &func,
    const std::vector<Expr *> &evaluated_args, bool pure) {
  std::vector<const Type *> arg_types(evaluated_args.size());
  std::transform(evaluated_args.begin(), evaluated_args.end(),
                 arg_types.begin(), [](Expr *e) { return &e->getType(); });

  Expr &newfunc = [&]() -> Expr & {
    if (Declare *decl = ExtractDeclare(func)) {
      if (decl->getType().isGenericCallable()) {
        return GenericResolver(mod_, builder_, *decl, arg_types).Resolve();
      }
    }

    return VisitExpr(func);
  }();

  return builder_.getCall(loc, newfunc, evaluated_args, pure);
}

Node &GenericResolver::Visit(Call &call) {
  std::vector<Expr *> args = GetResolvedCallArgs(call.getArgs());
  return VisitWithEvaluatedArgs(call.getStart(), call.getFunc(), args,
                                call.isPure());
}

Node &GenericResolver::Visit(Keep &keep) {
  return builder_.getKeep(keep.getStart(), keep.getName(),
                          VisitExpr(keep.getExpr()), VisitExpr(keep.getBody()));
}

Node &GenericResolver::Visit(Callable &callable) {
  UNREACHABLE(
      "TODO: Implement this. I think we should only get here from non-global "
      "callables declared within a function.");
}

Node &GenericResolver::Visit(Arg &arg) {
  return getNewCallable().getArg(arg.getArgNo());
}

Node &GenericResolver::Visit(Let &let) {
  return builder_.getLet(let.getStart(), let.getName(),
                         VisitExpr(let.getExpr()));
}

Node &GenericResolver::Visit(Readc &readc) {
  return builder_.getReadc(readc.getStart());
}

Node &GenericResolver::Visit(Str &str) {
  return builder_.getStr(str.getStart(), str.get());
}

Node &GenericResolver::Visit(Bool &b) {
  return builder_.getBool(b.getStart(), b.get());
}

Node &GenericResolver::Visit(Char &c) {
  return builder_.getChar(c.getStart(), c.getChar());
}

Node &GenericResolver::Visit(If &if_expr) {
  return builder_.getIf(
      if_expr.getStart(), if_expr.getType(), VisitExpr(if_expr.getCond()),
      VisitExpr(if_expr.getIf()), VisitExpr(if_expr.getElse()));
}

Node &GenericResolver::Visit(BinOp &binop) {
  return builder_.getBinOp(VisitExpr(binop.getLHS()), VisitExpr(binop.getRHS()),
                           binop.getType(), binop.getOp());
}

Node &GenericResolver::Visit(Int &i) {
  return builder_.getInt(i.getStart(), i.getInt());
}

Node &GenericResolver::Visit(Zero &zero) {
  return builder_.getZero(zero.getStart(), zero.getType());
}

Node &GenericResolver::Visit(Cast &cast) {
  return builder_.getCast(cast.getStart(), cast.getType(),
                          VisitExpr(cast.getExpr()));
}

}  // namespace lang
