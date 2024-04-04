#ifndef ASTBUILDER_H_
#define ASTBUILDER_H_

#include <functional>
#include <map>
#include <string_view>

#include "ast.h"
#include "lang.h"
#include "llvm/Support/Casting.h"

namespace lang {

// This is a helper class for constructing the AST that takes care of memory
// management.
class ASTBuilder {
 public:
  Str &getStr(const SourceLocation &start, std::string_view str) {
    return llvm::cast<Str>(
        *nodes_.emplace_back(new Str(start, str, getCharArrayType(str))));
  }

  Char &getChar(const SourceLocation &start, char c) {
    return llvm::cast<Char>(
        *nodes_.emplace_back(new Char(start, c, getCharType())));
  }

  Int &getInt(const SourceLocation &start, int i) {
    return llvm::cast<Int>(
        *nodes_.emplace_back(new Int(start, i, getIntType())));
  }

  Bool &getBool(const SourceLocation &start, bool b) {
    return llvm::cast<Bool>(
        *nodes_.emplace_back(new Bool(start, b, getBoolType())));
  }

  BinOp &getBinOp(Expr &lhs, Expr &rhs, const Type &type, BinOp::OpKind op) {
    assert(lhs.getType() == rhs.getType() &&
           "Operand types for bin op must be equal");
    return llvm::cast<BinOp>(
        *nodes_.emplace_back(new BinOp(lhs.getStart(), lhs, rhs, type, op)));
  }

  BinOp &getBinOp(Expr &lhs, Expr &rhs, BinOp::OpKind op) {
    assert(lhs.getType() == rhs.getType() &&
           "Operand types for bin op must be equal");
    const Type *type;
    switch (op) {
      case BinOp::OK_Lt:
      case BinOp::OK_Ge:
      case BinOp::OK_Eq:
      case BinOp::OK_Or:
        type = &getBoolType();
        break;
      default:
        type = &lhs.getType();
        break;
    }
    return llvm::cast<BinOp>(
        *nodes_.emplace_back(new BinOp(lhs.getStart(), lhs, rhs, *type, op)));
  }

  If &getIf(const SourceLocation &start, const Type &type, Expr &cond,
            Expr &if_body, Expr &else_body) {
    return llvm::cast<If>(
        *nodes_.emplace_back(new If(start, type, cond, if_body, else_body)));
  }

  If &getIf(const SourceLocation &start, Expr &cond, Expr &if_body,
            Expr &else_body) {
    return llvm::cast<If>(*nodes_.emplace_back(
        new If(start, if_body.getType(), cond, if_body, else_body)));
  }

  Zero &getZero(const SourceLocation &start, const Type &type) {
    return llvm::cast<Zero>(*nodes_.emplace_back(new Zero(start, type)));
  }

  Readc &getReadc(const SourceLocation &start) {
    const CallableType &ty = getCallableType(
        getCompositeType({&getIOType(), &getIntType()}), {&getIOType()});
    return llvm::cast<Readc>(*nodes_.emplace_back(new Readc(start, ty)));
  }

  Cast &getCast(const SourceLocation &start, const Type &type, Expr &expr) {
    return llvm::cast<Cast>(*nodes_.emplace_back(new Cast(start, type, expr)));
  }

  Let &getLet(const SourceLocation &start, std::string_view name, Expr &expr) {
    return llvm::cast<Let>(*nodes_.emplace_back(new Let(start, name, expr)));
  }

  Keep &getKeep(const SourceLocation &start, std::string_view name, Expr &expr,
                Expr &body) {
    return llvm::cast<Keep>(
        *nodes_.emplace_back(new Keep(start, name, expr, body)));
  }

  Callable &getCallable(const SourceLocation &start,
                        const CallableType &callable_ty,
                        const std::vector<SourceLocation> &arg_starts,
                        const std::vector<std::string> &arg_names) {
    const auto &arg_types = callable_ty.getArgTypes();
    assert(arg_types.size() == arg_names.size());
    assert(arg_types.size() == arg_starts.size());
    std::vector<Arg *> args;
    for (size_t i = 0; i < arg_types.size(); ++i) {
      Arg *ptr = new Arg(arg_starts.at(i), *arg_types.at(i), i);
      nodes_.emplace_back(ptr);
      args.push_back(ptr);
    }

    Callable *callable = new Callable(start, callable_ty, arg_names, args);
    nodes_.emplace_back(callable);
    for (Arg *arg : args)
      arg->setParent(*callable);
    return *callable;
  }

  Callable &getCallable(const SourceLocation &start, const Type &ret_type,
                        const std::vector<SourceLocation> &arg_starts,
                        const std::vector<std::string> &arg_names,
                        const std::vector<const Type *> &arg_types) {
    const CallableType &callable_ty = getCallableType(ret_type, arg_types);
    return getCallable(start, callable_ty, arg_starts, arg_names);
  }

  // const None &getNone() {
  //   return llvm::cast<None>(*nodes_.emplace_back(new None(getNoneType())));
  // }

  Call &getCall(const SourceLocation &start, Expr &func,
                const std::vector<Expr *> &args, bool pure = true) {
    const auto &callable_ty = llvm::cast<CallableType>(func.getType());
    const auto &ret_ty = callable_ty.getReturnType();
    assert(callable_ty.ArgumentTypesMatch(args));
    for (size_t i = 0; i < callable_ty.getNumArgs(); ++i) {
      assert(callable_ty.getArgType(i).isGeneric() ||
             callable_ty.getArgType(i) == args.at(i)->getType());
    }
    return llvm::cast<Call>(
        *nodes_.emplace_back(new Call(start, ret_ty, func, args, pure)));
  }

  AmbiguousCall &getAmbiguousCall(const SourceLocation &start,
                                  const std::vector<Expr *> &funcs,
                                  const std::vector<Expr *> &args) {
    assert(AmbiguousCall::CanMake(funcs));
    assert(std::all_of(funcs.begin(), funcs.end(), [&](const Expr *e) {
      return llvm::cast<CallableType>(e->getType()).ArgumentTypesMatch(args);
    }));
    assert(std::all_of(funcs.begin(), funcs.end(), [&funcs](const Expr *e) {
      return e->getType().getReturnType() ==
             funcs.front()->getType().getReturnType();
    }));
    return llvm::cast<AmbiguousCall>(*nodes_.emplace_back(new AmbiguousCall(
        start, funcs.front()->getType().getReturnType(), funcs, args)));
  }

  Composite &getComposite(const SourceLocation &start,
                          const std::vector<Expr *> &elems) {
    std::vector<const Type *> types;
    for (const Expr *elem : elems) {
      types.push_back(&elem->getType());
    }
    return llvm::cast<Composite>(*nodes_.emplace_back(
        new Composite(start, getCompositeType(types), elems)));
  }

  Set &getSet(const SourceLocation &start, Expr &expr, Expr &idx, Expr &store) {
    assert(llvm::isa<CompositeType>(expr.getType()) ||
           llvm::isa<ArrayType>(expr.getType()));
    assert(idx.getType().isNamedType("int"));
    return llvm::cast<Set>(
        *nodes_.emplace_back(new Set(start, expr, idx, store)));
  }

  Get &getGet(const SourceLocation &start, const Type &type, Expr &expr,
              Expr &idx) {
    assert(llvm::isa<CompositeType>(expr.getType()) ||
           llvm::isa<ArrayType>(expr.getType()));
    assert(idx.getType().isNamedType("int"));
    return llvm::cast<Get>(
        *nodes_.emplace_back(new Get(start, type, expr, idx)));
  }

  Declare &getDeclare(const SourceLocation &start, std::string_view name,
                      Expr &expr, bool is_write, bool is_cdecl) {
    Declare *decl = new Declare(start, name, expr, is_write, is_cdecl);
    nodes_.emplace_back(decl);
    return *decl;
  }

  Declare &getDeclare(const SourceLocation &start, std::string_view name,
                      const Type &type, bool is_write, bool is_cdecl) {
    Declare *decl = new Declare(start, name, type, is_write, is_cdecl);
    nodes_.emplace_back(decl);
    return *decl;
  }

  const NamedType &getNamedType(std::string_view name);

  // const Type &getNoneType() { return getNamedType("none"); }
  const Type &getIOType() { return getNamedType("IO"); }
  const Type &getCharType() { return getNamedType("char"); }
  const Type &getCharArrayType(std::string_view literal) {
    //  +1 for the null terminator.
    return getArrayType(getCharType(), literal.size() + 1);
  }
  const Type &getIntType() { return getNamedType("int"); }
  const Type &getBoolType() { return getNamedType("bool"); }
  const CallableType &getWriteType(const Type &arg_type) {
    return getCallableType(getIOType(), {&getIOType(), &arg_type});
  }

  const CallableType &getCallableType(
      const Type &ret_type, const std::vector<const Type *> &arg_types) {
    return llvm::cast<CallableType>(
        *types_.emplace_back(new CallableType(ret_type, arg_types)));
  }

  const ArrayType &getArrayType(const Type &type, size_t num) {
    return llvm::cast<ArrayType>(
        *types_.emplace_back(new ArrayType(type, num)));
  }

  const CompositeType &getCompositeType(
      const std::vector<const Type *> &types) {
    return llvm::cast<CompositeType>(
        *types_.emplace_back(new CompositeType(types)));
  }

  const GenericType &getGenericType() { return generic_; }
  const GenericRemainingType &getGenericRemainingType() {
    return generic_remaining_;
  }

 private:
  std::vector<std::unique_ptr<Node>> nodes_;
  std::vector<std::unique_ptr<const Type>> types_;
  std::map<std::string, const NamedType *, std::less<>> named_types_;
  GenericType generic_;
  GenericRemainingType generic_remaining_;
};

}  // namespace lang

#endif  // ASTBUILDER_H_
