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
  const Str &getStr(const SourceLocation &start, std::string_view str) {
    return llvm::cast<Str>(
        *nodes_.emplace_back(new Str(start, str, getCharArrayType(str))));
  }

  const Char &getChar(const SourceLocation &start, char c) {
    return llvm::cast<Char>(
        *nodes_.emplace_back(new Char(start, c, getCharType())));
  }

  const Int &getInt(const SourceLocation &start, int i) {
    return llvm::cast<Int>(
        *nodes_.emplace_back(new Int(start, i, getIntType())));
  }

  const Bool &getBool(const SourceLocation &start, bool b) {
    return llvm::cast<Bool>(
        *nodes_.emplace_back(new Bool(start, b, getBoolType())));
  }

  const BinOp &getBinOp(const Expr &lhs, const Expr &rhs, const Type &type,
                        BinOp::OpKind op) {
    assert(Equals(lhs.getType(), rhs.getType()) &&
           "Operand types for bin op must be equal");
    return llvm::cast<BinOp>(
        *nodes_.emplace_back(new BinOp(lhs.getStart(), lhs, rhs, type, op)));
  }

  const BinOp &getBinOp(const Expr &lhs, const Expr &rhs, BinOp::OpKind op) {
    assert(Equals(lhs.getType(), rhs.getType()) &&
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

  const If &getIf(const SourceLocation &start, const Type &type,
                  const Expr &cond, const Expr &if_body,
                  const Expr &else_body) {
    return llvm::cast<If>(
        *nodes_.emplace_back(new If(start, type, cond, if_body, else_body)));
  }

  const If &getIf(const SourceLocation &start, const Expr &cond,
                  const Expr &if_body, const Expr &else_body) {
    // TODO: Check body types.
    return llvm::cast<If>(*nodes_.emplace_back(
        new If(start, if_body.getType(), cond, if_body, else_body)));
  }

  const Zero &getZero(const SourceLocation &start, const Type &type) {
    return llvm::cast<Zero>(*nodes_.emplace_back(new Zero(start, type)));
  }

  const Write &getWrite(const SourceLocation &start, const Type &arg_type) {
    return llvm::cast<Write>(
        *nodes_.emplace_back(new Write(start, getWriteType(arg_type))));
  }

  const Readc &getReadc(const SourceLocation &start) {
    const CallableType &ty = getCallableType(
        getCompositeType({&getIOType(), &getIntType()}), {&getIOType()});
    return llvm::cast<Readc>(*nodes_.emplace_back(new Readc(start, ty)));
  }

  const Cast &getCast(const SourceLocation &start, const Type &type,
                      const Expr &expr) {
    return llvm::cast<Cast>(*nodes_.emplace_back(new Cast(start, type, expr)));
  }

  const Let &getLet(const SourceLocation &start, std::string_view name,
                    const Expr &expr) {
    return llvm::cast<Let>(*nodes_.emplace_back(new Let(start, name, expr)));
  }

  const Keep &getKeep(const SourceLocation &start, std::string_view name,
                      const Expr &expr, const Expr &body) {
    return llvm::cast<Keep>(
        *nodes_.emplace_back(new Keep(start, name, expr, body)));
  }

  const Callable &getCallable(const SourceLocation &start, const Expr &body,
                              const std::vector<std::string> &arg_names,
                              const std::vector<const Type *> &arg_types) {
    const CallableType &callable_ty =
        getCallableType(body.getType(), arg_types);
    return llvm::cast<Callable>(*nodes_.emplace_back(
        new Callable(start, callable_ty, body, arg_names)));
  }

  using ArgVectorTy = std::vector<const Arg *>;

  // Create a `Callable` where the return type is explicitly given. The body is
  // generated from the callback which receives two arguments:
  //
  //   1. A reference to the `Callable` where the body will be assigned to.
  //   2. The list of argument expressions passed to the callable.
  //
  using BodyCallbackTy =
      std::function<const Expr &(const CallableBase &, const ArgVectorTy &)>;
  const Callable &getCallable(const SourceLocation &start, const Type &ret_type,
                              const std::vector<SourceLocation> &arg_starts,
                              const std::vector<std::string> &arg_names,
                              const std::vector<const Type *> &arg_types,
                              const BodyCallbackTy &callback) {
    ArgVectorTy args;
    for (size_t i = 0; i < arg_types.size(); ++i) {
      const Arg *ptr = new Arg(arg_starts.at(i), *arg_types.at(i), i);
      nodes_.emplace_back(ptr);
      args.push_back(ptr);
    }

    const CallableType &callable_ty = getCallableType(ret_type, arg_types);
    Callable *callable = new Callable(start, callable_ty, arg_names);
    nodes_.emplace_back(callable);
    const Expr &body = callback(*callable, args);
    callable->setBody(body);
    return *callable;
  }

  // Create a `Callable` where the return type is inferred from the resulting
  // type of the body returned by the callback. The callback receives one
  // argument: the list of argument expressions passed to the callable.
  using BodyCallback2Ty = std::function<const Expr &(const ArgVectorTy &)>;
  const Callable &getCallable(const SourceLocation &start,
                              const std::vector<SourceLocation> &arg_starts,
                              const std::vector<std::string> &arg_names,
                              const std::vector<const Type *> &arg_types,
                              const BodyCallback2Ty &callback) {
    ArgVectorTy args;
    for (size_t i = 0; i < arg_types.size(); ++i) {
      const Arg *ptr = new Arg(arg_starts.at(i), *arg_types.at(i), i);
      nodes_.emplace_back(ptr);
      args.push_back(ptr);
    }

    const Expr &body = callback(args);
    return getCallable(start, body, arg_names, arg_types);
  }

  // const None &getNone() {
  //   return llvm::cast<None>(*nodes_.emplace_back(new None(getNoneType())));
  // }

  const Call &getCall(const SourceLocation &start, const Expr &func,
                      const std::vector<const Expr *> &args, bool pure = true) {
    const auto &callable_ty = llvm::cast<CallableType>(func.getType());
    const auto &ret_ty = callable_ty.getReturnType();
    assert(callable_ty.getNumArgs() == args.size() &&
           "Mismatch number of arguments and types");
    for (size_t i = 0; i < args.size(); ++i) {
      assert(Equals(callable_ty.getArgType(i), args.at(i)->getType()));
    }
    return llvm::cast<Call>(
        *nodes_.emplace_back(new Call(start, ret_ty, func, args, pure)));
  }

  const Composite &getComposite(const SourceLocation &start,
                                const std::vector<const Expr *> &elems) {
    std::vector<const Type *> types;
    for (const Expr *elem : elems) {
      types.push_back(&elem->getType());
    }
    return llvm::cast<Composite>(*nodes_.emplace_back(
        new Composite(start, getCompositeType(types), elems)));
  }

  const Set &getSet(const SourceLocation &start, const Expr &expr,
                    const Expr &idx, const Expr &store) {
    assert(llvm::isa<CompositeType>(expr.getType()) ||
           llvm::isa<ArrayType>(expr.getType()));
    assert(idx.getType().isNamedType("int"));
    return llvm::cast<Set>(
        *nodes_.emplace_back(new Set(start, expr, idx, store)));
  }

  const Get &getGet(const SourceLocation &start, const Type &type,
                    const Expr &expr, const Expr &idx) {
    assert(llvm::isa<CompositeType>(expr.getType()) ||
           llvm::isa<ArrayType>(expr.getType()));
    assert(idx.getType().isNamedType("int"));
    return llvm::cast<Get>(
        *nodes_.emplace_back(new Get(start, type, expr, idx)));
  }

  const Define &getDefine(const SourceLocation &start, std::string_view name,
                          const Expr &body) {
    return llvm::cast<Define>(
        *nodes_.emplace_back(new Define(start, name, body)));
  }

  const Declare &getDeclare(const SourceLocation &start, std::string_view name,
                            const Type &type) {
    return llvm::cast<Declare>(
        *nodes_.emplace_back(new Declare(start, name, type)));
  }

  const NamedType &getNamedType(std::string_view name);

  // const Type &getNoneType() { return getNamedType("none"); }
  const Type &getIOType() { return getNamedType("IO"); }
  const Type &getCharType() { return getNamedType("char"); }
  const Type &getCharArrayType(std::string_view literal) {
    // return getNamedType("char");
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

  bool Equals(const Type &lhs, const Type &rhs) const;
#define TYPE(name) bool Equals(const name &lhs, const Type &rhs) const;
#include "types.def"

 private:
  std::vector<std::unique_ptr<const Node>> nodes_;
  std::vector<std::unique_ptr<const Type>> types_;
  std::map<std::string, const NamedType *, std::less<>> named_types_;
};

}  // namespace lang

#endif  // ASTBUILDER_H_
