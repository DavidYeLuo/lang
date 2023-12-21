#ifndef AST_H_
#define AST_H_

#include <cassert>
#include <sstream>
#include <string>
#include <vector>

#include "llvm/Support/Casting.h"

namespace lang {

namespace builtins {

constexpr std::string_view kIOTypeName = "IO";
constexpr std::string_view kIntTypeName = "int";
constexpr std::string_view kBoolTypeName = "bool";
constexpr std::string_view kCharTypeName = "char";
constexpr std::string_view kCPtrTypeName = "cptr";
constexpr std::string_view kNoneTypeName = "none";

}  // namespace builtins

bool IsBuiltinType(std::string_view name);

class Node {
 public:
  enum Kind {
#define NODE(name) NK_##name,
#define EXPR_FIRST(name) NK_##name, NK_ExprFirst = NK_##name,
#define EXPR_LAST(name) NK_##name, NK_ExprLast = NK_##name,
#define TYPE_FIRST(name) NK_##name, NK_TypeFirst = NK_##name,
#define TYPE_LAST(name) NK_##name, NK_TypeLast = NK_##name,
#include "nodes.def"
  };

  Node(Kind kind) : kind_(kind) {}
  virtual ~Node() = default;
  Kind getKind() const { return kind_; }

 private:
  const Kind kind_;
};

class Type : public Node {
 public:
  Type(Kind kind) : Node(kind) {
    assert(NK_TypeFirst <= kind && kind <= NK_TypeLast);
  }

  static bool classof(const Node *node) {
    return NK_TypeFirst <= node->getKind() && node->getKind() <= NK_TypeLast;
  }

  virtual std::string toString() const = 0;

  bool isNamedType(std::string_view name) const;
  bool isCompositeOrArrayType() const;
  bool isCharArray() const;
  // bool isNoneType() const {
  //   return isNamedType("none");
  // }
  const Type &getReturnType() const;
};

class NamedType : public Type {
 public:
  NamedType(std::string_view name) : Type(NK_NamedType), name_(name) {}

  std::string_view getName() const { return name_; }

  std::string toString() const override { return name_; }

  static bool classof(const Node *node) {
    return node->getKind() == NK_NamedType;
  }

 private:
  const std::string name_;
};

class CallableType : public Type {
 public:
  CallableType(const Type &ret_type, const std::vector<const Type *> &arg_types)
      : Type(NK_CallableType), ret_type_(ret_type), arg_types_(arg_types) {}

  const Type &getReturnType() const { return ret_type_; }
  const auto &getArgTypes() const { return arg_types_; }
  size_t getNumArgs() const { return arg_types_.size(); }
  const auto &getArgType(size_t i) const { return *arg_types_.at(i); }

  static bool classof(const Node *node) {
    return node->getKind() == NK_CallableType;
  }

  std::string toString() const override {
    std::stringstream ss;
    ss << "\\";
    for (const Type *type : arg_types_) {
      ss << type->toString() << " ";
    }
    ss << " -> " << ret_type_.toString();
    return ss.str();
  }

 private:
  const Type &ret_type_;
  const std::vector<const Type *> arg_types_;
};

class ArrayType : public Type {
 public:
  ArrayType(const Type &type, size_t num)
      : Type(NK_ArrayType), type_(type), num_(num) {}

  static bool classof(const Node *node) {
    return node->getKind() == NK_ArrayType;
  }

  size_t getNumElems() const { return num_; }
  const Type &getElemType() const { return type_; }

  std::string toString() const override {
    std::stringstream ss;
    ss << "[" << num_ << " x " << type_.toString() << "]";
    return ss.str();
  }

 private:
  const Type &type_;
  const size_t num_;
};

class CompositeType : public Type {
 public:
  CompositeType(const std::vector<const Type *> &types)
      : Type(NK_CompositeType), types_(types) {
    assert(!types.empty());
  }

  static bool classof(const Node *node) {
    return node->getKind() == NK_CompositeType;
  }

  std::string toString() const override {
    std::stringstream ss;
    ss << "<";
    for (const Type *type : types_) {
      ss << type->toString() << " ";
    }
    ss << ">";
    return ss.str();
  }

  const auto &getTypes() const { return types_; }
  size_t getNumTypes() const { return types_.size(); }
  const Type &getTypeAt(size_t i) const { return *types_.at(i); }

 private:
  const std::vector<const Type *> types_;
};

class Expr : public Node {
 public:
  Expr(Kind kind, const Type &type) : Node(kind), type_(type) {}

  static bool classof(const Node *node) {
    return NK_ExprFirst <= node->getKind() && node->getKind() <= NK_ExprLast;
  }

  const Type &getType() const { return type_; }

 private:
  const Type &type_;
};

class Define : public Node {
 public:
  Define(std::string_view name, const Expr &body)
      : Node(NK_Define), name_(name), body_(body) {}

  std::string_view getName() const { return name_; }
  const Expr &getBody() const { return body_; }

  static bool classof(const Node *node) { return node->getKind() == NK_Define; }

 private:
  std::string name_;
  const Expr &body_;
};

// TODO: Declares should not be used like other expressions. It's just that it's
// much easier to treat them as expressions for internal use.
class Declare : public Expr {
 public:
  Declare(std::string_view name, const Type &type)
      : Expr(NK_Declare, type), name_(name) {}

  std::string_view getName() const { return name_; }

  static bool classof(const Node *node) {
    return node->getKind() == NK_Declare;
  }

 private:
  std::string name_;
};

class Composite : public Expr {
 public:
  Composite(const CompositeType &type, const std::vector<const Expr *> &exprs)
      : Expr(NK_Composite, type), exprs_(exprs) {}

  const auto &getElems() const { return exprs_; }
  size_t getNumElems() const { return exprs_.size(); }
  const Expr &getElem(size_t i) const { return *exprs_.at(i); }

 private:
  const std::vector<const Expr *> exprs_;
};

class Set : public Expr {
 public:
  Set(const Expr &expr, const Expr &idx, const Expr &store)
      : Expr(NK_Set, expr.getType()), expr_(expr), idx_(idx), store_(store) {}

  static bool classof(const Node *node) { return node->getKind() == NK_Set; }

  const Expr &getExpr() const { return expr_; }
  const Expr &getIdx() const { return idx_; }
  const Expr &getStore() const { return store_; }

 private:
  const Expr &expr_, &idx_, &store_;
};

class Get : public Expr {
 public:
  Get(const Type &type, const Expr &expr, const Expr &idx)
      : Expr(NK_Get, type), expr_(expr), idx_(idx) {}

  const Expr &getExpr() const { return expr_; }
  const Expr &getIdx() const { return idx_; }
  static bool classof(const Node *node) { return node->getKind() == NK_Get; }

 private:
  const Expr &expr_, &idx_;
};

class Let : public Expr {
 public:
  Let(std::string_view name, const Expr &expr)
      : Expr(NK_Let, expr.getType()), name_(name), expr_(expr) {}

  std::string_view getName() const { return name_; }
  const Expr &getExpr() const { return expr_; }

  static bool classof(const Node *node) { return node->getKind() == NK_Let; }

 private:
  std::string name_;
  const Expr &expr_;
};

class Keep : public Expr {
 public:
  Keep(std::string_view name, const Expr &expr, const Expr &body)
      : Expr(NK_Keep, body.getType()), name_(name), expr_(expr), body_(body) {}

  std::string_view getName() const { return name_; }
  const Expr &getExpr() const { return expr_; }
  const Expr &getBody() const { return body_; }

  static bool classof(const Node *node) { return node->getKind() == NK_Keep; }

 private:
  std::string name_;
  const Expr &expr_, &body_;
};

class CallableBase : public Expr {
 public:
  CallableBase(const CallableType &type,
               const std::vector<std::string> &arg_names)
      : Expr(NK_Callable, type), arg_names_(arg_names) {
    assert(type.getArgTypes().size() == arg_names.size() &&
           "Differring number of argument names and types.");
  }

  const auto &getArgNames() const { return arg_names_; }
  std::string_view getArgName(size_t i) const { return arg_names_[i]; }
  size_t getNumArgs() const { return arg_names_.size(); }

 private:
  const std::vector<std::string> arg_names_;
};

class Callable : public CallableBase {
 public:
  Callable(const CallableType &type, const Expr &body,
           const std::vector<std::string> &arg_names)
      : CallableBase(type, arg_names), body_(&body) {}

  const Expr &getBody() const { return *body_; }
  static bool classof(const Node *node) {
    return node->getKind() == NK_Callable;
  }

 private:
  friend class ASTBuilder;

  Callable(const CallableType &type, const std::vector<std::string> &arg_names)
      : CallableBase(type, arg_names) {}

  void setBody(const Expr &body) { body_ = &body; }

  const Expr *body_;
  const std::vector<std::string> arg_names_;
};

class Cast : public Expr {
 public:
  Cast(const Type &type, const Expr &expr) : Expr(NK_Cast, type), expr_(expr) {}

  const Expr &getExpr() const { return expr_; }

  static bool classof(const Node *node) { return node->getKind() == NK_Cast; }

 private:
  const Expr &expr_;
};

class Write : public Expr {
 public:
  Write(const Type &type) : Expr(NK_Write, type) { assert(CheckType(type)); }

  static bool classof(const Node *node) { return node->getKind() == NK_Write; }
  const Type &getArgType() const {
    return *llvm::cast<CallableType>(getType()).getArgTypes().at(1);
  }

 private:
  static bool CheckType(const Type &type);
};

class Readc : public Expr {
 public:
  Readc(const Type &type) : Expr(NK_Readc, type) { assert(CheckType(type)); }

  static bool classof(const Node *node) { return node->getKind() == NK_Readc; }

  const CallableType &getCallbackType() const {
    return llvm::cast<CallableType>(
        llvm::cast<CallableType>(getType()).getArgType(1));
  }

 private:
  static bool CheckType(const Type &type);
};

class Call : public Expr {
 public:
  Call(const Type &type, const Expr &func,
       const std::vector<const Expr *> &args, bool pure = true)
      : Expr(NK_Call, type), func_(func), args_(args), pure_(pure) {}

  static bool classof(const Node *node) { return node->getKind() == NK_Call; }

  const Expr &getFunc() const { return func_; }
  const std::vector<const Expr *> &getArgs() const { return args_; }
  bool isPure() const { return pure_; }

 private:
  const Expr &func_;
  const std::vector<const Expr *> args_;
  bool pure_;
};

class Zero : public Expr {
 public:
  Zero(const Type &type) : Expr(NK_Zero, type) {}

  static bool classof(const Node *node) { return node->getKind() == NK_Zero; }
};

class Int : public Expr {
 public:
  Int(int i, const Type &type) : Expr(NK_Int, type), i_(i) {}

  int getInt() const { return i_; }
  static bool classof(const Node *node) { return node->getKind() == NK_Int; }

 private:
  int i_;
};

class Char : public Expr {
 public:
  Char(char c, const Type &type) : Expr(NK_Char, type), c_(c) {}
  static bool classof(const Node *node) { return node->getKind() == NK_Char; }

  char getChar() const { return c_; }

 private:
  char c_;
};

// class None : public Expr {
//  public:
//   None(const Type &none_t) : Expr(NK_None, none_t) {}
// };

class Str : public Expr {
 public:
  Str(std::string_view str, const Type &type) : Expr(NK_Str, type), str_(str) {}

  std::string_view get() const { return str_; }

  static bool classof(const Node *node) { return node->getKind() == NK_Str; }

 private:
  const std::string str_;
};

class Bool : public Expr {
 public:
  Bool(bool b, const Type &type) : Expr(NK_Bool, type), b_(b) {
    assert(type.isNamedType("bool"));
  }

  bool get() const { return b_; }

  static bool classof(const Node *node) { return node->getKind() == NK_Bool; }

 private:
  const bool b_;
};

class BinOp : public Expr {
 public:
  enum OpKind {
    OK_Sub,
    OK_Add,
    OK_Lt,
    OK_Ge,
    OK_Eq,
    OK_Or,
    OK_Mod,
  };

  BinOp(const Expr &lhs, const Expr &rhs, const Type &type, OpKind op)
      : Expr(NK_BinOp, type), lhs_(lhs), rhs_(rhs), op_(op) {}

  const Expr &getLHS() const { return lhs_; }
  const Expr &getRHS() const { return rhs_; }
  OpKind getOp() const { return op_; }

 private:
  const Expr &lhs_, &rhs_;
  OpKind op_;
};

class If : public Expr {
 public:
  If(const Type &type, const Expr &cond, const Expr &if_body,
     const Expr &else_body)
      : Expr(NK_If, type),
        cond_(cond),
        if_body_(if_body),
        else_body_(else_body) {}

  const Expr &getCond() const { return cond_; }
  const Expr &getIf() const { return if_body_; }
  const Expr &getElse() const { return else_body_; }

 private:
  const Expr &cond_, &if_body_, &else_body_;
};

class Arg : public Expr {
 public:
  Arg(const Type &type, size_t arg_no) : Expr(NK_Arg, type), arg_no_(arg_no) {}

  size_t getArgNo() const { return arg_no_; }

 private:
  size_t arg_no_;
};

}  // namespace lang

#endif  // AST_H_
