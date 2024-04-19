#ifndef AST_H_
#define AST_H_

#include <algorithm>
#include <array>
#include <cassert>
#include <iostream>
#include <map>
#include <set>
#include <span>
#include <sstream>
#include <string>
#include <tuple>
#include <vector>

#include "lang.h"
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
#include "nodes.def"
  };

  static std::string AsString(Kind kind) {
    switch (kind) {
#define NODE(name) \
  case NK_##name:  \
    return #name;
#include "nodes.def"
    }
    __builtin_unreachable();
  }

  Node(Kind kind, const SourceLocation &start) : kind_(kind), start_(start) {}
  virtual ~Node() = default;
  Kind getKind() const { return kind_; }
  const SourceLocation &getStart() const { return start_; }
  std::string getNodeKindString() const { return AsString(kind_); }

 private:
  const Kind kind_;
  const SourceLocation start_;
};

class Expr;

class Type {
 public:
  enum Kind {
#define TYPE(name) TK_##name,
#define TYPE_FIRST(name) TK_##name, TK_TypeFirst = TK_##name,
#define TYPE_LAST(name) TK_##name, TK_TypeLast = TK_##name,
#include "types.def"
  };
  Type(Kind kind) : kind_(kind) {}
  virtual ~Type() = default;

  Kind getKind() const { return kind_; }

  std::string toString() const {
    std::string s;
    if (isMutable())
      s += "mut ";
    s += toStringImpl();
    return s;
  }

  bool isNamedType(std::string_view name) const;
  bool isBuiltinType() const;
  bool isCharArray() const;

  // A type is generic if it is a generic type or has members or arguments that
  // are generic. That is, this has some dependence on a generic type.
  virtual bool isGeneric() const = 0;
  bool isGenericCallable() const;
  bool isGenericRemainingCallable() const;

  enum class QualifierCmp {
    // Mutability must match exactly.
    Exact,

    // Ignore mutability when comparing.
    Ignore,

    // If the LHS is mutable, then the RHS must not be immutable because we'd
    // lose const-ness.
    RetainImmutability,
  };

  // Determins if this type can be mutated. Mainly the composite types use this.
  virtual bool isMutable() const { return false; }
  bool isImmutable() const { return !isMutable(); }
  bool CheckQualifiers(const Type &rhs, QualifierCmp cmp) const;

  bool operator==(const Type &other) const {
    return Equals(other, QualifierCmp::Exact);
  }
  virtual bool Equals(const Type &, QualifierCmp cmp) const = 0;

  bool CanConvertFrom(const Expr &e) const;
  bool CanConvertFrom(const Type &t) const;

  const Type &getReturnType() const;
  bool Matches(const Type &other) const;
  bool isAggregateType() const;
  bool isStructType() const;

 protected:
  virtual std::string toStringImpl() const = 0;

 private:
  const Kind kind_;
};

class GenericType : public Type {
 public:
  GenericType(bool mut = false) : Type(TK_GenericType), mutable_(mut) {}
  bool isGeneric() const override { return true; }
  bool isMutable() const override { return mutable_; }

  static bool classof(const Type *type) {
    return type->getKind() == TK_GenericType;
  }
  bool Equals(const Type &other, QualifierCmp) const override;

 protected:
  std::string toStringImpl() const override { return "GENERIC"; }

 private:
  bool mutable_;
};

class GenericRemainingType : public Type {
 public:
  GenericRemainingType() : Type(TK_GenericRemainingType) {}
  bool isGeneric() const override { return true; }

  static bool classof(const Type *type) {
    return type->getKind() == TK_GenericRemainingType;
  }
  bool Equals(const Type &other, QualifierCmp) const override;

 protected:
  std::string toStringImpl() const override { return "GENERIC_REMAINING"; }
};

class NamedType : public Type {
 public:
  NamedType(std::string_view name) : Type(TK_NamedType), name_(name) {}
  std::string_view getName() const { return name_; }
  bool isGeneric() const override { return false; }

  static bool classof(const Type *type) {
    return type->getKind() == TK_NamedType;
  }
  bool Equals(const Type &other, QualifierCmp) const override;

 protected:
  std::string toStringImpl() const override { return name_; }

 private:
  const std::string name_;
};

class CallableType : public Type {
 public:
  CallableType(const Type &ret_type, const std::vector<const Type *> &arg_types)
      : Type(TK_CallableType), ret_type_(ret_type), arg_types_(arg_types) {
    assert(!ret_type.isGeneric());
  }

  const Type &getReturnType() const { return ret_type_; }
  const auto &getArgTypes() const { return arg_types_; }
  size_t getNumArgs() const { return arg_types_.size(); }
  const auto &getArgType(size_t i) const { return *arg_types_.at(i); }
  bool isGeneric() const override {
    return std::ranges::any_of(arg_types_,
                               [](const Type *ty) { return ty->isGeneric(); });
  }

  static bool classof(const Type *type) {
    return type->getKind() == TK_CallableType;
  }

  // Similar to CallableType::Equals(const Type &) but it also takes into
  // account generic argument types.
  bool CallableTypesMatch(const CallableType &rhs) const;
  bool CanApplyArgs(std::span<const Type *const> args) const;
  bool CanApplyArgs(const std::vector<const Type *> &args) const {
    return CanApplyArgs(std::span{args.begin(), args.size()});
  }
  bool CanApplyArgs(std::span<Expr *const> args) const;
  bool CanApplyArgs(const std::vector<Expr *> &args) const {
    return CanApplyArgs(std::span{args.begin(), args.size()});
  }
  bool Equals(const Type &other, QualifierCmp) const override;

 protected:
  std::string toStringImpl() const override {
    return Concat(
        "\\",
        Join(arg_types_, " ", [](const Type *t) { return t->toString(); }),
        " -> ", ret_type_.toString());
  }

 private:
  const Type &ret_type_;
  const std::vector<const Type *> arg_types_;
};

bool CanApplyArgs(std::span<const Type *const> args1,
                  std::span<const Type *const> args2);

class ArrayType : public Type {
 public:
  ArrayType(const Type &type, size_t num, bool mut = false)
      : Type(TK_ArrayType), type_(type), num_(num), mutable_(mut) {}

  static bool classof(const Type *type) {
    return type->getKind() == TK_ArrayType;
  }

  bool isGeneric() const override { return type_.isGeneric(); }
  size_t getNumElems() const { return num_; }
  const Type &getElemType() const { return type_; }
  bool isMutable() const override { return mutable_; }
  bool Equals(const Type &other, QualifierCmp) const override;

 protected:
  std::string toStringImpl() const override {
    return Concat("[", num_, " x ", type_.toString(), "]");
  }

 private:
  const Type &type_;
  const size_t num_;
  const bool mutable_;
};

class CompositeType : public Type {
 public:
  CompositeType(const std::vector<const Type *> &types, bool mut = false)
      : Type(TK_CompositeType), types_(types), mutable_(mut) {
    assert(!types.empty());
  }

  static bool classof(const Type *type) {
    return type->getKind() == TK_CompositeType;
  }

  bool isMutable() const override { return mutable_; }

  const auto &getTypes() const { return types_; }
  size_t getNumTypes() const { return types_.size(); }
  const Type &getTypeAt(size_t i) const { return *types_.at(i); }
  bool isGeneric() const override {
    return std::ranges::any_of(types_,
                               [](const Type *ty) { return ty->isGeneric(); });
  }
  bool Equals(const Type &other, QualifierCmp) const override;

 protected:
  std::string toStringImpl() const override {
    return Concat(
        "<", Join(types_, " ", [](const Type *t) { return t->toString(); }),
        ">");
  }

 private:
  const std::vector<const Type *> types_;
  const bool mutable_;
};

class StructType : public Type {
 public:
  using TypeMap = std::map<std::string, const Type *, std::less<>>;
  StructType(const TypeMap &types, bool mut = false)
      : Type(TK_StructType), types_(types), mutable_(mut) {
    assert(!types.empty());
    assert(AllUniqueNames(types) && "Found duplicate member names");
  }

  static bool classof(const Type *type) {
    return type->getKind() == TK_StructType;
  }

  bool isMutable() const override { return mutable_; }

  const auto &getTypes() const { return types_; }
  bool hasField(std::string_view name) const { return types_.contains(name); }
  const Type &getField(std::string_view name) const {
    return *types_.find(name)->second;
  }

  using OrderedTypes = std::vector<std::pair<std::string_view, const Type *>>;
  OrderedTypes getOrderedTypes() const {
    OrderedTypes types;
    types.reserve(types_.size());
    for (auto it = types_.begin(); it != types_.end(); ++it)
      types.emplace_back(it->first, it->second);
    std::sort(types.begin(), types.end(), [](const auto &p1, const auto &p2) {
      return p1.first.compare(p2.first) < 0;
    });
    return types;
  }
  size_t getNumTypes() const { return types_.size(); }
  const Type &getTypeAt(std::string_view field) const {
    return *types_.find(field)->second;
  }
  bool isGeneric() const override {
    return std::any_of(types_.begin(), types_.end(),
                       [](auto it) { return it.second->isGeneric(); });
  }
  bool Equals(const Type &other, QualifierCmp) const override;

 protected:
  std::string toStringImpl() const override {
    return Concat("{",
                  JoinIter(types_, " ",
                           [](auto it) {
                             return Concat(it->first, ":",
                                           it->second->toString());
                           }),
                  "}");
  }

 private:
  static bool AllUniqueNames(const TypeMap &types) {
    std::set<std::string_view> names;
    for (auto it = types.begin(); it != types.end(); ++it)
      names.insert(it->first);
    return names.size() == types.size();
  }

  const TypeMap types_;
  const bool mutable_;
};

class TypeDef : public Node {
 public:
  TypeDef(const SourceLocation &start, std::string_view name, const Type &alias)
      : Node(NK_TypeDef, start), name_(name), alias_(alias) {}

  static bool classof(const Node *node) {
    return node->getKind() == NK_TypeDef;
  }

  std::string_view getName() const { return name_; }
  const Type &getAlias() const { return alias_; }

 private:
  std::string name_;
  const Type &alias_;
};

class Expr : public Node {
 public:
  Expr(Kind kind, const SourceLocation &start, const Type &type)
      : Node(kind, start), type_(type), owner_(nullptr) {}

  static bool classof(const Node *node) {
    return NK_ExprFirst <= node->getKind() && node->getKind() <= NK_ExprLast;
  }

  // TODO: Add `isConstantEvaluatable` to check if this type can be evaluated at
  // compile time.

  const Type &getType() const { return type_; }
  const auto &getUsers() const { return users_; }
  auto &getUsers() { return users_; }
  bool hasUsers() { return !users_.empty(); }
  void AddUser(Expr &e) { users_.insert(&e); }
  void RemoveUser(Expr &e) {
    assert(users_.contains(&e));
    users_.erase(&e);
  }

  Expr &getOwner() const { return *owner_; }
  bool hasOwner() const { return owner_; }
  void setOwner(Expr &owner) {
    assert(!hasOwner());
    assert(users_.contains(&owner) &&
           "This expression must be owned by one of the other expressions "
           "using it.");
    owner_ = &owner;
  }
  void swapOwner(Expr &newowner) {
    assert(hasOwner());
    assert(users_.contains(&newowner) &&
           "This expression must be owned by one of the other expressions "
           "using it.");
    owner_ = &newowner;
  }

 private:
  const Type &type_;
  std::set<Expr *> users_;
  Expr *owner_;
};

// A declaration is a named value that may be defined outside this module. If it
// does have a body, then it is a definition.
class Declare : public Expr {
 public:
  Declare(const SourceLocation &start, std::string_view name, const Type &type,
          bool is_builtin_write, bool is_cdecl)
      : Expr(NK_Declare, start, type),
        name_(name),
        body_(nullptr),
        builtin_write_(is_builtin_write),
        is_cdecl_(is_cdecl) {}
  Declare(const SourceLocation &start, std::string_view name, Expr &expr,
          bool is_builtin_write, bool is_cdecl)
      : Expr(NK_Declare, start, expr.getType()),
        name_(name),
        body_(&expr),
        builtin_write_(is_builtin_write),
        is_cdecl_(is_cdecl) {
    expr.AddUser(*this);
  }

  std::string_view getName() const { return name_; }
  void setName(std::string_view name) { name_ = name; }
  bool isDefinition() const { return body_; }
  bool isBuiltinWrite() const { return builtin_write_; }
  bool isCDecl() const { return is_cdecl_; }
  const Expr &getBody() const { return *body_; }
  Expr &getBody() { return *body_; }
  void setBody(Expr &expr) {
    assert(!body_);
    assert(getType() == expr.getType());
    body_ = &expr;
    expr.AddUser(*this);
  }
  bool isGenericCallable() const { return getType().isGenericCallable(); }

  static bool classof(const Node *node) {
    return node->getKind() == NK_Declare;
  }

 private:
  std::string name_;
  Expr *body_;

  // This is used to indicate to the compiler this should generate a printf.
  bool builtin_write_, is_cdecl_;
};

class Struct : public Expr {
 public:
  using FieldMap = std::map<std::string, Expr *, std::less<>>;
  Struct(const SourceLocation &start, const StructType &type,
         const FieldMap &fields)
      : Expr(NK_Struct, start, type), fields_(fields) {
    assert(!fields.empty());
    assert(AllUniqueNames(fields) && "Found duplicate member names");
    for (const auto &p : fields)
      p.second->AddUser(*this);
  }

  static bool classof(const Node *node) { return node->getKind() == NK_Struct; }
  const auto &getFields() const { return fields_; }
  size_t getNumElems() const { return fields_.size(); }
  const Expr &getField(std::string_view name) const {
    return *fields_.find(name)->second;
  }

 private:
  // TODO: Consolidate this with StructType::AllUniqueNames.
  static bool AllUniqueNames(const FieldMap &fields) {
    std::set<std::string_view> names;
    for (auto it = fields.begin(); it != fields.end(); ++it)
      names.insert(it->first);
    return names.size() == fields.size();
  }

  FieldMap fields_;
};

class StructGet : public Expr {
 public:
  StructGet(const SourceLocation &start, const Type &type, Expr &expr,
            std::string_view member);

  static bool classof(const Node *node) {
    return node->getKind() == NK_StructGet;
  }

  const Expr &getExpr() const { return expr_; }
  std::string_view getMember() const { return member_; }
  Expr &getExpr() { return expr_; }

 private:
  Expr &expr_;
  std::string member_;
};

class Composite : public Expr {
 public:
  Composite(const SourceLocation &start, const CompositeType &type,
            const std::vector<Expr *> &exprs)
      : Expr(NK_Composite, start, type), exprs_(exprs) {
    for (Expr *e : exprs)
      e->AddUser(*this);
  }

  static bool classof(const Node *node) {
    return node->getKind() == NK_Composite;
  }
  const auto &getElems() const { return exprs_; }
  size_t getNumElems() const { return exprs_.size(); }
  const Expr &getElem(size_t i) const { return *exprs_.at(i); }

 private:
  const std::vector<Expr *> exprs_;
};

class Set : public Expr {
 public:
  Set(const SourceLocation &start, Expr &expr, Expr &idx, Expr &store);

  static bool classof(const Node *node) { return node->getKind() == NK_Set; }

  const Expr &getExpr() const { return expr_; }
  const Expr &getIdx() const { return idx_; }
  const Expr &getStore() const { return store_; }
  Expr &getExpr() { return expr_; }
  Expr &getIdx() { return idx_; }
  Expr &getStore() { return store_; }

 private:
  Expr &expr_, &idx_, &store_;
};

class Get : public Expr {
 public:
  Get(const SourceLocation &start, const Type &type, Expr &expr, Expr &idx);

  static bool classof(const Node *node) { return node->getKind() == NK_Get; }

  const Expr &getExpr() const { return expr_; }
  const Expr &getIdx() const { return idx_; }
  Expr &getExpr() { return expr_; }
  Expr &getIdx() { return idx_; }

 private:
  Expr &expr_, &idx_;
};

class Let : public Expr {
 public:
  Let(const SourceLocation &start, std::string_view name, Expr &expr)
      : Expr(NK_Let, start, expr.getType()), name_(name), expr_(expr) {
    expr.AddUser(*this);
  }

  std::string_view getName() const { return name_; }
  const Expr &getExpr() const { return expr_; }
  Expr &getExpr() { return expr_; }

  static bool classof(const Node *node) { return node->getKind() == NK_Let; }

 private:
  std::string name_;
  Expr &expr_;
};

class Keep : public Expr {
 public:
  Keep(const SourceLocation &start, std::string_view name, Expr &expr,
       Expr &body)
      : Expr(NK_Keep, start, body.getType()),
        name_(name),
        expr_(expr),
        body_(body) {
    expr.AddUser(*this);
    body.AddUser(*this);
  }

  std::string_view getName() const { return name_; }
  const Expr &getExpr() const { return expr_; }
  const Expr &getBody() const { return body_; }
  Expr &getExpr() { return expr_; }
  Expr &getBody() { return body_; }

  static bool classof(const Node *node) { return node->getKind() == NK_Keep; }

 private:
  std::string name_;
  Expr &expr_, &body_;
};

class Callable;

class Arg : public Expr {
 public:
  Arg(const SourceLocation &start, const Type &type, size_t arg_no)
      : Expr(NK_Arg, start, type), arg_no_(arg_no), parent_(nullptr) {}

  static bool classof(const Node *node) { return node->getKind() == NK_Arg; }
  size_t getArgNo() const { return arg_no_; }
  const Callable &getParent() const { return *parent_; }

 private:
  friend class ASTBuilder;

  void setParent(Callable &parent);

  const size_t arg_no_;
  Callable *parent_;
};

class Callable : public Expr {
 public:
  Callable(const SourceLocation &start, const CallableType &type,
           const std::vector<std::string> &arg_names,
           const std::vector<Arg *> &args)
      : Expr(NK_Callable, start, type),
        arg_names_(arg_names),
        args_(args),
        body_(nullptr) {
    assert(type.getArgTypes().size() == arg_names.size() &&
           "Differring number of argument names and types.");
    assert(arg_names.size() == args_.size());
  }

  static bool classof(const Node *node) {
    return node->getKind() == NK_Callable;
  }

  const CallableType &getType() const {
    return llvm::cast<CallableType>(Expr::getType());
  }

  std::vector<SourceLocation> getArgLocs() const {
    std::vector<SourceLocation> locs;
    locs.reserve(args_.size());
    for (const Arg *arg : args_)
      locs.push_back(arg->getStart());
    return locs;
  }

  const auto &getArgNames() const { return arg_names_; }
  std::string_view getArgName(size_t i) const { return arg_names_[i]; }
  size_t getNumArgs() const { return arg_names_.size(); }
  const auto &getArgs() const { return args_; }
  auto &getArgs() { return args_; }
  const Arg &getArg(size_t i) const { return *args_.at(i); }
  Arg &getArg(size_t i) { return *args_.at(i); }
  const Arg &getLastArg() const { return *args_.back(); }
  Arg &getLastArg() { return *args_.back(); }

  const Expr &getBody() const { return *body_; }
  Expr &getBody() { return *body_; }

  void setBody(Expr &body) {
    assert(!body_);
    body_ = &body;
    body.AddUser(*this);
  }

  bool hasBody() const { return body_; }

 private:
  const std::vector<std::string> arg_names_;
  const std::vector<Arg *> args_;
  Expr *body_;
};

class Cast : public Expr {
 public:
  Cast(const SourceLocation &start, const Type &type, Expr &expr)
      : Expr(NK_Cast, start, type), expr_(expr) {
    expr.AddUser(*this);
  }

  const Expr &getExpr() const { return expr_; }
  Expr &getExpr() { return expr_; }

  static bool classof(const Node *node) { return node->getKind() == NK_Cast; }

 private:
  Expr &expr_;
};

//
// decl readc = \IO -> <IO int>
//
class Readc : public Expr {
 public:
  Readc(const SourceLocation &start, const Type &type)
      : Expr(NK_Readc, start, type) {
    assert(IsReadcType(type));
  }

  static bool classof(const Node *node) { return node->getKind() == NK_Readc; }

  const CallableType &getCallbackType() const {
    return llvm::cast<CallableType>(
        llvm::cast<CallableType>(getType()).getArgType(1));
  }

 private:
  static bool IsReadcType(const Type &type);
};

class Call : public Expr {
 public:
  Call(const SourceLocation &start, const Type &type, Expr &func,
       const std::vector<Expr *> &args, bool pure = true)
      : Expr(NK_Call, start, type), func_(&func), args_(args), pure_(pure) {
    assert(func.getType().getReturnType() == type);
    func.AddUser(*this);
    for (Expr *arg : args) {
      arg->AddUser(*this);
    }
  }

  static bool classof(const Node *node) { return node->getKind() == NK_Call; }

  const Expr &getFunc() const { return *func_; }
  Expr &getFunc() { return *func_; }
  const auto &getArgs() const { return args_; }
  bool isPure() const { return pure_; }
  size_t getNumArgs() const { return args_.size(); }
  const Expr &getArgAt(size_t i) const { return *args_.at(i); }
  Expr &getArgAt(size_t i) { return *args_.at(i); }

  auto getArgTypes() const {
    std::vector<const Type *> arg_types;
    arg_types.reserve(args_.size());
    for (const Expr *arg : args_)
      arg_types.push_back(&arg->getType());
    return arg_types;
  }

  void SwapFunc(Expr &other) {
    assert(other.getType().getReturnType() == func_->getType().getReturnType());
    assert(
        llvm::cast<CallableType>(other.getType()).CanApplyArgs(getArgTypes()));
    func_->RemoveUser(*this);
    func_ = &other;
    other.AddUser(*this);
  }

 private:
  Expr *func_;
  const std::vector<Expr *> args_;
  const bool pure_;
};

// This represents a callable that could resolve to more than one other
// callables. An example is with:
//
//   decl writeln = \IO GENERIC -> IO
//   decl writeln = \IO GENERIC GENERIC_REMAINING -> IO
//   def writeln = \IO io GENERIC arg GENERIC_REMAINING remaining -> IO
//     let io2 = IO write(io arg)
//     writeln(io2 remaining)
//
// where the last call to `writeln` can resolve to either of the two `writeln`
// declarations, but we won't know until lowering. The result will be an
// ambiguous call.
//
// NOTE this is only used for calls with GENERIC_REMAINING args.
class AmbiguousCall : public Expr {
 public:
  AmbiguousCall(const SourceLocation &start, const Type &type,
                const std::vector<Expr *> &funcs,
                const std::vector<Expr *> &args)
      : Expr(NK_AmbiguousCall, start, type), funcs_(funcs), args_(args) {
    assert(CanMake(funcs));
    assert(std::all_of(funcs.begin(), funcs.end(), [&type](const Expr *e) {
      return e->getType().getReturnType() == type;
    }));
    for (Expr *func : funcs)
      func->AddUser(*this);
    for (Expr *arg : args)
      arg->AddUser(*this);
  }

  static bool CanMake(const std::vector<Expr *> &funcs) {
    if (funcs.empty())
      return false;

    return std::any_of(funcs.begin(), funcs.end(), [](const Expr *e) {
      return e->getType().isGenericRemainingCallable();
    });
  }

  static bool classof(const Node *node) {
    return node->getKind() == NK_AmbiguousCall;
  }

  const std::vector<Expr *> &getFuncs() const { return funcs_; }
  const std::vector<Expr *> &getFuncs() { return funcs_; }
  const auto &getArgs() const { return args_; }
  size_t getNumArgs() const { return args_.size(); }
  const Expr &getArgAt(size_t i) const { return *args_.at(i); }
  Expr &getArgAt(size_t i) { return *args_.at(i); }

  auto getArgTypes() const {
    std::vector<const Type *> arg_types;
    arg_types.reserve(args_.size());
    for (const Expr *arg : args_)
      arg_types.push_back(&arg->getType());
    return arg_types;
  }

 private:
  std::vector<Expr *> funcs_;
  const std::vector<Expr *> args_;
};

class Zero : public Expr {
 public:
  Zero(const SourceLocation &start, const Type &type)
      : Expr(NK_Zero, start, type) {}

  static bool classof(const Node *node) { return node->getKind() == NK_Zero; }
};

class Int : public Expr {
 public:
  Int(const SourceLocation &start, int i, const Type &type)
      : Expr(NK_Int, start, type), i_(i) {}

  int getInt() const { return i_; }
  static bool classof(const Node *node) { return node->getKind() == NK_Int; }

 private:
  int i_;
};

class Char : public Expr {
 public:
  Char(const SourceLocation &start, char c, const Type &type)
      : Expr(NK_Char, start, type), c_(c) {}
  static bool classof(const Node *node) { return node->getKind() == NK_Char; }

  char getChar() const { return c_; }

 private:
  char c_;
};

class Str : public Expr {
 public:
  Str(const SourceLocation &start, std::string_view str, const Type &type)
      : Expr(NK_Str, start, type), str_(str) {}

  std::string_view get() const { return str_; }

  static bool classof(const Node *node) { return node->getKind() == NK_Str; }

 private:
  const std::string str_;
};

class Bool : public Expr {
 public:
  Bool(const SourceLocation &start, bool b, const Type &type)
      : Expr(NK_Bool, start, type), b_(b) {
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

  BinOp(const SourceLocation &start, Expr &lhs, Expr &rhs, const Type &type,
        OpKind op)
      : Expr(NK_BinOp, start, type), lhs_(lhs), rhs_(rhs), op_(op) {
    lhs.AddUser(*this);
    rhs.AddUser(*this);
  }
  static bool classof(const Node *node) { return node->getKind() == NK_BinOp; }

  const Expr &getLHS() const { return lhs_; }
  const Expr &getRHS() const { return rhs_; }
  Expr &getLHS() { return lhs_; }
  Expr &getRHS() { return rhs_; }
  OpKind getOp() const { return op_; }

 private:
  Expr &lhs_, &rhs_;
  OpKind op_;
};

class If : public Expr {
 public:
  If(const SourceLocation &start, const Type &type, Expr &cond, Expr &if_body,
     Expr &else_body)
      : Expr(NK_If, start, type),
        cond_(cond),
        if_body_(if_body),
        else_body_(else_body) {
    assert(type.CanConvertFrom(if_body));
    assert(type.CanConvertFrom(else_body));
    assert(cond.getType().isNamedType(builtins::kBoolTypeName));
    cond.AddUser(*this);
    if_body.AddUser(*this);
    else_body.AddUser(*this);
  }
  static bool classof(const Node *node) { return node->getKind() == NK_If; }

  const Expr &getCond() const { return cond_; }
  const Expr &getIf() const { return if_body_; }
  const Expr &getElse() const { return else_body_; }
  Expr &getCond() { return cond_; }
  Expr &getIf() { return if_body_; }
  Expr &getElse() { return else_body_; }

 private:
  Expr &cond_, &if_body_, &else_body_;
};

class Module {
 public:
  void AddDeclaration(std::string_view name, Declare &expr);
  void AddTypeDef(std::string_view name, const Type &);
  bool hasTypeDef(std::string_view name) const {
    return typedefs_.contains(name);
  }
  const Type &getTypeDef(std::string_view name) const {
    return *typedefs_.find(name)->second;
  }

  const auto &getAST() const { return ast_; }
  const auto &getGenerics() const { return generics_; }
  std::vector<Declare *> getDeclares(std::string_view name) const {
    auto found = top_level_exprs_.find(name);
    if (found == top_level_exprs_.end())
      return {};
    return found->second;
  }

  void MangleDecls();

 private:
  std::vector<Declare *> ast_;
  std::map<std::string, std::vector<Declare *>, std::less<>> top_level_exprs_;
  std::set<Declare *> generics_;
  std::map<std::string, const Type *, std::less<>> typedefs_;
};

}  // namespace lang

#endif  // AST_H_
