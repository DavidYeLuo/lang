#include "ast.h"

#include <span>

#include "mangle.h"

namespace lang {

bool Type::isAggregateType() const {
  return llvm::isa<CompositeType>(this) || llvm::isa<ArrayType>(this) ||
         llvm::isa<StructType>(this) || isGeneric();
}

bool Type::isStructType() const {
  return llvm::isa<StructType>(this) || isGeneric();
}

bool GenericType::Equals(const Type &other) const {
  return llvm::isa<GenericType>(other);
}

bool GenericRemainingType::Equals(const Type &other) const {
  return llvm::isa<GenericRemainingType>(other);
}

bool NamedType::Equals(const Type &rhs) const {
  if (const auto *named_rhs = llvm::dyn_cast<NamedType>(&rhs))
    return getName() == named_rhs->getName();

  return false;
}

bool ArrayType::Equals(const Type &rhs) const {
  if (const auto *array_rhs = llvm::dyn_cast<ArrayType>(&rhs)) {
    if (getNumElems() != array_rhs->getNumElems())
      return false;

    return getElemType().Equals(array_rhs->getElemType());
  }
  return false;
}

bool StructType::Equals(const Type &rhs) const {
  if (const auto *struct_rhs = llvm::dyn_cast<StructType>(&rhs)) {
    if (getNumTypes() != struct_rhs->getNumTypes())
      return false;

    for (auto it = types_.begin(); it != types_.end(); ++it) {
      std::string_view name(it->first);
      if (!struct_rhs->hasField(name))
        return false;

      if (!getField(name).Equals(struct_rhs->getField(name)))
        return false;
    }

    return true;
  }
  return false;
}

bool CompositeType::Equals(const Type &rhs) const {
  if (const auto *composite_rhs = llvm::dyn_cast<CompositeType>(&rhs)) {
    if (getTypes().size() != composite_rhs->getTypes().size())
      return false;

    for (size_t i = 0; i < getTypes().size(); ++i) {
      if (!getTypeAt(i).Equals(composite_rhs->getTypeAt(i)))
        return false;
    }

    return true;
  }
  return false;
}

bool CallableType::Equals(const Type &rhs) const {
  if (const auto *callable_rhs = llvm::dyn_cast<CallableType>(&rhs)) {
    if (getArgTypes().size() != callable_rhs->getArgTypes().size())
      return false;

    for (size_t i = 0; i < getArgTypes().size(); ++i) {
      if (!getArgTypes().at(i)->Equals(*callable_rhs->getArgTypes().at(i)))
        return false;
    }

    return getReturnType().Equals(callable_rhs->getReturnType());
  }
  return false;
}

bool CallableType::CanApplyArgs(std::span<const Type *const> args) const {
  return lang::CanApplyArgs(arg_types_, args);
}

bool CallableType::CanApplyArgs(std::span<Expr *const> args) const {
  std::vector<const Type *> arg_types(args.size());
  std::transform(args.begin(), args.end(), arg_types.begin(),
                 [](const Expr *e) { return &e->getType(); });
  return CanApplyArgs(arg_types);
}

bool CanApplyArgs(std::span<const Type *const> args1,
                  std::span<const Type *const> args2) {
  // Let's take this example:
  //
  //   def writeln = \IO io GENERIC arg -> IO
  //     let io2 = IO write(io arg)
  //     write(io2 "\n")
  //
  //   decl writeln = \IO GENERIC GENERIC_REMAINING -> IO
  //   def writeln = \IO io GENERIC arg GENERIC_REMAINING remaining -> IO
  //     let io2 = IO write(io arg)
  //     writeln(io2 remaining)
  //
  // In the last call to writeln, we won't know until lowering which `writeln`
  // this should actually point to since `remaining` could have any number of
  // arguments at this point. The same would be true if we had a.
  //
  //   decl writeln = \IO GENERIC_REMAINING -> IO
  //   writeln(io2 generic_arg remaining)
  //
  // So if two args are GENERIC_REMAINING, then they can always match.
  //
  // FIXME: Account for if GENERIC_REMAINING is NOT the last argument. That is,
  // it can be either the first or middle arguments.
  if (!args1.empty() && llvm::isa<GenericRemainingType>(args1.back()) &&
      !args2.empty() && llvm::isa<GenericRemainingType>(args2.back()))
    return true;

  if (!args1.empty() && llvm::isa<GenericRemainingType>(args1.back())) {
    if (args2.size() < args1.size())
      return false;
    // Check all arguments up to the generic one.
    auto dist = args1.end() - 1 - args1.begin();
    return CanApplyArgs(args1.subspan(0, dist), args2.subspan(0, dist));
  }
  if (!args2.empty() && llvm::isa<GenericRemainingType>(args2.back())) {
    if (args1.size() < args2.size())
      return false;
    // Check all arguments up to the generic one.
    auto dist = args2.end() - 1 - args2.begin();
    return CanApplyArgs(args1.subspan(0, dist), args2.subspan(0, dist));
  }

  if (args1.size() != args2.size())
    return false;

  for (size_t i = 0; i < args1.size(); ++i) {
    if (args1[i]->isGeneric() || args2[i]->isGeneric())
      continue;

    if (!args1[i]->CanConvertFrom(*args2[i]))
      return false;
  }

  return true;
}

bool CallableType::CallableTypesMatch(const CallableType &rhs) const {
  if (!isGeneric() && !rhs.isGeneric())
    return *this == rhs;
  if (getReturnType() != rhs.getReturnType())
    return false;
  return lang::CanApplyArgs(getArgTypes(), rhs.getArgTypes());
}

bool Type::Matches(const Type &other) const {
  if (const auto *this_callable_ty = llvm::dyn_cast<CallableType>(this)) {
    if (const auto *other_callable_ty = llvm::dyn_cast<CallableType>(&other))
      return this_callable_ty->CallableTypesMatch(*other_callable_ty);
  }

  // FIXME: This isn't inherently true. We have this check specifically for
  // doing early type checking during the parsing stage. This type of logic
  // should probably be moved to the parser rather than have it part of the
  // type class.
  if (other.isGeneric() || isGeneric())
    return true;

  return Equals(other);
}

void Module::MangleDecls() {
  for (Declare *decl : ast_) {
    if (decl->getName() != "main" && !decl->isCDecl() &&
        llvm::isa<CallableType>(decl->getType()) &&
        !decl->getType().isGenericCallable()) {
      decl->setName(Mangle(decl->getName(), decl->getType()));
    }
  }
}

void Arg::setParent(Callable &parent) {
  assert(!parent_);
  parent_ = &parent;
  parent.AddUser(*this);
}

const Type &Type::getReturnType() const {
  return llvm::cast<CallableType>(*this).getReturnType();
}

bool Type::isNamedType(std::string_view name) const {
  if (const auto *named_ty = llvm::dyn_cast<NamedType>(this))
    return named_ty->getName() == name;
  return false;
}

bool Type::isCharArray() const {
  if (const auto *array_ty = llvm::dyn_cast<ArrayType>(this))
    return array_ty->getElemType().isNamedType("char");
  return false;
}

bool Readc::IsReadcType(const Type &type) {
  if (const auto *callable_ty = llvm::dyn_cast<CallableType>(&type)) {
    if (const auto *comp_ty =
            llvm::dyn_cast<CompositeType>(&callable_ty->getReturnType())) {
      if (comp_ty->getNumTypes() != 2)
        return false;

      if (!comp_ty->getTypeAt(0).isNamedType("IO"))
        return false;

      if (!comp_ty->getTypeAt(1).isNamedType("int"))
        return false;

      if (callable_ty->getArgTypes().size() != 1)
        return false;

      return callable_ty->getArgType(0).isNamedType("IO");
    }
  }
  return false;
}

bool IsBuiltinType(std::string_view name) {
  return name == builtins::kIntTypeName || name == builtins::kCharTypeName ||
         name == builtins::kIOTypeName || name == builtins::kBoolTypeName ||
         name == builtins::kCPtrTypeName || name == builtins::kNoneTypeName;
}

bool Type::isBuiltinType() const {
  if (const auto *named_ty = llvm::dyn_cast<NamedType>(this))
    return IsBuiltinType(named_ty->getName());
  return false;
}

bool Type::isGenericCallable() const {
  if (const auto *ty = llvm::dyn_cast<CallableType>(this))
    return ty->isGeneric();
  return false;
}

bool Type::isGenericRemainingCallable() const {
  if (const auto *ty = llvm::dyn_cast<CallableType>(this)) {
    if (ty->getNumArgs() == 0)
      return false;

    return std::any_of(
        ty->getArgTypes().begin(), ty->getArgTypes().end(),
        [](const Type *t) { return llvm::isa<GenericRemainingType>(t); });
  }
  return false;
}

bool Type::CanConvertFrom(const Expr &e) const {
  return CanConvertFrom(e.getType());
}

bool Type::CanConvertFrom(const Type &from) const {
  // Any type can be converted to a generic type.
  //
  // Likewise, we do not know enough about converting from a generic type to a
  // non-generic type until lowering.
  if (isGeneric() || from.isGeneric())
    return true;

  return Equals(from);
}

void Module::AddTypeDef(std::string_view name, const Type &type) {
  assert(!typedefs_.contains(name));
  assert(!IsBuiltinType(name));
  typedefs_[std::string(name)] = &type;
}

void Module::AddDeclaration(std::string_view name, Declare &expr) {
  assert(expr.getName() == name);
  auto &decls = top_level_exprs_[std::string(name)];
  for (Declare *decl : decls) {
    if (expr.getType().isGeneric() == decl->getType().isGeneric())
      assert(!expr.getType().Matches(decl->getType()) &&
             "A declaration with this name and type already exist in the "
             "module. It cannot be re-added.");
    else
      assert(expr.getType() != decl->getType());
  }
  decls.push_back(&expr);
  ast_.push_back(&expr);
  if (expr.getType().isGeneric())
    generics_.insert(&expr);
}

Set::Set(const SourceLocation &start, Expr &expr, Expr &idx, Expr &store)
    : Expr(NK_Set, start, expr.getType()),
      expr_(expr),
      idx_(idx),
      store_(store) {
  assert(expr.getType().isAggregateType());
  assert(idx.getType().isNamedType("int"));

  if (const auto *comp_ty = llvm::dyn_cast<CompositeType>(&getType())) {
    Int &i = llvm::cast<Int>(idx);
    assert(comp_ty->getTypeAt(i.getInt()) == store.getType() &&
           "Store type does not match type type at the index of this composite "
           "type");
  }

  expr.AddUser(*this);
  idx.AddUser(*this);
  store.AddUser(*this);
}

Get::Get(const SourceLocation &start, const Type &type, Expr &expr, Expr &idx)
    : Expr(NK_Get, start, type), expr_(expr), idx_(idx) {
  assert(expr.getType().isAggregateType());
  assert(idx.getType().isNamedType("int"));

  if (llvm::isa<CompositeType>(getType())) {
    assert(llvm::isa<Int>(idx) &&
           "Composite types must be indexed by a constant value");
  }

  expr.AddUser(*this);
  idx.AddUser(*this);
}

StructGet::StructGet(const SourceLocation &start, const Type &type, Expr &expr,
                     std::string_view member)
    : Expr(NK_StructGet, start, type), expr_(expr), member_(member) {
  assert(expr.getType().isStructType());

  if (const auto *struct_ty = llvm::dyn_cast<StructType>(&expr.getType())) {
    assert(struct_ty->hasField(member));
  }

  expr.AddUser(*this);
}

}  // namespace lang
