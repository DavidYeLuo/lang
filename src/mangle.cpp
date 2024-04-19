#include "mangle.h"

namespace lang {

// TODO: Also account for qualifiers.
std::string Mangle(const Type &type) {
  switch (type.getKind()) {
    case Type::TK_GenericRemainingType:
    case Type::TK_GenericType:
      UNREACHABLE(
          "Generic types should not be directly used for code emission.");
    case Type::TK_NamedType:
      return std::string(llvm::cast<NamedType>(type).getName());
    case Type::TK_ArrayType: {
      const auto &array_ty = llvm::cast<ArrayType>(type);
      std::stringstream ss;
      ss << "arr_" << array_ty.getNumElems() << "_"
         << Mangle(array_ty.getElemType());
      return ss.str();
    }
    case Type::TK_CompositeType: {
      const auto &composite_ty = llvm::cast<CompositeType>(type);
      std::stringstream ss;
      ss << "ct";
      for (const Type *ty : composite_ty.getTypes()) {
        ss << "_" << Mangle(*ty);
      }
      return ss.str();
    }
    case Type::TK_CallableType: {
      const auto &callable_ty = llvm::cast<CallableType>(type);
      std::stringstream ss;
      ss << "ret_" << Mangle(callable_ty.getReturnType());
      for (size_t i = 0; i < callable_ty.getArgTypes().size(); ++i) {
        ss << "_arg" << i << "_" << Mangle(*callable_ty.getArgTypes().at(i));
      }
      return ss.str();
    }
    case Type::TK_StructType: {
      const auto &struct_ty = llvm::cast<StructType>(type);
      // Use the ordered version so we always produce the same mangle for a
      // type.
      auto types = struct_ty.getOrderedTypes();
      std::stringstream ss;
      ss << "struct";
      for (const auto &p : types)
        ss << "_" << p.first << "_" << Mangle(*p.second);
      return ss.str();
    }
  }
  __builtin_unreachable();
}

std::string Mangle(std::string_view name, const Type &type) {
  std::string s(name);
  s += "_";
  s += Mangle(type);
  return s;
}

}  // namespace lang
