#include "astbuilder.h"

#include <string_view>

#include "ast.h"

namespace lang {

const NamedType &ASTBuilder::getNamedType(std::string_view name) {
  auto found = named_types_.find(name);
  if (found != named_types_.end())
    return *found->second;

  const NamedType *ptr = new NamedType(name);
  types_.emplace_back(ptr);
  named_types_[std::string(name)] = ptr;
  return *ptr;
}

}  // namespace lang
