#ifndef MANGLE_H_
#define MANGLE_H_

#include <sstream>
#include <string>
#include <string_view>

#include "ast.h"
#include "lang.h"

namespace lang {

std::string Mangle(const Type &type);
std::string Mangle(std::string_view name, const Type &type);

}  // namespace lang

#endif  // MANGLE_H_
