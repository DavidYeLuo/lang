#ifndef COMPILER_H_
#define COMPILER_H_

#include <filesystem>
#include <ostream>
#include <string_view>

#include "ast.h"

// There's a bunch of conversion warnings in LLVM headers.
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wconversion"
#include "llvm/IR/Module.h"
#include "llvm/Passes/OptimizationLevel.h"
#pragma GCC diagnostic push

namespace lang {

enum class DumpType {
  File,
  ASM,
  IR,
};

bool Compile(lang::Module &mod, std::string_view outfile, DumpType dump,
             const std::filesystem::path &source = "",
             llvm::OptimizationLevel optlvl = llvm::OptimizationLevel::O0,
             bool sanitize_address = false);
bool Compile(lang::Module &mod, std::ostream &out, DumpType dump,
             std::string_view modname, const std::filesystem::path &source = "",
             llvm::OptimizationLevel optlvl = llvm::OptimizationLevel::O0,
             bool sanitize_address = false);

class ASTBuilder;
void Lower(Module &mod, ASTBuilder &builder);

}  // namespace lang

#endif  // COMPILER_H_
