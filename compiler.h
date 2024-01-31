#ifndef COMPILER_H_
#define COMPILER_H_

#include <filesystem>
#include <functional>
#include <ostream>
#include <sstream>
#include <string>

#include "ast.h"
#include "llvm/Analysis/CGSCCPassManager.h"
#include "llvm/Analysis/LoopAnalysisManager.h"
#include "llvm/IR/DIBuilder.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/PassManager.h"
#include "llvm/IR/Verifier.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"
#include "llvm/TargetParser/Host.h"

namespace lang {

enum DumpType {
  File,
  ASM,
  IR,
};

class Compiler {
 public:
  Compiler(llvm::Module &mod) : Compiler(mod, "") {}

  Compiler(llvm::Module &mod, std::filesystem::path source_path)
      : mod_(mod),
        di_builder_(mod),
        di_unit_(
            source_path.empty()
                ? *di_builder_.createFile(/*Filename=*/"<input>",
                                          /*Direcory=*/".")
                : *di_builder_.createFile(source_path.filename().c_str(),
                                          source_path.parent_path().c_str())),
        di_cu_(*di_builder_.createCompileUnit(
            llvm::dwarf::DW_LANG_C, &di_unit_,
            /*Producer=*/"Lang Compiler", /*isOptimized=*/false, /*Flags=*/"",
            /*RuntimeVersion=*/0)) {}

  llvm::DIBuilder &getDIBuilder() { return di_builder_; }

  void CompileDefine(const Define &define);
  void CompileDeclare(const Declare &declare);

  std::string Mangle(const Type &type) const;

  llvm::Value *getExpr(llvm::IRBuilder<> &builder, const Expr &expr);
  // Generate code that checks if the resulting expression value is equal to
  // zero.
  llvm::Value *getBoolExpr(llvm::IRBuilder<> &builder, const Expr &expr);
  llvm::Value *getCallable(llvm::IRBuilder<> &builder,
                           const Callable &callable);
  llvm::Value *getLet(llvm::IRBuilder<> &builder, const Let &let);
  llvm::Value *getKeep(llvm::IRBuilder<> &builder, const Keep &keep);
  llvm::Value *getCall(llvm::IRBuilder<> &builder, const Call &call);
  llvm::Value *getZero(llvm::IRBuilder<> &, const Zero &);
  llvm::Value *getWrite(const Write &write);
  llvm::Value *getReadc(llvm::IRBuilder<> &builder, const Readc &readc);
  llvm::Value *getBinOp(llvm::IRBuilder<> &builder, const BinOp &binop);
  llvm::Value *getIf(llvm::IRBuilder<> &builder, const If &);
  llvm::Value *getCast(llvm::IRBuilder<> &builder, const Cast &);
  llvm::Value *getGet(llvm::IRBuilder<> &builder, const Get &);
  llvm::Value *getSet(llvm::IRBuilder<> &builder, const Set &);
  llvm::Value *getComposite(llvm::IRBuilder<> &builder, const Composite &);

  llvm::Type *getLLVMType(const Expr &expr) const {
    return getLLVMType(expr.getType());
  }
  llvm::FunctionType *getLLVMFuncType(const Expr &expr) const {
    return getLLVMFuncType(expr.getType());
  }
  llvm::Type *getLLVMType(const Type &ty) const;
  llvm::Type *getLLVMType(const NamedType &type) const;
  llvm::Type *getLLVMType(const CompositeType &ty) const;
  llvm::Type *getLLVMType(const ArrayType &ty) const;
  llvm::FunctionType *getLLVMFuncType(const CallableType &ty) const;
  llvm::FunctionType *getLLVMFuncType(const Type &ty) const {
    return getLLVMFuncType(llvm::cast<CallableType>(ty));
  }

  llvm::Value *getStr(llvm::IRBuilder<> &builder, const Str &str) const {
    return builder.CreateGlobalStringPtr(str.get());
  }
  llvm::Value *getChar(llvm::IRBuilder<> &builder, const Char &c) const {
    return llvm::ConstantInt::getSigned(getCharType(), c.getChar());
  }

 private:
  bool ShouldReturnValueAsArgument(const Type &type) const {
    return type.isCompositeOrArrayType();
  }

  llvm::Function *getCPrintf() const;
  llvm::Function *getCGetc() const;
  llvm::Value *getLoadedCStdin(llvm::IRBuilder<> &builder) const;

  llvm::IntegerType *getIntType(size_t num_bits) const {
    return llvm::IntegerType::get(mod_.getContext(), num_bits);
  }

  llvm::Type *getStrType() const { return getCharType()->getPointerTo(); }
  llvm::Type *getCharType() const { return getIntType(8); }

  llvm::Function *getMainWrapper(llvm::FunctionType *func_ty) const;

  llvm::Value *getExprImpl(llvm::IRBuilder<> &builder, const Expr &expr);

  // This exists to handle PHI nodes where calling Value::replaceAllUsesWith
  // doesn't also replace the incoming values of the phi node with the
  // replacement.
  void ReplaceAllUsesWith(llvm::Value *replacement, llvm::Value *replacing);

  // This also handles storing large data structures like composites or arrays
  // into pointers which will involve memcpys.
  void DoStore(llvm::IRBuilder<> &builder, llvm::Value *store_ptr,
               const Expr &expr);

  void FillFuncBody(const Callable &callable, llvm::Function *func);

  // llvm::DISubroutineType *CreateFunctionType(size_t num_args);
  llvm::DIType *getDIType(const Type &type) {
    switch (type.getKind()) {
#define TYPE(name)      \
  case Type::TK_##name: \
    return getDIType(llvm::cast<name>(type));
#include "types.def"
    }
  }
#define TYPE(name) llvm::DIType *getDIType(const name &);
#include "types.def"

  llvm::Module &mod_;
  std::unique_ptr<NamedType> io_type_;
  std::unique_ptr<NamedType> str_type_;
  std::map<const Node *, llvm::Value *> processed_exprs_;
  llvm::DIBuilder di_builder_;
  llvm::DIFile &di_unit_;
  llvm::DICompileUnit &di_cu_;
};

bool Compile(const std::vector<const Node *> &ast, std::string_view outfile,
             DumpType dump, std::filesystem::path source = "");
bool Compile(const std::vector<const Node *> &ast, std::ostream &out,
             DumpType dump, std::string_view modname,
             std::filesystem::path source = "");

}  // namespace lang

#endif  // COMPILER_H_
