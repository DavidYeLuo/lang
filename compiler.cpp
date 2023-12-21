#include <fstream>
#include <iostream>
#include <string_view>
#include <vector>

#define HAS_LSAN __has_feature(address_sanitizer)
#if HAS_LSAN
#include <sanitizer/lsan_interface.h>
#endif

#include "astdumper.h"
#include "compiler.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/raw_os_ostream.h"
#include "llvm/Support/raw_ostream.h"

namespace lang {

void Compiler::CompileDefine(const Define &define) {
  if (const auto *func_body = llvm::dyn_cast<Callable>(&define.getBody())) {
    const auto &func_ty = llvm::cast<CallableType>(func_body->getType());
    llvm::FunctionType *llvm_func_ty = getLLVMFuncType(func_ty);

    llvm::Function *func;
    if (define.getName() == "main") {
      assert(func_ty.getReturnType().isNamedType(builtins::kIOTypeName) &&
             "Return type of main should be IO");
      assert(func_ty.getArgTypes().size() == 1 &&
             "`main` func should have one argument of IO");
      assert(func_ty.getArgTypes()[0]->isNamedType(builtins::kIOTypeName) &&
             "First argument to main should be IO");
      func = getMainWrapper(llvm_func_ty);
    } else {
      func =
          llvm::Function::Create(llvm_func_ty, llvm::Function::ExternalLinkage,
                                 define.getName(), mod_);
    }

    // TODO: Maybe getCallable can handle all this?
    assert(!processed_exprs_.contains(func_body));
    processed_exprs_.try_emplace(func_body, func);

    bool return_as_arg = ShouldReturnValueAsArgument(func_ty.getReturnType());
    if (return_as_arg) {
      func->getArg(0)->setName("return_val");
    }
    for (size_t i = 0; i < func_body->getArgNames().size(); ++i) {
      func->getArg(i + return_as_arg)->setName(func_body->getArgName(i));
    }

    // Create the function body.
    llvm::BasicBlock *bb =
        llvm::BasicBlock::Create(mod_.getContext(), "entry", func);
    llvm::IRBuilder<> builder(bb);

    llvm::Value *ret = getExpr(builder, func_body->getBody());
    if (return_as_arg) {
      ReplaceAllUsesWith(func->getArg(0), ret);
      builder.CreateRetVoid();
    } else {
      builder.CreateRet(ret);
    }

    if (llvm::verifyFunction(*func, &llvm::errs())) {
      mod_.dump();
      std::cerr << "Function not well-formed" << std::endl;
      func->print(llvm::errs());
      __builtin_trap();
    }
  } else {
    // TODO: Handle non-function definitions.
    __builtin_trap();
  }
}

void Compiler::CompileDeclare(const Declare &declare) {
  if (llvm::isa<CallableType>(declare.getType())) {
    llvm::Value *func =
        mod_.getOrInsertFunction(declare.getName(), getLLVMFuncType(declare))
            .getCallee();
    assert(!processed_exprs_.contains(&declare));
    processed_exprs_.try_emplace(&declare, func);
  } else {
    mod_.getOrInsertGlobal(declare.getName(), getLLVMType(declare));
  }
}

// The original function was called `main`. `main` is the underlying entry
// point that libc will dispatch to, so instead create a wrapper that is the
// actual `main` then dispatch to this function.
llvm::Function *Compiler::getMainWrapper(llvm::FunctionType *func_ty) const {
  llvm::Function *impl_func = llvm::Function::Create(
      func_ty, llvm::Function::ExternalLinkage, "main_impl", mod_);

  llvm::FunctionType *main_func_ty =
      llvm::FunctionType::get(getIntType(32), {}, /*isVarArg=*/false);
  llvm::Function *main_func = llvm::Function::Create(
      main_func_ty, llvm::Function::ExternalLinkage, "main", mod_);

  llvm::BasicBlock *bb =
      llvm::BasicBlock::Create(mod_.getContext(), "entry", main_func);
  llvm::IRBuilder<> builder(bb);

  // TODO: Dispatch argc + argv to impl func.
  auto *zero = llvm::ConstantInt::get(getIntType(32), 0);
  builder.CreateCall(impl_func, {zero});
  builder.CreateRet(zero);

  return impl_func;
}

llvm::FunctionType *Compiler::getLLVMFuncType(const CallableType &ty) const {
  std::vector<llvm::Type *> func_args;
  bool return_as_arg = ShouldReturnValueAsArgument(ty.getReturnType());
  if (return_as_arg) {
    // The return type of this function is too large to return in IR (such as a
    // composite type with many elements). So we should instead allocate the
    // buffer in the caller and the function is in charge of "returning" the
    // value by initializing the first argument.
    func_args.push_back(llvm::PointerType::getUnqual(mod_.getContext()));
  }

  for (const Type *arg_ty : ty.getArgTypes()) {
    llvm::Type *llvm_arg_ty = getLLVMType(*arg_ty);
    if (!llvm_arg_ty->isFirstClassType()) {
      // Function arguments must have first-class types!
      llvm_arg_ty = llvm::PointerType::getUnqual(mod_.getContext());
    } else if (llvm::isa<CompositeType>(arg_ty) ||
               llvm::isa<ArrayType>(arg_ty)) {
      // Composite types result in LLVM struct types, but we manifest composite
      // types as allocas (ptr types), so when passed as arguments, we should
      // ensure they are represented as pointers.
      llvm_arg_ty = llvm::PointerType::getUnqual(mod_.getContext());
    }
    func_args.push_back(llvm_arg_ty);
  }
  llvm::Type *ret_ty = return_as_arg ? llvm::Type::getVoidTy(mod_.getContext())
                                     : getLLVMType(ty.getReturnType());
  return llvm::FunctionType::get(ret_ty, func_args, /*isVarArg=*/false);
}

llvm::Type *Compiler::getLLVMType(const Type &ty) const {
  switch (ty.getKind()) {
    case Node::NK_NamedType:
      return getLLVMType(llvm::cast<NamedType>(ty));
    case Node::NK_CompositeType:
      return getLLVMType(llvm::cast<CompositeType>(ty));
    case Node::NK_CallableType:
      return getLLVMFuncType(llvm::cast<CallableType>(ty));
    case Node::NK_ArrayType:
      return getLLVMType(llvm::cast<ArrayType>(ty));
#define NODE(name) case Node::NK_##name:
#define TYPE(name)
#include "nodes.def"
      std::cerr << "Unhandled type: " << ty.toString() << std::endl;
      __builtin_trap();
  }
}

llvm::Type *Compiler::getLLVMType(const ArrayType &type) const {
  return llvm::ArrayType::get(getLLVMType(type.getElemType()),
                              type.getNumElems());
}

llvm::Type *Compiler::getLLVMType(const CompositeType &type) const {
  std::vector<llvm::Type *> types;
  for (const Type *ty : type.getTypes()) types.push_back(getLLVMType(*ty));
  return llvm::StructType::create(types);
}

llvm::Type *Compiler::getLLVMType(const NamedType &type) const {
  // if (type.getName() == "str") return getStrType();
  if (type.getName() == builtins::kIOTypeName) return getIntType(32);
  if (type.getName() == builtins::kIntTypeName) return getIntType(32);
  if (type.getName() == builtins::kCharTypeName) return getCharType();
  if (type.getName() == builtins::kNoneTypeName)
    return llvm::Type::getVoidTy(mod_.getContext());
  if (type.getName() == builtins::kBoolTypeName) return getIntType(1);
  if (type.getName() == builtins::kCPtrTypeName)
    return llvm::PointerType::getUnqual(mod_.getContext());
  std::cerr << "Unhandled type name: " << type.getName() << std::endl;
  __builtin_trap();
}

void Compiler::ReplaceAllUsesWith(llvm::Value *replacement,
                                  llvm::Value *replacing) {
  // Do a normal replacement.
  replacing->replaceAllUsesWith(replacement);

  // Doing a traditional Value::replaceAllUsesWith doesn't replace the incoming
  // values of the phi node with the replacement, so instead we need to manually
  // call RAUW on the incoming nodes. Then we can just delete the phi node.
  if (auto *phi = llvm::dyn_cast<llvm::PHINode>(replacing)) {
    for (llvm::Value *incoming : phi->incoming_values()) {
      ReplaceAllUsesWith(replacement, incoming);
    }
  }
}

llvm::Value *Compiler::getCallable(llvm::IRBuilder<> &builder,
                                   const Callable &callable) {
  const Type &ty = callable.getType();
  llvm::FunctionType *llvm_func_ty = getLLVMFuncType(callable.getType());

  llvm::Function *func = llvm::Function::Create(
      llvm_func_ty, llvm::Function::ExternalLinkage, "", mod_);

  bool return_as_arg = ShouldReturnValueAsArgument(ty.getReturnType());
  if (return_as_arg) {
    func->getArg(0)->setName("return_val");
  }
  for (size_t i = 0; i < callable.getNumArgs(); ++i) {
    func->getArg(i + return_as_arg)->setName(callable.getArgName(i));
  }

  // Create the function body.
  llvm::BasicBlock *bb =
      llvm::BasicBlock::Create(mod_.getContext(), "entry", func);
  llvm::IRBuilder<> nested_builder(bb);

  llvm::Value *ret = getExpr(nested_builder, callable.getBody());
  if (return_as_arg) {
    ReplaceAllUsesWith(func->getArg(0), ret);
    nested_builder.CreateRetVoid();
  } else {
    nested_builder.CreateRet(ret);
  }

  if (llvm::verifyFunction(*func, &llvm::errs())) {
    mod_.dump();
    std::cerr << "Function not well-formed" << std::endl;
    func->print(llvm::errs());
    __builtin_trap();
  }

  return func;
}

llvm::Value *Compiler::getLet(llvm::IRBuilder<> &builder, const Let &let) {
  llvm::Value *val = getExpr(builder, let.getExpr());
  val->setName(let.getName());
  return val;
}

llvm::Value *Compiler::getKeep(llvm::IRBuilder<> &builder, const Keep &keep) {
  llvm::Value *val = getExpr(builder, keep.getExpr());
  if (!val->getType()->isVoidTy()) val->setName(keep.getName());
  return getExpr(builder, keep.getBody());
}

std::string Compiler::Mangle(const Type &type) const {
  switch (type.getKind()) {
    case Node::NK_NamedType:
      return std::string(llvm::cast<NamedType>(type).getName());
    case Node::NK_ArrayType: {
      const auto &array_ty = llvm::cast<ArrayType>(type);
      std::stringstream ss;
      ss << "arr_" << array_ty.getNumElems() << "_"
         << Mangle(array_ty.getElemType());
      return ss.str();
    }
    case Node::NK_CompositeType: {
      const auto &composite_ty = llvm::cast<CompositeType>(type);
      std::stringstream ss;
      ss << "ct";
      for (const Type *ty : composite_ty.getTypes()) {
        ss << "_" << Mangle(*ty);
      }
      return ss.str();
    }
    case Node::NK_CallableType: {
      const auto &callable_ty = llvm::cast<CallableType>(type);
      std::stringstream ss;
      ss << "ret_" << Mangle(callable_ty.getReturnType());
      for (size_t i = 0; i < callable_ty.getArgTypes().size(); ++i) {
        ss << "_arg" << i << "_" << Mangle(*callable_ty.getArgTypes().at(i));
      }
      return ss.str();
    }
#define NODE(name) case Node::NK_##name:
#define TYPE(name)
#include "nodes.def"
      std::cerr << "Unhandled type: " << type.toString() << std::endl;
      __builtin_trap();
  }
}

llvm::Function *Compiler::getCGetc() const {
  llvm::FunctionType *c_func_ty = llvm::FunctionType::get(
      getIntType(32), {llvm::PointerType::getUnqual(mod_.getContext())},
      /*isVarArg=*/true);
  llvm::Function *c_func = llvm::cast<llvm::Function>(
      mod_.getOrInsertFunction("getc", c_func_ty).getCallee());
  return c_func;
}

llvm::Function *Compiler::getCPrintf() const {
  llvm::FunctionType *c_func_ty = llvm::FunctionType::get(
      getIntType(32), {getStrType()}, /*isVarArg=*/true);
  llvm::Function *c_func = llvm::cast<llvm::Function>(
      mod_.getOrInsertFunction("printf", c_func_ty).getCallee());
  return c_func;
}

llvm::Value *Compiler::getExpr(llvm::IRBuilder<> &builder, const Expr &expr) {
  if (llvm::isa<Let>(expr)) return getExprImpl(builder, expr);

  auto found = processed_exprs_.find(&expr);
  if (found != processed_exprs_.end()) return found->second;

  llvm::Type *type = getLLVMType(expr.getType());

  // Do this to prevent infinite recursion.
  llvm::GlobalValue *tmp;
  if (type->isFunctionTy()) {
    tmp = llvm::Function::Create(llvm::cast<llvm::FunctionType>(type),
                                 llvm::Function::ExternalLinkage, "", mod_);
  } else {
    tmp = llvm::cast<llvm::GlobalValue>(mod_.getOrInsertGlobal("", type));
  }
  processed_exprs_[&expr] = tmp;
  llvm::Value *res = getExprImpl(builder, expr);
  processed_exprs_[&expr] = res;
  tmp->replaceAllUsesWith(res);
  tmp->eraseFromParent();
  return res;
}

llvm::Value *Compiler::getExprImpl(llvm::IRBuilder<> &builder,
                                   const Expr &expr) {
  switch (expr.getKind()) {
    case Node::NK_Declare:
      ASTDumper(expr, std::cerr).Dump();
      std::cerr << "Decl should not be handled like other expressions"
                << std::endl;
      __builtin_trap();
    // case Node::NK_None:
    //   return getNone();
    case Node::NK_Get:
      return getGet(builder, llvm::cast<Get>(expr));
    case Node::NK_Set:
      return getSet(builder, llvm::cast<Set>(expr));
    case Node::NK_Cast:
      return getCast(builder, llvm::cast<Cast>(expr));
    case Node::NK_Zero:
      return getZero(builder, llvm::cast<Zero>(expr));
    case Node::NK_Write:
      return getWrite(llvm::cast<Write>(expr));
    case Node::NK_Readc:
      return getReadc(builder, llvm::cast<Readc>(expr));
    case Node::NK_Str:
      return getStr(builder, llvm::cast<Str>(expr));
    case Node::NK_Char:
      return getChar(builder, llvm::cast<Char>(expr));
    case Node::NK_Call:
      return getCall(builder, llvm::cast<Call>(expr));
    case Node::NK_Let:
      return getLet(builder, llvm::cast<Let>(expr));
    case Node::NK_Keep:
      return getKeep(builder, llvm::cast<Keep>(expr));
    case Node::NK_Callable:
      return getCallable(builder, llvm::cast<Callable>(expr));
    case Node::NK_Composite:
      return getComposite(builder, llvm::cast<Composite>(expr));
    case Node::NK_Arg: {
      const Arg &arg = llvm::cast<Arg>(expr);
      llvm::Function *func = builder.GetInsertBlock()->getParent();
      bool return_as_arg = func->getReturnType()->isVoidTy();
      return func->getArg(arg.getArgNo() + return_as_arg);
    }
    case Node::NK_Int:
      return llvm::ConstantInt::get(getIntType(32),
                                    llvm::cast<Int>(expr).getInt());
    case Node::NK_Bool:
      return llvm::ConstantInt::getBool(mod_.getContext(),
                                        llvm::cast<Bool>(expr).get());
    case Node::NK_BinOp:
      return getBinOp(builder, llvm::cast<BinOp>(expr));
    case Node::NK_If:
      return getIf(builder, llvm::cast<If>(expr));
#define NODE(name) case Node::NK_##name:
#define EXPR(name)
#include "nodes.def"
      __builtin_trap();
  }
}

llvm::Value *Compiler::getBinOp(llvm::IRBuilder<> &builder,
                                const BinOp &binop) {
  llvm::Value *lhs = getExpr(builder, binop.getLHS());
  llvm::Value *rhs = getExpr(builder, binop.getRHS());
  switch (binop.getOp()) {
    case BinOp::OK_Sub:
      return builder.CreateBinOp(llvm::Instruction::Sub, lhs, rhs);
    case BinOp::OK_Add:
      return builder.CreateBinOp(llvm::Instruction::Add, lhs, rhs);
    case BinOp::OK_Lt:
      return builder.CreateICmpSLT(lhs, rhs);
    case BinOp::OK_Ge:
      return builder.CreateICmpSGE(lhs, rhs);
    case BinOp::OK_Eq:
      return builder.CreateICmpEQ(lhs, rhs);
    case BinOp::OK_Or:
      return builder.CreateOr(lhs, rhs);
    case BinOp::OK_Mod:
      return builder.CreateSRem(lhs, rhs);
  }
}

llvm::Value *Compiler::getBoolExpr(llvm::IRBuilder<> &builder,
                                   const Expr &expr) {
  if (expr.getType().isNamedType(builtins::kIntTypeName)) {
    llvm::Value *val = getExpr(builder, expr);
    return builder.CreateICmpNE(val, llvm::ConstantInt::get(val->getType(), 0));
  } else if (expr.getType().isNamedType(builtins::kBoolTypeName)) {
    return getExpr(builder, expr);
  } else {
    std::cerr << "Unable to implicitly convert type to bool: "
              << expr.getType().toString() << std::endl;
    __builtin_trap();
  }
}

llvm::Value *Compiler::getIf(llvm::IRBuilder<> &builder, const If &if_expr) {
  llvm::Value *cond = getBoolExpr(builder, if_expr.getCond());
  llvm::Function *func = builder.GetInsertBlock()->getParent();

  llvm::BasicBlock *true_bb =
      llvm::BasicBlock::Create(mod_.getContext(), "true_entry", func);
  llvm::BasicBlock *false_bb =
      llvm::BasicBlock::Create(mod_.getContext(), "false_entry");
  llvm::BasicBlock *merge_bb = llvm::BasicBlock::Create(mod_.getContext());
  builder.CreateCondBr(cond, true_bb, false_bb);

  // True block
  builder.SetInsertPoint(true_bb);
  // Save a copy of all cached expressions then restore the old copy after we
  // process the branch. We need to do this so we don't cache an LLVM expression
  // that can be used in the other block. This could lead to an instruction that
  // does not dominate all uses.
  auto processed_exprs_cpy = processed_exprs_;
  llvm::Value *true_expr = getExpr(builder, if_expr.getIf());
  processed_exprs_ = processed_exprs_cpy;
  builder.CreateBr(merge_bb);
  true_bb = builder.GetInsertBlock();

  // False block
  func->insert(func->end(), false_bb);
  builder.SetInsertPoint(false_bb);
  llvm::Value *false_expr = getExpr(builder, if_expr.getElse());
  processed_exprs_ = processed_exprs_cpy;
  builder.CreateBr(merge_bb);
  false_bb = builder.GetInsertBlock();

  // Merge block
  func->insert(func->end(), merge_bb);
  builder.SetInsertPoint(merge_bb);

  assert(true_expr->getType() == false_expr->getType());
  llvm::PHINode *phi = builder.CreatePHI(true_expr->getType(),
                                         /*NumReservedValues=*/2);
  phi->addIncoming(true_expr, true_bb);
  phi->addIncoming(false_expr, false_bb);
  return phi;
}

llvm::Value *Compiler::getLoadedCStdin(llvm::IRBuilder<> &builder) const {
  llvm::Type *opaque_ptr = llvm::PointerType::getUnqual(mod_.getContext());
  return builder.CreateLoad(opaque_ptr,
                            mod_.getOrInsertGlobal("stdin", opaque_ptr));
}

llvm::Value *Compiler::getReadc(llvm::IRBuilder<> &builder,
                                const Readc &readc) {
  constexpr std::string_view kReadcName = "readc";
  if (auto *func = mod_.getFunction(kReadcName)) return func;

  auto *func_ty = llvm::cast<llvm::FunctionType>(getLLVMType(readc));
  assert(func_ty->getReturnType()->isVoidTy());
  assert(func_ty->getNumParams() == 2);

  llvm::Function *func = llvm::Function::Create(
      func_ty, llvm::Function::ExternalLinkage, kReadcName, mod_);

  // Dispatch to `getc(stdin)`.
  llvm::BasicBlock *bb =
      llvm::BasicBlock::Create(mod_.getContext(), "entry", func);
  llvm::IRBuilder<> readc_builder(bb);

  llvm::Value *getc_call =
      readc_builder.CreateCall(getCGetc(), {getLoadedCStdin(readc_builder)});

  // TODO: Handle the IO argument.

  // Store the int in the return argument.
  llvm::Value *gep = readc_builder.CreateConstGEP2_32(
      getLLVMType(readc.getType().getReturnType()), func->getArg(0), 0, 1);
  readc_builder.CreateStore(getc_call, gep);
  readc_builder.CreateRetVoid();

  return func;
}

llvm::Value *Compiler::getZero(llvm::IRBuilder<> &builder, const Zero &zero) {
  llvm::AllocaInst *alloc = builder.CreateAlloca(getLLVMType(zero));
  size_t size = *alloc->getAllocationSize(mod_.getDataLayout());
  builder.CreateMemSet(alloc, llvm::Constant::getNullValue(getIntType(8)), size,
                       llvm::MaybeAlign());
  return alloc;
}

llvm::Value *Compiler::getWrite(const Write &write) {
  auto *func_ty = llvm::cast<llvm::FunctionType>(getLLVMType(write.getType()));
  const Type &arg_ty = write.getArgType();
  llvm::Function *func =
      llvm::Function::Create(func_ty, llvm::Function::ExternalLinkage,
                             "write_" + Mangle(arg_ty), mod_);

  // Dispatch to a C-style printf.
  llvm::BasicBlock *bb =
      llvm::BasicBlock::Create(mod_.getContext(), "entry", func);
  llvm::IRBuilder<> builder(bb);

  llvm::Value *ret;
  if (arg_ty.isCharArray()) {
    llvm::Value *str_format = builder.CreateGlobalStringPtr("%s");
    ret = builder.CreateCall(getCPrintf(), {str_format, func->getArg(1)});
  } else if (arg_ty.isNamedType(builtins::kIntTypeName)) {
    llvm::Value *int_format = builder.CreateGlobalStringPtr("%d");
    ret = builder.CreateCall(getCPrintf(), {int_format, func->getArg(1)});
  } else if (arg_ty.isNamedType(builtins::kCharTypeName)) {
    llvm::Value *int_format = builder.CreateGlobalStringPtr("%c");
    ret = builder.CreateCall(getCPrintf(), {int_format, func->getArg(1)});
  } else if (arg_ty.isNamedType(builtins::kCPtrTypeName)) {
    llvm::Value *int_format = builder.CreateGlobalStringPtr("%p");
    ret = builder.CreateCall(getCPrintf(), {int_format, func->getArg(1)});
  } else {
    std::cerr << "Unhandled type for writing: " << arg_ty.toString()
              << std::endl;
    __builtin_trap();
  }

  builder.CreateRet(ret);
  assert(!llvm::verifyFunction(*func, &llvm::errs()));
  return func;
}

llvm::Value *Compiler::getGet(llvm::IRBuilder<> &builder, const Get &get) {
  llvm::Value *expr = getExpr(builder, get.getExpr());
  llvm::Type *expr_ty = getLLVMType(get.getExpr());
  llvm::Value *gep =
      builder.CreateGEP(expr_ty, expr,
                        {llvm::Constant::getNullValue(getIntType(32)),
                         getExpr(builder, get.getIdx())});
  if (get.getType().isCompositeOrArrayType()) return gep;
  llvm::Type *res_type = getLLVMType(get.getType());
  return builder.CreateLoad(res_type, gep);
}

llvm::Value *Compiler::getSet(llvm::IRBuilder<> &builder, const Set &set) {
  llvm::Value *expr = getExpr(builder, set.getExpr());

  // NOTE: This can lead to multiple copies of the same buffer for code like
  // this:
  //
  //   def write_to_buff = \[3 x str] buff int x str s -> [3 x str]
  //     SET buff x s
  //
  //   def main = \IO io -> IO
  //     let buff = [3 x str] zero
  //     let buff2 = [3 x str] call write_to_buff buff 0 "abc" end
  //     let buff3 = [3 x str] call write_to_buff buff2 1 "xyz" end
  //     let buff4 = [3 x str] call write_to_buff buff3 2 "123" end
  //     ...
  //
  // This will result in 4 separate buffers that result in 3 different memcpys.
  // In the general case, this is correct since if we had something like:
  //
  //   def main = \IO io -> IO
  //     let buff = [3 x str] zero
  //     let buff2 = [3 x str] call write_to_buff buff 0 "abc" end
  //     let buff3 = [3 x str] call write_to_buff buff2 1 "xyz" end
  //     let buff4 = [3 x str] call write_to_buff buff3 2 "123" end
  //     let something = int call buff2 end  # Do something with one of the
  //     older buffers.
  //     ...
  //
  // then the separate buffers are necessary since we'd need to handle the case
  // where we do something to the original buffer. Stack space can be retrieved
  // if we were able to assert each buffer was used exactly once (such as in the
  // first example).
  //
  llvm::Type *expr_ty = getLLVMType(set.getExpr());
  llvm::AllocaInst *cpy = builder.CreateAlloca(getLLVMType(set));
  size_t size = *cpy->getAllocationSize(mod_.getDataLayout());
  builder.CreateMemCpy(cpy, llvm::MaybeAlign(), expr, llvm::MaybeAlign(), size);

  llvm::Value *gep =
      builder.CreateGEP(expr_ty, cpy,
                        {llvm::Constant::getNullValue(getIntType(32)),
                         getExpr(builder, set.getIdx())});
  DoStore(builder, gep, set.getStore());
  return cpy;
}

llvm::Value *Compiler::getCast(llvm::IRBuilder<> &builder, const Cast &cast) {
  llvm::Value *expr = getExpr(builder, cast.getExpr());
  const Type &from = cast.getExpr().getType();
  const Type &to = cast.getType();

  if (from.isNamedType(builtins::kIntTypeName) &&
      to.isNamedType(builtins::kCharTypeName))
    return builder.CreateTrunc(expr, getLLVMType(to));

  if (from.isNamedType(builtins::kCharTypeName) &&
      to.isNamedType(builtins::kIntTypeName))
    return builder.CreateSExt(expr, getLLVMType(to));

  if (from.isNamedType(builtins::kIntTypeName) &&
      to.isNamedType(builtins::kCPtrTypeName))
    return builder.CreateIntToPtr(expr, getLLVMType(to));

  if (llvm::isa<CompositeType>(from) &&
      to.isNamedType(builtins::kCPtrTypeName)) {
    assert(expr->getType()->isPointerTy());
    return expr;
  }

  if (from.isCharArray()) {
    assert(expr->getType()->isPointerTy());
    if (to.isCharArray()) {
      const ArrayType &from_arr = llvm::cast<ArrayType>(from);
      const ArrayType &to_arr = llvm::cast<ArrayType>(to);
      llvm::Type *llvm_to = getLLVMType(to);
      llvm::Type *llvm_from = getLLVMType(from);

      if (to_arr.getNumElems() > from_arr.getNumElems()) {
        // Create a copy of a larger size then return that.
        llvm::AllocaInst *alloc = builder.CreateAlloca(llvm_to);
        size_t to_size = *alloc->getAllocationSize(mod_.getDataLayout());
        size_t from_size = mod_.getDataLayout().getTypeAllocSize(llvm_from);
        assert(to_size > from_size);
        builder.CreateMemSet(alloc, llvm::Constant::getNullValue(getIntType(8)),
                             to_size, llvm::MaybeAlign());
        builder.CreateMemCpy(alloc, llvm::MaybeAlign(), expr,
                             llvm::MaybeAlign(), from_size);
        return alloc;
      } else if (to_arr.getNumElems() < from_arr.getNumElems()) {
        return expr;
      } else {
        return expr;
      }
    } else if (to.isNamedType(builtins::kCPtrTypeName)) {
      return expr;
    }
  }

  if (from.isNamedType(builtins::kCPtrTypeName)) {
    assert(expr->getType()->isPointerTy());
    if (to.isNamedType(builtins::kBoolTypeName)) {
      size_t to_size =
          mod_.getDataLayout().getTypeAllocSizeInBits(expr->getType());
      llvm::IntegerType *inttype =
          llvm::IntegerType::get(mod_.getContext(), to_size);
      llvm::Value *asint = builder.CreatePtrToInt(expr, inttype);
      return builder.CreateICmpNE(asint, llvm::ConstantInt::get(inttype, 0));
    }
  }

  // if (const auto *arr_ty = llvm::dyn_cast<ArrayType>(&from)) {
  //   if (arr_ty->getElemType().isNamedType(kCharTypeName) &&
  //       to.isNamedType(kStrName))
  //     return expr;
  // }

  std::cerr << "Unhandled cast from " << from.toString() << " to "
            << to.toString() << std::endl;
  __builtin_trap();
}

llvm::Value *Compiler::getCall(llvm::IRBuilder<> &builder, const Call &call) {
  std::vector<llvm::Value *> args;
  const Type &func_type = call.getFunc().getType();
  bool return_as_arg = ShouldReturnValueAsArgument(func_type.getReturnType());
  if (return_as_arg) {
    // The return type of this function is too large to return in IR (such as a
    // composite type with many elements). So we should instead allocate the
    // buffer in the caller and the function is in charge of "returning" the
    // value by initializing the first argument.
    llvm::Value *alloc =
        builder.CreateAlloca(getLLVMType(func_type.getReturnType()));
    args.push_back(alloc);
  }

  for (const auto &arg : call.getArgs()) {
    llvm::Value *val = getExpr(builder, *arg);
    args.push_back(val);
  }
  llvm::Value *callable = getExpr(builder, call.getFunc());
  llvm::FunctionCallee callee(getLLVMFuncType(call.getFunc()), callable);

  llvm::Value *ret = builder.CreateCall(callee, args);
  return return_as_arg ? args.front() : ret;
}

void Compiler::DoStore(llvm::IRBuilder<> &builder, llvm::Value *store_ptr,
                       const Expr &expr) {
  llvm::Value *res = getExpr(builder, expr);
  if (expr.getType().isCompositeOrArrayType()) {
    size_t size = mod_.getDataLayout().getTypeAllocSize(getLLVMType(expr));
    builder.CreateMemCpy(store_ptr, llvm::MaybeAlign(), res, llvm::MaybeAlign(),
                         size);
  } else {
    builder.CreateStore(res, store_ptr);
  }
}

llvm::Value *Compiler::getComposite(llvm::IRBuilder<> &builder,
                                    const Composite &comp) {
  // Since the composite elements may not be constant, we can instead just
  // alloca our structure.
  llvm::Type *comp_ty = getLLVMType(comp);
  llvm::Value *buff = builder.CreateAlloca(comp_ty);
  for (size_t i = 0; i < comp.getNumElems(); ++i) {
    const Expr &elem = comp.getElem(i);
    llvm::Value *ptr = builder.CreateConstGEP2_32(comp_ty, buff, 0, i);
    DoStore(builder, ptr, elem);
  }
  return buff;
}

bool Compile(const std::vector<const Node *> &ast, std::string_view outfile,
             DumpType dump) {
  std::ofstream out(outfile.data());
  if (!out) {
    std::cerr << "Could not open file " << outfile << std::endl;
    return false;
  }
  return Compile(ast, out, dump, outfile);
}

bool Compile(const std::vector<const Node *> &ast, std::ostream &out,
             DumpType dump, std::string_view modname) {
#if HAS_LSAN
  // lsab with libLLVM-16 is reporting leaks from within llvm internals.
  __lsan::ScopedDisabler disable;
#endif

  // Open a new module.
  llvm::LLVMContext context;
  llvm::Module mod(modname, context);

  // Create a new builder for the module.
  llvm::IRBuilder<> Builder(context);

  // Initialize the target registry etc.
  llvm::InitializeAllTargetInfos();
  llvm::InitializeAllTargets();
  llvm::InitializeAllTargetMCs();
  llvm::InitializeAllAsmParsers();
  llvm::InitializeAllAsmPrinters();

  auto triple = llvm::sys::getDefaultTargetTriple();
  mod.setTargetTriple(triple);

  std::string error;
  auto target = llvm::TargetRegistry::lookupTarget(triple, error);

  // Print an error and exit if we couldn't find the requested target.
  // This generally occurs if we've forgotten to initialise the
  // TargetRegistry or we have a bogus target triple.
  if (!target) {
    std::cerr << error;
    return false;
  }

  auto CPU = "generic";
  auto Features = "";

  llvm::TargetOptions target_opts;
  auto RM = llvm::Reloc::Model::PIC_;
  auto TheTargetMachine =
      target->createTargetMachine(triple, CPU, Features, target_opts, RM);

  mod.setDataLayout(TheTargetMachine->createDataLayout());

  std::vector<std::unique_ptr<Node>> gc;
  Compiler compiler(mod);

  for (const auto *node : ast) {
    if (const auto *def = llvm::dyn_cast<Define>(node))
      compiler.CompileDefine(*def);
    else if (const auto *decl = llvm::dyn_cast<Declare>(node))
      compiler.CompileDeclare(*decl);
    else {
      std::cerr << "Unknown top level entity: " << node->getKind() << std::endl;
      __builtin_trap();
    }
  }

  // Create the analysis managers.
  llvm::LoopAnalysisManager LAM;
  llvm::FunctionAnalysisManager FAM;
  llvm::CGSCCAnalysisManager CGAM;
  llvm::ModuleAnalysisManager MAM;

  // Create the new pass manager builder.
  // Take a look at the PassBuilder constructor parameters for more
  // customization, e.g. specifying a TargetMachine or various debugging
  // options.
  llvm::PassBuilder PB;

  // Register all the basic analyses with the managers.
  PB.registerModuleAnalyses(MAM);
  PB.registerCGSCCAnalyses(CGAM);
  PB.registerFunctionAnalyses(FAM);
  PB.registerLoopAnalyses(LAM);
  PB.crossRegisterProxies(LAM, FAM, CGAM, MAM);

  llvm::raw_os_ostream dest(out);

  switch (dump) {
    case File: {
      std::error_code EC;
      llvm::legacy::PassManager pass;
      llvm::buffer_ostream buff(dest);
      if (TheTargetMachine->addPassesToEmitFile(pass, buff, nullptr,
                                                llvm::CGFT_ObjectFile)) {
        std::cerr << "TheTargetMachine can't emit a file of this type";
        return false;
      }
      pass.run(mod);
      dest.flush();
      break;
    }
    case ASM: {
      llvm::legacy::PassManager pass;
      llvm::buffer_ostream buff(dest);
      if (TheTargetMachine->addPassesToEmitFile(pass, buff, nullptr,
                                                llvm::CGFT_AssemblyFile)) {
        std::cerr << "TheTargetMachine can't emit a file of this type";
        return false;
      }
      pass.run(mod);
      dest.flush();
      break;
    }
    case IR:
      mod.print(dest, nullptr);
      break;
  }

  return true;
}

}  // namespace lang
