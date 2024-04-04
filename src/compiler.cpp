#include <filesystem>
#include <fstream>
#include <iostream>
#include <string_view>
#include <vector>

#if defined(__clang__)
#if defined(__has_feature)
#define HAS_LSAN __has_feature(address_sanitizer)
#else
#define HAS_LSAN 0
#endif
#elif defined(__GNUC__)
#define HAS_LSAN defined(__SANITIZE_ADDRESS__)
#else
#error "Unknown compiler"
#endif

#if HAS_LSAN
#include <sanitizer/lsan_interface.h>
#endif

#include "astdumper.h"
#include "compiler.h"
#include "llvm/IR/DIBuilder.h"
#include "llvm/IR/DebugInfoMetadata.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/raw_os_ostream.h"
#include "llvm/Support/raw_ostream.h"
#include "mangle.h"

namespace lang {

llvm::DIType *Compiler::getDIType(const NamedType &named_ty) {
  if (named_ty.getName() == builtins::kIOTypeName)
    return di_builder_.createBasicType(named_ty.getName(), /*SizeInBits=*/32,
                                       llvm::dwarf::DW_ATE_unsigned);
  if (named_ty.getName() == builtins::kIntTypeName)
    return di_builder_.createBasicType(named_ty.getName(), /*SizeInBits=*/32,
                                       llvm::dwarf::DW_ATE_signed);
  if (named_ty.getName() == builtins::kBoolTypeName)
    return di_builder_.createBasicType(named_ty.getName(), /*SizeInBits=*/8,
                                       llvm::dwarf::DW_ATE_boolean);
  if (named_ty.getName() == builtins::kCharTypeName)
    return di_builder_.createBasicType(named_ty.getName(), /*SizeInBits=*/8,
                                       llvm::dwarf::DW_ATE_signed_char);
  if (named_ty.getName() == builtins::kCPtrTypeName)
    return di_builder_.createBasicType(named_ty.getName(), /*SizeInBits=*/64,
                                       llvm::dwarf::DW_ATE_address);
  if (named_ty.getName() == builtins::kNoneTypeName)
    return di_builder_.createBasicType(named_ty.getName(), /*SizeInBits=*/64,
                                       llvm::dwarf::DW_ATE_unsigned);

  UNREACHABLE("Unhandled named type dwarf ecoding");
}

llvm::DIType *Compiler::getDIType(const GenericRemainingType &) {
  UNREACHABLE("Generic types should not be directly used for code emission.");
}

llvm::DIType *Compiler::getDIType(const GenericType &) {
  UNREACHABLE("Generic types should not be directly used for code emission.");
}

llvm::DIType *Compiler::getDIType(const CompositeType &comp_ty) {
  llvm::Type *llvm_ty = getLLVMType(comp_ty);
  return di_builder_.createStructType(
      &di_unit_, /*Name=*/"", &di_unit_, /*LineNumber=*/0,
      mod_.getDataLayout().getTypeAllocSizeInBits(llvm_ty),
      mod_.getDataLayout().getTypeAllocSizeInBits(llvm_ty),
      llvm::DINode::FlagZero, /*DerivedFrom=*/nullptr, llvm::DINodeArray());
}

llvm::DIType *Compiler::getDIType(const ArrayType &arr_ty) {
  return di_builder_.createArrayType(
      arr_ty.getNumElems(),
      mod_.getDataLayout().getTypeAllocSizeInBits(getLLVMType(arr_ty)),
      getDIType(arr_ty.getElemType()), llvm::DINodeArray());
}

llvm::DIType *Compiler::getDIType(const CallableType &callable_ty) {
  std::vector<llvm::Metadata *> EltTys;

  // Add the result type.
  EltTys.push_back(getDIType(callable_ty.getReturnType()));

  for (size_t i = 0; i < callable_ty.getNumArgs(); ++i)
    EltTys.push_back(getDIType(callable_ty.getArgType(i)));

  return di_builder_.createSubroutineType(
      di_builder_.getOrCreateTypeArray(EltTys));
}

void Compiler::CompileDeclare(const Declare &declare) {
  // Generic functions should never be compiled. Instead, the correct
  // function should be generated with a call to this define.
  if (declare.isGenericCallable())
    return;

  getDeclare(declare);
}

llvm::Value *Compiler::getDeclare(const Declare &declare) {
  if (auto *glob = mod_.getNamedValue(declare.getName()))
    return glob;

  if (const auto *func_ty = llvm::dyn_cast<CallableType>(&declare.getType())) {
    llvm::FunctionType *llvm_func_ty = getLLVMFuncType(*func_ty);

    llvm::Function *func;
    if (declare.getName() == "main") {
      assert(func_ty->getReturnType().isNamedType(builtins::kIOTypeName) &&
             "Return type of main should be IO");
      assert(func_ty->getArgTypes().size() == 1 &&
             "`main` func should have one argument of IO");
      assert(func_ty->getArgTypes()[0]->isNamedType(builtins::kIOTypeName) &&
             "First argument to main should be IO");
      func = getMainWrapper(llvm_func_ty);
    } else {
      func =
          llvm::Function::Create(llvm_func_ty, llvm::Function::ExternalLinkage,
                                 declare.getName(), mod_);
    }

    if (!declare.isDefinition()) {
      processed_exprs_.try_emplace(&declare, func);
    } else {
      FillFuncBody(llvm::cast<Callable>(declare.getBody()), func,
                   declare.getName());
    }

    return func;
  } else {
    auto *glob = new llvm::GlobalVariable(
        mod_, getLLVMType(declare), /*isConstant=*/true,
        /*Linkage=*/llvm::GlobalValue::ExternalLinkage,
        /*Initializer=*/nullptr, declare.getName());
    // FIXME: We need some way of handling constants (expressions that don't
    // need an IRBuilder).
    return glob;
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
  if (llvm::isa<CallableType>(ty.getReturnType()))
    ret_ty = llvm::PointerType::getUnqual(mod_.getContext());
  return llvm::FunctionType::get(ret_ty, func_args, /*isVarArg=*/false);
}

llvm::Type *Compiler::getLLVMType(const Type &ty) const {
  switch (ty.getKind()) {
#define TYPE(name)      \
  case Type::TK_##name: \
    return getLLVMType(llvm::cast<name>(ty));
#include "types.def"
  }
  __builtin_unreachable();
}

llvm::Type *Compiler::getLLVMType(const ArrayType &type) const {
  return llvm::ArrayType::get(getLLVMType(type.getElemType()),
                              type.getNumElems());
}

llvm::Type *Compiler::getLLVMType(const CompositeType &type) const {
  std::vector<llvm::Type *> types;
  for (const Type *ty : type.getTypes())
    types.push_back(getLLVMType(*ty));
  return llvm::StructType::create(types);
}

llvm::Type *Compiler::getLLVMType(const NamedType &type) const {
  if (type.getName() == builtins::kIOTypeName)
    return getIntType(32);
  if (type.getName() == builtins::kIntTypeName)
    return getIntType(32);
  if (type.getName() == builtins::kCharTypeName)
    return getCharType();
  if (type.getName() == builtins::kNoneTypeName)
    return llvm::Type::getVoidTy(mod_.getContext());
  if (type.getName() == builtins::kBoolTypeName)
    return getIntType(1);
  if (type.getName() == builtins::kCPtrTypeName)
    return llvm::PointerType::getUnqual(mod_.getContext());
  UNREACHABLE("Unhandled type name: %s", type.getName().data());
}

void Compiler::ReplaceAllUsesWith(llvm::Value *replacement,
                                  llvm::Value *replacing) {
  if (replacement == replacing)
    return;

  // Do a normal replacement.
  replacing->replaceAllUsesWith(replacement);

  // Doing a traditional Value::replaceAllUsesWith doesn't replace the incoming
  // values of the phi node with the replacement, so instead we need to manually
  // call RAUW on the incoming nodes. Then we can just delete the phi node.
  if (auto *phi = llvm::dyn_cast<llvm::PHINode>(replacing)) {
    for (llvm::Value *incoming : phi->incoming_values())
      ReplaceAllUsesWith(replacement, incoming);
  }
}

const Expr &ResolveLets(const Expr &expr) {
  if (const auto *let = llvm::dyn_cast<Let>(&expr))
    return ResolveLets(let->getExpr());
  return expr;
}

llvm::Value *Compiler::ManifestGenericCallable(llvm::IRBuilder<> &builder,
                                               const Call &call) {
  const CallableType &callable_ty =
      llvm::cast<CallableType>(call.getFunc().getType());
  assert(callable_ty.isGenericCallable());

  std::vector<const Expr *> args;
  auto old_generic_exprs_ = applied_generic_exprs_;
  ASTBuilder astbuilder;

  const Expr *dummy_func;
  const Expr &func = ResolveLets(call.getFunc());

  // Attempt to match the arguments to the parameters.
  std::vector<const Type *> arg_types;
  for (size_t i = 0; i < call.getNumArgs(); ++i) {
    arg_types.push_back([&]() -> const Type * {
      if (!callable_ty.getArgType(i).isGeneric())
        return &callable_ty.getArgType(i);
      if (!call.getArgAt(i).getType().isGeneric())
        return &call.getArgAt(i).getType();
      auto found =
          applied_generic_exprs_.find(&llvm::cast<Arg>(call.getArgAt(i)));
      assert(found != applied_generic_exprs_.end());
      return &found->second->getType();
    }());
    assert(!arg_types.back()->isGeneric());
  }

  if (const auto *decl = llvm::dyn_cast<Declare>(&func)) {
    const auto &new_callable_ty =
        astbuilder.getCallableType(callable_ty.getReturnType(), arg_types);
    dummy_func = &astbuilder.getDeclare(func.getStart(), decl->getName(),
                                        new_callable_ty, /*is_write=*/false,
                                        /*is_cdecl=*/false);
  } else {
    const Callable &callable = llvm::cast<Callable>(func);

    Callable &newcallable = astbuilder.getCallable(
        callable.getStart(), callable.getType().getReturnType(),
        callable.getArgLocs(), callable.getArgNames(), arg_types);
    for (size_t i = 0; i < newcallable.getNumArgs(); ++i) {
      applied_generic_exprs_[&callable.getArg(i)] = &newcallable.getArg(i);
    }
    newcallable.setBody(const_cast<Expr &>(callable.getBody()));
    dummy_func = &newcallable;

    auto found = named_generic_callables_.find(&callable);
    if (found != named_generic_callables_.end()) {
      named_generic_callables_[llvm::cast<Callable>(dummy_func)] =
          found->second;
    }
  }

  assert(!dummy_func->getType().isGeneric());

  Call dummy_call(call.getStart(), call.getType(),
                  const_cast<Expr &>(*dummy_func), call.getArgs(),
                  call.isPure());

  llvm::Value *result = getCall(builder, dummy_call);
  applied_generic_exprs_ = old_generic_exprs_;
  return result;
}

llvm::Value *Compiler::getCallable(llvm::IRBuilder<> &builder,
                                   const Callable &callable) {
  std::string name = [&]() -> std::string {
    auto found = named_generic_callables_.find(&callable);
    if (found == named_generic_callables_.end())
      return "";
    return Mangle(found->second, callable.getType());
  }();
  llvm::FunctionType *llvm_func_ty = getLLVMFuncType(callable.getType());
  llvm::Function *func = llvm::Function::Create(
      llvm_func_ty, llvm::Function::ExternalLinkage, name, mod_);

  FillFuncBody(callable, func, name);
  return func;
}

void Compiler::FillFuncBody(const Callable &callable, llvm::Function *func,
                            std::string_view name) {
  assert(!processed_exprs_.contains(&callable));
  processed_exprs_.try_emplace(&callable, func);

  const SourceLocation &start = callable.getStart();
  llvm::DISubprogram *SP = di_builder_.createFunction(
      &di_unit_, name, name, &di_unit_, start.getRow(),
      llvm::cast<llvm::DISubroutineType>(getDIType(callable.getType())),
      start.getRow(), llvm::DINode::FlagPrototyped,
      llvm::DISubprogram::SPFlagDefinition);
  func->setSubprogram(SP);

  const Type &ty = callable.getType();
  bool return_as_arg = ShouldReturnValueAsArgument(ty.getReturnType());
  if (return_as_arg)
    func->getArg(0)->setName("return_val");

  for (size_t i = 0; i < callable.getNumArgs(); ++i)
    func->getArg(i + return_as_arg)->setName(callable.getArgName(i));

  // Create the function body.
  llvm::BasicBlock *bb =
      llvm::BasicBlock::Create(mod_.getContext(), "entry", func);
  llvm::IRBuilder<> nested_builder(bb);

  auto processed_exprs_cpy = processed_exprs_;
  llvm::Value *ret = getExpr(nested_builder, callable.getBody());
  processed_exprs_ = processed_exprs_cpy;
  if (return_as_arg) {
    ReplaceAllUsesWith(func->getArg(0), ret);
    nested_builder.CreateRetVoid();
  } else {
    nested_builder.CreateRet(ret);
  }

  assert(func->getSubprogram());
  di_builder_.finalizeSubprogram(func->getSubprogram());

  if (llvm::verifyFunction(*func, &llvm::errs())) {
    func->print(llvm::errs());
    UNREACHABLE("Function for callable %p not well-formed", &callable);
  }
}

llvm::Value *Compiler::getLet(llvm::IRBuilder<> &builder, const Let &let) {
  llvm::Value *val = getExpr(builder, let.getExpr());
  val->setName(let.getName());
  return val;
}

llvm::Value *Compiler::getKeep(llvm::IRBuilder<> &builder, const Keep &keep) {
  llvm::Value *val = getExpr(builder, keep.getExpr());
  if (!val->getType()->isVoidTy())
    val->setName(keep.getName());
  return getExpr(builder, keep.getBody());
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
  llvm::Function *func = builder.GetInsertBlock()->getParent();
  builder.SetCurrentDebugLocation(
      llvm::DILocation::get(di_unit_.getContext(), expr.getStart().getRow(),
                            expr.getStart().getCol(), func->getSubprogram()));

  if (llvm::isa<Let>(expr))
    return getExprImpl(builder, expr);

  auto found = processed_exprs_.find(&expr);
  if (found != processed_exprs_.end())
    return found->second;

  if (const Arg *arg = llvm::dyn_cast<Arg>(&expr)) {
    auto found_arg = applied_generic_exprs_.find(arg);
    if (found_arg != applied_generic_exprs_.end())
      return getExpr(builder, *found_arg->second);
  }

  if (const auto *callable = llvm::dyn_cast<Callable>(&expr)) {
    // A callable will be in charge of creating its own global and caching
    // itself.
    return getCallable(builder, *callable);
  }

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
    case Node::NK_Declare: {
      const Declare &decl = llvm::cast<Declare>(expr);
      if (decl.isBuiltinWrite())
        return ManifestWrite(decl);
      return getDeclare(decl);
    }
    case Node::NK_AmbiguousCall:
      UNREACHABLE("This should've been resolved during lowering");
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
      UNREACHABLE("Callables should be handled on their own in getExpr");
    case Node::NK_Composite:
      return getComposite(builder, llvm::cast<Composite>(expr));
    case Node::NK_Arg: {
      const Arg &arg = llvm::cast<Arg>(expr);
      const Callable &parent = arg.getParent();
      // This should've already been cached.
      llvm::Function *func =
          llvm::cast<llvm::Function>(processed_exprs_.at(&parent));
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
  }
  __builtin_unreachable();
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
  __builtin_unreachable();
}

llvm::Value *Compiler::getBoolExpr(llvm::IRBuilder<> &builder,
                                   const Expr &expr) {
  if (expr.getType().isNamedType(builtins::kIntTypeName)) {
    llvm::Value *val = getExpr(builder, expr);
    return builder.CreateICmpNE(val, llvm::ConstantInt::get(val->getType(), 0));
  } else if (expr.getType().isNamedType(builtins::kBoolTypeName)) {
    return getExpr(builder, expr);
  } else {
    UNREACHABLE("Unable to implicitly convert type to bool: %s",
                expr.getType().toString().c_str());
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
  if (auto *func = mod_.getFunction(kReadcName))
    return func;

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

llvm::Value *Compiler::ManifestWrite(const Declare &write) {
  assert(write.isBuiltinWrite());
  const Type &type = write.getType();
  const Type &arg_ty = llvm::cast<CallableType>(type).getArgType(1);
  auto *func_ty = llvm::cast<llvm::FunctionType>(getLLVMType(type));
  std::string name(Mangle("write", type));
  llvm::Function *func = llvm::Function::Create(
      func_ty, llvm::Function::ExternalLinkage, name, mod_);

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
    UNREACHABLE("Unhandled type for writing: %s", arg_ty.toString().c_str());
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
  if (get.getType().isCompositeOrArrayType())
    return gep;
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

  UNREACHABLE("Unhandled cast from %s to %s", from.toString().c_str(),
              to.toString().c_str())
}

llvm::Value *Compiler::getCall(llvm::IRBuilder<> &builder, const Call &call) {
  // We should manifest a new callable if any of the arguments are generic, but
  // don't do this for builtin functions like `write` since we are in charge of
  // emission.
  assert(!call.getFunc().getType().isGeneric());

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

  llvm::CallInst *ret = builder.CreateCall(callee, args);
  llvm::Function *func = builder.GetInsertBlock()->getParent();
  assert(func->getSubprogram());
  ret->setDebugLoc(
      llvm::DILocation::get(di_unit_.getContext(), call.getStart().getRow(),
                            call.getStart().getCol(), func->getSubprogram()));
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

bool Compile(lang::Module &mod, std::string_view outfile, DumpType dump,
             std::filesystem::path source) {
  std::ofstream out(outfile.data());
  if (!out) {
    std::cerr << "Could not open file " << outfile << std::endl;
    return false;
  }
  return Compile(mod, out, dump, outfile, source);
}

bool Compile(lang::Module &lang_mod, std::ostream &out, DumpType dump,
             std::string_view modname, std::filesystem::path source) {
#if HAS_LSAN
  // lsan with libLLVM-16 is reporting leaks from within llvm internals.
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

  Compiler compiler(lang_mod, mod, source);

  for (const Declare *decl : lang_mod.getAST()) {
    compiler.CompileDeclare(*decl);
  }

  compiler.getDIBuilder().finalize();

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
