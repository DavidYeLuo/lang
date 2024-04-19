#include <iostream>
#include <sstream>

#include "ast.h"
#include "astbuilder.h"
#include "astdumper.h"
#include "astvisitor.h"
#include "genericresolver.h"
#include "mangle.h"

namespace lang {

namespace {

class ASTLowerer : public NonConstASTVisitor<> {
 public:
  ASTLowerer(Module &mod, ASTBuilder &builder) : mod_(mod), builder_(builder) {}

  void Lower(Declare &decl) { Visit(decl); }

 protected:
  void Visit(Node &node) {
    if (visited_.contains(&node))
      return;
    visited_.insert(&node);
    NonConstASTVisitor::Visit(node);
  }

  void Visit(TypeDef &) {
    // TODO: It might be helpful to have a visitor only for expressions.
    UNREACHABLE("ASTLowerer should only cover expressions.");
  }

  void Visit(Declare &decl) {
    if (decl.isDefinition())
      Visit(decl.getBody());
  }

  void Visit(BinOp &binop) {
    Visit(binop.getLHS());
    Visit(binop.getRHS());
  }

  void Visit(Cast &cast) { Visit(cast.getExpr()); }

  void Visit(Callable &callable) { Visit(callable.getBody()); }

  void Visit(AmbiguousCall &call) {
    // We need to resolve which function an ambigous call refers to.
    //
    // TODO: Actually determine if we can ever reach this point. I *think* the
    // only way we should have an AmbiguousCall is if we are in a
    // GENERIC_REMAINING callable and we use the GENERIC_REMAINING argument in a
    // call. But I don't *think* we should ever explicitly lower a
    // GENERIC_REMAINING callable since those are indirectly lowered via normal
    // calls in normal callables. That is, if we have a call to a
    // GENERIC_REMAINING callable, the Visit(Call &) function should replace it
    // with a non-generic call to the right function.
    UNREACHABLE("TODO: Implement this");
  }

  void Visit(Call &call) {
    const CallableType &callable_ty =
        llvm::cast<CallableType>(call.getFunc().getType());

    if (callable_ty.isGeneric()) {
      auto arg_types = call.getArgTypes();
      assert(std::none_of(arg_types.begin(), arg_types.end(),
                          [](const Type *t) { return t->isGeneric(); }));
      Declare &decl = *ExtractDeclare(call.getFunc());

      // FIXME: Let's say we have
      //
      //   def writeln = \IO io GENERIC arg -> IO
      //     let io2 = IO write(io arg)
      //     write(io2 "\n")
      //
      //   # Notice this is commented out
      //   #decl writeln = \IO GENERIC GENERIC_REMAINING -> IO
      //   def writeln = \IO io GENERIC arg GENERIC_REMAINING remaining ->
      //   IO
      //     let io2 = IO write(io arg)
      //     writeln(io2 remaining)
      //
      // Then the parser can only see that the last writeln call can only
      // resolve to the first `writeln` definition. This is valid since
      // `remaining` can be one or more arguments. We won't know until here
      // when expanding `remaining` that it is invalid. We should check for
      // this.
      Declare &newdecl =
          GenericResolver(mod_, builder_, decl, arg_types).Resolve();
      assert(!newdecl.getType().isGeneric());
      call.SwapFunc(newdecl);
    }

    for (Expr *arg : call.getArgs())
      Visit(*arg);
    Visit(call.getFunc());
  }

  void Visit(Composite &comp) {
    for (Expr *e : comp.getElems())
      Visit(*e);
  }

  void Visit(Struct &s) {
    for (const auto &p : s.getFields())
      Visit(*p.second);
  }

  void Visit(Get &get) {
    Visit(get.getExpr());
    Visit(get.getIdx());
  }

  void Visit(StructGet &get) { Visit(get.getExpr()); }

  void Visit(Set &set) {
    Visit(set.getExpr());
    Visit(set.getIdx());
    Visit(set.getStore());
  }

  void Visit(If &if_expr) {
    Visit(if_expr.getCond());
    Visit(if_expr.getIf());
    Visit(if_expr.getElse());
  }

  void Visit(Let &let) { Visit(let.getExpr()); }

  void Visit(Keep &keep) {
    Visit(keep.getExpr());
    Visit(keep.getBody());
  }

  // These are "atomic" expressions that don't contain other nodes.
  void Visit(Arg &) {}
  void Visit(Str &) {}
  void Visit(Char &) {}
  void Visit(Bool &) {}
  void Visit(Zero &) {}
  void Visit(Int &) {}
  void Visit(Readc &) {}

 private:
  Module &mod_;
  ASTBuilder &builder_;
  std::set<Node *> visited_;
};

}  // namespace

void Lower(Module &mod, ASTBuilder &builder) {
  auto ast = mod.getAST();
  for (Declare *decl : ast) {
    if (decl->getType().isGeneric())
      continue;

    ASTLowerer(mod, builder).Lower(*decl);
  }
  mod.MangleDecls();
}

}  // namespace lang
