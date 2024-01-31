#include "astdumper.h"

#include <set>

#include "llvm/Support/Casting.h"

namespace lang {

void ASTDumper::Dump(const Node &node) {
  if (visited_.find(&node) != visited_.end()) {
    Pad() << "..." << &node << "...\n";
    return;
  }

  visited_.insert(&node);

  switch (node.getKind()) {
#define NODE(name)      \
  case Node::NK_##name: \
    return Dump(llvm::cast<name>(node));
#include "nodes.def"
  }
}

std::ostream &ASTDumper::Pad() {
  for (size_t i = 0; i < indent_; ++i) out_ << kPadding;
  return out_;
}

void ASTDumper::Dump(const Define &define) {
  Pad() << "Define " << &define << " name=" << define.getName() << "\n";
  Indent();
  Dump(define.getBody());
  Dedent();
}

void ASTDumper::Dump(const Declare &declare) {
  Pad() << "Declare " << &declare << " name=" << declare.getName()
        << " type=" << declare.getType().toString() << "\n";
}

void ASTDumper::Dump(const Let &let) {
  Pad() << "Let name=" << let.getName() << " " << &let << "\n";
  Indent();
  Dump(let.getExpr());
  Dedent();
}

void ASTDumper::Dump(const Keep &keep) {
  Pad() << "Keep name=" << keep.getName() << " " << &keep << "\n";
  Indent();
  Dump(keep.getExpr());
  Dump(keep.getBody());
  Dedent();
}

void ASTDumper::Dump(const Callable &callable) {
  const auto &callable_type = llvm::cast<CallableType>(callable.getType());
  Pad() << "Callable " << callable_type.toString() << " " << &callable << "\n";
  Indent();
  Pad() << "args:\n";

  Indent();
  for (size_t i = 0; i < callable.getNumArgs(); ++i) {
    Pad() << callable.getArgName(i) << " "
          << callable_type.getArgType(i).toString() << "\n";
  }
  Dedent();

  Pad() << "return:\n";
  Indent();
  Dump(callable.getBody());
  Dedent();

  Dedent();
}

void ASTDumper::Dump(const Cast &cast) {
  Pad() << "Cast " << &cast << " " << cast.getType().toString() << "\n";
  Indent();
  Dump(cast.getExpr());
  Dedent();
}

void ASTDumper::Dump(const Write &write) {
  Pad() << "Write " << &write << " " << write.getType().toString() << "\n";
}

void ASTDumper::Dump(const Readc &readc) {
  Pad() << "Readc " << &readc << " " << readc.getType().toString() << "\n";
}

void ASTDumper::Dump(const Call &call) {
  Pad() << "Call " << &call << "\n";
  Indent();

  Dump(call.getFunc());

  Pad() << "args:\n";

  Indent();
  for (const Expr *arg : call.getArgs()) {
    Dump(*arg);
  }
  Dedent();

  Dedent();
}

void ASTDumper::Dump(const Zero &zero) {
  Pad() << "Zero " << &zero << " " << zero.getType().toString();
}

void ASTDumper::Dump(const Composite &comp) {
  Pad() << "Composite " << &comp << "\n";
  Indent();

  for (const Expr *elem : comp.getElems()) {
    Dump(*elem);
  }

  Dedent();
}

void ASTDumper::Dump(const Set &set) {
  Pad() << "Set " << &set << " " << set.getType().toString() << "\n";
  Indent();
  Dump(set.getExpr());
  Dump(set.getIdx());
  Dump(set.getStore());
  Dedent();
}

void ASTDumper::Dump(const Get &get) {
  Pad() << "Get " << &get << " " << get.getType().toString() << "\n";
  Indent();
  Dump(get.getExpr());
  Dump(get.getIdx());
  Dedent();
}

void ASTDumper::Dump(const Int &i) { Pad() << "Int " << i.getInt() << "\n"; }

void ASTDumper::Dump(const Str &s) { Pad() << "Str `" << s.get() << "`\n"; }

void ASTDumper::Dump(const Char &c) {
  Pad() << "Char `" << c.getChar() << "`\n";
}

void ASTDumper::Dump(const Bool &b) { Pad() << "Bool `" << b.get() << "`\n"; }

void ASTDumper::Dump(const If &if_expr) {
  Pad() << "If " << &if_expr << "\n";
  Indent();

  Pad() << "cond:\n";
  Indent();
  Dump(if_expr.getCond());
  Dedent();

  Pad() << "if body:\n";
  Indent();
  Dump(if_expr.getIf());
  Dedent();

  Pad() << "else body:\n";
  Indent();
  Dump(if_expr.getElse());
  Dedent();

  Dedent();
}

void ASTDumper::Dump(const Arg &arg) {
  Pad() << "Arg #" << arg.getArgNo() << " " << &arg << "\n";
}

void ASTDumper::Dump(const BinOp &binop) {
  std::string op;
  switch (binop.getOp()) {
    case BinOp::OK_Sub:
      op = "SUB";
      break;
    case BinOp::OK_Add:
      op = "ADD";
      break;
    case BinOp::OK_Lt:
      op = "LT";
      break;
    case BinOp::OK_Ge:
      op = "GE";
      break;
    case BinOp::OK_Eq:
      op = "EQ";
      break;
    case BinOp::OK_Or:
      op = "OR";
      break;
    case BinOp::OK_Mod:
      op = "MOD";
      break;
  }
  Pad() << "BinOp " << op << " " << &binop << "\n";
  Indent();

  Dump(binop.getLHS());
  Dump(binop.getRHS());

  Dedent();
}

}  // namespace lang
