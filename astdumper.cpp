#include "astdumper.h"

#include <set>

#include "llvm/Support/Casting.h"

namespace lang {

void ASTDumper::Visit(const Node &node) {
  if (visited_.find(&node) != visited_.end()) {
    Pad() << "... " << Node::AsString(node.getKind()) << " " << &node
          << " ...\n";
    return;
  }

  visited_.insert(&node);

  ASTVisitor::Visit(node);
}

std::ostream &ASTDumper::Pad() {
  for (size_t i = 0; i < indent_; ++i) out_ << kPadding;
  return out_;
}

void ASTDumper::Visit(const Define &define) {
  Pad() << "Define " << &define << " name=" << define.getName() << "\n";
  Indent();
  Visit(define.getBody());
  Dedent();
}

void ASTDumper::Visit(const Declare &declare) {
  Pad() << "Declare " << &declare << " name=" << declare.getName()
        << " type=" << declare.getType().toString() << "\n";
}

void ASTDumper::Visit(const Let &let) {
  Pad() << "Let name=" << let.getName() << " " << &let << "\n";
  Indent();
  Visit(let.getExpr());
  Dedent();
}

void ASTDumper::Visit(const Keep &keep) {
  Pad() << "Keep name=" << keep.getName() << " " << &keep << "\n";
  Indent();
  Visit(keep.getExpr());
  Visit(keep.getBody());
  Dedent();
}

void ASTDumper::Visit(const Callable &callable) {
  const auto &callable_type = llvm::cast<CallableType>(callable.getType());
  Pad() << "Callable " << callable_type.toString() << " " << &callable << "\n";
  Indent();
  Pad() << "args:\n";

  Indent();
  for (size_t i = 0; i < callable.getNumArgs(); ++i) {
    Pad() << callable.getArgName(i) << " "
          << callable_type.getArgType(i).toString() << " "
          << &callable.getArg(i) << "\n";
  }
  Dedent();

  Pad() << "return:\n";
  Indent();
  if (callable.hasBody())
    Visit(callable.getBody());
  else
    Pad() << "<still constructing this callable body>\n";
  Dedent();

  Dedent();
}

void ASTDumper::Visit(const Cast &cast) {
  Pad() << "Cast " << &cast << " " << cast.getType().toString() << "\n";
  Indent();
  Visit(cast.getExpr());
  Dedent();
}

void ASTDumper::Visit(const Readc &readc) {
  Pad() << "Readc " << &readc << " " << readc.getType().toString() << "\n";
}

void ASTDumper::Visit(const Call &call) {
  Pad() << "Call " << &call << "\n";
  Indent();

  Visit(call.getFunc());

  Pad() << "args:\n";

  Indent();
  for (const Expr *arg : call.getArgs()) {
    Visit(*arg);
  }
  Dedent();

  Dedent();
}

void ASTDumper::Visit(const Zero &zero) {
  Pad() << "Zero " << &zero << " " << zero.getType().toString();
}

void ASTDumper::Visit(const Composite &comp) {
  Pad() << "Composite " << &comp << "\n";
  Indent();

  for (const Expr *elem : comp.getElems()) {
    Visit(*elem);
  }

  Dedent();
}

void ASTDumper::Visit(const Set &set) {
  Pad() << "Set " << &set << " " << set.getType().toString() << "\n";
  Indent();
  Visit(set.getExpr());
  Visit(set.getIdx());
  Visit(set.getStore());
  Dedent();
}

void ASTDumper::Visit(const Get &get) {
  Pad() << "Get " << &get << " " << get.getType().toString() << "\n";
  Indent();
  Visit(get.getExpr());
  Visit(get.getIdx());
  Dedent();
}

void ASTDumper::Visit(const Int &i) { Pad() << "Int " << i.getInt() << "\n"; }

void ASTDumper::Visit(const Str &s) {
  if (s.get() == "\n")
    Pad() << "Str `\\n`\n";
  else
    Pad() << "Str `" << s.get() << "`\n";
}

void ASTDumper::Visit(const Char &c) {
  Pad() << "Char `" << c.getChar() << "`\n";
}

void ASTDumper::Visit(const Bool &b) { Pad() << "Bool `" << b.get() << "`\n"; }

void ASTDumper::Visit(const If &if_expr) {
  Pad() << "If " << &if_expr << "\n";
  Indent();

  Pad() << "cond:\n";
  Indent();
  Visit(if_expr.getCond());
  Dedent();

  Pad() << "if body:\n";
  Indent();
  Visit(if_expr.getIf());
  Dedent();

  Pad() << "else body:\n";
  Indent();
  Visit(if_expr.getElse());
  Dedent();

  Dedent();
}

void ASTDumper::Visit(const Arg &arg) {
  Pad() << "Arg #" << arg.getArgNo() << " " << arg.getType().toString() << " "
        << &arg << " (parent callable " << &arg.getParent() << ")\n";
}

void ASTDumper::Visit(const BinOp &binop) {
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

  Visit(binop.getLHS());
  Visit(binop.getRHS());

  Dedent();
}

}  // namespace lang
