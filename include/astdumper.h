#ifndef ASTDUMPER_H_
#define ASTDUMPER_H_

#include <cassert>
#include <ostream>
#include <set>

#include "ast.h"
#include "astvisitor.h"

namespace lang {

class ASTDumper : public ConstASTVisitor<> {
 public:
  ASTDumper(std::ostream &out) : out_(out) {}

  void Dump(const Node &node) { Visit(node); }

 private:
  void Visit(const Node &node) override;
#define NODE(name) void Visit(const name &) override;
#include "nodes.def"

  std::ostream &Pad();
  void Indent() { indent_++; }
  void Dedent() {
    assert(indent_ > 0);
    indent_--;
  }

  std::ostream &out_;
  size_t indent_ = 0;
  std::set<const Node *> visited_;

  static constexpr char kPadding[] = "  ";
};

inline void Dump(const Node &node) { ASTDumper(std::cerr).Dump(node); }

}  // namespace lang

#endif  // ASTDUMPER_H_
