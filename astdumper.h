#ifndef ASTDUMPER_H_
#define ASTDUMPER_H_

#include <cassert>
#include <ostream>
#include <set>

#include "ast.h"

namespace lang {

class ASTDumper {
 public:
  ASTDumper(const Node &node, std::ostream &out) : node_(node), out_(out) {}

  void Dump() {
    visited_.clear();
    Dump(node_);
  }

 private:
  void Dump(const Node &node);
#define NODE(name) void Dump(const name &);
#define TYPE(name)
#include "nodes.def"

  std::ostream &Pad();
  void Indent() { indent_++; }
  void Dedent() {
    assert(indent_ > 0);
    indent_--;
  }

  const Node &node_;
  std::ostream &out_;
  size_t indent_ = 0;
  std::set<const Node *> visited_;

  static constexpr char kPadding[] = "  ";
};

}  // namespace lang

#endif  // ASTDUMPER_H_
