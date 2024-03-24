#ifndef ASTVISITOR_H_
#define ASTVISITOR_H_

#include "ast.h"

namespace lang {

template <typename RetTy = void>
class ConstASTVisitor {
 public:
  virtual RetTy Visit(const Node &node) {
    switch (node.getKind()) {
#define NODE(name)      \
  case Node::NK_##name: \
    return Visit(llvm::cast<name>(node));
#include "nodes.def"
    }
  }

 protected:
#define NODE(name) virtual RetTy Visit(const name &) = 0;
#include "nodes.def"
};

template <typename RetTy = void>
class NonConstASTVisitor {
 public:
  virtual RetTy Visit(Node &node) {
    switch (node.getKind()) {
#define NODE(name)      \
  case Node::NK_##name: \
    return Visit(llvm::cast<name>(node));
#include "nodes.def"
    }
  }

 protected:
#define NODE(name) virtual RetTy Visit(name &) = 0;
#include "nodes.def"
};

}  // namespace lang

#endif  // ASTVISITOR_H_
