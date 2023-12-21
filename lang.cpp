#include <fstream>
#include <iostream>

#include "argparse.h"
#include "astdumper.h"
#include "compiler.h"
#include "lexer.h"
#include "parser.h"

int main(int argc, char **argv) {
  argparse::ArgParser argparser(argc, argv);
  argparser.AddPosArg("file");
  argparser.AddOptArg<bool>("emit-llvm").setStoreTrue();
  argparser.AddOptArg<bool>("dump-ast").setStoreTrue();
  argparser.AddOptArg("output", 'o');

  auto f = argparser.get("file");
  if (!f) return 1;

  std::ifstream input(*f);
  if (!input) {
    std::cerr << "Could not open file " << *f << std::endl;
    return 1;
  }

  lang::Lexer lexer(input);
  lang::Parser parser(lexer);
  auto maybe_ast = parser.Parse();
  if (!maybe_ast) {
    std::cerr << maybe_ast.getError() << std::endl;
    return 1;
  }

  if (*argparser.get<bool>("dump-ast")) {
    for (const lang::Node *node : *maybe_ast)
      lang::ASTDumper(*node, std::cout).Dump();
    return 0;
  }

  auto mode = lang::File;
  if (*argparser.get<bool>("emit-llvm")) mode = lang::IR;

  std::string outname;
  if (auto output = argparser.get("output")) {
    if (std::string_view(*output) == "-") {
      return Compile(*maybe_ast, std::cout, mode, /*modname=*/"") ? 0 : 1;
    }

    outname.append(*output);
  } else {
    outname.append(*f).append(".obj");
  }
  return Compile(*maybe_ast, outname, mode) ? 0 : 1;
}
