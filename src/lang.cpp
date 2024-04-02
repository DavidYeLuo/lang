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
  argparser.AddOptArg<bool>("dump-ast-lower").setStoreTrue();
  argparser.AddOptArg("output", 'o');

  auto f = argparser.get("file");
  if (!f)
    return 1;

  std::ifstream input(*f);
  if (!input) {
    std::cerr << "Could not open file " << *f << std::endl;
    return 1;
  }

  lang::Lexer lexer(input);
  lang::Parser parser(lexer);
  auto maybe_mod = parser.Parse();
  if (!maybe_mod) {
    std::cerr << maybe_mod.getError() << std::endl;
    return 1;
  }

  lang::Module &mod = **maybe_mod;
  if (*argparser.get<bool>("dump-ast")) {
    lang::ASTDumper dumper(std::cout);
    for (const lang::Node *node : mod.getAST())
      dumper.Dump(*node);
    return 0;
  }

  lang::ASTBuilder builder;
  lang::Lower(mod, builder);

  if (*argparser.get<bool>("dump-ast-lower")) {
    lang::ASTDumper dumper(std::cout);
    for (const lang::Node *node : mod.getAST())
      dumper.Dump(*node);
    return 0;
  }

  auto mode = lang::File;
  if (*argparser.get<bool>("emit-llvm"))
    mode = lang::IR;

  std::string outname;
  if (auto output = argparser.get("output")) {
    if (std::string_view(*output) == "-") {
      return Compile(mod, std::cout, mode, /*modname=*/"", *f) ? 0 : 1;
    }

    outname.append(*output);
  } else {
    outname.append(*f).append(".obj");
  }
  return Compile(mod, outname, mode, *f) ? 0 : 1;
}
