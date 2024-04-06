#include <fstream>
#include <iostream>

#include "argparse.h"
#include "astdumper.h"
#include "compiler.h"
#include "lexer.h"
#include "llvm/Passes/OptimizationLevel.h"
#include "parser.h"

int main(int argc, char **argv) {
  argparse::ArgParser argparser(argc, argv);
  argparser.AddPosArg("file");
  // TODO: Consolidate these emit flags.
  argparser.AddOptArg<bool>("emit-llvm").setStoreTrue();
  argparser.AddOptArg<bool>("emit-asm").setStoreTrue();
  argparser.AddOptArg<bool>("dump-ast").setStoreTrue();
  argparser.AddOptArg<bool>("dump-ast-lower").setStoreTrue();
  argparser.AddOptArg<bool>("opt").setStoreTrue();
  argparser.AddOptArg<bool>("sanitize-address").setStoreTrue();
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

  auto mode = lang::DumpType::File;
  if (*argparser.get<bool>("emit-llvm"))
    mode = lang::DumpType::IR;
  else if (*argparser.get<bool>("emit-asm"))
    mode = lang::DumpType::ASM;

  auto optlvl = *argparser.get<bool>("opt") ? llvm::OptimizationLevel::O3
                                            : llvm::OptimizationLevel::O0;

  std::string outname;
  if (auto output = argparser.get("output")) {
    if (std::string_view(*output) == "-")
      return Compile(mod, std::cout, mode, /*modname=*/"", *f, optlvl,
                     *argparser.get<bool>("sanitize-address"))
                 ? 0
                 : 1;

    outname.append(*output);
  } else {
    outname.append(*f).append(".obj");
  }
  return Compile(mod, outname, mode, *f, optlvl,
                 *argparser.get<bool>("sanitize-address"))
             ? 0
             : 1;
}
