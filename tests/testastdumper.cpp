#include <gtest/gtest.h>

#include <fstream>
#include <sstream>

#include "astdumper.h"
#include "lang.h"
#include "lexer.h"
#include "parser.h"

namespace {
// s will be the first word/token of the next line
void nextLine(std::stringstream &ss, std::string &s);
void testIfTrue(std::stringstream &ss);

TEST(ASTDumper, iftrue) {
  std::ifstream input(EXAMPLES_DIR "/if-true.lang");
  lang::Lexer lexer(input);
  lang::Parser parser(lexer);
  lang::Result<lang::Module *> maybe_mod = parser.Parse();
  ASSERT_FALSE(maybe_mod.hasError());

  lang::Module &mod = **maybe_mod;
  std::stringstream ss;
  lang::ASTDumper dumper(ss);
  for (const lang::Node *node : mod.getAST()) {
    dumper.Dump(*node);
  }

  // NOTE: This test checks for correct symbols prefix
  // BUG: This test ignores space indentation
  // TODO: Implement test that checks for space indentation
  testIfTrue(ss);
}

// Just to make sure that our test is testing correctly for if-true.lang
// Aka testing the tester
// Testing testIfTrue(std::stringstream &ss)
TEST(ASTdumper, iftrue_reference) {
  // Expected test dump for if-true.lang
  constexpr std::string_view kExpected =
      "Declare 0:0 0x5a71ac885fa0 name=write type=\\IO GENERIC -> IO "
      "iswrite=1\n"
      "Declare 1:1 0x5a71ac8865b0 name=main type=\\IO -> IO iswrite=0\n"
      "└──Callable \\IO -> IO 0x5a71ac886430\n"
      "   ├──args:\n"
      "   │  └──io IO 0x5a71ac886390\n"
      "   └──return:\n"
      "      └──Call 0x5a71ac887030 3:3 IO\n"
      "         ├──... Declare 0x5a71ac885fa0 ...\n"
      "         └──args:\n"
      "            ├──Arg #0 IO 0x5a71ac886390 (parent callable "
      "0x5a71ac886430)\n"
      "            └──If 0x5a71ac886e80\n"
      "               ├──cond:\n"
      "               │  └──Bool `1`\n"
      "               ├──if body:\n"
      "               │  └──Int 5\n"
      "               └──else body:\n"
      "                  └──Int 6\n";
  std::stringstream ss;
  ss << kExpected;
  testIfTrue(ss);
}

void nextLine(std::stringstream &ss, std::string &s) {
  getline(ss, s);
  ss >> s;
}

// BUG: This test ignores space indentation
void testIfTrue(std::stringstream &ss) {
  std::string s;
  ss >> s;
  ASSERT_EQ(s, "Declare");
  nextLine(ss, s);
  ASSERT_EQ(s, "Declare");
  nextLine(ss, s);
  ASSERT_EQ(s, "└──Callable");
  nextLine(ss, s);
  ASSERT_EQ(s, "├──args:");
  nextLine(ss, s);
  ASSERT_EQ(s, "│");
  ss >> s;
  ASSERT_EQ(s, "└──io");
  nextLine(ss, s);
  ASSERT_EQ(s, "└──return:");
  nextLine(ss, s);
  ASSERT_EQ(s, "└──Call");
  nextLine(ss, s);
  ASSERT_EQ(s, "├──...");
  nextLine(ss, s);
  ASSERT_EQ(s, "└──args:");
  nextLine(ss, s);
  ASSERT_EQ(s, "├──Arg");
  nextLine(ss, s);
  ASSERT_EQ(s, "└──If");
  nextLine(ss, s);
  ASSERT_EQ(s, "├──cond:");
  nextLine(ss, s);
  ASSERT_EQ(s, "│");
  ss >> s;
  ASSERT_EQ(s, "└──Bool");
  nextLine(ss, s);
  ASSERT_EQ(s, "├──if");
  nextLine(ss, s);
  ASSERT_EQ(s, "│");
  ss >> s;
  ASSERT_EQ(s, "└──Int");
  nextLine(ss, s);
  ASSERT_EQ(s, "└──else");
  nextLine(ss, s);
  ASSERT_EQ(s, "└──Int");
}
}  // namespace
