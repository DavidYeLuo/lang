#include <gtest/gtest.h>

#include <fstream>
#include <istream>

#include "ast.h"
#include "astbuilder.h"
#include "parser.h"

namespace {

TEST(Parser, GenericRemainingArgumentTypesMatch) {
  lang::ASTBuilder builder;
  const auto &callable_ty = builder.getCallableType(
      builder.getIOType(), {&builder.getIOType(), &builder.getGenericType()});
  std::vector<const lang::Type *> args{&builder.getIOType(),
                                       &builder.getGenericRemainingType()};
  ASSERT_TRUE(callable_ty.CanApplyArgs(args));

  // This should also match because we don't know at parse time if the remaining
  // args can fit enough for this callable until we lower it.
  const auto &callable_ty2 = builder.getCallableType(
      builder.getIOType(), {&builder.getIOType(), &builder.getGenericType(),
                            &builder.getGenericRemainingType()});
  ASSERT_TRUE(callable_ty2.CanApplyArgs(args));
}

TEST(Errors, ReturnTypeMismatch) {
  constexpr char kMismatch[] = "def main = \\IO io -> IO 2\n";
  constexpr std::string_view kExpectedError =
      "1:25: Expression type mismatch; found `int` but expected `IO`\n"
      "def main = \\IO io -> IO 2\n"
      "                        ^";
  std::stringstream input;
  input << kMismatch;
  lang::Lexer lexer(input);
  lang::Parser parser(lexer);
  auto res = parser.Parse();
  ASSERT_TRUE(res.hasError());
  ASSERT_EQ(res.getError(), kExpectedError) << res.getError();
}

TEST(Errors, MultipleCallableOverrides) {
  std::ifstream input(EXAMPLES_DIR "/multiple-callable-overrides.lang");
  constexpr std::string_view kExpectedError =
      "9:15: Callable `writeln` with type `\\IO int GENERIC -> IO` is handled "
      "by another callable\n"
      "def writeln = \\IO io int arg1 GENERIC arg2 -> IO\n"
      "              ^\n"
      "5:1: note: Declared here\n"
      "def writeln = \\IO io GENERIC arg1 GENERIC arg2 -> IO\n"
      "^";
  lang::Lexer lexer(input);
  lang::Parser parser(lexer);
  auto res = parser.Parse();
  ASSERT_TRUE(res.hasError());
  ASSERT_EQ(res.getError(), kExpectedError) << res.getError();
}

TEST(Errors, VariadicMultipleCallableOverrides) {
  std::ifstream input(EXAMPLES_DIR
                      "/variadic-args-multiple-candidates-error.lang");
  constexpr std::string_view kExpectedError =
      "5:15: Callable `writeln` with type `\\IO GENERIC GENERIC_REMAINING -> "
      "IO` "
      "is handled by another callable\n"
      "def writeln = \\IO io GENERIC arg GENERIC_REMAINING remaining -> IO\n"
      "              ^\n"
      "1:1: note: Declared here\n"
      "def writeln = \\IO io GENERIC arg GENERIC arg2 -> IO\n"
      "^";
  lang::Lexer lexer(input);
  lang::Parser parser(lexer);
  auto res = parser.Parse();
  ASSERT_TRUE(res.hasError());
  ASSERT_EQ(res.getError(), kExpectedError) << res.getError();
}

}  // namespace
