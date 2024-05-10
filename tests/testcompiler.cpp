#include <gtest/gtest.h>

#include <fstream>
#include <sstream>
#include <string>
#include <string_view>

#include "astbuilder.h"
#include "compiler.h"
#include "lexer.h"
#include "parser.h"

namespace {

// TODO: It would be nice if we could use something like lit + FileCheck here
// since this is explicitly checking IR.

// Test that we do not create new buffers when they are reused as the first
// argument. That is, the buffer.lang example should look something like this:
//
// ```
//   define i32 @main_impl(i32 %io) {
//   entry:
//     %buff = alloca [3 x [4 x i8]]
//     ... do some stores
//     ... do some prints
//     ret i32 0
//   }
// ```
//
// We can do this since ownesrhip of the first buffer is passed directly to the
// first function, so the first function can just reuse the space for that
// argument. The same applies for the return values which are passed as
// arguments to the next function.
//
// LLVM optimization passes should take care of this for us.
TEST(Compiler, ReuseFirstArgument) {
  std::ifstream input(EXAMPLES_DIR "/buffer.lang");
  lang::Lexer lexer(input);
  lang::Parser parser(lexer);
  auto maybe_mod = parser.Parse();
  ASSERT_FALSE(maybe_mod.hasError()) << maybe_mod.getError();
  lang::Module &mod = **maybe_mod;
  lang::ASTBuilder builder;
  lang::Lower(mod, builder);
  std::stringstream ss;
  ASSERT_TRUE(lang::Compile(mod, ss, lang::DumpType::IR, /*modname=*/"",
                            /*source=*/"", llvm::OptimizationLevel::O3));

  // NOTE: @main_impl will get optimized out and its body will be inlined into
  // @main.
  std::string ir = ss.str();
  auto found_main_impl_def = ir.find("define i32 @main");
  ASSERT_NE(found_main_impl_def, std::string::npos)
      << "Couldn't find `main` definition";

  constexpr std::string_view kAlloca = "alloca";
  auto found_alloca = ir.find(kAlloca, found_main_impl_def);
  ASSERT_NE(found_alloca, std::string::npos)
      << "Couldn't find alloca in `main_impl` definition";

  auto found_first_ret = ir.find("ret", found_alloca);
  ASSERT_NE(found_first_ret, std::string::npos)
      << "Couldn't find call to `write` following alloca";

  auto found_other_alloca = ir.find(kAlloca, found_alloca + kAlloca.size());
  if (found_other_alloca != std::string::npos) {
    ASSERT_GT(found_other_alloca, found_first_ret)
        << "Found more than one alloca";
  }
}

// Test that when generating a function from generic args that we do not
// accidentally make reduntand calls. See regression-test-3.lang where we
// accidentally called `read_token` in `consume` multiple times: once for the
// original call, and extra ones where all the `let` expressions were used. This
// occurred because we'd clone everything including expressions from `let`
// statements since the `let`s point back to `res` which was a call. This led to
// multiple invocations of `read_token`. This test is just to make sure we don't
// make multiple calls to `read_token`.
//
// Note that on the language level, multiple calls to `read_token(io old_row
// old_col)` are fine because they return the exact same value, but codegen
// is incorrect because calls to `readc` under the hood don't actually take the
// `io` argument into account. They just dispatch to a regular gets. At some
// point in the future, this could be replaced with a runtime library that takes
// the `io` argument into account.
TEST(Compiler, Regression3) {
  std::ifstream input(EXAMPLES_DIR "/regression-test-3.lang");
  lang::Lexer lexer(input);
  lang::Parser parser(lexer);
  auto maybe_mod = parser.Parse();
  ASSERT_FALSE(maybe_mod.hasError()) << maybe_mod.getError();
  lang::Module &mod = **maybe_mod;
  lang::ASTBuilder builder;
  lang::Lower(mod, builder);
  std::stringstream ss;
  ASSERT_TRUE(lang::Compile(mod, ss, lang::DumpType::IR, /*modname=*/""));

  std::string ir = ss.str();
  auto found_consume_def = ir.find("define ptr @consume");
  ASSERT_NE(found_consume_def, std::string::npos)
      << "Couldn't find `consume` definition";

  constexpr std::string_view kReadToken = "call ptr @read_token";
  auto found_read_token = ir.find(kReadToken, found_consume_def);
  ASSERT_NE(found_read_token, std::string::npos)
      << "Couldn't find read_token call in `consume` definition";

  auto found_other_read_token =
      ir.find(kReadToken, found_read_token + kReadToken.size());
  ASSERT_EQ(found_other_read_token, std::string::npos)
      << "Found more than one call to read_token";
}

}  // namespace
