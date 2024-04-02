#include <gtest/gtest.h>

#include <sstream>
#include <type_traits>

#include "argparse.h"

// NOTE: It's much easier to test passing a char** when creating the string
// literals.
#pragma GCC diagnostic ignored "-Wwritable-strings"

namespace {

TEST(ArgParse, ArgcArgv) {
  // Test that we can pass argc and argv in their normal types from main
  // directly to ArgParser.
  int argc = 1;
  char **argv = nullptr;
  [[maybe_unused]] argparse::ArgParser parser(argc, argv);
}

TEST(ArgParse, SimplePosArgs) {
  constexpr char *kArgv[] = {
      "exe",
      "arg1",
      "arg2",
      nullptr,
  };
  constexpr int kArgc = 3;

  argparse::ArgParser parser(kArgc, kArgv);
  parser.AddPosArg<const char *>("pos1");
  parser.AddPosArg<const char *>("pos2");

  auto pos1 = parser.get<const char *>("pos1");
  ASSERT_STREQ(*pos1, "arg1");

  auto pos2 = parser.get<const char *>("pos2");
  ASSERT_STREQ(*pos2, "arg2");
}

TEST(ArgParse, PosArgDefaultValue) {
  constexpr char *kArgv[] = {
      "exe",
      nullptr,
  };
  constexpr int kArgc = 1;

  argparse::ArgParser parser(kArgc, kArgv);
  auto &arg1 = parser.AddPosArg<const char *>("arg1");
  ASSERT_EQ(parser.get<const char *>("arg1").get(), nullptr);

  arg1.setDefault([]() -> const char * { return "default value"; });
  ASSERT_EQ(*parser.get<const char *>("arg1"), "default value");
}

TEST(ArgParse, PosArgDefaultType) {
  constexpr char *kArgv[] = {
      "exe",
      "pos1",
      nullptr,
  };
  constexpr int kArgc = 2;

  argparse::ArgParser parser(kArgc, kArgv);
  parser.AddPosArg("arg1");
  auto res = *parser.get("arg1");
  static_assert(std::is_same_v<decltype(res), const char *>);
  ASSERT_STREQ(*parser.get("arg1"), "pos1");
}

TEST(ArgParse, SimpleOptArg) {
  constexpr char *kArgv[] = {
      "exe",
      "--param",
      "abc",
      nullptr,
  };
  constexpr int kArgc = 3;

  argparse::ArgParser parser(kArgc, kArgv);
  parser.AddOptArg("param");
  ASSERT_STREQ(*parser.get("param"), "abc");
}

TEST(ArgParse, StoreTrueFalse) {
  constexpr char *kArgv[] = {
      "exe",
      "--store-true",
      "--store-false",
      nullptr,
  };
  constexpr int kArgc = 3;

  argparse::ArgParser parser(kArgc, kArgv);
  parser.AddOptArg<bool>("store-true").setStoreTrue();
  parser.AddOptArg<bool>("store-false").setStoreFalse();
  parser.AddOptArg<bool>("store-true-not-provided").setStoreTrue();
  parser.AddOptArg<bool>("store-false-not-provided").setStoreFalse();

  ASSERT_TRUE(*parser.get<bool>("store-true"));
  ASSERT_FALSE(*parser.get<bool>("store-false"));
  ASSERT_FALSE(*parser.get<bool>("store-true-not-provided"));
  ASSERT_TRUE(*parser.get<bool>("store-false-not-provided"));
}

TEST(ArgParse, OptArgShortName) {
  constexpr char *kArgv[] = {
      "exe",
      "-o",
      "out",
      nullptr,
  };
  constexpr int kArgc = 3;

  argparse::ArgParser parser(kArgc, kArgv);
  parser.AddOptArg("output", 'o');
  ASSERT_STREQ(*parser.get("output"), "out");
}

}  // namespace
