#include <gtest/gtest.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/wait.h>

#include <filesystem>
#include <format>
#include <fstream>
#include <sstream>
#include <string>
#include <string_view>
#include <vector>

#include "astbuilder.h"
#include "compiler.h"
#include "lang.h"
#include "lexer.h"
#include "parser.h"

namespace {

// FIXME: Ensure the commands and classes below are threadsafe such that the
// tests can be run in parralel.
class TmpFile {
 public:
  TmpFile() : path_(tmpnam(nullptr)) {}
  ~TmpFile() { std::filesystem::remove(path_); }

  std::string_view getPath() const { return path_; }
  std::string getContents() const {
    if (std::filesystem::exists(path_)) {
      std::stringstream ss;
      ss << std::ifstream(path_).rdbuf();
      return ss.str();
    }
    return "";
  }

 private:
  std::string path_;
};

#if defined(__linux__)
int RunCommand(std::string_view cmd, std::string *out = nullptr,
               std::string *err = nullptr, std::string *in_text = nullptr) {
  std::string sys_cmd;
  if (in_text)
    sys_cmd.append("echo -n '").append(*in_text).append("' | ");
  sys_cmd.append(cmd);

  TmpFile out_file, err_file;
  if (out)
    sys_cmd.append(" >").append(out_file.getPath());
  if (err)
    sys_cmd.append(" 2>").append(err_file.getPath());

  std::cerr << "Running cmd: " << sys_cmd << std::endl;

  int res = system(sys_cmd.c_str());
  if (out)
    *out = out_file.getContents();
  if (err)
    *err = err_file.getContents();

  if (WIFEXITED(res))
    return WEXITSTATUS(res);
  return res;
}

void BuildAndCheckOutput(const std::vector<const lang::Node *> &ast,
                         std::string_view expected,
                         std::string *in_text = nullptr) {
  constexpr std::string_view kOut = "a.out";
  TmpFile obj_file;
  ASSERT_TRUE(lang::Compile(ast, obj_file.getPath(), lang::File));
  ASSERT_EQ(RunCommand(std::format("clang {} -o {}", obj_file.getPath(), kOut)),
            0);

  std::string out;
  ASSERT_EQ(
      RunCommand(std::format("./{}", kOut), &out, /*err=*/nullptr, in_text), 0);
  ASSERT_EQ(out, expected);
}
#else
#error "Unhanlded OS"
#endif

void BuildAndCheckOutput(std::istream &input, std::string_view expected,
                         std::string *in_text = nullptr) {
  lang::Lexer lexer(input);
  lang::Parser parser(lexer);
  auto maybe_ast = parser.Parse();
  ASSERT_FALSE(maybe_ast.hasError()) << maybe_ast.getError();
  BuildAndCheckOutput(*maybe_ast, expected, in_text);
}

void BuildAndCheckOutput(std::string_view code, std::string_view expected,
                         std::string *in_text = nullptr) {
  std::stringstream input;
  input << code;
  BuildAndCheckOutput(input, expected, in_text);
}

constexpr char kHelloWorldStr[] =
    "def main = \\IO io -> IO\n"
    "  call write io \"Hello world\\n\" end";

TEST(Lexer, HelloWorld) {
  using lang::Token;

  std::stringstream input(kHelloWorldStr);
  lang::Lexer lexer(input);

  using lang::pos_t;
  auto CheckToken = [](const Token &tok, lang::Token::TokenKind expected_kind,
                       pos_t expected_start_row, pos_t expected_start_col,
                       pos_t expected_end_row, pos_t expected_end_col,
                       std::string_view expected_chars) {
    ASSERT_EQ(tok.getKind(), expected_kind);
    ASSERT_EQ(tok.getStart().getRow(), expected_start_row);
    ASSERT_EQ(tok.getStart().getCol(), expected_start_col);
    ASSERT_EQ(tok.getEnd().getRow(), expected_end_row);
    ASSERT_EQ(tok.getEnd().getCol(), expected_end_col);
    ASSERT_EQ(tok.getChars(), expected_chars);
  };

  CheckToken(*lexer.Lex(), Token::TK_Def, 1, 1, 1, 4, "def");
  CheckToken(*lexer.Lex(), Token::TK_Identifier, 1, 5, 1, 9, "main");
  CheckToken(*lexer.Lex(), Token::TK_Assign, 1, 10, 1, 11, "=");
  CheckToken(*lexer.Lex(), Token::TK_Lambda, 1, 12, 1, 13, "\\");
  CheckToken(*lexer.Lex(), Token::TK_Identifier, 1, 13, 1, 15, "IO");
  CheckToken(*lexer.Lex(), Token::TK_Identifier, 1, 16, 1, 18, "io");
  CheckToken(*lexer.Lex(), Token::TK_Arrow, 1, 19, 1, 21, "->");
  CheckToken(*lexer.Lex(), Token::TK_Identifier, 1, 22, 1, 24, "IO");
  CheckToken(*lexer.Lex(), Token::TK_Call, 2, 3, 2, 7, "call");
  CheckToken(*lexer.Lex(), Token::TK_Identifier, 2, 8, 2, 13, "write");
  CheckToken(*lexer.Lex(), Token::TK_Identifier, 2, 14, 2, 16, "io");
  CheckToken(*lexer.Lex(), Token::TK_Str, 2, 17, 2, 32, "\"Hello world\n\"");
  CheckToken(*lexer.Lex(), Token::TK_End, 2, 33, 2, 36, "end");
  ASSERT_EQ(lexer.Lex()->getKind(), Token::TK_EOF);
}

TEST(Errors, ShowLineInError) {
  constexpr char kMissingType[] =
      "def main = \\IO io -> IO\n"
      "  let i = 2\n"
      "  io";
  constexpr std::string_view kExpectedError =
      "2:11: Expected a type; instead found `2`\n"
      "  let i = 2\n"
      "          ^";
  std::stringstream input;
  input << kMissingType;
  lang::Lexer lexer(input);
  lang::Parser parser(lexer);
  auto res = parser.Parse();
  ASSERT_TRUE(res.hasError());
  ASSERT_EQ(res.getError(), kExpectedError);
}

TEST(Errors, ReturnTypeMismatch) {
  constexpr char kMismatch[] = "def main = \\IO io -> IO 2\n";
  constexpr std::string_view kExpectedError =
      "1:25: Expression type mismatch; found `int` but expected `IO`";
  std::stringstream input;
  input << kMismatch;
  lang::Lexer lexer(input);
  lang::Parser parser(lexer);
  auto res = parser.Parse();
  ASSERT_TRUE(res.hasError());
  ASSERT_TRUE(res.getError().starts_with(kExpectedError)) << res.getError();
}

TEST(Errors, MultipleCallableOverrides) {
  std::ifstream input("examples/multiple-callable-overrides.lang");
  constexpr std::string_view kExpectedError =
      "9:15: Callable `writeln` with arg types `IO int GENERIC ` is handled by "
      "another callable\n"
      "def writeln = \\IO io int arg1 GENERIC arg2 -> IO\n"
      "              ^\n";
  lang::Lexer lexer(input);
  lang::Parser parser(lexer);
  auto res = parser.Parse();
  ASSERT_TRUE(res.hasError());
  ASSERT_TRUE(res.getError().starts_with(kExpectedError)) << res.getError();
}

TEST(E2E, HelloWorld) { BuildAndCheckOutput(kHelloWorldStr, "Hello world\n"); }

class LangCompilerE2E : public testing::Test {
 public:
  static constexpr std::string_view kLangCompilerName = "./lang.out";

 protected:
  void SetUp() override {
    // The second stage compiler creates an object file called obj.obj. We will
    // use the second stage to create this, so let's delete it first.
    std::filesystem::remove("obj.obj");

    ASSERT_EQ(RunCommand("./lang examples/compiler.lang"), 0);
    ASSERT_EQ(
        RunCommand(std::format("clang examples/compiler.lang.obj $(llvm-config "
                               "--ldflags --system-libs --libs core) -o {}",
                               kLangCompilerName)),
        0);
  }
};

// Test building hello-world.lang with the compiler in the language.
TEST_F(LangCompilerE2E, HelloWorld) {
  std::ifstream t("examples/hello-world.lang");
  std::stringstream buffer;
  buffer << t.rdbuf();
  std::string in(buffer.str());

  // This creates an object file called obj.obj.
  ASSERT_EQ(
      RunCommand(kLangCompilerName, /*out=*/nullptr, /*err=*/nullptr, &in), 0);
  ASSERT_EQ(RunCommand("clang obj.obj -o hello-world.out"), 0);
  std::string out;

  // TODO: Right now, the executable return value is meaningless, so ignore it
  // for now, but we should check it in the future.
  RunCommand("./hello-world.out", &out);
  ASSERT_EQ(out, "Hello world\n");
}

TEST(E2E, HelloWorldLet) {
  std::ifstream input("examples/hello-world-let.lang");
  BuildAndCheckOutput(input, "Hello world\n");
}

TEST(E2E, WriteInt) {
  constexpr char kWriteIntStr[] =
      "def main = \\IO io -> IO\n"
      "  call write io 5 end";
  BuildAndCheckOutput(kWriteIntStr, "5");
}

TEST(E2E, If) {
  constexpr char kIfTrueStr[] =
      "def main = \\IO io -> IO\n"
      "  let x = int if true 5 else 6\n"
      "  call write io x end";
  BuildAndCheckOutput(kIfTrueStr, "5");

  constexpr char kIfFalseStr[] =
      "def main = \\IO io -> IO\n"
      "  let x = int if false 5 else 6\n"
      "  call write io x end";
  BuildAndCheckOutput(kIfFalseStr, "6");
}

TEST(E2E, Fib) {
  constexpr char kFibStr[] =
      "def fib = \\int n -> int             \n"
      "  if LT n 2                          \n"
      "    n                                \n"
      "  else                               \n"
      "    let x = int call fib SUB n 1 end \n"
      "    let y = int call fib SUB n 2 end \n"
      "    ADD x y                          \n"
      "def main = \\IO io -> IO             \n"
      "  let x = int call fib 10 end        \n"
      "  call write io x end";

  // fib(10) = 55
  BuildAndCheckOutput(kFibStr, "55");
}

TEST(E2E, FibNonRecurse) {
  constexpr char kFibNonRecurse[] =
      "\
def fib_impl = \\int lim int x int fib_n_1 int fib_n -> int \
  if EQ x lim                                               \
    fib_n                                                   \
  else                                                      \
    call fib_impl                                           \
      lim                                                   \
      ADD x 1                                               \
      fib_n                                                 \
      ADD fib_n fib_n_1                                     \
    end                                                     \
                                                            \
def fib = \\int n -> int                                    \
  if LT n 2                                                 \
    n                                                       \
  else                                                      \
    call fib_impl n 2 1 1 end                               \
                                                            \
def main = \\IO io -> IO                                    \
  let x = int call fib 10 end                               \
  call write io x end";

  BuildAndCheckOutput(kFibNonRecurse, "55");
}

TEST(E2E, ProjectEuler1) {
  constexpr char kPE1[] =
      "\
def sum = \\int lim int x int accum -> int  \
  if GE x lim                               \
    accum                                   \
  else                                      \
    if OR EQ MOD x 3 0 EQ MOD x 5 0         \
      call sum lim ADD x 1 ADD accum x end  \
    else                                    \
      call sum lim ADD x 1 accum end        \
                                            \
def main = \\IO io -> IO                    \
  let x = int call sum 1000 0 0 end         \
  call write io x end";
  BuildAndCheckOutput(kPE1, "233168");
}

TEST(E2E, FuncAsArgument) {
  std::ifstream input("examples/func-as-argument.lang");
  BuildAndCheckOutput(input, "11");
}

TEST(E2E, Readc) {
  constexpr char kReadc[] =
      "\
def main = \\IO io -> IO \
  let x = <IO int> call readc io end \
  let io2 = IO GET IO x 0 \
  let c = int GET int x 1 \
  call write io2 CAST char c end";
  std::string in("a");
  BuildAndCheckOutput(kReadc, "a", &in);
}

TEST(E2E, ReadAllStdin) {
  constexpr char kReadc[] =
      "\
  def echo = \\IO io -> IO \
    let x = <IO int> call readc io end \
    let io2 = IO GET IO x 0 \
    let c = int GET int x 1 \
    if LT c 0 \
      io2 \
    else \
      let io3 = IO call write io2 CAST char c end \
      call echo io3 end \
  def main = \\IO io -> IO \
    call echo io end";
  std::string in(kReadc);
  BuildAndCheckOutput(kReadc, kReadc, &in);
}

TEST(E2E, Buffer) {
  std::ifstream input("examples/buffer.lang");
  constexpr char kExpected[] =
      "abc\n"
      "xyz\n"
      "123\n";
  BuildAndCheckOutput(input, kExpected);
}

TEST(E2E, PrintChars) {
  std::ifstream input("examples/composite-type.lang");
  constexpr char kExpected[] =
      "97 a\n"
      "98 b\n"
      "99 c\n"
      "100 d\n"
      "101 e\n"
      "102 f\n"
      "103 g\n"
      "104 h\n"
      "105 i\n"
      "106 j\n"
      "107 k\n"
      "108 l\n"
      "109 m\n"
      "110 n\n"
      "111 o\n"
      "112 p\n"
      "113 q\n"
      "114 r\n"
      "115 s\n"
      "116 t\n"
      "117 u\n"
      "118 v\n"
      "119 w\n"
      "120 x\n"
      "121 y\n"
      "122 z\n";
  BuildAndCheckOutput(input, kExpected);
}

TEST(E2E, CharsArrayStr) {
  std::ifstream input("examples/char-array-str.lang");
  BuildAndCheckOutput(input, "abc\n");
}

TEST(E2E, ReadSingleToken) {
  std::ifstream t("examples/hello-world.lang");
  std::stringstream buffer;
  buffer << t.rdbuf();
  std::string in(buffer.str());

  std::ifstream input("examples/read-token.lang");
  BuildAndCheckOutput(input, "def\n", &in);
}

TEST(E2E, ReadAllTokens) {
  std::ifstream t("examples/hello-world.lang");
  std::stringstream buffer;
  buffer << t.rdbuf();
  std::string in(buffer.str());

  std::ifstream input("examples/read-all-tokens.lang");
  constexpr char kExpected[] =
      "def\n"
      "main\n"
      "=\n"
      "\\IO\n"
      "io\n"
      "->\n"
      "IO\n"
      "call\n"
      "write\n"
      "io\n"
      "\"Hello world\\n\"\n"
      "end\n";
  BuildAndCheckOutput(input, kExpected, &in);
}

TEST(E2E, HelloWorldWithComments) {
  std::ifstream input("examples/hello-world-comments.lang");
  BuildAndCheckOutput(input, "Hello world\n");
}

TEST(RegressionTests, Regression1) {
  // Assert we can compile this normally. Prior, this would hang infinitely in
  // doRAU when replacing a value with itself.
  std::ifstream input("examples/regression-test-1.lang");
  lang::Lexer lexer(input);
  lang::Parser parser(lexer);
  auto maybe_ast = parser.Parse();
  ASSERT_FALSE(maybe_ast.hasError()) << maybe_ast.getError();
  TmpFile obj_file;
  ASSERT_TRUE(lang::Compile(*maybe_ast, obj_file.getPath(), lang::File));
}

TEST(E2E, TypeDeduction) {
  std::ifstream input("examples/type-deduction.lang");
  BuildAndCheckOutput(input, "abc\n123\n");
}

TEST(E2E, Metaprogramming) {
  std::ifstream input("examples/metaprogramming.lang");
  BuildAndCheckOutput(input, "abc\n123\ndef456\n");
}

TEST(E2E, AltCallSyntax) {
  std::ifstream input("examples/alt-call-syntax.lang");
  BuildAndCheckOutput(input, "result is: 55\nabc\n123\ndef456\n");
}

}  // namespace
