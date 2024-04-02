#include <gtest/gtest.h>

#include <sstream>

#include "lang.h"
#include "lexer.h"

namespace {

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

}  // namespace
