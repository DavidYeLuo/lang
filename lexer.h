#ifndef LEXER_H_
#define LEXER_H_

#include <format>
#include <istream>
#include <string>
#include <string_view>

#include "lang.h"

namespace lang {

class Token {
 public:
  enum TokenKind {
    TK_Def,
    TK_Decl,
    TK_Identifier,
    TK_Lambda,
    TK_Assign,
    TK_Arrow,
    TK_Readc,
    TK_Call,
    TK_ImpureCall,
    // TK_None,
    TK_Str,
    TK_Char,
    TK_Let,
    TK_Keep,
    TK_Int,
    TK_If,
    TK_Else,
    TK_True,
    TK_False,
    TK_Zero,
    TK_LAngleBrack,  // <
    TK_RAngleBrack,  // >
    TK_LSqBrack,     // [
    TK_RSqBrack,     // ]
    TK_LParen,       // (
    TK_RParen,       // )
    TK_CAST,
    TK_GET,
    TK_SET,
    TK_LT,
    TK_GE,
    TK_EQ,
    TK_OR,
    TK_ADD,
    TK_SUB,
    TK_MOD,
    TK_End,
    TK_GENERIC,
    TK_EOF,
  };

  Token(TokenKind kind, SourceLocation start, SourceLocation end,
        std::string_view chars)
      : kind_(kind), start_(start), end_(end), chars_(chars) {}

  SourceLocation getStart() const { return start_; }
  SourceLocation getEnd() const { return end_; }
  TokenKind getKind() const { return kind_; }
  std::string_view getChars() const { return chars_; }

  static Token Eof() { return Token(TK_EOF, {}, {}, ""); }

  bool isa(TokenKind kind) const { return kind_ == kind; }
  bool isBinOpKind() const {
    switch (kind_) {
      case TK_ADD:
      case TK_SUB:
      case TK_LT:
      case TK_GE:
      case TK_EQ:
      case TK_OR:
      case TK_MOD:
        return true;
      default:
        return false;
    }
  }

  std::string toString() const {
    return std::format("<kind={} start={},{} end={},{} chars='{}'>", (int)kind_,
                       start_.getRow(), start_.getCol(), end_.getRow(),
                       end_.getCol(), chars_);
  }

 private:
  TokenKind kind_;
  SourceLocation start_, end_;
  std::string chars_;
};

class Lexer {
 public:
  Lexer(std::istream &input) : input_(input) {}

  Result<Token> Lex() {
    if (has_lookahead_tok_) {
      has_lookahead_tok_ = false;
      return lookahead_tok_;
    }
    return LexImpl();
  }

  Result<Token> Peek() {
    if (!has_lookahead_tok_) {
      auto res = LexImpl();
      if (!res)
        return res;
      has_lookahead_tok_ = true;
      lookahead_tok_ = *res;
    }
    return lookahead_tok_;
  }

  // Note the current location may by pointing to whitespace. To get the
  // location of the next token AFTER whitespace, instead opt for peeking and
  // checking the resulting token.
  SourceLocation getCurrentLoc() const {
    return SourceLocation(row_, col_, input_.tellg());
  }

  SourceLocation PeekLoc() {
    // Note this is what the comment above `getCurrentLoc` recommends but it
    // will crash on a bad peek.
    return Peek()->getStart();
  }

  std::istream &getInput() const { return input_; }

 private:
  int getNextChar();
  int PeekNextChar(SourceLocation &loc);
  void ConsumeChar(char expected) {
    [[maybe_unused]] char c = getNextChar();
    assert(c == expected && "Did not consume the expected character");
  }

  Result<Token> LexImpl();

  std::istream &input_;
  pos_t row_ = 1, col_ = 0;
  bool has_lookahead_tok_ = false;
  Token lookahead_tok_ = Token::Eof();
};

}  // namespace lang

#endif  // LEXER_H_
