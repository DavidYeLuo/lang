#include "lexer.h"

#include <algorithm>
#include <cassert>
#include <cctype>
#include <map>

#include "lang.h"

namespace lang {

namespace {

const std::map<std::string, Token::TokenKind> kKeywordMap{
    {"def", Token::TK_Def},
    {"decl", Token::TK_Decl},
    {"cdecl", Token::TK_CDecl},
    {"call", Token::TK_Call},
    {"impurecall", Token::TK_ImpureCall},
    {"readc", Token::TK_Readc},
    {"end", Token::TK_End},
    {"let", Token::TK_Let},
    {"keep", Token::TK_Keep},
    {"as", Token::TK_As},
    {"mut", Token::TK_Mut},
    {"zero", Token::TK_Zero},
    {"if", Token::TK_If},
    {"else", Token::TK_Else},
    {"true", Token::TK_True},
    {"false", Token::TK_False},
    {"LT", Token::TK_LT},
    {"GE", Token::TK_GE},
    {"EQ", Token::TK_EQ},
    {"OR", Token::TK_OR},
    {"ADD", Token::TK_ADD},
    {"SUB", Token::TK_SUB},
    {"MOD", Token::TK_MOD},
    {"CAST", Token::TK_CAST},
    {"GET", Token::TK_GET},
    {"SET", Token::TK_SET},
    {"GENERIC", Token::TK_GENERIC},
    {"GENERIC_REMAINING", Token::TK_GENERIC_REMAINING},
};

}  // namespace

int Lexer::getNextChar() {
  assert(input_ && "Attempting to read from bad input.");

  if (input_.eof()) {
    // This means we have toggled the eof bit prior. So we don't need to update
    // any members.
    return EOF;
  }

  auto ch = input_.get();
  if (ch == '\n') {
    ++row_;
    col_ = 0;
  } else {
    ++col_;
  }
  return ch;
}

int Lexer::PeekNextChar(SourceLocation &loc) {
  assert(input_ && "Attempting to read from bad input.");

  loc.setRow(row_);
  loc.setCol(col_ + 1);

  if (input_.eof()) {
    loc.setEof();
    return EOF;
  }

  auto ch = input_.peek();
  if (ch == EOF) {
    // The `peek` has just set the eof bit.
    loc.setEof();
  } else {
    assert(
        input_.good() &&
        "None of the bits must be set otherwise tellg will set the fail bit");
    loc.setPositionIndicator(input_.tellg() +
                             static_cast<std::istream::off_type>(1));
  }
  assert(input_);
  return ch;
}

Result<Token> Lexer::LexImpl() {
  SourceLocation start;
  int ch = PeekNextChar(start);

  // Skip over individual things.
  while (1) {
    // Check for EOF.
    if (ch == EOF)
      return Result<Token>(Token::TK_EOF, start, start, "");

    // Check for WS.
    if (isspace(ch)) {
      while (isspace(ch)) {
        getNextChar();
        ch = PeekNextChar(start);
        if (ch == EOF)
          return Result<Token>(Token::TK_EOF, start, start, "");
      }
      continue;
    }

    // Check for comments.
    if (ch == '#') {
      // Consume all characters until the end of the line.
      while (ch != '\n') {
        getNextChar();
        ch = PeekNextChar(start);
        if (ch == EOF)
          return Result<Token>(Token::TK_EOF, start, start, "");
      }
      ConsumeChar('\n');
      ch = PeekNextChar(start);
      continue;
    }

    break;
  }

  ch = getNextChar();
  assert(!isspace(ch) && ch != '#' &&
         "Should've skipped over whitespace and comments by this point.");

  auto next_pos =
      !input_.good()
          ? SourceLocation::eof()
          : (input_.tellg() + static_cast<std::istream::off_type>(1));
  assert(input_);

  // TODO: Most of these can be in a char map.
  switch (ch) {
    case '\\':
      return Result<Token>(Token::TK_Lambda, start,
                           SourceLocation(row_, col_ + 1, next_pos), "\\");
    case '=':
      return Result<Token>(Token::TK_Assign, start,
                           SourceLocation(row_, col_ + 1, next_pos), "=");
    case ':':
      return Result<Token>(Token::TK_Colon, start,
                           SourceLocation(row_, col_ + 1, next_pos), ":");
    case '<':
      return Result<Token>(Token::TK_LAngleBrack, start,
                           SourceLocation(row_, col_ + 1, next_pos), "<");
    case '>':
      return Result<Token>(Token::TK_RAngleBrack, start,
                           SourceLocation(row_, col_ + 1, next_pos), ">");
    case '[':
      return Result<Token>(Token::TK_LSqBrack, start,
                           SourceLocation(row_, col_ + 1, next_pos), "[");
    case ']':
      return Result<Token>(Token::TK_RSqBrack, start,
                           SourceLocation(row_, col_ + 1, next_pos), "]");
    case '(':
      return Result<Token>(Token::TK_LParen, start,
                           SourceLocation(row_, col_ + 1, next_pos), "(");
    case ')':
      return Result<Token>(Token::TK_RParen, start,
                           SourceLocation(row_, col_ + 1, next_pos), ")");
    case '-': {
      int next = getNextChar();
      if (next != '>')
        return Diagnostic(input_)
               << start << ": Expected `->`; "
               << " instead found `-" << next << "`" << DumpLine{start};
      return Result<Token>(Token::TK_Arrow, start,
                           SourceLocation(row_, col_ + 1, next_pos), "->");
    }
    case '"': {
      std::string buff;
      buff += static_cast<char>(ch);
      ch = getNextChar();
      while (ch != '"') {
        if (ch == EOF) {
          return Diagnostic(input_)
                 << getCurrentLoc() << ": Unexpected EOF while parsing string";
        }
        if (ch == '\\') {
          ch = getNextChar();
          if (ch == 'n')
            ch = '\n';
          else if (ch == 't')
            ch = '\t';
        }
        buff += static_cast<char>(ch);
        ch = getNextChar();
      }
      buff += static_cast<char>(ch);
      assert(buff.front() == '"' && buff.back() == '"');
      SourceLocation end;
      PeekNextChar(end);
      return Result<Token>(Token::TK_Str, start, end, buff);
    }
    case '\'': {
      std::string buff;
      buff += static_cast<char>(ch);
      ch = getNextChar();
      if (ch == '\\') {
        // Handle literals starting with `\`.
        ch = getNextChar();
        if (ch == EOF) {
          return Diagnostic(input_)
                 << getCurrentLoc()
                 << ": Unexpected EOF while parsing character";
        }
        if (ch == 'n')
          ch = '\n';
        else if (ch == 't')
          ch = '\t';
      }
      buff += static_cast<char>(ch);
      ch = getNextChar();
      if (ch != '\'')
        return Diagnostic(input_)
               << start << ": Expected char literal to end with closing `'`; "
               << " instead found `" << ch << "`" << DumpLine{start};
      buff += static_cast<char>(ch);
      SourceLocation end;
      PeekNextChar(end);
      return Result<Token>(Token::TK_Char, start, end, buff);
    }
    default:
      break;
  }

  std::string buff;
  buff += static_cast<char>(ch);
  ch = input_.peek();

  while (isalnum(ch) || ch == '_') {
    buff += static_cast<char>(getNextChar());
    ch = input_.peek();
  }

  SourceLocation end;
  PeekNextChar(end);
  auto found_kw = kKeywordMap.find(buff);
  if (found_kw != kKeywordMap.end())
    return Result<Token>(found_kw->second, start, end, buff);

  if (std::all_of(buff.begin(), buff.end(), isdigit))
    return Result<Token>(Token::TK_Int, start, end, buff);

  if ((isalpha(buff.front()) || buff.front() == '_') &&
      std::all_of(buff.begin() + 1, buff.end(),
                  [](char c) { return isalnum(c) || c == '_'; }))
    return Result<Token>(Token::TK_Identifier, start, end, buff);

  return Diagnostic(input_)
         << start << ": Unable to lex `" << buff << "`" << DumpLine{start};
}

}  // namespace lang
