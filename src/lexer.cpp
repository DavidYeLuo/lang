#include "lexer.h"

#include <algorithm>
#include <cassert>
#include <cctype>

#include "lang.h"

namespace lang {

int Lexer::getNextChar() {
  int ch = input_.get();
  if (ch == '\n') {
    ++row_;
    col_ = 0;
  } else {
    ++col_;
  }
  return ch;
}

int Lexer::PeekNextChar(SourceLocation &loc) {
  int ch = input_.peek();
  loc.setRow(row_);
  loc.setCol(col_ + 1);
  loc.setPositionIndicator(input_.tellg() +
                           static_cast<std::istream::off_type>(1));
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

  auto next_pos = input_.tellg() + static_cast<std::istream::off_type>(1);

  switch (ch) {
    case '\\':
      return Result<Token>(Token::TK_Lambda, start,
                           SourceLocation(row_, col_ + 1, next_pos), "\\");
    case '=':
      return Result<Token>(Token::TK_Assign, start,
                           SourceLocation(row_, col_ + 1, next_pos), "=");
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
      char next = getNextChar();
      if (next != '>')
        return Diagnostic(input_)
               << start << ": Expected `->`; "
               << " instead found `-" << next << "`" << DumpLine{start};
      return Result<Token>(Token::TK_Arrow, start,
                           SourceLocation(row_, col_ + 1, next_pos), "->");
    }
    case '"': {
      std::string buff;
      buff += ch;
      ch = getNextChar();
      while (ch != '"') {
        if (ch == '\\') {
          ch = getNextChar();
          if (ch == 'n')
            ch = '\n';
          else if (ch == 't')
            ch = '\t';
        }
        buff += ch;
        ch = getNextChar();
      }
      buff += ch;
      assert(buff.front() == '"' && buff.back() == '"');
      SourceLocation end;
      PeekNextChar(end);
      return Result<Token>(Token::TK_Str, start, end, buff);
    }
    case '\'': {
      std::string buff;
      buff += ch;
      ch = getNextChar();
      if (ch == '\\') {
        // Handle literals starting with `\`.
        ch = getNextChar();
        if (ch == 'n')
          ch = '\n';
        else if (ch == 't')
          ch = '\t';
      }
      buff += ch;
      ch = getNextChar();
      if (ch != '\'')
        return Diagnostic(input_)
               << start << ": Expected char literal to end with closing `'`; "
               << " instead found `" << ch << "`" << DumpLine{start};
      buff += ch;
      SourceLocation end;
      PeekNextChar(end);
      return Result<Token>(Token::TK_Char, start, end, buff);
    }
    default:
      break;
  }

  std::string buff;
  buff += ch;
  ch = input_.peek();

  while (isalnum(ch) || ch == '_') {
    buff += getNextChar();
    ch = input_.peek();
  }

  SourceLocation end;
  PeekNextChar(end);

  if (buff == "def")
    return Result<Token>(Token::TK_Def, start, end, buff);
  if (buff == "decl")
    return Result<Token>(Token::TK_Decl, start, end, buff);
  if (buff == "cdecl")
    return Result<Token>(Token::TK_CDecl, start, end, buff);
  if (buff == "call")
    return Result<Token>(Token::TK_Call, start, end, buff);
  if (buff == "impurecall")
    return Result<Token>(Token::TK_ImpureCall, start, end, buff);
  if (buff == "readc")
    return Result<Token>(Token::TK_Readc, start, end, buff);
  if (buff == "end")
    return Result<Token>(Token::TK_End, start, end, buff);
  if (buff == "let")
    return Result<Token>(Token::TK_Let, start, end, buff);
  if (buff == "keep")
    return Result<Token>(Token::TK_Keep, start, end, buff);
  if (buff == "zero")
    return Result<Token>(Token::TK_Zero, start, end, buff);
  if (buff == "if")
    return Result<Token>(Token::TK_If, start, end, buff);
  if (buff == "else")
    return Result<Token>(Token::TK_Else, start, end, buff);
  // if (buff == "None") return Result<Token>(Token::TK_None, start, end, buff);
  if (buff == "true")
    return Result<Token>(Token::TK_True, start, end, buff);
  if (buff == "false")
    return Result<Token>(Token::TK_False, start, end, buff);
  if (buff == "LT")
    return Result<Token>(Token::TK_LT, start, end, buff);
  if (buff == "GE")
    return Result<Token>(Token::TK_GE, start, end, buff);
  if (buff == "EQ")
    return Result<Token>(Token::TK_EQ, start, end, buff);
  if (buff == "OR")
    return Result<Token>(Token::TK_OR, start, end, buff);
  if (buff == "ADD")
    return Result<Token>(Token::TK_ADD, start, end, buff);
  if (buff == "SUB")
    return Result<Token>(Token::TK_SUB, start, end, buff);
  if (buff == "MOD")
    return Result<Token>(Token::TK_MOD, start, end, buff);
  if (buff == "CAST")
    return Result<Token>(Token::TK_CAST, start, end, buff);
  if (buff == "GET")
    return Result<Token>(Token::TK_GET, start, end, buff);
  if (buff == "SET")
    return Result<Token>(Token::TK_SET, start, end, buff);
  if (buff == "GENERIC")
    return Result<Token>(Token::TK_GENERIC, start, end, buff);
  if (buff == "GENERIC_REMAINING")
    return Result<Token>(Token::TK_GENERIC_REMAINING, start, end, buff);

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
