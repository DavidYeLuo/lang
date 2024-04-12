#include <istream>
#include <sstream>
#include <string>

#include "lang.h"

namespace lang {

void GetSrcLineAndArrow(const std::istream::pos_type pos, std::istream &input,
                        std::ostream &out) {
  assert(input);

  // Save the position.
  bool hit_eof = input.eof();
  auto old_pos =
      !hit_eof ? input.tellg() : static_cast<std::istream::pos_type>(0);
  assert(input);

  constexpr size_t kMaxRewind = 80;  // To prevent scanning long lines,
                                     // we'll only scan backwords (and
                                     // forwards up to this many charaters).

  if (pos == SourceLocation::eof())
    input.seekg(0, std::ios_base::end);
  else
    input.seekg(pos);
  assert(input && "Failed to seek to the position indicator");

  // Rewind until we hit the start of the line.
  size_t num_rewinded_chars = 0;
  input.seekg(-1, std::ios_base::cur);  // Move back one so we can peek
                                        // starting from current pos.
  assert(input);
  for (; num_rewinded_chars < kMaxRewind; ++num_rewinded_chars) {
    if (input.eof())
      break;
    int ch = input.peek();
    if (ch == '\n' || input.tellg() == 0)
      break;
    input.seekg(-1, std::ios_base::cur);
  }
  assert(input);

  if (input.peek() == '\n') {
    input.seekg(1, std::ios_base::cur);
    assert(num_rewinded_chars);
    num_rewinded_chars--;
  }
  assert(input);

  // Retrieve the line.
  out << '\n';
  for (size_t i = 0; i < kMaxRewind * 2; ++i) {
    if (input.eof() || input.peek() == EOF)
      break;
    int ch = input.get();
    if (ch == '\n')
      break;
    out << static_cast<char>(ch);
  }
  out << '\n';
  assert(input);

  // Print the ^
  for (size_t i = 0; i < num_rewinded_chars; ++i)
    out << ' ';
  out << '^';

  // Reset the position.
  assert(input);
  if (hit_eof) {
    input.seekg(0, std::ios_base::end);
  } else {
    input.seekg(old_pos);
  }
  assert(input);
}

}  // namespace lang
