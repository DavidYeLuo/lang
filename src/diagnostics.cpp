#include <istream>
#include <sstream>
#include <string>

#include "lang.h"

namespace lang {

void GetSrcLineAndArrow(const std::istream::pos_type pos, std::istream &input,
                        std::ostream &out) {
  // Save the position.
  const auto old_pos = input.tellg();
  if (old_pos == -1)
    return;

  constexpr size_t kMaxRewind = 80;  // To prevent scanning long lines,
                                     // we'll only scan backwords (and
                                     // forwards up to this many charaters).

  input.seekg(pos);

  // Rewind until we hit the start of the line.
  size_t num_rewinded_chars = 0;
  input.seekg(-1, std::ios_base::cur);  // Move back one so we can peek
                                        // starting from current pos.
  for (; num_rewinded_chars < kMaxRewind; ++num_rewinded_chars) {
    int ch = input.peek();
    assert(ch != EOF);
    if (ch == '\n' || input.tellg() == 0)
      break;
    input.seekg(-1, std::ios_base::cur);
  }

  if (input.peek() == '\n') {
    input.seekg(1, std::ios_base::cur);
    assert(num_rewinded_chars);
    num_rewinded_chars--;
  }

  // Retrieve the line.
  out << '\n';
  for (size_t i = 0; i < kMaxRewind * 2; ++i) {
    int ch = input.get();
    if (ch == '\n' || ch == EOF)
      break;
    out << static_cast<char>(ch);
  }
  out << '\n';

  // Print the ^
  for (size_t i = 0; i < num_rewinded_chars; ++i)
    out << ' ';
  out << '^';

  // Reset the position.
  input.seekg(old_pos);
}

}  // namespace lang
