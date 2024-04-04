#ifndef LANG_H_
#define LANG_H_

#include <cassert>
#include <cstdint>
#include <cstdio>
#include <istream>
#include <memory>
#include <sstream>
#include <string_view>
#include <type_traits>
#include <utility>

#define UNREACHABLE(msg, ...)                                                \
  {                                                                          \
    fprintf(stderr, "%s:%d: %s: UNREACHABLE: " msg "\n", __FILE__, __LINE__, \
            __PRETTY_FUNCTION__ __VA_OPT__(, ) __VA_ARGS__);                 \
    __builtin_trap();                                                        \
  }

namespace lang {

using pos_t = uint32_t;

class SourceLocation {
 public:
  SourceLocation() = default;
  SourceLocation(pos_t row, pos_t col, std::istream::pos_type pos)
      : row_(row), col_(col), pos_(pos) {}

  pos_t getRow() const { return row_; }
  pos_t getCol() const { return col_; }
  auto getPositionIndicator() const { return pos_; }

  void setRow(pos_t row) { row_ = row; }
  void setCol(pos_t col) { col_ = col; }
  void setPositionIndicator(std::istream::pos_type pos) { pos_ = pos; }

  bool isValid() const { return row_ && col_; }

 private:
  pos_t row_, col_;
  std::istream::pos_type pos_;
};

inline std::ostream &operator<<(std::ostream &out, const SourceLocation &loc) {
  out << loc.getRow() << ":" << loc.getCol();
  return out;
}

//
// Usage:
//
//   return Result<...>::Diagnostic(input)
//     << loc << ": Expression type mismatch; found `"
//     << chars << "`" << DumpLine{loc};
//
// which results in an error looking something like:
//
//   2:11: Expected a type; instead found `2`
//   let i = 2
//           ^
//
// The original diagnostic, starting with the dump of the source location,
// followed by ": ", then a diagnostic message, then the line in the original
// source and an arrow pointing to the location.
//

void GetSrcLineAndArrow(const std::istream::pos_type pos, std::istream &input,
                        std::ostream &out);

struct DumpLine {
  const SourceLocation &loc;
};

class Diagnostic {
 public:
  Diagnostic(std::istream &input) : input_(input) {}

  Diagnostic(Diagnostic &&other) = default;

  template <typename U>
  Diagnostic &operator<<(const U &other) {
    ss_ << other;
    return *this;
  }

  Diagnostic &operator<<(const DumpLine &other) {
    GetSrcLineAndArrow(other.loc.getPositionIndicator(), input_, ss_);
    return *this;
  }

  std::string get() { return ss_.str(); }

 private:
  std::istream &input_;
  std::stringstream ss_;
};

// A result is similar to an `std::optional` but it instead carries an error
// message that can be propagated.
template <typename T>
class Result {
 public:
  // We are converting between two underlying types which cannot possibly
  // convert between each other. This can only be done if the From result is an
  // error.
  template <typename From,
            std::enable_if_t<!std::is_convertible_v<From, T>, bool> = true>
  Result(const Result<From> &other) : err_(other.getError()) {}

  // We are converting between two result types which are convertible. This
  // could mean the From result is an error, or valid.
  template <typename From,
            std::enable_if_t<std::is_convertible_v<From, T>, bool> = true>
  Result(const Result<From> &other) {
    if (other.hasError()) {
      err_ = other.getError();
    } else {
      result_.reset(new T(other.get()));
    }
  }

  template <
      typename... ArgsTy,
      std::enable_if_t<std::is_constructible_v<T, ArgsTy...>, bool> = true>
  Result(ArgsTy... args) : result_(new T(std::forward<ArgsTy>(args)...)) {}

  Result(Diagnostic &diag) : result_(nullptr), err_(diag.get()) {}
  Result(Diagnostic &&diag) : result_(nullptr), err_(diag.get()) {}

  static Result Error(std::string_view err) {
    Result res;
    res.result_ = nullptr;
    res.err_ = err;
    return res;
  }

  bool hasError() const { return !result_; }
  std::string_view getError() const {
    assert(hasError());
    return err_;
  }
  operator bool() const { return !hasError(); }

  const T &get() const {
    assert(!hasError());
    return *result_;
  }
  T &get() {
    assert(!hasError());
    return *result_;
  }
  const T &operator*() const { return get(); }
  const T *operator->() const { return &get(); }

 private:
  std::unique_ptr<T> result_;
  std::string err_;
};

template <typename Arg>
void Join(std::stringstream &ss, const Arg &arg) {
  ss << arg;
}

template <typename Arg, typename... Args>
void Join(std::stringstream &ss, const Arg &arg, const Args &...args) {
  ss << arg;
  Join(ss, args...);
}

template <typename... Args>
std::string Join(const Args &...args) {
  std::stringstream ss;
  Join(ss, args...);
  return ss.str();
}

}  // namespace lang

#endif  // LANG_H_
