#ifndef ARGPARSE_H_
#define ARGPARSE_H_

#include <algorithm>
#include <cassert>
#include <cstring>
#include <functional>
#include <iostream>
#include <map>
#include <memory>
#include <ostream>
#include <set>
#include <span>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

namespace argparse {

class ArgumentBase {
 public:
  virtual ~ArgumentBase() = default;
};

template <typename T>
constexpr bool CanStoreTrueFalse() {
  return std::is_same_v<T, bool>;
}

template <typename T>
class Argument : public ArgumentBase {
 public:
  enum StorageMode {
    Default,
    StoreTrue,
    StoreFalse,
  };

  // NOTE: This requires the type T having a copy ctor.
  using default_callback_t = std::function<T()>;
  Argument &setDefault(const default_callback_t &cb) {
    default_callback_ = cb;
    return *this;
  }

  Argument &setStoreTrue() {
    static_assert(CanStoreTrueFalse<T>());
    mode_ = StoreTrue;
    return *this;
  }

  Argument &setStoreFalse() {
    static_assert(CanStoreTrueFalse<T>());
    mode_ = StoreFalse;
    return *this;
  }

  StorageMode getMode() const { return mode_; }

  const default_callback_t &getDefaultCallback() const {
    return default_callback_;
  }

 private:
  default_callback_t default_callback_;
  StorageMode mode_ = Default;
};

class ArgParser {
 public:
  ArgParser(int argc, char *const *argv, std::ostream &err)
      : argc_(static_cast<size_t>(argc)), argv_(argv), err_(err) {
    assert(argc > 0);
  }
  ArgParser(int argc, char *const *argv) : ArgParser(argc, argv, std::cerr) {}

  using default_t = const char *;

  template <typename T = default_t>
  Argument<T> &AddPosArg(std::string_view argname) {
    auto found_arg = arg_map_.find(argname);
    if (found_arg != arg_map_.end()) {
      err_ << "Positional argument `" << argname
           << "` was already registered\n";
      return static_cast<Argument<T> &>(*found_arg->second);
    }

    auto p = std::make_pair(getTypeID<T>(), std::string(argname));
    arg_types_.insert(p);
    pos_args_.emplace_back(argname);
    Argument<T> *arg = new Argument<T>();
    arg_map_.try_emplace(std::string(argname), arg);
    return *arg;
  }

  template <typename T = default_t>
  Argument<T> &AddOptArg(std::string_view argname) {
    auto found_arg = arg_map_.find(argname);
    if (found_arg != arg_map_.end()) {
      err_ << "Optional argument `" << argname << "` was already registered\n";
      return static_cast<Argument<T> &>(*found_arg->second);
    }

    auto p = std::make_pair(getTypeID<T>(), std::string(argname));
    arg_types_.insert(p);
    opt_args_.emplace(argname);
    Argument<T> *arg = new Argument<T>();
    arg_map_.try_emplace(std::string(argname), arg);
    return *arg;
  }

  template <typename T = default_t>
  Argument<T> &AddOptArg(std::string_view argname, char shortname) {
    auto it = shortname_map_.try_emplace(shortname, argname);
    if (!it.second) {
      err_ << "Optional argument `" << argname << "` with short name `"
           << shortname << "` was already registered\n";
    }
    return AddOptArg(argname);
  }

  template <typename T = default_t>
  std::unique_ptr<T> get(std::string_view argname) const {
    auto p = std::make_pair(getTypeID<T>(), std::string(argname));
    if (!arg_types_.contains(p)) {
      err_ << "Type mismatch for argument `" << argname << "`\n";
      return nullptr;
    }

    auto pos_arg = std::find(pos_args_.begin(), pos_args_.end(), argname);
    if (pos_arg != pos_args_.end()) {
      auto idx = pos_arg - pos_args_.begin() + 1;  // +1 since argv[0] is the
                                                   // executable.
      if (idx < argc_)
        return std::make_unique<T>(argv_[idx]);

      // See if this has a default callback.
      const Argument<T> &arg =
          static_cast<Argument<T> &>(*arg_map_.find(argname)->second);
      if (const auto &default_cb = arg.getDefaultCallback())
        return std::make_unique<T>(default_cb());

      err_ << "Not enough positional argument provided for `" << argname
           << "`; " << argc_ << " arguments found, but `" << argname
           << "` is positional argument number " << idx << "\n";
      return nullptr;
    }

    auto found_opt_arg = opt_args_.find(argname);
    if (found_opt_arg != opt_args_.end()) {
      for (size_t i = 1; i < argc_; ++i) {
        std::string_view param(argv_[i]);
        bool found_flag = [&]() {
          if (param.starts_with("-") && param.size() == 2) {
            // This is a short name. Find the corresponding long name.
            auto found_name = shortname_map_.find(param.at(1));
            if (found_name == shortname_map_.end())
              return false;

            return argname == found_name->second;
          }

          return param.starts_with("--") && param.substr(2) == argname;
        }();

        if (!found_flag)
          continue;

        // Found it. Check the modes.
        const auto &arg =
            static_cast<Argument<T> &>(*arg_map_.find(argname)->second);
        if constexpr (CanStoreTrueFalse<T>()) {
          if (arg.getMode() == Argument<T>::StoreTrue)
            return std::make_unique<T>(true);
          if (arg.getMode() == Argument<T>::StoreFalse)
            return std::make_unique<T>(false);
        }

        // Default mode. Get the next argument.
        if (i + 1 < argc_)
          return std::make_unique<T>(argv_[i + 1]);

        // Can't get next argument because this is the last parameter. Check if
        // there's a default callable.
        if (const auto &default_cb = arg.getDefaultCallback())
          return std::make_unique<T>(default_cb());

        // Can't do anything.
        err_ << "No argument provided for `" << argname << "`\n";
        return nullptr;
      }
    }

    // Couldn't find this argument in argc. Perhaps it either has a default
    // callback or an alternate storage mode.
    auto found_arg = arg_map_.find(argname);
    if (found_arg != arg_map_.end()) {
      const auto &arg = static_cast<Argument<T> &>(*found_arg->second);
      if constexpr (CanStoreTrueFalse<T>()) {
        if (arg.getMode() == Argument<T>::StoreTrue)
          return std::make_unique<T>(false);
        if (arg.getMode() == Argument<T>::StoreFalse)
          return std::make_unique<T>(true);
      }

      // Default mode. Check for a default callback.
      if (const auto &default_cb = arg.getDefaultCallback())
        return std::make_unique<T>(default_cb());

      // If this is an optional argument, the default value is always null. This
      // is WAI and doesn't mean we should throw an error.
      if (found_opt_arg != opt_args_.end())
        return nullptr;
    }

    err_ << "Unregistered type for `" << argname
         << "`; ensure this type was added via `AddPosArg` or `AddOptArg`\n";
    return nullptr;
  }

  size_t getNumPosArgs() const { return pos_args_.size(); }

 private:
  template <typename T>
  uintptr_t getTypeID() const {
    // `static const` in a local variable ensures this variable is initialized
    // once and will contain a single unique address. Having `T` ensures that
    // only one instance for each type T will exist (so multiple calls to
    // `AddPosArg<int>` will ensure only one static local will exist for each
    // `int`. Having it be a pointer ensures we don't need to initialize or
    // create any instance of `T`.
    //
    // NOTE: Don't compile with `-fno-threadsafe-statics` if you wish
    // initialization of this to be threadsafe.
    static const T *kTypePtr = nullptr;
    return reinterpret_cast<uintptr_t>(&kTypePtr);
  }

  const size_t argc_;
  char *const *const argv_;
  std::ostream &err_;

  using key_t = std::pair<uintptr_t, std::string>;
  std::set<key_t> arg_types_;
  std::vector<std::string> pos_args_;
  std::set<std::string, std::less<>> opt_args_;
  std::map<std::string, std::unique_ptr<ArgumentBase>, std::less<>> arg_map_;
  std::map<char, std::string> shortname_map_;
};

}  // namespace argparse

#endif  // ARGPARSE_H_
