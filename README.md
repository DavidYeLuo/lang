# Language

![Build Status Sanitized w/Clang](https://github.com/PiJoules/lang/actions/workflows/build-sanitized-clang.yml/badge.svg)
![Build Status Sanitized w/GCC](https://github.com/PiJoules/lang/actions/workflows/build-sanitized-gcc.yml/badge.svg)

Hopefully I don't abandon this one also. This is the furthest I've gotten with
any personal project.

```sh
# Example hello world.
$ cat examples/hello-world.lang
def main = \IO io -> IO
  call write io "Hello world\n" end

# Create an object file.
$ ./lang examples/hello-world.lang

# Link into a.out
$ clang examples/hello-world.lang.obj
$ ./a.out
Hello world
```

## Deps

- [LLVM v16.0.6](https://github.com/llvm/llvm-project/releases/tag/llvmorg-16.0.6)
  - Primarily used for llvm, but can also use clang-16 for the compiler
  - Works with g++ 12
- [Googletest v1.12.1](https://github.com/google/googletest/releases/tag/release-1.12.1)

## Build

```sh
$ mkdir build
$ cd build
$ cmake .. -GNinja
$ ninja
```

Useful cmake flags:

- **-DCMAKE_CXX_COMPILER=...**
  - Path to C++ compiler (and linker) to use.
- **-DCMAKE_C_COMPILER=...**
  - Path to C compiler to use. This is really only used by tests to link object files.
- **-DLLVM_CONFIG=...**
  - Specifies the path to an `llvm-config` to use. By default this points to `llvm-config`.
- **-DCLANG_FORMAT=...**
  - Specifies the path to a `clang-format` to use. By
    default this points to `clang-format`. This is only needed for formatting.
- **-DCMAKE_CXX_FLAGS=...**
  - Extra flags to pass to the compiler. The github
    actions sanitized builder sets these as `"-fsanitize=address
    -fsanitize=undefined -ftrivial-auto-var-init=pattern"`.
- **-DCMAKE_EXE_LINKER_FLAGS=...**
  - Link flags to use. The github actions sanitized
    builder sets this to `"-fuse-ld=lld"`.
- **-DCMAKE_CXX_COMPILER_LAUNCHER=ccache**
  - Useful for local rebuilds.
- **-DCMAKE_EXPORT_COMPILE_COMMANDS=ON**
  - Build `compile_commands.json`.

## Test

This was only tested on x86_64-linux.

```sh
$ ./bin/lang-tests  # Inside the build dir.
```

### Reproducing the github actions builders

The github actions can also be tested locally via  docker containers using
[act](https://github.com/nektos/act).

```
$ act --workflows .github/workflows/build-sanitized-clang.yml
```

The github actions builder uses this configuration:

```
cmake .. -GNinja -DCMAKE_C_COMPILER=clang-16 -DCMAKE_CXX_COMPILER=clang++-16 \
  -DLLVM_CONFIG=llvm-config-16 -DCLANG_FORMAT=clang-format-16 \
  -DCMAKE_CXX_FLAGS="-fsanitize=address -fsanitize=undefined -ftrivial-auto-var-init=pattern" \
  -DCMAKE_EXE_LINKER_FLAGS="-fuse-ld=lld"
```
