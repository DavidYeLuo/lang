# Language

Hopefully I don't abandon this one also. This is the furthest I've gotten with
any personal project.

```sh
$ make -j8  # Make the compiler `lang`

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
- [Googletest v1.12.1](https://github.com/google/googletest/releases/tag/release-1.12.1)

## Build

Ensure `GTEST_LIB` and `GTEST_HDR` are set as environment variables, otherwise
they will need to be added to every `make` command. `GTEST_HDR` should point to
`$GTEST_DIR/googletest/include` and `GTEST_LIB` should point to `$GTEST_DIR/build/lib`.

```sh
$ make -j8
```

## Test

This was only tested on x86_64-linux.

```sh
$ make test
$ ./test
```
