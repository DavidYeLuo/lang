# Language

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
- [Googletest v1.12.1](https://github.com/google/googletest/releases/tag/release-1.12.1)

## Build

```sh
$ mkdir build
$ cd build
$ cmake .. -GNinja -DCMAKE_CXX_COMPILER=clang++ -DCMAKE_C_COMPILER=clang -DCMAKE_EXPORT_COMPILE_COMMANDS=ON
$ ninja
```

## Test

This was only tested on x86_64-linux.

```sh
$ ./test  # Inside the build dir.
```
