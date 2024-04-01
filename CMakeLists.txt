cmake_minimum_required(VERSION 3.25)
project(lang)

set(LLVM_CONFIG llvm-config CACHE STRING "llvm-config to use")
set(CLANG_FORMAT clang-format CACHE STRING "clang-format to use")

message(STATUS "LLVM_CONFIG: ${LLVM_CONFIG}")
message(STATUS "CLANG_FORMAT: ${CLANG_FORMAT}")

set(CXXFLAGS
    -Wall -Werror -std=c++20 -fno-rtti -fno-exceptions -g
    -ftrivial-auto-var-init=pattern -fsanitize=address -fsanitize=undefined -fPIC)
add_compile_options(${CXXFLAGS})
add_link_options(-fuse-ld=lld -Wl,--gc-sections ${CXXFLAGS})
include_directories(AFTER ${CMAKE_SOURCE_DIR}/)

execute_process(COMMAND ${LLVM_CONFIG} --cxxflags OUTPUT_VARIABLE LLVM_CONFIG_CXX_FLAGS
  OUTPUT_STRIP_TRAILING_WHITESPACE)
execute_process(COMMAND ${LLVM_CONFIG} --ldflags OUTPUT_VARIABLE LLVM_CONFIG_LD_FLAGS
  OUTPUT_STRIP_TRAILING_WHITESPACE)
execute_process(COMMAND ${LLVM_CONFIG} --system-libs OUTPUT_VARIABLE LLVM_CONFIG_SYSTEM_LIBS
  OUTPUT_STRIP_TRAILING_WHITESPACE)
execute_process(COMMAND ${LLVM_CONFIG} --libs core OUTPUT_VARIABLE LLVM_CONFIG_CORE_LIBS
  OUTPUT_STRIP_TRAILING_WHITESPACE)

separate_arguments(LLVM_CONFIG_CXX_FLAGS)
separate_arguments(LLVM_CONFIG_LD_FLAGS)
separate_arguments(LLVM_CONFIG_SYSTEM_LIBS)
separate_arguments(LLVM_CONFIG_CORE_LIBS)

message(STATUS "LLVM_CONFIG_CXX_FLAGS: ${LLVM_CONFIG_CXX_FLAGS}")
message(STATUS "LLVM_CONFIG_LD_FLAGS: ${LLVM_CONFIG_LD_FLAGS}")
message(STATUS "LLVM_CONFIG_SYSTEM_LIBS: ${LLVM_CONFIG_SYSTEM_LIBS}")
message(STATUS "LLVM_CONFIG_CORE_LIBS: ${LLVM_CONFIG_CORE_LIBS}")

add_library(lang_interface INTERFACE EXCLUDE_FROM_ALL lang.h)
add_library(argparse INTERFACE EXCLUDE_FROM_ALL argparse.h)
add_library(nodes INTERFACE EXCLUDE_FROM_ALL nodes.def)
add_library(types INTERFACE EXCLUDE_FROM_ALL types.def)

add_library(mangle OBJECT EXCLUDE_FROM_ALL mangle.cpp mangle.h)
target_compile_options(mangle BEFORE PRIVATE ${LLVM_CONFIG_CXX_FLAGS})

add_library(ast OBJECT EXCLUDE_FROM_ALL
  ast.h
  ast.cpp
  astbuilder.h
  astbuilder.cpp
  astcloner.h
  astcloner.cpp
  astdumper.h
  astdumper.cpp)
target_compile_options(ast BEFORE PRIVATE ${LLVM_CONFIG_CXX_FLAGS})
target_link_libraries(ast lang_interface nodes types mangle)

add_library(lexer OBJECT EXCLUDE_FROM_ALL lexer.cpp lexer.h)
target_link_libraries(lexer lang_interface)

add_library(parser OBJECT EXCLUDE_FROM_ALL parser.cpp parser.h)
target_compile_options(parser BEFORE PRIVATE ${LLVM_CONFIG_CXX_FLAGS})
target_link_libraries(parser lexer lang_interface)

add_library(compiler OBJECT EXCLUDE_FROM_ALL
  compiler.h
  compiler.cpp
  lower.cpp)
target_compile_options(compiler BEFORE PRIVATE ${LLVM_CONFIG_CXX_FLAGS})
target_link_libraries(compiler ast lang_interface mangle)

add_executable(lang lang.cpp)
target_compile_options(lang BEFORE PRIVATE ${LLVM_CONFIG_CXX_FLAGS})
target_link_libraries(lang
  lang_interface
  argparse
  ast
  lexer
  parser
  compiler
  mangle
  ${LLVM_CONFIG_LD_FLAGS}
  ${LLVM_CONFIG_SYSTEM_LIBS}
  ${LLVM_CONFIG_CORE_LIBS})

# STAGE 2 COMPILER
add_custom_command(OUTPUT lang-stage2
  COMMAND $<TARGET_FILE:lang> ${CMAKE_SOURCE_DIR}/examples/compiler.lang
  COMMAND ${CMAKE_C_COMPILER} ${CMAKE_SOURCE_DIR}/examples/compiler.lang.obj -o lang-stage2
    ${LLVM_CONFIG_LD_FLAGS}
    ${LLVM_CONFIG_SYSTEM_LIBS}
    ${LLVM_CONFIG_CORE_LIBS})
add_custom_target(generate-lang-stage2 ALL DEPENDS lang-stage2)


# TESTS
include(FetchContent)
FetchContent_Declare(
  googletest
  URL https://github.com/google/googletest/archive/03597a01ee50ed33e9dfd640b249b4be3799d395.zip
)
# For Windows: Prevent overriding the parent project's compiler/linker settings
set(gtest_force_shared_crt ON CACHE BOOL "" FORCE)
FetchContent_MakeAvailable(googletest)

add_executable(test test.cpp testlexer.cpp testargparse.cpp testparser.cpp)
target_compile_options(test BEFORE PRIVATE ${LLVM_CONFIG_CXX_FLAGS})
target_link_libraries(test
  lang_interface
  argparse
  ast
  lexer
  parser
  compiler
  mangle
  GTest::gtest_main
  ${LLVM_CONFIG_LD_FLAGS}
  ${LLVM_CONFIG_SYSTEM_LIBS}
  ${LLVM_CONFIG_CORE_LIBS})
target_compile_definitions(test PUBLIC
  EXAMPLES_DIR="${CMAKE_SOURCE_DIR}/examples/"
  COMPILER_PATH="${CMAKE_C_COMPILER}"
  LLVM_CONFIG="${LLVM_CONFIG}")
add_dependencies(test lang)

# FORMATTING
file(GLOB all_srcs CONFIGURE_DEPENDS "*.h" "*.cpp")
add_custom_target(format ${CLANG_FORMAT} --style=file -i ${all_srcs})
add_custom_target(format-check ${CLANG_FORMAT} --style=file ${all_srcs} --dry-run -Werror)
