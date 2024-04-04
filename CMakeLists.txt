cmake_minimum_required(VERSION 3.22)
project(lang)

set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin/)

set(LLVM_CONFIG llvm-config CACHE STRING "llvm-config to use")
set(CLANG_FORMAT clang-format CACHE STRING "clang-format to use")

message(STATUS "LLVM_CONFIG: ${LLVM_CONFIG}")
message(STATUS "CLANG_FORMAT: ${CLANG_FORMAT}")

set(CXXFLAGS
    -Wall -Werror -std=c++20 -fno-rtti -fno-exceptions -g
    -ftrivial-auto-var-init=pattern -fsanitize=address -fsanitize=undefined -fPIC)
add_compile_options(${CXXFLAGS})
add_link_options(-fuse-ld=lld -Wl,--gc-sections ${CXXFLAGS})
include_directories(AFTER ${CMAKE_SOURCE_DIR}/include/)

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

add_subdirectory(src)


# STAGE 2 COMPILER
add_custom_command(OUTPUT lang-stage2
  COMMAND $<TARGET_FILE:lang> ${CMAKE_SOURCE_DIR}/examples/compiler.lang
  COMMAND ${CMAKE_C_COMPILER} ${CMAKE_SOURCE_DIR}/examples/compiler.lang.obj -o lang-stage2
    ${LLVM_CONFIG_LD_FLAGS}
    ${LLVM_CONFIG_SYSTEM_LIBS}
    ${LLVM_CONFIG_CORE_LIBS})
add_custom_target(generate-lang-stage2 ALL DEPENDS lang-stage2)


# TESTS
add_subdirectory(tests)


# FORMATTING
file(GLOB all_srcs CONFIGURE_DEPENDS "include/*.h" "src/*.cpp" "tests/*.cpp")
add_custom_target(format ${CLANG_FORMAT} --style=file -i ${all_srcs})
add_custom_target(format-check ${CLANG_FORMAT} --style=file ${all_srcs} --dry-run -Werror)
