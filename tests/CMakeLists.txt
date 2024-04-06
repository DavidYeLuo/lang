include(FetchContent)
FetchContent_Declare(
  googletest
  URL https://github.com/google/googletest/archive/03597a01ee50ed33e9dfd640b249b4be3799d395.zip
)
# For Windows: Prevent overriding the parent project's compiler/linker settings
set(gtest_force_shared_crt ON CACHE BOOL "" FORCE)
FetchContent_MakeAvailable(googletest)

set(E2E_TEST_LINK_FLAGS "")

option(ADDRESS_SANITIZE_TESTS "Instrument E2E tests with AddressSanitizer")
if(${ADDRESS_SANITIZE_TESTS})
  set(E2E_TEST_LINK_FLAGS "${E2E_TEST_LINK_FLAGS} -fsanitize=address")
  message(STATUS "Address sanitizing tests")
endif()

add_executable(lang-tests
  teste2e.cpp
  testlexer.cpp
  testargparse.cpp
  testparser.cpp
  testcompiler.cpp)

target_compile_options(lang-tests BEFORE PRIVATE ${LLVM_CONFIG_CXX_FLAGS})

target_compile_definitions(lang-tests PUBLIC
  EXAMPLES_DIR="${CMAKE_SOURCE_DIR}/examples/"
  COMPILER_PATH="${CMAKE_C_COMPILER}"
  LLVM_CONFIG="${LLVM_CONFIG}"
  LANG_EXE="$<TARGET_FILE:lang>"
  COMPILER_LINK_FLAGS="${E2E_TEST_LINK_FLAGS}")

if(${ADDRESS_SANITIZE_TESTS})
  target_compile_definitions(lang-tests PUBLIC ADDRESS_SANITIZE_TESTS=1)
else()
  target_compile_definitions(lang-tests PUBLIC ADDRESS_SANITIZE_TESTS=0)
endif()

target_link_libraries(lang-tests
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

add_dependencies(lang-tests lang)
