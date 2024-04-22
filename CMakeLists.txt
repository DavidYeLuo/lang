cmake_minimum_required(VERSION 3.22)
project(lang)

set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin/)

set(LLVM_CONFIG llvm-config CACHE STRING "llvm-config to use")
set(CLANG_FORMAT clang-format CACHE STRING "clang-format to use")

message(STATUS "LLVM_CONFIG: ${LLVM_CONFIG}")
message(STATUS "CLANG_FORMAT: ${CLANG_FORMAT}")

# These will always come after -DCMAKE_C/CXX_FLAGS.
add_compile_options(
    # Always have warnings as errors.
    -Wall -Werror -Wconversion -Wno-sign-conversion

    # Always use c++20.
    -std=c++20

    # Never enable RTTI and exceptions.
    -fno-rtti -fno-exceptions

    # Always enable debuginfo.
    -g

    -fdata-sections -ffunction-sections

    # Always position independent.
    -fPIC)

if(CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
  add_compile_options(-Wno-sign-compare)
endif()

add_link_options(-Wl,--gc-sections)
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

# EXAMPLES BUILD
set(bin_example_dir ${CMAKE_BINARY_DIR}/bin/examples)
set(obj_example_dir ${CMAKE_BINARY_DIR}/bin/examples/obj)
file(MAKE_DIRECTORY ${bin_example_dir})
file(MAKE_DIRECTORY ${obj_example_dir})
# Find all lang files in the examples directory
file(GLOB example_sources CONFIGURE_DEPENDS "examples/*.lang")
# Generate build commands for each example
set(build_example_commands "")
foreach(example_source ${example_sources})
    # Get file name without file path and extension
    get_filename_component(example_name ${example_source} NAME_WE)
    # Generate object file
    add_custom_command(
        OUTPUT ${obj_example_dir}/${example_name}.obj
        COMMAND ${CMAKE_BINARY_DIR}/bin/lang ${example_source} 
                    -o ${obj_example_dir}/${example_name}.obj
        DEPENDS ${example_source}
    )
    # Generate executable
    add_custom_command(
        OUTPUT ${bin_example_dir}/${example_name}
        COMMAND ${CMAKE_C_COMPILER} ${obj_example_dir}/${example_name}.obj 
                    -o ${bin_example_dir}/${example_name}
        DEPENDS ${obj_example_dir}/${example_name}.obj
    )
    list(APPEND build_example_commands ${bin_example_dir}/${example_name})
endforeach()
add_custom_target(
    build_examples
    DEPENDS ${build_example_commands}
)
