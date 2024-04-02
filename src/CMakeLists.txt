add_library(lang_interface EXCLUDE_FROM_ALL
  ../include/lang.h
  diagnostics.cpp)
add_library(argparse INTERFACE EXCLUDE_FROM_ALL ../include/argparse.h)
add_library(nodes INTERFACE EXCLUDE_FROM_ALL ../include/nodes.def)
add_library(types INTERFACE EXCLUDE_FROM_ALL ../include/types.def)

add_library(mangle OBJECT EXCLUDE_FROM_ALL mangle.cpp ../include/mangle.h)
target_compile_options(mangle BEFORE PRIVATE ${LLVM_CONFIG_CXX_FLAGS})

add_library(ast OBJECT EXCLUDE_FROM_ALL
  ../include/ast.h
  ../include/astbuilder.h
  ../include/astcloner.h
  ../include/astdumper.h
  ast.cpp
  astbuilder.cpp
  astcloner.cpp
  astdumper.cpp)
target_compile_options(ast BEFORE PRIVATE ${LLVM_CONFIG_CXX_FLAGS})
target_link_libraries(ast lang_interface nodes types mangle)

add_library(lexer OBJECT EXCLUDE_FROM_ALL lexer.cpp ../include/lexer.h)
target_link_libraries(lexer lang_interface)

add_library(parser OBJECT EXCLUDE_FROM_ALL parser.cpp ../include/parser.h)
target_compile_options(parser BEFORE PRIVATE ${LLVM_CONFIG_CXX_FLAGS})
target_link_libraries(parser lexer lang_interface)

add_library(compiler OBJECT EXCLUDE_FROM_ALL
  ../include/compiler.h
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
