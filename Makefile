CC = clang
CXX = clang++
CPPFLAGS = -Wall -Werror -std=c++20 -fno-rtti -fno-exceptions -g -ferror-limit=1 -ftrivial-auto-var-init=pattern -fsanitize=address -fsanitize=undefined $(EXTRA_CPPFLAGS)
LDFLAGS = -fuse-ld=lld -Wl,--gc-sections $(EXTRA_LDFLAGS)
LLVM_CONFIG ?= llvm-config
CLANG_FORMAT ?= clang-format
LLVM_CONFIG_CXX_FLAGS = $(shell $(LLVM_CONFIG) --cxxflags)
LLVM_CONFIG_LD_FLAGS = $(shell $(LLVM_CONFIG) --ldflags)
LLVM_CONFIG_SYSTEM_LIBS = $(shell $(LLVM_CONFIG) --system-libs)
LLVM_CONFIG_LIBS = $(shell $(LLVM_CONFIG) --libs core)

CPPFLAGS := $(LLVM_CONFIG_CXX_FLAGS) $(CPPFLAGS)
LDFLAGS := $(LLVM_CONFIG_LD_FLAGS) $(LLVM_CONFIG_SYSTEM_LIBS) $(LLVM_CONFIG_LIBS) $(LDFLAGS)

# Add new source files here.
SRCS = compiler.cpp parser.cpp lexer.cpp astdumper.cpp
TEST_SRCS = test.cpp testargparse.cpp
HDRS = lang.h nodes.def ast.h astbuilder.h compiler.h lexer.h parser.h astdumper.h argparse.h astvisitor.h

OBJS = $(SRCS:.cpp=.o)
TEST_OBJS = $(TEST_SRCS:.cpp=.o)
EXE = lang
EXE_STAGE2 = lang-stage2
TEST = test

.PHONY: all clean format format-checks

all: $(EXE) $(TEST)

$(EXE): $(OBJS) lang.o
	$(CXX) $(CPPFLAGS) $(LDFLAGS) $^ -o $@

# -I$(GTEST_HDR) isn't needed, but it doesn't hurt to have it here.
%.o: %.cpp $(HDRS)
	$(CXX) $(CPPFLAGS) -c $< -o $@ -I$(GTEST_HDR)

# This is run with:
#
#   $ make test GTEST_HDR=<path to include dir> GTEST_LIB=<path to lib dir>
#
$(TEST): $(TEST_OBJS) $(OBJS) $(EXE)
	$(CXX) $(CPPFLAGS) $(LDFLAGS) $(TEST_OBJS) $(OBJS) -o $@ -lgtest_main -lgtest -L$(GTEST_LIB) -I$(GTEST_HDR)

$(EXE_STAGE2): $(EXE) examples/compiler.lang
	./$(EXE) examples/compiler.lang
	$(CC) examples/compiler.lang.obj $(LLVM_CONFIG_LD_FLAGS) $(LLVM_CONFIG_SYSTEM_LIBS) $(LLVM_CONFIG_LIBS) -o $@

clean:
	rm -rf $(EXE) *.o $(TEST) *.out examples/*.obj

format:
	$(CLANG_FORMAT) --style=file -i $(HDRS) $(SRCS) $(TEST_SRCS) lang.cpp

format-check:
	$(CLANG_FORMAT) --style=file $(HDRS) $(SRCS) $(TEST_SRCS) lang.cpp --dry-run -Werror
