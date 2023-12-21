CC = clang  # version?
CXX = clang++
CPPFLAGS = -Wall -Werror -std=c++20 -fno-rtti -fno-exceptions -g $(EXTRA_CPPFLAGS) -ferror-limit=1 -ftrivial-auto-var-init=pattern -fsanitize=address -fsanitize=undefined
LDFLAGS = -fuse-ld=lld -Wl,--gc-sections $(EXTRA_LDFLAGS)
LLVM_CONFIG ?= llvm-config
LLVM_CONFIG_CXX_FLAGS = $(shell $(LLVM_CONFIG) --cxxflags)
LLVM_CONFIG_LD_FLAGS = $(shell $(LLVM_CONFIG) --ldflags)
LLVM_CONFIG_SYSTEM_LIBS = $(shell $(LLVM_CONFIG) --system-libs)
LLVM_CONFIG_LIBS = $(shell $(LLVM_CONFIG) --libs core)

CPPFLAGS := $(LLVM_CONFIG_CXX_FLAGS) $(CPPFLAGS)
LDFLAGS := $(LLVM_CONFIG_LD_FLAGS) $(LLVM_CONFIG_SYSTEM_LIBS) $(LLVM_CONFIG_LIBS) $(LDFLAGS)

# Add new source files here.
SRCS = compiler.cpp parser.cpp lexer.cpp astdumper.cpp
TEST_SRCS = test.cpp testargparse.cpp
HDRS = lang.h nodes.def ast.h astbuilder.h compiler.h lexer.h parser.h astdumper.h argparse.h

OBJS = $(SRCS:.cpp=.o)
EXE = lang
TEST = test

.PHONY: all clean format format-checks

all: $(EXE) $(TEST)

$(EXE): $(OBJS) lang.o
	$(CXX) $(CPPFLAGS) $(LDFLAGS) $^ -o $@

%.o: %.cpp $(HDRS)
	$(CXX) $(CPPFLAGS) -c $< -o $@

# This is run with:
#
#   $ make test GTEST_HDR=<path to include dir> GTEST_LIB=<path to lib dir>
#
$(TEST): $(TEST_SRCS) $(OBJS) $(EXE)
	$(CXX) $(CPPFLAGS) $(LDFLAGS) $(TEST_SRCS) $(OBJS) -o $@ -lgtest_main -lgtest -L$(GTEST_LIB) -I$(GTEST_HDR)

clean:
	rm -rf $(EXE) *.o $(TEST) *.out examples/*.obj

format:
	clang-format --style=google -i $(HDRS) $(SRCS) $(TEST_SRCS) lang.cpp

format-check:
	clang-format --style=google $(HDRS) $(SRCS) $(TEST_SRCS) lang.cpp --dry-run -Werror
