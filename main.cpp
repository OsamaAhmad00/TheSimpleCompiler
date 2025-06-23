#include "Compiler.hpp"

int main(int argc, char* argv[]) {
    std::vector<std::string> args(argv, argv + argc);
    return ClangCompiler{ "clang", "simple" }.compile(args);
}