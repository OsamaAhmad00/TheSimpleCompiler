<div align="center"> <img src="logo.png" alt="Logo" width=500px> </div>

# The Simple Compiler

This is an educational, single-file compiler for the "Simple" programming language, designed to demonstrate fundamental compiler concepts including lexical analysis, parsing, abstract syntax tree (AST) construction, and LLVM IR (Intermediate Representation) code generation. The compiler processes source files written in the Simple language, generates LLVM IR, and uses Clang to produce executable binaries. The entire implementation is contained in a single C++ file with less than 1000 lines of code when instructive comments and unnecessary code are removed, although it may appear longer due to detailed documentation for educational purposes.

## Overview

The Simple Compiler consists of:
- **Lexer**: Tokenizes input into keywords, identifiers, literals, and operators.
- **Parser**: Builds an AST using recursive descent parsing with operator precedence.
- **Code Generator**: Traverses the AST to produce LLVM IR, handling variables, functions, and control flow.
- **Compiler Driver**: Integrates with Clang to compile IR into executables.

The project is written in C++ and uses the LLVM infrastructure for code generation, making it portable across platforms supported by LLVM.

## Language Syntax

The Simple language is a minimal, imperative language with support for functions, variables, control flow, and basic I/O. Below is the complete syntax.

### Tokens
- **Keywords**: `if`, `else`, `elseif`, `while`, `goto`, `print`, `read`, `return`, `def`, `extern`, `label`, `let`, `true`, `false`.
- **Operators**:
  - Arithmetic: `+`, `-`, `*`, `/`, `%` (modulus).
  - Bitwise: `&` (and), `|` (or), `^` (xor), `~` (not), `<<` (shift left), `>>` (shift right).
  - Comparison: `>`, `<`, `<=`, `>=`, `==`, `!=`.
  - Assignment: `=`.
  - Delimiters: `(`, `)`, `{`, `}`, `;`, `:`, `,`.
- **Literals**:
  - Numbers: Integer literals (e.g., `42`, `-123`).
  - Strings: Double-quoted strings with escape sequences (`\n`, `\t`, `\"`, `\\`).
  - Booleans: `true` (evaluates to 1), `false` (evaluates to 0).
- **Identifiers**: Alphanumeric sequences starting with a letter (e.g., `x`, `my_var`).
- **Comments**: Single-line comments starting with `//`.

### Grammar
The following is a simplified grammar for the Simple language:

```bnf
program        ::= (function_def | function_decl | global_var)*
function_def   ::= 'def' IDENTIFIER '(' [IDENTIFIER (',' IDENTIFIER)*] ')' '=' statement
function_decl  ::= 'extern' IDENTIFIER '(' [IDENTIFIER (',' IDENTIFIER)*] ')' ';'
global_var     ::= 'let' IDENTIFIER ['=' expr] ';' | 'extern' IDENTIFIER ';'
statement      ::= print_stmt | if_stmt | while_stmt | goto_stmt | label_stmt | return_stmt | block_stmt | assign_stmt | expr_stmt
print_stmt     ::= 'print' '(' expr ')' ';'
if_stmt        ::= 'if' '(' expr ')' statement ['else' statement] | 'if' '(' expr ')' statement ('elseif' '(' expr ')' statement)* ['else' statement]
while_stmt     ::= 'while' '(' expr ')' statement
goto_stmt      ::= 'goto' IDENTIFIER ';'
label_stmt     ::= 'label' IDENTIFIER ':' statement
return_stmt    ::= 'return' expr ';'
block_stmt     ::= '{' statement* '}'
assign_stmt    ::= 'let' IDENTIFIER ['=' expr] ';'
expr_stmt      ::= expr ';'
expr           ::= NUMBER | STRING | 'true' | 'false' | 'read' '(' ')' | IDENTIFIER | IDENTIFIER '(' [expr (',' expr)*] ')' | '(' expr ')' | '~' expr | expr op expr
op             ::= '+' | '-' | '*' | '/' | '%' | '&' | '|' | '^' | '<<' | '>>' | '>' | '<' | '<=' | '>=' | '==' | '!='
```

### Supported Operations
- **Arithmetic**: Addition, subtraction, multiplication, division, modulus.
- **Bitwise Operations**: AND, OR, XOR, NOT, left shift, right shift.
- **Comparison**: Greater than, less than, less than or equal, greater than or equal, equal, not equal.
- **Control Flow**: Conditional branching (`if`, `elseif`, `else`), loops (`while`), jumps (`goto`, `label`).
- **Functions**: Definition and calls with parameters, external declarations (`extern`).
- **Variables**: Local variables (`let` in functions), global variables (`let` or `extern` at top-level).
- **I/O**: `print` for outputting numbers or strings, `read` for reading integers.

## Sample Program

Below is an example program demonstrating multiple files, `extern` functions and globals, and interoperability with C code.

### File 1: `utils.simple`
```simple
// Global variable definition
let counter = 0;

// Function to increment and return counter
def increment_counter() = {
    let counter = counter + 1;
    return counter;
}
```

### File 2: `main.simple`
```simple
// External global variable
extern counter;

// External functions
extern increment_counter();
extern strlen(i64); // From libc
extern print_message(i64); // Defined in C file

def main() = {
    print("Hello, world!");
    let message = "The Simple Compiler";
    print(strlen(message)); // Prints 19
    print_message(message); // Calls C function
    let count = increment_counter();
    print(count); // Prints 1
    let count = increment_counter();
    print(count); // Prints 2
    print(counter); // Prints 2
}
```

### File 3: `helpers.c`
```c
#include <stdio.h>

void print_message(long msg) {
    printf("Message from C: %s\n", (char*)msg);
}
```

**Expected Output**:
```
Hello, world!
19
Message from C: The Simple Compiler
1
2
2
```

### Compiling the Program
```bash
./TheSimpleCompiler utils.simple main.simple helpers.c -o example
./example
```

This example shows:
- `utils.simple` defines a global `counter` and a function `increment_counter`.
- `main.simple` uses `extern` to access `counter` and `increment_counter`, calls the libc function `strlen`, and a C function `print_message`.
- `helpers.c` provides a C implementation of `print_message`.
- The compiler processes both `.simple` files, generates LLVM IR, and links with `helpers.c` using Clang.

## Limitations

- **Data Types**: Only supports 64-bit integers (`i64`) and strings (as pointers). Variables can only hold numbers, so assigning a string to a variable stores its address. When such a variable is passed to `print`, the address (a number) is printed instead of the string content. The `print` function only recognizes strings when passed directly (e.g., `print("text")`). To mitigate this, use the `printf` function via `extern` for proper string handling.
- **Global Initialization**: Global variables can only be initialized with constant integers (not expressions or strings).
- **I/O**: Support for I/O is limited, internally calling `printf` and `scanf`. To overcome this limitation, you can call `printf` and `scanf` directly via `extern`.
- **Error Handling**: Basic error messages without line/column numbers, which can make debugging harder.
- **No Optimizations**: Generates straightforward LLVM IR without optimizations like constant folding or dead code elimination.
- **Function Return Types**: All functions return `i64`, and void functions return `0` by default.
- **No Nested Scopes**: Variables are either local to a function or global, with no block-level scoping.

## How to Extend the Compiler

The compiler is designed for extensibility. Here are some ways to enhance it:

1. **Add New Operators**:
   - Add a new `TokenType` (e.g., `LOGICAL_NOT` for `!`).
   - Update `Lexer::next_token` to recognize the operator.
   - Modify `Parser::parse_expr` to handle it as a unary or binary expression.
   - Implement code generation in `CodeGenerator::visit_unary_expr` or `visit_bin_expr`.

   **Example**: Adding logical NOT (`!`):
   ```cpp
   // In TokenType
   enum TokenType { ..., LOGICAL_NOT, ... };

   // In Lexer::next_token
   if (c == '!') {
       if (peek() == '=') { consume(); return {NE, "!="}; }
       return {LOGICAL_NOT, "!"};
   }

   // In Parser::parse_expr
   if (peek() == BIT_NOT || peek() == LOGICAL_NOT) {
       TokenType op = peek();
       consume(op);
       Expr* e = parse_expr();
       return new_node<UnaryExpr>(op, e);
   }

   // In CodeGenerator::visit_unary_expr
   case LOGICAL_NOT:
       code() << "  " << res << " = icmp eq i64 " << e << ", 0\n";
       code() << "  " << consume_reg() << " = zext i1 " << res << " to i64\n";
       break;
   ```

2. **Support New Types**:
   - Extend the AST with new expression nodes (e.g., `FloatExpr`).
   - Update the `Parser` to recognize new literals or keywords.
   - Modify the `CodeGenerator` to emit appropriate LLVM IR (e.g., `double` for floating-point).

3. **Add Line/Column Tracking**:
   - Add `line` and `column` fields to `Token`.
   - Update `Lexer` to track line/column during tokenization.
   - Enhance `Parser` to include position in error messages.

4. **Improve I/O**:
   - Add support for more escape sequences in strings (e.g., `\r`, `\"`).
   - Extend `print`/`read` to handle additional formats (e.g., floating-point).

5. **Add Optimizations**:
   - Implement an optimization pass (e.g., constant folding) between parsing and code generation.
   - Use LLVM’s optimization passes by passing flags to Clang (e.g., `-O2`).

6. **Support Arrays or Structs**:
   - Add new AST nodes for arrays or structs.
   - Update the `Parser` to handle array indexing or struct field access.
   - Generate LLVM IR for aggregate types using `alloca` and `getelementpointer`.

## Learning Compiler Construction

To continue extending this project, you need to understand compiler theory and the target you're compiling to. Key areas include:

- **Compiler Theory**: Study lexical analysis, parsing, semantic analysis, optimization, and code generation. The course [Compilers](https://www.edx.org/learn/computer-science/stanford-university-compilers) covers these topics.
- **Target Platform (LLVM IR)**: Since this compiler generates LLVM IR, learn its syntax, instructions, and type system. LLVM Code Generation by Quentin Colombet can be helpful.

## How to Build

### Prerequisites
- **C++ Compiler**: Clang++ with support for C++23.
- **Clang**: Version 15 or higher for compiling generated LLVM IR.

### Build Instructions
1. **Clone the Repository**:
   ```bash
   git clone https://github.com/OsamaAhmad00/TheSimpleCompiler.git
   cd TheSimpleCompiler
   ```

2. **Compile the Compiler**:
   ```bash
   clang++ main.cpp -std=c++23 -o TheSimpleCompiler
   ```

3. **Test the Compiler**:
   ```bash
   ./TheSimpleCompiler utils.simple main.simple helpers.c -o example
   ./example
   ```

### Notes
- The compiler expects input files with the `.simple` extension.
- Ensure Clang (version 15 or higher) is in your `PATH`.
- The compiler generates temporary `.bc` (LLVM IR) files in the system’s temp directory.

## Running the Compiler

```bash
./TheSimpleCompiler <input1.simple> [<input2.simple>...] [<other_files>] [clang options]
```

- `<input.simple>`: Source files in the Simple language.
- `<other_files>`: Additional `.simple` files, C files, or object files to link.
- `[clang options]`: Optional Clang flags (e.g., `-O2` for optimization, `-g` for debugging).

**Example**:
```bash
./TheSimpleCompiler utils.simple main.simple helpers.c -o example -O2
```

## Additional Information

- **LLVM Integration**: The use of LLVM IR allows targeting multiple architectures (e.g., x86, ARM) via Clang.
- **C Interoperability**: The compiler supports `extern` declarations for C functions (e.g., `strlen`) and globals, enabling seamless integration with C code.
- **Libc Linking**: No extra steps are needed to link with `libc` (e.g., for `strlen`, `printf`, `scanf`), as it is automatically linked thanks to LLVM.