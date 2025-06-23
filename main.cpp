#include <cassert>
#include <iostream>
#include <string>
#include <utility>
#include <vector>
#include <cctype>
#include <filesystem>
#include <fstream>
#include <span>
#include <unordered_set>
#include <sstream>

enum TokenType { NUMBER, STRING, IDENTIFIER, IF, ELSE, ELSEIF, WHILE, GOTO, PRINT, READ, RETURN, DEF, EXTERN,
    PLUS, MINUS, MUL, DIV, MOD, BIT_AND, BIT_OR, BIT_XOR, BIT_NOT, SHL, SHR, LPAREN, RPAREN, LBRACE, RBRACE,
    SEMI, GT, LT, LE, GE, EQEQ, NE, EQ, COLON, COMMA, LABEL, LET, TRUE, FALSE, EOF_TOKEN };

struct Token {
    TokenType type { };
    std::string value { };
};

/**
 * Represents a lexical analyzer that tokenizes an input stream based on predefined rules.
 *
 * The `Lexer` class is responsible for reading character streams and producing tokens
 * according to its logic for whitespace, identifiers, literals, comments, and operators.
 * It provides utilities for peeking, consuming, and recognizing characters or symbols
 * within the input stream. 
 */
template <typename InStream>
class Lexer {
    InStream* _in;
    InStream& in() { return *_in; }

    char peek() {
        if (is_eof()) throw std::runtime_error("Unexpected end of file");
        return (char)in().peek();
    }

    // Quick fix: peek first to recognize EOF even without consumption
    bool is_eof() { in().peek(); return in().eof(); }

    char consume() {
        if (is_eof()) throw std::runtime_error("Unexpected end of file");
        return (char)in().get();
    }

    void consume(char c, bool or_eof = false) {
        if (is_eof()) {
            if (or_eof) return;
            throw std::runtime_error("Expected character " + std::string(1, c) + ", got end of file");
        }
        auto next = consume();
        if (next != c) {
            throw std::runtime_error("Expected character '" + std::string(1, c) + "', got " + std::string(1, next));
        }
    }

public:

    explicit Lexer(InStream& in) : _in(&in) { }

    /**
     * Extracts and returns the next token from the input stream.
     *
     * This function reads characters sequentially from the input stream
     * to construct valid tokens based on predefined lexicon rules. It handles:
     * - Whitespace: Skips over all whitespace characters.
     * - Comments: Processes and skips single-line comments starting with '//' and resumes parsing.
     * - Numbers: Parses numeric literals, including negative numbers.
     * - Strings: Parses string literals enclosed in double quotes, handling escape sequences.
     * - Identifiers and Keywords: Parses alphanumeric identifiers, distinguishing recognized keywords.
     * - Operators and Delimiters: Identifies operators (+, -, *, /, etc.), delimiters (parentheses, braces, etc.), and composite operators (e.g., ==, <=, >=, <<, >>).
     *
     * The function uses helper methods like `peek()` to inspect the next character
     * and `consume()` to advance the input stream. These methods ensure that EOF is handled safely.
     *
     * @return A `Token` object representing the next token extracted from the input stream.
     */
    Token next_token() {
        // In the following checks, if the next char can't be EOF, don't check for EOF. Use
        // peek() or consume() directly, they check for EOF and throw an exception if encountered.

        while (!is_eof() && std::isspace(peek())) consume();

        if (is_eof()) return {EOF_TOKEN, ""};

        auto c = consume();

        if (c == '/') {
            if (peek() == '/') {
                while (!is_eof() && peek() != '\n') consume();
                consume('\n', true);
                return next_token();
            }
            return {DIV, "/"};
        }

        if (c == '-' || std::isdigit(c)) {
            if (c == '-' && !std::isdigit(peek())) {
                return {MINUS, "-"};
            }
            std::string num(1, c);
            while (std::isdigit(peek())) {
                num += consume();
            }
            return {NUMBER, num};
        }

        if (c == '"') {
            std::string str;
            while (peek() != '"') {
                if (peek() == '\\') {
                    // String Escaping
                    consume();
                    char next = consume();
                    // You can add more here
                    if (next == 'n') str += '\n';
                    else if (next == 't') str += '\t';
                    else str += next;
                } else {
                    str += consume();
                }
            }
            consume('"');
            return {STRING, str};
        }

        if (std::isalpha(c)) {
            std::string id(1, c);
            while (std::isalnum(peek()) || peek() == '_') {
                id += consume();
            }
            if (id == "if") return {IF, id};
            if (id == "else") return {ELSE, id};
            if (id == "elseif") return {ELSEIF, id};
            if (id == "while") return {WHILE, id};
            if (id == "goto") return {GOTO, id};
            if (id == "print") return {PRINT, id};
            if (id == "read") return {READ, id};
            if (id == "return") return {RETURN, id};
            if (id == "def") return {DEF, id};
            if (id == "extern") return {EXTERN, id};
            if (id == "label") return {LABEL, id};
            if (id == "let") return {LET, id};
            if (id == "true") return {TRUE, id};
            if (id == "false") return {FALSE, id};
            return {IDENTIFIER, id};
        }

        if (c == '+') { return {PLUS, "+"}; }
        // if (c == '-') { return {MINUS, "-"}; }  // This case is handled above
        if (c == '*') { return {MUL, "*"}; }
        if (c == '/') { return {DIV, "/"}; }
        if (c == '%') { return {MOD, "%"}; }
        if (c == '&') { return {BIT_AND, "&"}; }
        if (c == '|') { return {BIT_OR, "|"}; }
        if (c == '^') { return {BIT_XOR, "^"}; }
        if (c == '~') { return {BIT_NOT, "~"}; }
        if (c == '(') { return {LPAREN, "("}; }
        if (c == ')') { return {RPAREN, ")"}; }
        if (c == '{') { return {LBRACE, "{"}; }
        if (c == '}') { return {RBRACE, "}"}; }
        if (c == ';') { return {SEMI, ";"}; }
        if (c == ':') { return {COLON, ":"}; }
        if (c == ',') { return {COMMA, ","}; }

        if (c == '<') {
            if (peek() == '<') { consume(); return {SHL, "<<"}; }
            if (peek() == '=') { consume(); return {LE, "<="}; }
            return {LT, "<"};
        }

        if (c == '>') {
            if (peek() == '>') { consume(); return {SHR, ">>"}; }
            if (peek() == '=') { consume(); return {GE, ">="}; }
            return {GT, ">"};
        }

        if (c == '=') {
            if (peek() == '=') { consume(); return {EQEQ, "=="}; }
            return {EQ, "="};
        }

        if (c == '!') {
            if (peek() == '=') { consume(); return {NE, "!="}; }
            return {EOF_TOKEN, ""};
        }

        return {EOF_TOKEN, ""};
    }
};

struct ASTVisitor;

struct ASTNode {
    virtual ~ASTNode() = default;
    virtual std::string accept(ASTVisitor& visitor);
};

struct Expr : ASTNode {
    std::string accept(ASTVisitor& visitor) override;
};

struct Statement : ASTNode {
    std::string accept(ASTVisitor& visitor) override;
};

struct FuncDef : ASTNode {
    std::string name;
    std::vector<std::string> params;
    Statement* body;
    FuncDef(std::string n, const std::vector<std::string>& p, Statement* b)
        : name(std::move(n)), params(p), body(b) { }
    std::string accept(ASTVisitor& visitor) override;
};

struct FuncDecl : ASTNode {
    std::string name;
    std::vector<std::string> params;
    FuncDecl(std::string n, const std::vector<std::string>& p) : name(std::move(n)), params(p) { }
    std::string accept(ASTVisitor& visitor) override;
};

struct BinaryExpr : Expr {
    TokenType op;
    Expr* left;
    Expr* right;
    BinaryExpr(TokenType o, Expr* l, Expr* r) : op(o), left(l), right(r) { }
    std::string accept(ASTVisitor& visitor) override;
};

struct UnaryExpr : Expr {
    TokenType op;
    Expr* expr;
    UnaryExpr(TokenType o, Expr* e) : op(o), expr(e) { }
    std::string accept(ASTVisitor& visitor) override;
};

struct BooleanExpr : Expr {
    Expr* expr;
    explicit BooleanExpr(Expr* e) : expr(e) { }
    std::string accept(ASTVisitor& visitor) override;
};

struct CallExpr : Expr {
    std::string func_name;
    std::vector<Expr*> args;
    CallExpr(std::string fn, const std::vector<Expr*>& a) : func_name(std::move(fn)), args(a) { }
    std::string accept(ASTVisitor& visitor) override;
};

struct ReadExpr : Expr {
    std::string accept(ASTVisitor& visitor) override;
};

struct VarExpr : Expr {
    std::string name;
    explicit VarExpr(std::string n) : name(std::move(n)) { }
    std::string accept(ASTVisitor& visitor) override;
};

struct NumberExpr : Expr {
    int64_t value;
    explicit NumberExpr(const int64_t v) : value(v) { }
    std::string accept(ASTVisitor& visitor) override;
};

struct StringExpr : Expr {
    std::string value;
    explicit StringExpr(std::string v) : value(std::move(v)) { }
    std::string accept(ASTVisitor& visitor) override;
};

struct BlockStmt : Statement {
    std::vector<Statement*> statements;
    explicit BlockStmt(std::vector<Statement*> s) : statements(std::move(s)) { }
    std::string accept(ASTVisitor& visitor) override;
};

struct ReturnStmt : Statement {
    Expr* expr;
    explicit ReturnStmt(Expr* e) : expr(e) { }
    std::string accept(ASTVisitor& visitor) override;
};

struct PrintStmt : Statement {
    Expr* expr;
    explicit PrintStmt(Expr* e) : expr(e) { }
    std::string accept(ASTVisitor& visitor) override;
};

struct AssignStmt : Statement {
    std::string name;
    Expr* expr;
    AssignStmt(std::string v, Expr* e) : name(std::move(v)), expr(e) { }
    std::string accept(ASTVisitor& visitor) override;
};

struct GlobalVarDecl : Statement {
    std::string name;
    Expr* init;
    GlobalVarDecl(std::string n, Expr* i) : name(std::move(n)), init(i) { }
    std::string accept(ASTVisitor& visitor) override;
};

struct ExternGlobalDecl : Statement {
    std::string name;
    explicit ExternGlobalDecl(std::string n) : name(std::move(n)) { }
    std::string accept(ASTVisitor& visitor) override;
};

struct IfStmt : Statement {
    Expr* cond;
    Statement* then_body;
    std::vector<std::pair<Expr*, Statement*>> elseif_branches;
    Statement* else_body;
    IfStmt(Expr* c, Statement* t, const std::vector<std::pair<Expr*, Statement*>>& ei, Statement* e)
        : cond(c), then_body(t), elseif_branches(ei), else_body(e) { }
    std::string accept(ASTVisitor& visitor) override;
};

struct WhileStmt : Statement {
    Expr* cond;
    Statement* body;
    WhileStmt(Expr* c, Statement* b) : cond(c), body(b) { }
    std::string accept(ASTVisitor& visitor) override;
};

struct GotoStmt : Statement {
    std::string label;
    explicit GotoStmt(std::string l) : label(std::move(l)) { }
    std::string accept(ASTVisitor& visitor) override;
};

struct LabelStmt : Statement {
    std::string name;
    Statement* stmt;
    LabelStmt(std::string n, Statement* s) : name(std::move(n)), stmt(s) { }
    std::string accept(ASTVisitor& visitor) override;
};

struct ExprStatement : Statement {
    Expr* expr;
    explicit ExprStatement(Expr* e) : expr(e) { }
    std::string accept(ASTVisitor& visitor) override;
};

/**
 * The ASTVisitor class implements the visitor design pattern for traversing and operating on AST nodes.
 * 
 * This pattern provides a flexible way to perform operations on AST nodes without modifying their classes.
 * Instead of embedding operation-specific code within node classes (like code generation, type checking,
 * or optimization), we can create separate visitor classes for each operation.
 *
 * A key advantage of this pattern is that visitors can maintain their own state (like symbol tables,
 * type information, or generated code) without polluting the AST nodes with operation-specific data.
 * For example, the CodeGenerator visitor maintains state about registers and variables, while a
 * visitor that counts AST nodes can just contain a counter.
 *
 * The class provides default implementations for all visit methods that delegate to their parent's
 * implementation. This means you only need to override the methods relevant to your specific operation.
 * For example, a node-counting visitor might only override visit_node(), while a code generator
 * would need separate implementations for each node type.
 *
 * Benefits of this approach:
 * - Separation of concerns: Each visitor handles one specific operation
 * - State management: Visitors can maintain operation-specific state without modifying AST nodes
 * - Extensibility: New operations can be added by creating new visitors without modifying AST nodes
 * - Type safety: The visitor pattern ensures proper handling of each node type
 * - Maintainability: Operations are centralized in visitor classes rather than scattered across node classes
 *
 * To implement a new operation on the AST:
 * 1. Create a new class inheriting from ASTVisitor
 * 2. Add any operation-specific state as member variables
 * 3. Override the relevant visit methods for the nodes you want to process
 * 4. Call accept() on the root node with your visitor
 */
struct ASTVisitor {
    virtual std::string visit_node(ASTNode& node) { (void)node; return ""; }

    virtual std::string visit_def(FuncDef& node) { return visit_node(node); }
    virtual std::string visit_decl(FuncDecl& node) { return visit_node(node); }

    [[nodiscard]] virtual std::string visit_expr(Expr& node) { (void)node; assert(false && "This method shouldn't be reached, or should be overridden"); }
    [[nodiscard]] virtual std::string visit_bin_expr(BinaryExpr& node) { return visit_expr(node); }
    [[nodiscard]] virtual std::string visit_unary_expr(UnaryExpr& node) { return visit_expr(node); }
    [[nodiscard]] virtual std::string visit_bool_expr(BooleanExpr& node) { return visit_expr(node); }
    [[nodiscard]] virtual std::string visit_call(CallExpr& node) { return visit_expr(node); }
    [[nodiscard]] virtual std::string visit_read(ReadExpr& node) { return visit_expr(node); }
    [[nodiscard]] virtual std::string visit_var(VarExpr& node) { return visit_expr(node); }
    [[nodiscard]] virtual std::string visit_num(NumberExpr& node) { return visit_expr(node); }
    [[nodiscard]] virtual std::string visit_string(StringExpr& node) { return visit_expr(node); }

    virtual std::string visit_statement(Statement& node) { return visit_node(node); }
    virtual std::string visit_block(BlockStmt& node) { return visit_statement(node); }
    virtual std::string visit_return(ReturnStmt& node) { return visit_statement(node); }
    virtual std::string visit_print(PrintStmt& node) { return visit_statement(node); }
    virtual std::string visit_global(GlobalVarDecl& node) { return visit_statement(node); }
    virtual std::string visit_extern_global(ExternGlobalDecl& node) { return visit_statement(node); }
    virtual std::string visit_assign(AssignStmt& node) { return visit_statement(node); }
    virtual std::string visit_if(IfStmt& node) { return visit_statement(node); }
    virtual std::string visit_while(WhileStmt& node) { return visit_statement(node); }
    virtual std::string visit_goto(GotoStmt& node) { return visit_statement(node); }
    virtual std::string visit_label(LabelStmt& node) { return visit_statement(node); }
    virtual std::string visit_expr_statement(ExprStatement& node) { return visit_statement(node); }

    virtual ~ASTVisitor() = default;
};

std::string ASTNode::accept(ASTVisitor &visitor) { return visitor.visit_node(*this); }
std::string Expr::accept(ASTVisitor &visitor) { return visitor.visit_expr(*this); }
std::string BinaryExpr::accept(ASTVisitor &visitor) { return visitor.visit_bin_expr(*this); }
std::string UnaryExpr::accept(ASTVisitor &visitor) { return visitor.visit_unary_expr(*this); }
std::string BooleanExpr::accept(ASTVisitor &visitor) { return visitor.visit_bool_expr(*this); }
std::string CallExpr::accept(ASTVisitor &visitor) { return visitor.visit_call(*this); }
std::string ReadExpr::accept(ASTVisitor &visitor) { return visitor.visit_read(*this); }
std::string VarExpr::accept(ASTVisitor &visitor) { return visitor.visit_var(*this); }
std::string NumberExpr::accept(ASTVisitor &visitor) { return visitor.visit_num(*this); }
std::string StringExpr::accept(ASTVisitor &visitor) { return visitor.visit_string(*this); }
std::string Statement::accept(ASTVisitor &visitor) { return visitor.visit_statement(*this); }
std::string BlockStmt::accept(ASTVisitor &visitor) { return visitor.visit_block(*this); }
std::string ReturnStmt::accept(ASTVisitor &visitor) { return visitor.visit_return(*this); }
std::string PrintStmt::accept(ASTVisitor &visitor) { return visitor.visit_print(*this); }
std::string GlobalVarDecl::accept(ASTVisitor &visitor) { return visitor.visit_global(*this); }
std::string ExternGlobalDecl::accept(ASTVisitor &visitor) { return visitor.visit_extern_global(*this); }
std::string AssignStmt::accept(ASTVisitor &visitor) { return visitor.visit_assign(*this); }
std::string IfStmt::accept(ASTVisitor &visitor) { return visitor.visit_if(*this); }
std::string WhileStmt::accept(ASTVisitor &visitor) { return visitor.visit_while(*this); }
std::string GotoStmt::accept(ASTVisitor &visitor) { return visitor.visit_goto(*this); }
std::string LabelStmt::accept(ASTVisitor &visitor) { return visitor.visit_label(*this); }
std::string ExprStatement::accept(ASTVisitor &visitor) { return visitor.visit_expr_statement(*this); }
std::string FuncDef::accept(ASTVisitor &visitor) { return visitor.visit_def(*this); }
std::string FuncDecl::accept(ASTVisitor &visitor) { return visitor.visit_decl(*this); }

struct IntermediateCode {
    std::vector<ASTNode*> globals;  // Can contain extern globals
    std::vector<FuncDecl*> declarations;
    std::vector<FuncDef*> functions;
    std::vector<ASTNode*> all_nodes;

    IntermediateCode() = default;
    IntermediateCode(const IntermediateCode&) = delete;
    IntermediateCode& operator=(const IntermediateCode&) = delete;
    IntermediateCode(IntermediateCode&&) = default;
    IntermediateCode& operator=(IntermediateCode&&) = default;

    ~IntermediateCode() {
        for (auto node : all_nodes) {
            delete node;
        }
    }
};

/**
 * The Parser class provides functionality for parsing an input token stream
 * into an abstract syntax tree (AST) representation. It processes tokens,
 * constructs nodes for expressions and statements, and handles various
 * language-specific syntax rules.
 */
template <typename InStream>
class Parser {
    Lexer<InStream> lexer;
    Token current { };
    IntermediateCode intermediate_code;

    template <typename T, typename... Args>
    T* new_node(Args&&... args) {
        auto* node = new T(std::forward<Args>(args)...);
        intermediate_code.all_nodes.push_back(node);
        return node;
    }

public:

    explicit Parser(InStream& in) : lexer(in), current(lexer.next_token()) { }

    void consume(const TokenType type) {
        if (peek() == type) current = lexer.next_token();
        else throw std::runtime_error("Unexpected token: " + current.value);
    }

    [[nodiscard]] TokenType peek() const { return current.type; }

    /**
     * Parses an expression from the current token stream and returns the corresponding expression node.
     *
     * This function identifies and processes various types of expressions:
     * - Unary expressions: Handles bitwise NOT operator (~).
     * - Numbers: Parses numeric literals and creates a corresponding `NumberExpr` node.
     * - Strings: Parses string literals and creates a `StringExpr` node.
     * - Boolean values: Parses `TRUE` and `FALSE` tokens and creates a `NumberExpr` with values 1 or 0, respectively.
     * - READ expressions: Parses function-like `READ()` and generates a `ReadExpr` node.
     * - Identifiers: Processes variable identifiers or function calls with arguments and creates appropriate `VarExpr` or `CallExpr` nodes.
     * - Parenthesized expressions: Handles parentheses and delegates parsing to `parse_expr_with_precedence`.
     *
     * The function assumes a valid token stream and relies heavily on helper methods like `consume()` to
     * advance tokens and `new_node<T>()` to create the appropriate AST nodes.
     *
     * @return A pointer to the root of the parsed expression node (`Expr`).
     * @throws std::runtime_error If an unrecognized or invalid token is encountered during parsing.
     */
    Expr* parse_expr() {
        if (peek() == BIT_NOT) {
            consume(BIT_NOT);
            Expr* e = parse_expr();
            return new_node<UnaryExpr>(BIT_NOT, e);
        }
        
        if (peek() == NUMBER) {
            const int64_t val = std::stoll(current.value);
            consume(NUMBER);
            return new_node<NumberExpr>(val);
        }

        if (peek() == STRING) {
            std::string val = current.value;
            consume(STRING);
            return new_node<StringExpr>(val);
        }

        if (peek() == TRUE) {
            consume(TRUE);
            return new_node<NumberExpr>(1);
        }

        if (peek() == FALSE) {
            consume(FALSE);
            return new_node<NumberExpr>(0);
        }

        if (peek() == READ) {
            consume(READ);
            consume(LPAREN);
            consume(RPAREN);
            return new_node<ReadExpr>();
        }

        if (peek() == IDENTIFIER) {
            std::string name = current.value;
            consume(IDENTIFIER);
            if (peek() == LPAREN) {
                consume(LPAREN);
                std::vector<Expr*> args;
                if (peek() != RPAREN) {
                    args.push_back(parse_expr_with_precedence());
                    while (peek() == COMMA) {
                        consume(COMMA);
                        args.push_back(parse_expr_with_precedence());
                    }
                }
                consume(RPAREN);
                return new_node<CallExpr>(name, args);
            }
            return new_node<VarExpr>(name);
        }

        if (peek() == LPAREN) {
            consume(LPAREN);
            Expr* e = parse_expr_with_precedence();
            consume(RPAREN);
            return e;
        }

        throw std::runtime_error("Invalid expression: " + current.value);
    }


    /**
     * Parses an expression with operator precedence from the current token stream.
     *
     * This function processes a left-associative expression by recursively handling operators
     * and their precedence levels, ensuring proper evaluation order. It constructs and returns
     * an abstract syntax tree (AST) node representing the expression.
     *
     * The function performs the following:
     * - Parses the left-hand side by delegating to `parse_expr()`.
     * - Determines the operator's precedence based on the current token, classifying operators into tiers:
     *   - Multiplication (`MUL`), division (`DIV`), and modulus (`MOD`) have the highest precedence.
     *   - Bitwise shift left (`SHL`) and shift right (`SHR`) follow.
     *   - Addition (`PLUS`) and subtraction (`MINUS`) are next.
     *   - Bitwise AND, OR, and XOR have decreasing precedence.
     *   - Relational operators like greater than (`GT`), less than (`LT`), less than or equal (`LE`),
     *     greater than or equal (`GE`), equal to (`EQEQ`), and not equal (`NE`) have the lowest precedence.
     * - Compares the newly computed precedence with the current precedence.
     * - If the operator's precedence is higher than the provided `precedence`, it consumes the operator,
     *   parses the right-hand side recursively, and constructs a `BinaryExpr` node.
     * - Continues until an operator with lesser or equal precedence is encountered.
     *
     * @param precedence The minimum precedence level required for processing operators.
     * @return A pointer to the root of the parsed expression subtree (`Expr`).
     * @throws std::runtime_error If an invalid token is encountered during parsing.
     */
    Expr* parse_expr_with_precedence(const int precedence = 0) {
        Expr* left = parse_expr();
        while (true) {
            TokenType op = peek();
            const int new_precedence =
                (op == MUL || op == DIV || op == MOD)                                    ? 6 :
                (op == SHL || op == SHR)                                                 ? 5 :
                (op == PLUS || op == MINUS)                                              ? 4 :
                (op == BIT_AND)                                                          ? 3 :
                (op == BIT_OR || op == BIT_XOR)                                          ? 2 :
                (op == GT || op == LT || op == LE || op == GE || op == EQEQ || op == NE) ? 1 : 0;
            if (new_precedence <= precedence) break;
            consume(op);
            Expr* right = parse_expr_with_precedence(new_precedence);
            left = new_node<BinaryExpr>(op, left, right);
        }
        return left;
    }

    template <typename T>
    T* parse_let() {
        consume(LET);
        std::string name = current.value;
        consume(IDENTIFIER);
        Expr* init = nullptr;
        if (peek() == EQ) {
            consume(EQ);
            init = parse_expr_with_precedence();
        }
        consume(SEMI);
        return new_node<T>(name, init);
    }

    /**
     * Parses a statement from the input stream and constructs a corresponding syntax tree node.
     *
     * This function provides support for parsing various types of statements by analyzing
     * the input stream and branching based on detected keywords or syntax patterns.
     * Supported statement types include:
     * - Print statements (`PRINT`): Parses and constructs a print statement.
     * - Conditional blocks (`IF` with `ELSEIF`/`ELSE`): Handles conditional logic with optional branches.
     * - Loops (`WHILE`): Parses while-loop constructs.
     * - Unconditional jumps (`GOTO`): Parses `GOTO` statements with label references.
     * - Label declarations (`LABEL`): Parses labeled statement blocks.
     * - Return statements (`RETURN`): Parses return expressions.
     * - Block statements (enclosed by braces `{}`): Parses groups of statements.
     * - Variable assignments (`LET`): Parses `LET` variable assignments.
     * - Expression statements: Parses standalone expressions as valid statements.
     *
     * The function relies on helper operations such as `peek()`, `consume()`,
     * and `parse_expr_with_precedence()` to analyze tokens, update the input stream,
     * and construct the appropriate statement node.
     *
     * @return A `Statement*` object representing the parsed statement.
     */
    Statement* parse_statement() {
        if (peek() == PRINT) {
            consume(PRINT);
            consume(LPAREN);
            Expr* e = parse_expr_with_precedence();
            consume(RPAREN);
            consume(SEMI);
            return new_node<PrintStmt>(e);
        }

        if (peek() == IF) {
            consume(IF);
            consume(LPAREN);
            Expr* cond = new_node<BooleanExpr>(parse_expr_with_precedence());
            consume(RPAREN);
            Statement* then_body = parse_statement();
            std::vector<std::pair<Expr*, Statement*>> elseif_branches;
            while (peek() == ELSEIF) {
                consume(ELSEIF);
                consume(LPAREN);
                Expr* elseif_cond = new_node<BooleanExpr>(parse_expr_with_precedence());
                consume(RPAREN);
                Statement* elseif_body = parse_statement();
                elseif_branches.emplace_back(elseif_cond, elseif_body);
            }
            Statement* else_body = nullptr;
            if (peek() == ELSE) {
                consume(ELSE);
                else_body = parse_statement();
            }
            return new_node<IfStmt>(cond, then_body, elseif_branches, else_body);
        }

        if (peek() == WHILE) {
            consume(WHILE);
            consume(LPAREN);
            Expr* cond = new_node<BooleanExpr>(parse_expr_with_precedence());
            consume(RPAREN);
            Statement* body = parse_statement();
            return new_node<WhileStmt>(cond, body);
        }

        if (peek() == GOTO) {
            consume(GOTO);
            std::string label = current.value;
            consume(IDENTIFIER);
            consume(SEMI);
            return new_node<GotoStmt>(label);
        }

        if (peek() == LABEL) {
            consume(LABEL);
            std::string label = current.value;
            consume(IDENTIFIER);
            consume(COLON);
            Statement* stmt = parse_statement();
            return new_node<LabelStmt>(label, stmt);
        }

        if (peek() == RETURN) {
            consume(RETURN);
            Expr* e = parse_expr_with_precedence();
            consume(SEMI);
            return new_node<ReturnStmt>(e);
        }

        if (peek() == LBRACE) {
            consume(LBRACE);
            std::vector<Statement*> statements;
            while (peek() != RBRACE && peek() != EOF_TOKEN) {
                statements.push_back(parse_statement());
            }
            consume(RBRACE);
            return new_node<BlockStmt>(statements);
        }

        if (peek() == LET) {
            return parse_let<AssignStmt>();
        }

        auto expr_statement = new_node<ExprStatement>(parse_expr_with_precedence());
        consume(SEMI);
        return expr_statement;
    }

    std::vector<std::string> parse_func_common() {
        consume(LPAREN);
        std::vector<std::string> params;
        if (peek() == IDENTIFIER) {
            params.push_back(current.value);
            consume(IDENTIFIER);
            while (peek() == COMMA) {
                consume(COMMA);
                params.push_back(current.value);
                consume(IDENTIFIER);
            }
        }
        consume(RPAREN);
        return params;
    }

    FuncDef* parse_func_def() {
        consume(DEF);
        const std::string name = current.value;
        consume(IDENTIFIER);
        const auto params = parse_func_common();
        consume(EQ);
        Statement* body = parse_statement();
        return new_node<FuncDef>(name, params, body);
    }

    FuncDecl* parse_func_decl(const std::string& name) {
        const auto params = parse_func_common();
        consume(SEMI);
        return new_node<FuncDecl>(name, params);
    }

    ExternGlobalDecl* parse_extern_global(const std::string& name) {
        consume(SEMI);
        return new_node<ExternGlobalDecl>(name);
    }

    IntermediateCode parse() {
        while (peek() != EOF_TOKEN) {
            if (peek() == DEF) {
                intermediate_code.functions.push_back(parse_func_def());
            } else if (peek() == EXTERN) {
                consume(EXTERN);
                auto name = current.value;
                consume(IDENTIFIER);
                if (peek() == SEMI) {
                    intermediate_code.globals.push_back(parse_extern_global(name));
                } else {
                    intermediate_code.declarations.push_back(parse_func_decl(name));
                }
            } else if (peek() == LET) {
                intermediate_code.globals.push_back(parse_let<GlobalVarDecl>());
            } else {
                throw std::runtime_error("Invalid top-level construct: " + current.value);
            }
        }
        return std::move(intermediate_code);
    }
};

/**
 * A class responsible for generating LLVM IR (Intermediate Representation)
 * code by traversing and processing an abstract syntax tree (AST).
 *
 * This class implements a visitor pattern to handle different types of AST
 * nodes and produces corresponding LLVM IR code. The generated code consists
 * of two main sections:
 * - Header: Declarations and metadata required at the start of the IR file.
 * - Code: Definitions of functions and statements.
 *
 * Internally, the class maintains counters for registers, labels, and string
 * identifiers to ensure unique names during code generation. It also keeps
 * track of defined variables within the current function scope and global
 * variables for external linkage.
 */
template <typename OutStream>
class CodeGenerator : public ASTVisitor {
    // The final output stream
    OutStream* out;

    // The generated LLVM bitcode consists of two main parts: the header, which includes
    // declarations and metadata, and the code section, which defines the actual functions
    std::ostringstream _header;
    std::ostringstream _code;

    std::unordered_set<std::string> defined_vars = { };
    std::unordered_set<std::string> global_vars = { };
    std::string func_name;
    unsigned long long reg_count = 0;
    unsigned long long str_count = 0;
    unsigned long long label_count = 0;

    std::ostringstream& header() { return _header; }
    std::ostringstream& code() { return _code; }

    void reset() {
        reg_count = 0;
        defined_vars.clear();
    }

    std::string consume_reg() { return "%reg" + std::to_string(reg_count++); }

    std::string consume_str_id() { return "@.str." + std::to_string(str_count++); }

    std::string consume_label(const std::string &name = "label") { return name + "." + std::to_string(label_count++); }

    std::string get_label_name(const std::string& name) { return name + "." + func_name; }

    void define_var(const std::string& name) {
        bool is_new = defined_vars.insert(name).second;
        if (is_new) {
            code() << "  %" << name << " = alloca i64\n";
        }
    }

    void write_list(const std::vector<std::string>& list, std::string prefix, std::ostringstream& stream) {
        prefix = "i64 " + prefix;
        if (!list.empty()) {
            stream << prefix << list[0];
            for (size_t i = 1; i < list.size(); ++i) {
                stream << ", " << prefix << list[i];
            }
        }
    }

public:

    explicit CodeGenerator(OutStream& _out) : out(&_out) { }

    std::string visit_def(FuncDef& node) override {
        reset();
        func_name = node.name;
        code() << "define i64 @" << node.name << "(";
        write_list(node.params, "%param.", code());
        code() << ") {\n";
        code() << "entry:\n";
        for (const auto& param : node.params) {
            define_var(param);
            code() << "  store i64 %param." << param << ", ptr %" << param << "\n";
        }
        if (auto expr_statement = dynamic_cast<ExprStatement*>(node.body); expr_statement != nullptr) {
            // Statements don't return a value, but this is a special case,
            // where we should return the expression in the expression statement
            ReturnStmt ret { expr_statement->expr };
            ret.accept(*this);
        } else {
            node.body->accept(*this);
        }
        code() << "  ret i64 0\n";
        code() << "}\n\n";
        return "";
    }

    std::string visit_decl(FuncDecl& node) override {
        header() << "declare i64 @" << node.name << "(";
        write_list(node.params, "%", header());
        header() << ")\n";
        return "";
    }

    std::string visit_bin_expr(BinaryExpr& node) override {
        std::string l = node.left->accept(*this);
        std::string r = node.right->accept(*this);
        std::string inst;
        bool is_boolean = false;
        switch (node.op) {
            case PLUS: inst = "add"; break;
            case MINUS: inst = "sub"; break;
            case MUL: inst = "mul"; break;
            case DIV: inst = "sdiv"; break;
            case MOD: inst = "srem"; break;
            case BIT_AND: inst = "and"; break;
            case BIT_OR: inst = "or"; break;
            case BIT_XOR: inst = "xor"; break;
            case SHL: inst = "shl"; break;
            case SHR: inst = "ashr"; break;
            case GT: inst = "icmp sgt"; is_boolean = true; break;
            case LT: inst = "icmp slt"; is_boolean = true; break;
            case LE: inst = "icmp sle"; is_boolean = true; break;
            case GE: inst = "icmp sge"; is_boolean = true; break;
            case EQEQ: inst = "icmp eq"; is_boolean = true; break;
            case NE: inst = "icmp ne"; is_boolean = true; break;
            default: assert(false && "Invalid operator"); return "";
        }
        std::string res = consume_reg();
        code() << "  " << res << " = " << inst << " i64 " << l << ", " << r << "\n";
        if (is_boolean) {
            // This is a hack to make all produced values i64
            auto new_res = consume_reg();
            code() << "  " << new_res << " = zext i1 " << res << " to i64\n";
            return new_res;
        }
        return res;
    }

    std::string visit_unary_expr(UnaryExpr& node) override {
        std::string e = node.expr->accept(*this);
        std::string res = consume_reg();
        switch (node.op) {
            // You can add more
            case BIT_NOT: code() << "  " << res << " = xor i64 " << e << ", -1\n"; break;
            default: assert(false && "Invalid unary operator"); return "";
        }
        return res;
    }

    std::string visit_bool_expr(BooleanExpr& node) override {
        std::string val = node.expr->accept(*this);
        std::string res = consume_reg();
        code() << "  " << res << " = trunc i64 " << val << " to i1\n";
        return res;
    }

    std::string visit_call(CallExpr& node) override {
        std::vector<std::string> arg_regs;
        arg_regs.reserve(node.args.size());
        for (auto* arg : node.args) {
            arg_regs.push_back(arg->accept(*this));
        }
        std::string res = consume_reg();
        code() << "  " << res << " = call i64 @" << node.func_name << "(";
        write_list(arg_regs, "", code());
        code() << ")\n";
        return res;
    }

    std::string visit_read(ReadExpr& node) override {
        (void)node;
        const std::string ptr = consume_reg();
        std::string ptr_i64 = consume_reg();
        code() << "  " << ptr << " = alloca i64\n";
        code() << "  " << ptr_i64 << " = ptrtoint ptr " << ptr << " to i64\n";
        code() << "  call i32 (i64, ...) @scanf(i64 ptrtoint (ptr getelementptr inbounds "
                  "([4 x i8], ptr @.read_num, i64 0, i64 0) to i64), i64 " << ptr_i64 << ")\n";
        std::string res = consume_reg();
        code() << "  " << res << " = load i64, ptr " << ptr << "\n";
        return res;
    }

    std::string visit_num(NumberExpr& node) override {
        return std::to_string(node.value);
    }

    std::string visit_string(StringExpr& node) override {
        std::string str_id = consume_str_id();
        std::string escaped;
        for (char c : node.value) {
            if (c == '\n') escaped += "\\0A";
            else if (c == '"') escaped += "\\22";
            else if (c == '\\') escaped += "\\5C";
            else escaped += c;
        }
        escaped += "\\00";
        header() << str_id << " = private constant [" << (node.value.size() + 1) << " x i8] c\"" << escaped << "\"\n";
        std::string ptr = consume_reg();
        code() << "  " << ptr << " = getelementptr inbounds [" << (node.value.size() + 1) << " x i8], ptr " << str_id << ", i64 0, i64 0\n";
        std::string res = consume_reg();
        code() << "  " << res << " = ptrtoint ptr " << ptr << " to i64\n";  // A hack to make it consistent with the rest of the code
        return res;
    }

    std::string visit_block(BlockStmt& node) override {
        for (auto* statement : node.statements) {
            statement->accept(*this);
        }
        return "";
    }

    std::string visit_return(ReturnStmt& node) override {
        std::string val = node.expr->accept(*this);
        code() << "  ret i64 " << val << "\n";
        return "";
    }

    std::string visit_print(PrintStmt& node) override {
        std::string val = node.expr->accept(*this);
        code() << "  call i32 (i64, ...) @printf(i64 ptrtoint (ptr getelementptr inbounds (";
        if (dynamic_cast<StringExpr*>(node.expr)) {
            code() << "[5 x i8], ptr @.print_str";
        } else {
            code() << "[4 x i8], ptr @.print_num";
        }
        code() << ", i64 0, i64 0) to i64), i64 " << val << ")\n";
        return "";
    }

    std::string visit_global(GlobalVarDecl& node) override {
        bool is_new = global_vars.insert(node.name).second;
        if (!is_new) {
            throw std::runtime_error("Multiple definitions of global variable: " + node.name);
        }
        if (node.init != nullptr && !dynamic_cast<NumberExpr*>(node.init)) {
            // For simplicity. If you want, you can make use of @llvm.global_ctors.
            throw std::runtime_error("Cannot assign a non-const number to a global variable: " + node.name);
        }
        std::string init_val = node.init ? node.init->accept(*this) : "0";
        header() << "@" << node.name << " = global i64 " << init_val << "\n";
        return "";
    }

    std::string visit_extern_global(ExternGlobalDecl& node) override {
        bool is_new = global_vars.insert(node.name).second;
        if (!is_new) {
            throw std::runtime_error("Multiple definitions of (extern) global variable: " + node.name);
        }
        header() << "@" << node.name << " = external global i64 \n";
        return "";
    }

    std::string visit_assign(AssignStmt& node) override {
        std::string val = node.expr ? node.expr->accept(*this) : "0";
        if (global_vars.contains(node.name)) {
            code() << "  store i64 " << val << ", ptr @" << node.name << "\n";
        } else {
            define_var(node.name);
            code() << "  store i64 " << val << ", ptr %" << node.name << "\n";
        }
        return "";
    }

    std::string visit_var(VarExpr& node) override {
        std::string res = consume_reg();
        if (global_vars.contains(node.name)) {
            code() << "  " << res << " = load i64, ptr @" << node.name << "\n";
        } else if (defined_vars.contains(node.name)) {
            code() << "  " << res << " = load i64, ptr %" << node.name << "\n";
        } else {
            throw std::runtime_error("Undefined variable: " + node.name);
        }
        return res;
    }

    std::string visit_if(IfStmt& node) override {
        std::vector<std::string> labels;
        labels.reserve(node.elseif_branches.size() + 2);
        labels.push_back(consume_label("then"));
        auto elseif_label_base = label_count;
        for (size_t i = 0; i < node.elseif_branches.size(); ++i) {
            labels.push_back(consume_label("elseif"));
        }
        auto else_label = consume_label("else");
        auto end_label = consume_label("end");
        labels.push_back(node.else_body ? else_label : end_label);

        auto cond_val = node.cond->accept(*this);
        code() << "  br i1 " << cond_val << ", label %" << labels[0] << ", label %" << labels[1] << "\n";

        code() << labels[0] << ":\n";
        node.then_body->accept(*this);
        code() << "  br label %" << end_label << "\n";

        for (size_t i = 0; i < node.elseif_branches.size(); ++i) {
            auto branch_num = elseif_label_base + i;
            code() << labels[i + 1] << ":\n";
            auto elseif_cond = node.elseif_branches[i].first->accept(*this);
            code() << "  br i1 " << elseif_cond << ", label %elseif_body." << branch_num << ", label %" << labels[i + 2] << "\n";
            code() << "elseif_body." << branch_num << ":\n";
            node.elseif_branches[i].second->accept(*this);
            code() << "  br label %" << end_label << "\n";
        }

        if (node.else_body) {
            code() << else_label << ":\n";
            node.else_body->accept(*this);
            code() << "  br label %" << end_label << "\n";
        }

        code() << end_label << ":\n";
        return "";
    }

    std::string visit_while(WhileStmt& node) override {
        auto cond_label = "while_cond." + std::to_string(label_count);
        auto body_label = "while_body." + std::to_string(label_count);
        auto end_label = "while_end." + std::to_string(label_count);
        ++label_count;

        code() << "  br label %" << cond_label << "\n";
        code() << cond_label << ":\n";
        auto cond_val = node.cond->accept(*this);
        code() << "  br i1 " << cond_val << ", label %" << body_label << ", label %" << end_label << "\n";
        code() << body_label << ":\n";
        node.body->accept(*this);
        code() << "  br label %" << cond_label << "\n";
        code() << end_label << ":\n";
        return "";
    }

    std::string visit_goto(GotoStmt& node) override {
        code() << "  br label %" << get_label_name(node.label) << "\n";
        return "";
    }

    std::string visit_label(LabelStmt& node) override {
        auto label = get_label_name(node.name);
        code() << "  br label %" << label << "\n";
        code() << label << ":\n";
        node.stmt->accept(*this);
        return "";
    }

    std::string visit_expr_statement(ExprStatement& node) override {
        node.expr->accept(*this);
        return "";
    }

    void generate(const IntermediateCode& ir) {
        header() << "; ModuleID = 'simple'\n";

        // This is a hack, declaring printf and scanf as taking an i64
        header() << "declare i32 @printf(i64, ...)\n";
        header() << "declare i32 @scanf(i64, ...)\n";
        for (auto* decl : ir.declarations) {
            decl->accept(*this);
        }

        for (auto* global : ir.globals) {
            global->accept(*this);
        }

        header() << "@.print_num = private constant [5 x i8] c\"%ld\\0A\\00\"\n";
        header() << "@.print_str = private constant [4 x i8] c\"%s\\0A\\00\"\n";
        header() << "@.read_num = private constant [4 x i8] c\"%ld\\00\"\n";

        for (auto* func : ir.functions) {
            func->accept(*this);
        }

        *out << header().str() << '\n' << code().str();
    }
};

template <typename InStream>
class FrontEnd : private Parser<InStream> {
public:
    explicit FrontEnd(InStream& in) : Parser<InStream>(in) { }
    IntermediateCode process() { return this->parse(); }
};

template <typename OutStream>
class BackEnd : private CodeGenerator<OutStream> {
public:
    explicit BackEnd(OutStream& out_stream)
        : CodeGenerator<OutStream>(out_stream) { }
    void process(const IntermediateCode& ir) { return this->generate(ir); }
};

template <typename FrontEnd, typename BackEnd>
class Compiler {
    FrontEnd frontend;
    BackEnd backend;
public:
    Compiler(FrontEnd _frontend, BackEnd _backend)
        : frontend(std::move(_frontend)), backend(std::move(_backend)) { }

    void run() {
        IntermediateCode ir = frontend.process();
        backend.process(ir);
    }
};

class ClangCompiler {
    std::string clang_executable;
    std::string extension;

    static bool compile_simple_file(const std::filesystem::path& input_name, const std::filesystem::path& output_name) {
        std::ifstream input_file(input_name);
        if (!input_file.is_open()) {
            std::cerr << "Error: Cannot open input file " << input_name << "\n";
            return false;
        }

        std::ofstream output_file(output_name);
        if (!output_file.is_open()) {
            std::cerr << "Error: Cannot open output file " << output_name << "\n";
            return false;
        }

        try {
            FrontEnd frontend(input_file);
            BackEnd backend(output_file);
            Compiler compiler { std::move(frontend), std::move(backend) };
            compiler.run();
        } catch (const std::exception& e) {
            std::cerr << "Error in " << input_name << ": " << e.what() << "\n";
            return false;
        }

        return true;
    }

    static std::string escape_shell_arg(const std::string& arg) {
        std::string escaped;
        escaped += '"';
        for (char c : arg) {
            if (c == '"') escaped += "\\\"";
            else escaped += c;
        }
        escaped += '"';
        return escaped;
    }

public:
    ClangCompiler(std::string _clang_executable, std::string _extension)
        : clang_executable(std::move(_clang_executable)), extension("." + std::move(_extension)) { }

    [[nodiscard]] int compile(const std::span<std::string>& args) const {
        if (args.size() < 2) {
            std::cerr << "Usage: " << args[0] << " <file1> [file2] ... [clang options]\n";
            return 1;
        }

        std::vector<std::string> simple_files;
        std::vector<std::string> other_files;
        std::vector<std::string> bc_files;
        std::vector<std::string> extra_args;

        bool is_parsing_files = true;
        for (size_t i = 1; i < args.size(); ++i) {
            std::string arg = args[i];
            if (is_parsing_files && arg[0] != '-') {
                if (arg.size() > extension.size() && arg.substr(arg.size() - extension.size()) == extension) {
                    simple_files.push_back(std::move(arg));
                } else {
                    other_files.push_back(std::move(arg));
                }
            } else {
                is_parsing_files = false;
                extra_args.push_back(std::move(arg));
            }
        }

        if (simple_files.empty() && other_files.empty()) {
            std::cerr << "Error: No input files provided\n";
            return 1;
        }

        const auto temp_dir = std::filesystem::temp_directory_path();
        for (const auto& simple_file : simple_files) {
            auto bc_file = temp_dir / (std::filesystem::path(simple_file).stem().string() + ".bc");
            if (!compile_simple_file(simple_file, bc_file)) {
                return 1;
            }
            bc_files.push_back(bc_file.string());
        }

        std::string clang_command = clang_executable;
        for (const auto& bc_file : bc_files) {
            clang_command += " " + escape_shell_arg(bc_file);
        }
        for (const auto& other_file : other_files) {
            clang_command += " " + escape_shell_arg(other_file);
        }
        for (const auto& arg : extra_args) {
            clang_command += " " + escape_shell_arg(arg);
        }

        clang_command += " -Wno-override-module";

        int result = std::system(clang_command.c_str());
        if (result != 0) {
            std::cerr << "Error: clang failed with exit code " << result << "\n";
            return 1;
        }

        return 0;
    }
};

int main(int argc, char* argv[]) {
    std::vector<std::string> args(argv, argv + argc);
    return ClangCompiler{ "clang", "simple" }.compile(args);
}