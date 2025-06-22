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
    PLUS, MINUS, MUL, DIV, MOD, BIT_AND, BIT_OR, BIT_XOR, BIT_NOT, SHL, SHR,
    LPAREN, RPAREN, LBRACE, RBRACE, SEMI, GT, LT, LE, GE, EQEQ, NE, EQ, COLON, COMMA, LABEL, LET, TRUE, FALSE, EOF_TOKEN };

struct Token {
    TokenType type { };
    std::string value { };
};

template <typename InStream>
class Lexer {
    InStream* _in;
    InStream& in() { return *_in; }

    bool not_eof() { return !in().eof(); }
    char peek() { return (char)in().peek(); }
    char consume() { return (char)in().get(); }

public:
    explicit Lexer(InStream& in) : _in(&in) { }

    Token next_token() {
        while (not_eof() && peek() == ' ') consume();
        while (not_eof() && std::isspace(peek())) consume();

        if (not_eof() && peek() == '/') {
            consume();
            if (not_eof() && peek() == '/') {
                while (not_eof() && peek() != '\n') consume();
                if (not_eof() && peek() == '\n') consume();
                return next_token();
            } else {
                return {DIV, "/"};
            }
        }

        if (in().eof()) return {EOF_TOKEN, ""};

        auto c = (char)peek();
        if (c == '-' || std::isdigit(c)) {
            std::string num;
            if (c == '-') {
                num += (char)consume();
                if (not_eof() && !std::isdigit(peek())) {
                    return {MINUS, "-"};
                }
            }
            while (not_eof() && std::isdigit(peek())) {
                num += (char)consume();
            }
            return {NUMBER, num};
        }
        if (c == '"') {
            consume();
            std::string str;
            while (not_eof() && peek() != '"') {
                if (peek() == '\\') {
                    consume();
                    if (not_eof()) {
                        char next = (char)consume();
                        if (next == 'n') str += '\n';
                        else if (next == '"') str += '"';
                        else str += next;
                    }
                } else {
                    str += (char)consume();
                }
            }
            if (not_eof()) consume();
            return {STRING, str};
        }
        if (std::isalpha(c)) {
            std::string id;
            while (not_eof() && (std::isalnum(peek()) || peek() == '_')) {
                id += (char)consume();
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
        if (c == '+') { consume(); return {PLUS, "+"}; }
        // if (c == '-') { consume(); return {MINUS, "-"}; }  // This case is handled above
        if (c == '*') { consume(); return {MUL, "*"}; }
        if (c == '/') { consume(); return {DIV, "/"}; }
        if (c == '%') { consume(); return {MOD, "%"}; }
        if (c == '&') { consume(); return {BIT_AND, "&"}; }
        if (c == '|') { consume(); return {BIT_OR, "|"}; }
        if (c == '^') { consume(); return {BIT_XOR, "^"}; }
        if (c == '~') { consume(); return {BIT_NOT, "~"}; }
        if (c == '<') {
            consume();
            if (not_eof() && peek() == '<') { consume(); return {SHL, "<<"}; }
            if (not_eof() && peek() == '=') { consume(); return {LE, "<="}; }
            return {LT, "<"};
        }
        if (c == '>') {
            consume();
            if (not_eof() && peek() == '>') { consume(); return {SHR, ">>"}; }
            if (not_eof() && peek() == '=') { consume(); return {GE, ">="}; }
            return {GT, ">"};
        }
        if (c == '=') {
            consume();
            if (not_eof() && peek() == '=') { consume(); return {EQEQ, "=="}; }
            return {EQ, "="};
        }
        if (c == '!') {
            consume();
            if (not_eof() && peek() == '=') { consume(); return {NE, "!="}; }
            return {EOF_TOKEN, ""};
        }
        if (c == '(') { consume(); return {LPAREN, "("}; }
        if (c == ')') { consume(); return {RPAREN, ")"}; }
        if (c == '{') { consume(); return {LBRACE, "{"}; }
        if (c == '}') { consume(); return {RBRACE, "}"}; }
        if (c == ';') { consume(); return {SEMI, ";"}; }
        if (c == ':') { consume(); return {COLON, ":"}; }
        if (c == ',') { consume(); return {COMMA, ","}; }
        consume();
        return {EOF_TOKEN, ""};
    }
};

struct Visitor;

struct ASTNode {
    virtual ~ASTNode() = default;
    virtual std::string accept(Visitor& visitor);
};

struct Expr : ASTNode {
    std::string accept(Visitor& visitor) override;
};

struct Statement : ASTNode {
    std::string accept(Visitor& visitor) override;
};

struct FuncDef : ASTNode {
    std::string name;
    std::vector<std::string> params;
    Statement* body;
    FuncDef(std::string n, const std::vector<std::string>& p, Statement* b)
        : name(std::move(n)), params(p), body(b) { }
    std::string accept(Visitor& visitor) override;
};

struct FuncDecl : ASTNode {
    std::string name;
    std::vector<std::string> params;
    FuncDecl(std::string n, const std::vector<std::string>& p) : name(std::move(n)), params(p) { }
    std::string accept(Visitor& visitor) override;
};

struct GlobalVarDecl : ASTNode {
    std::string name;
    Expr* init;
    GlobalVarDecl(std::string n, Expr* i) : name(std::move(n)), init(i) { }
    std::string accept(Visitor& visitor) override;
};

struct BinaryExpr : Expr {
    TokenType op;
    Expr* left { };
    Expr* right { };
    BinaryExpr(TokenType o, Expr* l, Expr* r) : op(o), left(l), right(r) { }
    std::string accept(Visitor& visitor) override;
};

struct UnaryExpr : Expr {
    TokenType op;
    Expr* expr { };
    UnaryExpr(TokenType o, Expr* e) : op(o), expr(e) { }
    std::string accept(Visitor& visitor) override;
};

struct BooleanExpr : Expr {
    Expr* expr;
    explicit BooleanExpr(Expr* e) : expr(e) { }
    std::string accept(Visitor& visitor) override;
};

struct CallExpr : Expr {
    std::string func_name;
    std::vector<Expr*> args;
    CallExpr(std::string fn, const std::vector<Expr*>& a) : func_name(std::move(fn)), args(a) { }
    std::string accept(Visitor& visitor) override;
};

struct ReadExpr : Expr {
    std::string accept(Visitor& visitor) override;
};

struct VarExpr : Expr {
    std::string name;
    explicit VarExpr(std::string n) : name(std::move(n)) { }
    std::string accept(Visitor& visitor) override;
};

struct NumberExpr : Expr {
    int64_t value { };
    explicit NumberExpr(const int64_t v) : value(v) { }
    std::string accept(Visitor& visitor) override;
};

struct StringExpr : Expr {
    std::string value;
    explicit StringExpr(std::string v) : value(std::move(v)) { }
    std::string accept(Visitor& visitor) override;
};

struct BlockStmt : Statement {
    std::vector<Statement*> statements;
    explicit BlockStmt(std::vector<Statement*> s) : statements(std::move(s)) { }
    std::string accept(Visitor& visitor) override;
};

struct ReturnStmt : Statement {
    Expr* expr { };
    explicit ReturnStmt(Expr* e) : expr(e) { }
    std::string accept(Visitor& visitor) override;
};

struct PrintStmt : Statement {
    Expr* expr { };
    explicit PrintStmt(Expr* e) : expr(e) { }
    std::string accept(Visitor& visitor) override;
};

struct AssignStmt : Statement {
    std::string var;
    Expr* expr { };
    AssignStmt(std::string v, Expr* e) : var(std::move(v)), expr(e) { }
    std::string accept(Visitor& visitor) override;
};

struct IfStmt : Statement {
    Expr* cond;
    Statement* then_body;
    std::vector<std::pair<Expr*, Statement*>> elseif_branches;
    Statement* else_body;
    IfStmt(Expr* c, Statement* t, const std::vector<std::pair<Expr*, Statement*>>& ei, Statement* e)
        : cond(c), then_body(t), elseif_branches(ei), else_body(e) { }
    std::string accept(Visitor& visitor) override;
};

struct WhileStmt : Statement {
    Expr* cond;
    Statement* body;
    WhileStmt(Expr* c, Statement* b) : cond(c), body(b) { }
    std::string accept(Visitor& visitor) override;
};

struct GotoStmt : Statement {
    std::string label;
    explicit GotoStmt(std::string l) : label(std::move(l)) { }
    std::string accept(Visitor& visitor) override;
};

struct LabelStmt : Statement {
    std::string name;
    Statement* stmt;
    LabelStmt(std::string n, Statement* s) : name(std::move(n)), stmt(s) { }
    std::string accept(Visitor& visitor) override;
};

struct ExprStatement : Statement {
    Expr* expr { };
    explicit ExprStatement(Expr* e) : expr(e) { }
    std::string accept(Visitor& visitor) override;
};

struct Visitor {
    virtual std::string visit_node(ASTNode* node) { (void)node; return ""; }

    virtual std::string visit_def(FuncDef* node) { return visit_node(node); }
    virtual std::string visit_decl(FuncDecl* node) { return visit_node(node); }

    virtual std::string visit_global_var_decl(GlobalVarDecl* node) { return visit_node(node); }
    virtual std::string visit_expr(Expr* node) { (void)node; assert(false && "This method shouldn't be reached, or should be overridden"); }
    virtual std::string visit_bin_expr(BinaryExpr* node) { return visit_expr(node); }
    virtual std::string visit_unary_expr(UnaryExpr* node) { return visit_expr(node); }
    virtual std::string visit_bool_expr(BooleanExpr* node) { return visit_expr(node); }
    virtual std::string visit_call(CallExpr* node) { return visit_expr(node); }
    virtual std::string visit_read(ReadExpr* node) { return visit_expr(node); }
    virtual std::string visit_var(VarExpr* node) { return visit_expr(node); }
    virtual std::string visit_num(NumberExpr* node) { return visit_expr(node); }
    virtual std::string visit_string(StringExpr* node) { return visit_expr(node); }

    virtual std::string visit_statement(Statement* node) { return visit_node(node); }
    virtual std::string visit_block(BlockStmt* node) { return visit_statement(node); }
    virtual std::string visit_return(ReturnStmt* node) { return visit_statement(node); }
    virtual std::string visit_print(PrintStmt* node) { return visit_statement(node); }
    virtual std::string visit_assign(AssignStmt* node) { return visit_statement(node); }
    virtual std::string visit_if(IfStmt* node) { return visit_statement(node); }
    virtual std::string visit_while(WhileStmt* node) { return visit_statement(node); }
    virtual std::string visit_goto(GotoStmt* node) { return visit_statement(node); }
    virtual std::string visit_label(LabelStmt* node) { return visit_statement(node); }
    virtual std::string visit_expr_statement(ExprStatement* node) { return visit_statement(node); }

    virtual ~Visitor() = default;
};

std::string ASTNode::accept(Visitor &visitor) { return visitor.visit_node(this); }
std::string Expr::accept(Visitor &visitor) { return visitor.visit_expr(this); }
std::string BinaryExpr::accept(Visitor &visitor) { return visitor.visit_bin_expr(this); }
std::string UnaryExpr::accept(Visitor &visitor) { return visitor.visit_unary_expr(this); }
std::string BooleanExpr::accept(Visitor &visitor) { return visitor.visit_bool_expr(this); }
std::string CallExpr::accept(Visitor &visitor) { return visitor.visit_call(this); }
std::string ReadExpr::accept(Visitor &visitor) { return visitor.visit_read(this); }
std::string VarExpr::accept(Visitor &visitor) { return visitor.visit_var(this); }
std::string NumberExpr::accept(Visitor &visitor) { return visitor.visit_num(this); }
std::string StringExpr::accept(Visitor &visitor) { return visitor.visit_string(this); }
std::string Statement::accept(Visitor &visitor) { return visitor.visit_statement(this); }
std::string BlockStmt::accept(Visitor &visitor) { return visitor.visit_block(this); }
std::string ReturnStmt::accept(Visitor &visitor) { return visitor.visit_return(this); }
std::string PrintStmt::accept(Visitor &visitor) { return visitor.visit_print(this); }
std::string AssignStmt::accept(Visitor &visitor) { return visitor.visit_assign(this); }
std::string IfStmt::accept(Visitor &visitor) { return visitor.visit_if(this); }
std::string WhileStmt::accept(Visitor &visitor) { return visitor.visit_while(this); }
std::string GotoStmt::accept(Visitor &visitor) { return visitor.visit_goto(this); }
std::string LabelStmt::accept(Visitor &visitor) { return visitor.visit_label(this); }
std::string ExprStatement::accept(Visitor &visitor) { return visitor.visit_expr_statement(this); }
std::string FuncDef::accept(Visitor &visitor) { return visitor.visit_def(this); }
std::string FuncDecl::accept(Visitor &visitor) { return visitor.visit_decl(this); }
std::string GlobalVarDecl::accept(Visitor &visitor) { return visitor.visit_global_var_decl(this); }

struct IntermediateCode {
    std::vector<GlobalVarDecl*> globals;
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
                    args.push_back(parse_binary_expr());
                    while (peek() == COMMA) {
                        consume(COMMA);
                        args.push_back(parse_binary_expr());
                    }
                }
                consume(RPAREN);
                return new_node<CallExpr>(name, args);
            }
            return new_node<VarExpr>(name);
        }
        if (peek() == LPAREN) {
            consume(LPAREN);
            Expr* e = parse_binary_expr();
            consume(RPAREN);
            return e;
        }
        throw std::runtime_error("Invalid expression: " + current.value);
    }

    Expr* parse_binary_expr(const int precedence = 0) {
        Expr* left = parse_expr();
        while (true) {
            TokenType op = peek();
            const int new_precedence = (op == PLUS || op == MINUS) ? 10 :
                                       (op == MUL || op == DIV || op == MOD) ? 20 :
                                       (op == GT || op == LT || op == LE || op == GE || op == EQEQ || op == NE) ? 5 :
                                       (op == BIT_AND) ? 8 :
                                       (op == BIT_OR || op == BIT_XOR) ? 6 :
                                       (op == SHL || op == SHR) ? 12 : 0;
            if (new_precedence <= precedence) break;
            consume(op);
            Expr* right = parse_binary_expr(new_precedence);
            left = new_node<BinaryExpr>(op, left, right);
        }
        return left;
    }

    Statement* parse_statement() {
        if (peek() == PRINT) {
            consume(PRINT);
            consume(LPAREN);
            Expr* e = parse_binary_expr();
            consume(RPAREN);
            consume(SEMI);
            return new_node<PrintStmt>(e);
        }
        if (peek() == IF) {
            consume(IF);
            consume(LPAREN);
            Expr* cond = new_node<BooleanExpr>(parse_binary_expr());
            consume(RPAREN);
            Statement* then_body = parse_statement();
            std::vector<std::pair<Expr*, Statement*>> elseif_branches;
            while (peek() == ELSEIF) {
                consume(ELSEIF);
                consume(LPAREN);
                Expr* elseif_cond = new_node<BooleanExpr>(parse_binary_expr());
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
            Expr* cond = new_node<BooleanExpr>(parse_binary_expr());
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
            Expr* e = parse_binary_expr();
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
            consume(LET);
            std::string var = current.value;
            consume(IDENTIFIER);
            consume(EQ);
            Expr* e = parse_binary_expr();
            consume(SEMI);
            return new_node<AssignStmt>(var, e);
        }
        auto expr_statement = new_node<ExprStatement>(parse_binary_expr());
        consume(SEMI);
        return expr_statement;
    }

    std::vector<std::string> parse_func_common() {
        consume(IDENTIFIER);
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
        const auto params = parse_func_common();
        consume(EQ);
        Statement* body = parse_statement();
        return new_node<FuncDef>(name, params, body);
    }

    FuncDecl* parse_func_decl() {
        consume(EXTERN);
        const std::string name = current.value;
        const auto params = parse_func_common();
        consume(SEMI);
        return new_node<FuncDecl>(name, params);
    }

    GlobalVarDecl* parse_global_var() {
        consume(LET);
        std::string name = current.value;
        consume(IDENTIFIER);
        Expr* init = nullptr;
        if (peek() == EQ) {
            consume(EQ);
            init = parse_binary_expr();
        }
        consume(SEMI);
        return new_node<GlobalVarDecl>(name, init);
    }

    IntermediateCode parse() {
        while (peek() != EOF_TOKEN) {
            if (peek() == DEF) {
                intermediate_code.functions.push_back(parse_func_def());
            } else if (peek() == EXTERN) {
                intermediate_code.declarations.push_back(parse_func_decl());
            } else if (peek() == LET) {
                intermediate_code.globals.push_back(parse_global_var());
            } else {
                throw std::runtime_error("Invalid top-level construct: " + current.value);
            }
        }
        return std::move(intermediate_code);
    }
};

template <typename OutStream>
class CodeGenerator : public Visitor {
    OutStream* out;
    std::ostringstream _header;
    std::ostringstream _code;
    std::unordered_set<std::string> defined_vars = { };
    std::unordered_set<std::string> global_vars = { };
    std::string func_name;
    unsigned long long reg = 0;
    unsigned long long str_count = 0;

    std::ostringstream& header() { return _header; }
    std::ostringstream& code() { return _code; }

    void reset() {
        reg = 0;
        defined_vars.clear();
    }

    std::string consume_reg() { return "%reg" + std::to_string(reg++); }
    std::string consume_str_id() { return "@.str." + std::to_string(str_count++); }

    void define_var(const std::string& name) {
        bool is_new = defined_vars.insert(name).second;
        if (is_new) {
            code() << "  %" << name << " = alloca i64\n";
        }
    }

public:
    explicit CodeGenerator(OutStream& _out) : out(&_out) { }

    void add_global(const std::string& name) {
        global_vars.insert(name);
    }

    std::string visit_global_var_decl(GlobalVarDecl* node) override {
        add_global(node->name);
        std::string init_val = node->init ? node->init->accept(*this) : "0";
        header() << "@" << node->name << " = global i64 " << init_val << "\n";
        return "";
    }

    std::string visit_def(FuncDef* node) override {
        reset();
        func_name = node->name;
        code() << "define i64 @" << node->name << "(";
        for (size_t i = 0; i < node->params.size(); ++i) {
            code() << "i64 %" << node->params[i];
            if (i < node->params.size() - 1) code() << ", ";
        }
        code() << ") {\n";
        code() << "entry:\n";
        for (const auto& param : node->params) {
            auto param_name = param + "." + node->name;
            define_var(param_name);
            code() << "  store i64 %" << param << ", ptr %" << param_name << "\n";
        }
        node->body->accept(*this);
        code() << "  ret i64 0\n";
        code() << "}\n\n";
        return "";
    }

    std::string visit_decl(FuncDecl* node) override {
        header() << "declare i64 @" << node->name << "(";
        for (size_t i = 0; i < node->params.size(); ++i) {
            header() << "i64";
            if (i < node->params.size() - 1) header() << ", ";
        }
        header() << ")\n";
        return "";
    }

    std::string visit_bin_expr(BinaryExpr* node) override {
        std::string l = node->left->accept(*this);
        std::string r = node->right->accept(*this);
        std::string inst;
        bool is_boolean = false;
        switch (node->op) {
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

    std::string visit_unary_expr(UnaryExpr* node) override {
        std::string e = node->expr->accept(*this);
        std::string inst;
        switch (node->op) {
            case BIT_NOT: inst = "xor"; break;
            default: assert(false && "Invalid unary operator"); return "";
        }
        std::string res = consume_reg();
        code() << "  " << res << " = " << inst << " i64 " << e << ", -1\n";
        return res;
    }

    std::string visit_bool_expr(BooleanExpr* node) override {
        std::string val = node->expr->accept(*this);
        std::string res = consume_reg();
        code() << "  " << res << " = trunc i64 " << val << " to i1\n";
        return res;
    }

    std::string visit_call(CallExpr* node) override {
        std::vector<std::string> arg_regs;
        arg_regs.reserve(node->args.size());
        for (auto* arg : node->args) {
            arg_regs.push_back(arg->accept(*this));
        }
        std::string res = consume_reg();
        code() << "  " << res << " = call i64 @" << node->func_name << "(";
        for (size_t i = 0; i < arg_regs.size(); ++i) {
            code() << "i64 " << arg_regs[i];
            if (i < arg_regs.size() - 1) code() << ", ";
        }
        code() << ")\n";
        return res;
    }

    std::string visit_read(ReadExpr* node) override {
        (void)node;

        const std::string ptr = consume_reg();
        std::string ptr_i64 = consume_reg();
        code() << "  " << ptr << " = alloca i64\n";
        code() << "  " << ptr_i64 << " = ptrtoint ptr " << ptr << " to i64\n";

        code() << "  call i32 (i64, ...) @scanf(i64 ptrtoint (ptr getelementptr inbounds ([4 x i8], ptr @.read_num, i64 0, i64 0) to i64), i64 " << ptr_i64 << ")\n";

        std::string res = consume_reg();
        code() << "  " << res << " = load i64, ptr " << ptr << "\n";
        return res;
    }

    std::string visit_var(VarExpr* node) override {
        std::string res = consume_reg();
        if (global_vars.count(node->name)) {
            code() << "  " << res << " = load i64, ptr @" << node->name << "\n";
        } else {
            code() << "  " << res << " = load i64, ptr %" << node->name << "." << func_name << "\n";
        }
        return res;
    }

    std::string visit_num(NumberExpr* node) override {
        return std::to_string(node->value);
    }

    std::string visit_string(StringExpr* node) override {
        std::string str_id = consume_str_id();
        std::string escaped;
        for (char c : node->value) {
            if (c == '\n') escaped += "\\0A";
            else if (c == '"') escaped += "\\22";
            else if (c == '\\') escaped += "\\5C";
            else escaped += c;
        }
        escaped += "\\00";
        header() << str_id << " = private constant [" << (node->value.size() + 1) << " x i8] c\"" << escaped << "\"\n";
        std::string ptr = consume_reg();
        code() << "  " << ptr << " = getelementptr inbounds [" << (node->value.size() + 1) << " x i8], ptr " << str_id << ", i64 0, i64 0\n";
        std::string res = consume_reg();
        code() << "  " << res << " = ptrtoint ptr " << ptr << " to i64\n";  // A hack to make it consistent with the rest of the code
        return res;
    }

    std::string visit_block(BlockStmt* node) override {
        for (auto* statement : node->statements) {
            statement->accept(*this);
        }
        return "";
    }

    std::string visit_return(ReturnStmt* node) override {
        std::string val = node->expr->accept(*this);
        code() << "  ret i64 " << val << "\n";
        return "";
    }

    std::string visit_print(PrintStmt* node) override {
        if (dynamic_cast<StringExpr*>(node->expr)) {
            std::string val = node->expr->accept(*this);
            code() << "  call i32 (i64, ...) @printf(i64 ptrtoint (ptr getelementptr inbounds ([5 x i8], ptr @.print_str, i64 0, i64 0) to i64), i64 " << val << ")\n";
        } else {
            std::string val = node->expr->accept(*this);
            code() << "  call i32 (i64, ...) @printf(i64 ptrtoint (ptr getelementptr inbounds ([4 x i8], ptr @.print_num, i64 0, i64 0) to i64), i64 " << val << ")\n";
        }
        return "";
    }

    std::string visit_assign(AssignStmt* node) override {
        std::string val = node->expr->accept(*this);
        if (global_vars.count(node->var)) {
            code() << "  store i64 " << val << ", ptr @" << node->var << "\n";
        } else {
            auto var_name = node->var + "." + func_name;
            define_var(var_name);
            code() << "  store i64 " << val << ", ptr %" << var_name << "\n";
        }
        return "";
    }

    std::string visit_if(IfStmt* node) override {
        std::vector<std::string> labels;
        labels.push_back("then" + std::to_string(reg));
        for (size_t i = 0; i < node->elseif_branches.size(); ++i) {
            labels.push_back("elseif" + std::to_string(reg + i + 1));
        }
        labels.push_back(node->else_body ? "else" + std::to_string(reg + node->elseif_branches.size() + 1) : "end" + std::to_string(reg));
        std::string end_label = "end" + std::to_string(reg);
        reg += node->elseif_branches.size() + 2;

        std::string cond_val = node->cond->accept(*this);
        code() << "  br i1 " << cond_val << ", label %" << labels[0] << ", label %" << labels[1] << "\n";

        code() << labels[0] << ":\n";
        node->then_body->accept(*this);
        code() << "  br label %" << end_label << "\n";

        for (size_t i = 0; i < node->elseif_branches.size(); ++i) {
            code() << labels[i + 1] << ":\n";
            std::string elseif_cond = node->elseif_branches[i].first->accept(*this);
            code() << "  br i1 " << elseif_cond << ", label %elseif_body" << (reg - node->elseif_branches.size() - 1 + i) << ", label %" << labels[i + 2] << "\n";
            code() << "elseif_body" << (reg - node->elseif_branches.size() - 1 + i) << ":\n";
            node->elseif_branches[i].second->accept(*this);
            code() << "  br label %" << end_label << "\n";
        }

        if (node->else_body) {
            code() << labels.back() << ":\n";
            node->else_body->accept(*this);
            code() << "  br label %" << end_label << "\n";
        }

        code() << end_label << ":\n";
        return "";
    }

    std::string visit_while(WhileStmt* node) override {
        std::string cond_label = "while_cond" + std::to_string(reg);
        std::string body_label = "while_body" + std::to_string(reg);
        std::string end_label = "while_end" + std::to_string(reg);
        reg++;

        code() << "  br label %" << cond_label << "\n";
        code() << cond_label << ":\n";
        std::string cond_val = node->cond->accept(*this);
        code() << "  br i1 " << cond_val << ", label %" << body_label << ", label %" << end_label << "\n";
        code() << body_label << ":\n";
        node->body->accept(*this);
        code() << "  br label %" << cond_label << "\n";
        code() << end_label << ":\n";
        return "";
    }

    std::string visit_goto(GotoStmt* node) override {
        code() << "  br label %" << node->label << "." << func_name << "\n";
        return "";
    }

    std::string visit_label(LabelStmt* node) override {
        auto label = node->name + "." + func_name;
        code() << "  br label %" << label << "\n";
        code() << label << ":\n";
        node->stmt->accept(*this);
        return "";
    }

    std::string visit_expr_statement(ExprStatement* node) override {
        node->expr->accept(*this);
        return "";
    }

    void generate(const IntermediateCode& ir) {
        header() << "; ModuleID = 'simple'\n";
        header() << "@.print_num = private constant [5 x i8] c\"%ld\\0A\\00\"\n";
        header() << "@.print_str = private constant [4 x i8] c\"%s\\0A\\00\"\n";
        header() << "@.read_num = private constant [4 x i8] c\"%ld\\00\"\n";

        for (auto* global : ir.globals) {
            global->accept(*this);
        }
        for (auto* func : ir.functions) {
            func->accept(*this);
        }
        // This is a hack, declaring printf and scanf as taking an i64
        header() << "declare i32 @printf(i64, ...)\n";
        header() << "declare i32 @scanf(i64, ...)\n";
        for (auto* decl : ir.declarations) {
            decl->accept(*this);
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

    int compile(const std::span<std::string>& args) const {
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