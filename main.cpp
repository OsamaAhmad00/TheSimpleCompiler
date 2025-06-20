#include <cassert>
#include <iostream>
#include <string>
#include <utility>
#include <vector>
#include <cctype>
#include <fstream>
#include <unordered_set>
#include <sstream>

enum TokenType { NUMBER, IDENTIFIER, IF, ELSE, PRINT, READ, RETURN, DEF, EXTERN, CALL,
    PLUS, MINUS, MUL, DIV, LPAREN, RPAREN, LBRACE, RBRACE, SEMI, GT, EQ, COMMA, EOF_TOKEN };

struct Token {
    TokenType type { };
    std::string value { };
};

template <typename InStream>
class Lexer {

    InStream* _in;

    InStream& in() { return *_in; }

public:

    explicit Lexer(InStream& in) : _in(&in) { }

    Token next_token() {
        while (!in().eof() && in().peek() == ' ') in().get();
        while (!in().eof() && std::isspace(in().peek())) in().get();
        if (in().eof()) return {EOF_TOKEN, ""};

        char c = in().peek();
        if (std::isdigit(c)) {
            std::string num;
            while (!in().eof() && std::isdigit(in().peek())) {
                num += in().get();
            }
            return {NUMBER, num};
        }
        if (std::isalpha(c)) {
            std::string id;
            while (!in().eof() && (std::isalnum(in().peek()) || in().peek() == '_')) {
                id += in().get();
            }
            if (id == "if") return {IF, id};
            if (id == "else") return {ELSE, id};
            if (id == "print") return {PRINT, id};
            if (id == "read") return {READ, id};
            if (id == "call") return {CALL, id};
            if (id == "return") return {RETURN, id};
            if (id == "def") return {DEF, id};
            if (id == "extern") return {EXTERN, id};
            return {IDENTIFIER, id};
        }
        if (c == '+') { in().get(); return {PLUS, "+"}; }
        if (c == '-') { in().get(); return {MINUS, "-"}; }
        if (c == '*') { in().get(); return {MUL, "*"}; }
        if (c == '/') { in().get(); return {DIV, "/"}; }
        if (c == '(') { in().get(); return {LPAREN, "("}; }
        if (c == ')') { in().get(); return {RPAREN, ")"}; }
        if (c == '{') { in().get(); return {LBRACE, "{"}; }
        if (c == '}') { in().get(); return {RBRACE, "}"}; }
        if (c == ';') { in().get(); return {SEMI, ";"}; }
        if (c == '>') { in().get(); return {GT, ">"}; }
        if (c == '=') { in().get(); return {EQ, "="}; }
        if (c == ',') { in().get(); return {COMMA, ","}; }
        in().get();
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
    FuncDef(std::string  n, const std::vector<std::string>& p, Statement* b)
        : name(std::move(n)), params(p), body(b) { }
    std::string accept(Visitor& visitor) override;
};

struct FuncDecl : ASTNode {
    std::string name;
    std::vector<std::string> params;
    FuncDecl(std::string  n, const std::vector<std::string>& p) : name(std::move(n)), params(p) { }
    std::string accept(Visitor& visitor) override;
};

struct BinaryExpr : Expr {
    std::string op;
    Expr* left { };
    Expr* right { };
    BinaryExpr(std::string  o, Expr* l, Expr* r) : op(std::move(o)), left(l), right(r) { }
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
    explicit VarExpr(std::string  n) : name(std::move(n)) { }
    std::string accept(Visitor& visitor) override;
};

struct NumberExpr : Expr {
    int value { };
    explicit NumberExpr(const int v) : value(v) { }
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
    AssignStmt(std::string  v, Expr* e) : var(std::move(v)), expr(e) { }
    std::string accept(Visitor& visitor) override;
};

struct IfStmt : Statement {
    Expr* cond;
    std::vector<Statement*> then_statements;
    std::vector<Statement*> else_statements;
    IfStmt(Expr* c, const std::vector<Statement*>& t, const std::vector<Statement*>& e)
        : cond(c), then_statements(t), else_statements(e) { }
    std::string accept(Visitor& visitor) override;
};

struct ExprStatement : Statement {
    Expr* expr { };
    explicit ExprStatement(Expr* e) : expr(e) { }
    std::string accept(Visitor& visitor) override;
};

struct Visitor {
    virtual std::string visit_node(ASTNode& node) { return ""; }

    virtual std::string visit_def(FuncDef& node) { return visit_node(node); }
    virtual std::string visit_decl(FuncDecl& node) { return visit_node(node); }

    virtual std::string visit_expr(Expr& node) { assert(false && "This method shouldn't be reached, or should be overridden"); }
    virtual std::string visit_bin_expr(BinaryExpr& node) { return visit_expr(node); }
    virtual std::string visit_call(CallExpr& node) { return visit_expr(node); }
    virtual std::string visit_read(ReadExpr& node) { return visit_expr(node); }
    virtual std::string visit_var(VarExpr& node) { return visit_expr(node); }
    virtual std::string visit_num(NumberExpr& node) { return visit_expr(node); }

    virtual std::string visit_statement(Statement& node) { return visit_node(node); }
    virtual std::string visit_block(BlockStmt& node) { return visit_statement(node); }
    virtual std::string visit_return(ReturnStmt& node) { return visit_statement(node); }
    virtual std::string visit_print(PrintStmt& node) { return visit_statement(node); }
    virtual std::string visit_assign(AssignStmt& node) { return visit_statement(node); }
    virtual std::string visit_if(IfStmt& node) { return visit_statement(node); }
    virtual std::string visit_expr_statement(ExprStatement& node) { return visit_statement(node); }

    virtual ~Visitor() = default;
};

std::string ASTNode::accept(Visitor &visitor) { return visitor.visit_node(*this); }
std::string Expr::accept(Visitor &visitor) { return visitor.visit_expr(*this); }
std::string BinaryExpr::accept(Visitor &visitor) { return visitor.visit_bin_expr(*this); }
std::string CallExpr::accept(Visitor &visitor) { return visitor.visit_call(*this); }
std::string ReadExpr::accept(Visitor &visitor) { return visitor.visit_read(*this); }
std::string VarExpr::accept(Visitor &visitor) { return visitor.visit_var(*this); }
std::string NumberExpr::accept(Visitor &visitor) { return visitor.visit_num(*this); }
std::string Statement::accept(Visitor &visitor) { return visitor.visit_statement(*this); }
std::string BlockStmt::accept(Visitor &visitor) { return visitor.visit_block(*this); }
std::string ReturnStmt::accept(Visitor &visitor) { return visitor.visit_return(*this); }
std::string PrintStmt::accept(Visitor &visitor) { return visitor.visit_print(*this); }
std::string AssignStmt::accept(Visitor &visitor) { return visitor.visit_assign(*this); }
std::string IfStmt::accept(Visitor &visitor) { return visitor.visit_if(*this); }
std::string ExprStatement::accept(Visitor &visitor) { return visitor.visit_expr_statement(*this); }
std::string FuncDef::accept(Visitor &visitor) { return visitor.visit_def(*this); }
std::string FuncDecl::accept(Visitor &visitor) { return visitor.visit_decl(*this); }

struct IntermediateCode {
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
        if (peek() == NUMBER) {
            const int val = std::stoi(current.value);
            consume(NUMBER);
            return new_node<NumberExpr>(val);
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
            return new_node<VarExpr>(name);
        }

        if (peek() == CALL) {
            consume(CALL);
            std::string name = current.value;
            consume(IDENTIFIER);
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

        if (peek() == LPAREN) {
            consume(LPAREN);
            Expr* e = parse_binary_expr();
            consume(RPAREN);
            return e;
        }

        throw std::runtime_error("Invalid expression");
    }

    Expr* parse_binary_expr(const int precedence = 0) {
        Expr* left = parse_expr();
        while (true) {
            TokenType op = peek();
            const int new_precedence = (op == PLUS || op == MINUS) ? 10 : (op == MUL || op == DIV) ? 20 : (op == GT) ? 5 : 0;
            if (new_precedence <= precedence) break;
            consume(op);
            Expr* right = parse_binary_expr(new_precedence);
            left = new_node<BinaryExpr>(op == PLUS ? "+" : op == MINUS ? "-" : op == MUL ? "*" : op == DIV ? "/" : ">", left, right);
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
            Expr* cond = parse_binary_expr();
            consume(RPAREN);
            consume(LBRACE);
            std::vector<Statement*> thenStmts;
            while (peek() != RBRACE) {
                thenStmts.push_back(parse_statement());
            }
            consume(RBRACE);
            consume(ELSE);
            consume(LBRACE);
            std::vector<Statement*> elseStmts;
            while (peek() != RBRACE) {
                elseStmts.push_back(parse_statement());
            }
            consume(RBRACE);
            return new_node<IfStmt>(cond, thenStmts, elseStmts);
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
            while (peek() != RBRACE) {
                statements.push_back(parse_statement());
            }
            consume(RBRACE);
            return new_node<BlockStmt>(statements);
        }

        if (peek() == IDENTIFIER) {
            const std::string var = current.value;
            consume(IDENTIFIER);
            consume(EQ);
            Expr* e = parse_binary_expr();
            consume(SEMI);
            return new_node<AssignStmt>(var, e);
        }

        auto expr_statement = new_node<ExprStatement>(parse_expr());
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
        const auto body = parse_statement();
        return new_node<FuncDef>(name, params, body);
    }

    FuncDecl* parse_func_decl() {
        consume(EXTERN);
        const std::string name = current.value;
        const auto params = parse_func_common();
        consume(SEMI);
        return new_node<FuncDecl>(name, params);
    }

    IntermediateCode parse() {
        while (peek() != EOF_TOKEN) {
            if (peek() == DEF) {
                intermediate_code.functions.push_back(parse_func_def());
            } else if (peek() == EXTERN) {
                intermediate_code.declarations.push_back(parse_func_decl());
            } else {
                throw std::runtime_error("Global values are not allowed: " + current.value);
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
    std::string func_name;
    int reg = 0;

    std::ostringstream& header() { return _header; }
    std::ostringstream& code() { return _code; }

    void reset() {
        reg = 0;
        defined_vars.clear();
    }

    std::string consume_reg() { return "%reg" + std::to_string(reg++); }

    void define_var(const std::string& name) {
        bool is_new = defined_vars.insert(name).second;
        if (is_new) {
            code() << "  %" << name << " = alloca i32\n";
        }
    }

public:

    explicit CodeGenerator(OutStream& out) : out(&out) { }

    std::string visit_def(FuncDef &node) override {
        reset();
        func_name = node.name;
        code() << "define i32 @" << node.name << "(";
        for (size_t i = 0; i < node.params.size(); ++i) {
            code() << "i32 %" << node.params[i];
            if (i < node.params.size() - 1) code() << ", ";
        }
        code() << ") #0 {\n";
        code() << "entry:\n";
        for (const auto& param : node.params) {
            auto param_name = param + "." + node.name;
            define_var(param_name);
            code() << "  store i32 %" << param << ", i32* %" << param_name << "\n";
        }
        node.body->accept(*this);
        code() << "  ret i32 0\n";
        code() << "}\n\n";
        return "";
    }

    std::string visit_decl(FuncDecl &node) override {
        header() << "declare i32 @" << node.name << "(";
        for (size_t i = 0; i < node.params.size(); ++i) {
            header() << "i32";
            if (i < node.params.size() - 1) header() << ", ";
        }
        header() << ")\n";
        return "";
    }

    std::string visit_bin_expr(BinaryExpr &node) override {
        std::string l = node.left->accept(*this);
        std::string r = node.right->accept(*this);
        std::string inst;
        if (node.op == "+") inst = "add";
        else if (node.op == "-") inst = "sub";
        else if (node.op == "*") inst = "mul";
        else if (node.op == "/") inst = "sdiv";
        else if (node.op == ">") inst = "icmp sgt";
        std::string res = consume_reg();
        code() << "  " << res << " = " << inst << " i32 " << l << ", " << r << "\n";
        return res;
    }

    std::string visit_call(CallExpr &node) override {
        std::vector<std::string> arg_regs;
        arg_regs.reserve(node.args.size());
        for (auto* arg : node.args) {
            arg_regs.push_back(arg->accept(*this));
        }
        std::string res = consume_reg();
        code() << "  " << res << " = call i32 @" << node.func_name << "(";
        for (size_t i = 0; i < arg_regs.size(); ++i) {
            code() << "i32 " << arg_regs[i];
            if (i < arg_regs.size() - 1) code() << ", ";
        }
        code() << ")\n";
        return res;
    }

    std::string visit_read(ReadExpr &node) override {
        const std::string tmp = "%tmp" + std::to_string(reg);
        std::string res = consume_reg();
        code() << "  " << tmp << " = alloca i32\n";
        code() << "  call i32 (i8*, ...) @scanf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @.str.read, i32 0, i32 0), i32* " << tmp << ")\n";
        code() << "  " << res << " = load i32, i32* " << tmp << "\n";
        return res;
    }

    std::string visit_var(VarExpr &node) override {
        std::string res = consume_reg();
        code() << "  " << res << " = load i32, i32* %" << node.name << "." << func_name << "\n";
        return res;
    }

    std::string visit_num(NumberExpr &node) override {
        return std::to_string(node.value);
    }

    std::string visit_block(BlockStmt &node) override {
        for (auto* statement : node.statements) {
            statement->accept(*this);
        }
        return "";
    }

    std::string visit_return(ReturnStmt &node) override {
        std::string val = node.expr->accept(*this);
        code() << "  ret i32 " << val << "\n";
        return "";
    }

    std::string visit_print(PrintStmt &node) override {
        const std::string val = node.expr->accept(*this);
        code() << "  call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.str, i32 0, i32 0), i32 " << val << ")\n";
        return "";
    }

    std::string visit_assign(AssignStmt &node) override {
        auto var_name = node.var + "." + func_name;
        define_var(var_name);
        std::string val = node.expr->accept(*this);
        code() << "  store i32 " << val << ", i32* %" << var_name << "\n";
        return "";
    }

    std::string visit_if(IfStmt &node) override {
        const std::string condition_val = node.cond->accept(*this);
        const std::string then_label = "then" + std::to_string(reg);
        const std::string else_label = "else" + std::to_string(reg);
        const std::string end_label = "end" + std::to_string(reg);
        reg++;
        code() << "  br i1 " << condition_val << ", label %" << then_label << ", label %" << else_label << "\n";
        code() << then_label << ":\n";
        for (auto* statement : node.then_statements) {
            statement->accept(*this);
        }
        code() << "  br label %" << end_label << "\n";
        code() << else_label << ":\n";
        for (auto* statement : node.else_statements) {
            statement->accept(*this);
        }
        code() << "  br label %" << end_label << "\n";
        code() << end_label << ":\n";
        return "";
    }

    std::string visit_expr_statement(ExprStatement &node) override {
        node.expr->accept(*this);
        return "";
    }

    void generate(const IntermediateCode& ir) {
        header() << "; ModuleID = 'simple'\n";
        header() << "@.str = private constant [4 x i8] c\"%d\\0A\\00\"\n";
        header() << "@.str.read = private constant [3 x i8] c\"%d\\00\"\n";

        for (auto func : ir.functions)
            func->accept(*this);

        header() << "declare i32 @scanf(i8*, ...)\n";
        header() << "declare i32 @printf(i8*, ...)\n";
        for (auto decl : ir.declarations)
            decl->accept(*this);

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
    Compiler(FrontEnd frontend, BackEnd backend)
        : frontend(std::move(frontend)), backend(std::move(backend)) { }

    void run() {
        IntermediateCode ir = frontend.process();
        backend.process(ir);
    }
};

int main() {
    std::string src = R"(
        def func() = {
            x = 5;
            return x;
        }

        def p(a) = print(call func() + a);

        def main() = {
            call p(3);
            x = 5;
            call p(4);
        }
    )";
    std::stringstream ss(src);

    FrontEnd frontend(ss);
    BackEnd backend(std::cout);
    Compiler compiler { std::move(frontend), std::move(backend) };
    compiler.run();
}