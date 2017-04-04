#include <cctype>
#include <cstdio>
#include <map>
#include <memory>
#include <string>
#include <utility>
#include <vector>

enum Token {
    TOK_EOF = -1,
    TOK_DEF = -2,
    TOK_EXTERN = -3,
    TOK_IDENTIFIER = -4,
    TOK_NUMBER = -5
};

std::string gIdentifierStr;
double gNumVal;

int GetToken() {
    static int last_char = ' ';
    while (isspace(last_char)) {
        last_char = getchar();
    }

    if (isalpha(last_char)) {
        gIdentifierStr = last_char;
        while (isalnum((last_char = getchar()))) {
            gIdentifierStr += last_char;
        }
        if (gIdentifierStr == "def") {
            return TOK_DEF;
        }
        if (gIdentifierStr == "extern") {
            return TOK_EXTERN;
        }
        return TOK_IDENTIFIER;
    }

    if (isdigit(last_char) || last_char == '.') {
        std::string gNumStr;
        do {
            gNumStr += last_char;
            last_char = getchar();
        } while (isdigit(last_char) || last_char == '.');
        gNumVal = stod(gNumStr);
        return TOK_NUMBER;
    }

    if (last_char == '#') {
        do {
            last_char = getchar();
        } while (last_char != EOF and last_char != '\n' and last_char != '\r');
        if (last_char != EOF) {
            return GetToken();
        }
    }
    if (last_char == EOF) {
        return TOK_EOF;
    }
    int ThisChar = last_char;
    last_char = getchar();
    return ThisChar;
}

// parser

class ExprAST {
  public:
    virtual ~ExprAST(){};
};

class NumberExprAST : public ExprAST {
    double val_;

  public:
    NumberExprAST(double val) : val_(val){};
};

class VariableExprAST : public ExprAST {
    std::string name_;

  public:
    VariableExprAST(const std::string& name) : name_(name){};
};

class BinaryExprAST : public ExprAST {
    char op_;
    std::unique_ptr<ExprAST> lhs_, rhs_;

  public:
    BinaryExprAST(
        char op, std::unique_ptr<ExprAST> lhs, std::unique_ptr<ExprAST> rhs)
        : op_(op), lhs_(std::move(lhs)), rhs_(std::move(rhs)) {}
};

class CallExprAST : public ExprAST {
    std::string callee_;
    std::vector<std::unique_ptr<ExprAST>> args_;

  public:
    CallExprAST(
        const std::string& callee, std::vector<std::unique_ptr<ExprAST>> args)
        : callee_(callee), args_(std::move(args)){};
};

class PrototypeAST {
    std::string name_;
    std::vector<std::string> args_;

  public:
    PrototypeAST(const std::string& name, std::vector<std::string> args)
        : name_(name), args_(std::move(args)) {}

    const std::string& GetName() const {
        return name_;
    }
};

class FunctionAST {
    std::unique_ptr<PrototypeAST> proto_;
    std::unique_ptr<ExprAST> body_;

  public:
    FunctionAST(
        std::unique_ptr<PrototypeAST> proto, std::unique_ptr<ExprAST> body)
        : proto_(std::move(proto)), body_(std::move(body)) {}
};

int gCurTok;
int GetNextToken() {
    return gCurTok = GetToken();
}

std::unique_ptr<ExprAST> LogError(const std::string& str) {
    fprintf(stderr, "LogError: %s\n", str.data());
    return nullptr;
}

std::unique_ptr<PrototypeAST> LogErrorP(const std::string& str) {
    LogError(str);
    return nullptr;
}

std::unique_ptr<ExprAST> ParseExpression();

/// numberexpr -> number
std::unique_ptr<ExprAST> ParseNumberExpr() {
    auto result = std::make_unique<NumberExprAST>(gNumVal);
    GetNextToken();
    return std::move(result);
}

/// parenexpr -> '(' expression ')'
std::unique_ptr<ExprAST> ParseParenExpr() {
    GetNextToken();
    auto v = ParseExpression();
    if (!v)
        return nullptr;
    if (gCurTok != ')') {
        return LogError("expect ')'");
    }
    GetNextToken();
    return v;
}

/// identifierexpr -> identifier
///                 | identifier '(' expression* ')'
std::unique_ptr<ExprAST> ParseIdentifierExpr() {
    std::string id_name = gIdentifierStr;
    GetNextToken();
    if (gCurTok != '(') {
        return std::make_unique<VariableExprAST>(id_name);
    }

    GetNextToken();
    std::vector<std::unique_ptr<ExprAST>> args;
    if (gCurTok != ')') {
        for (;;) {
            if (auto arg = ParseExpression()) {
                args.push_back(std::move(arg));
            } else {
                return nullptr;
            }
            if (gCurTok == ')') {
                break;
            }
            if (gCurTok != ',') {
                return LogError("expect ')' or ',' in argument list");
            }
            GetNextToken();  // consume ','
        }
    }
    GetNextToken();  // consume ')'
    return std::make_unique<CallExprAST>(id_name, std::move(args));
}

/// primary -> identifierexpr | numberexpr | parenexpr
std::unique_ptr<ExprAST> ParsePrimary() {
    switch (gCurTok) {
        case TOK_IDENTIFIER:
            return ParseIdentifierExpr();
        case TOK_NUMBER:
            return ParseNumberExpr();
        case '(':
            return ParseParenExpr();
        default:
            return LogError("unknown token when expecting an expression");
    }
}

std::map<char, int> gBinopPrecedence;

int GetTokPrecedence() {
    if (not isascii(gCurTok)) {
        return -1;
    }
    auto prec_iter = gBinopPrecedence.find(gCurTok);
    if (prec_iter == gBinopPrecedence.end()) {
        return -1;
    }
    return prec_iter->second;
}

/// binoprhs -> ('+' primary)*
std::unique_ptr<ExprAST> ParseBinOpRHS(
    int expr_prec, std::unique_ptr<ExprAST> lhs) {
    for (;;) {
        int token_prec = GetTokPrecedence();
        if (token_prec < expr_prec) {
            return lhs;
        }
        int op = gCurTok;
        GetNextToken();
        auto rhs = ParsePrimary();
        if (not rhs) {
            return nullptr;
        }
        int next_prec = GetTokPrecedence();
        if (token_prec < next_prec) {
            rhs = ParseBinOpRHS(token_prec + 1, std::move(rhs));
            if (not rhs) {
                return nullptr;
            }
        }

        lhs =
            std::make_unique<BinaryExprAST>(op, std::move(lhs), std::move(rhs));
    }
}

std::unique_ptr<ExprAST> ParseExpression() {
    auto lhs = ParsePrimary();
    if (not lhs) {
        return nullptr;
    }
    return ParseBinOpRHS(0, std::move(lhs));
}

/// prototype -> id '(' id* ')'
std::unique_ptr<PrototypeAST> ParsePrototype() {
    if (gCurTok != TOK_IDENTIFIER) {
        return LogErrorP("Expected function name in prototype");
    }
    std::string fn_name = gIdentifierStr;
    GetNextToken();
    if (gCurTok != '(') {
        return LogErrorP("Expect '(' in prototype");
    }

    std::vector<std::string> arg_names;
    for (; GetNextToken() == TOK_IDENTIFIER;) {
        arg_names.push_back(gIdentifierStr);
    }
    if (gCurTok != ')') {
        return LogErrorP("Expect ')' in prototype");
    }

    GetNextToken();
    return std::make_unique<PrototypeAST>(fn_name, std::move(arg_names));
}

/// definition -> 'def' prototype expression
std::unique_ptr<FunctionAST> ParseDefinition() {
    GetNextToken(); // consume 'def'
    auto proto = ParsePrototype();
    if (not proto) {
        return nullptr;
    }
    if (auto expr = ParseExpression()) {
        return std::make_unique<FunctionAST>(std::move(proto), std::move(expr));
    }
    return nullptr;
}

/// external -> 'extern' prototype
std::unique_ptr<PrototypeAST> ParseExtern() {
    GetNextToken(); // consume 'extern'
    return ParsePrototype();
}

/// toplevelexpr -> expression
std::unique_ptr<FunctionAST> ParseTopLevelExpr() {
    if (auto expr = ParseExpression()) {
        auto proto = std::make_unique<PrototypeAST>("", std::vector<std::string>());
        return std::make_unique<FunctionAST>(std::move(proto), std::move(expr));
    }
    return nullptr;
}

//===----------------------------------------------------------------------===//
// Top-Level parsing
//===----------------------------------------------------------------------===//

static void HandleDefinition() {
  if (ParseDefinition()) {
    fprintf(stderr, "Parsed a function definition.\n");
  } else {
    // Skip token for error recovery.
    GetNextToken();
  }
}

static void HandleExtern() {
  if (ParseExtern()) {
    fprintf(stderr, "Parsed an extern\n");
  } else {
    // Skip token for error recovery.
    GetNextToken();
  }
}

static void HandleTopLevelExpression() {
  // Evaluate a top-level expression into an anonymous function.
  if (ParseTopLevelExpr()) {
    fprintf(stderr, "Parsed a top-level expr\n");
  } else {
    // Skip token for error recovery.
    GetNextToken();
  }
}

/// top ::= definition | external | expression | ';'
static void MainLoop() {
  while (true) {
    fprintf(stderr, "ready> ");
    switch (gCurTok) {
    case TOK_EOF:
      return;
    case ';': // ignore top-level semicolons.
      GetNextToken();
      break;
    case TOK_DEF:
      HandleDefinition();
      break;
    case TOK_EXTERN:
      HandleExtern();
      break;
    default:
      HandleTopLevelExpression();
      break;
    }
  }
}

//===----------------------------------------------------------------------===//
// Main driver code.
//===----------------------------------------------------------------------===//

int main() {
  // Install standard binary operators.
  // 1 is lowest precedence.
  gBinopPrecedence['<'] = 10;
  gBinopPrecedence['+'] = 20;
  gBinopPrecedence['-'] = 20;
  gBinopPrecedence['*'] = 40; // highest.

  // Prime the first token.
  fprintf(stderr, "ready> ");
  GetNextToken();

  // Run the main "interpreter loop" now.
  MainLoop();

  return 0;
}

