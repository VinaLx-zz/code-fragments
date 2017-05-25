#include <cctype>
#include <cstdio>
#include <map>
#include <memory>
#include <string>
#include <utility>
#include <vector>
#include "llvm/ADT/APFloat.h"            // llvm::APFloat
#include "llvm/IR/Function.h"            // llvm::Function
#include "llvm/IR/IRBuilder.h"           // llvm::Value, llvm::IRBuilder
#include "llvm/IR/LLVMContext.h"         // llvm::LLVMContext
#include "llvm/IR/LegacyPassManager.h"   // llvm::legacy::FunctionPassManager
#include "llvm/IR/Module.h"              // llvm::Module
#include "llvm/IR/Type.h"                // llvm::Type
#include "llvm/IR/Verifier.h"            // llvm::VerifyFunction
#include "llvm/Transforms/Scalar.h"      // llvm::createReassociatePass
#include "llvm/Transforms/Scalar/GVN.h"  // llvm::createGVNPass

using namespace llvm;

enum Token {
    TOK_EOF = -1,
    TOK_DEF = -2,
    TOK_EXTERN = -3,
    TOK_IDENTIFIER = -4,
    TOK_NUMBER = -5,
    TOK_IF = -6,
    TOK_THEN = -7,
    TOK_ELSE = -8,
    TOK_FOR = -9,
    TOK_IN = -10
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
        if (gIdentifierStr == "if") {
            return TOK_IF;
        }
        if (gIdentifierStr == "else") {
            return TOK_ELSE;
        }
        if (gIdentifierStr == "then") {
            return TOK_THEN;
        }
        if (gIdentifierStr == "for") {
            return TOK_FOR;
        }
        if (gIdentifierStr == "in") {
            return TOK_IN;
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

llvm::LLVMContext gContext;
llvm::IRBuilder<> gBuilder(gContext);
std::unique_ptr<llvm::Module> gModule;
std::map<std::string, Value*> gNamedValues;

// chapeter 4: optimization

std::unique_ptr<llvm::legacy::FunctionPassManager> gFPM;

void InitializeModuleAndPassManager() {
    gModule = std::make_unique<llvm::Module>("my cool jit", gContext);
    gFPM = std::make_unique<llvm::legacy::FunctionPassManager>(gModule.get());
    gFPM->add(llvm::createInstructionCombiningPass());
    gFPM->add(llvm::createReassociatePass());
    gFPM->add(llvm::createGVNPass());
    gFPM->add(llvm::createCFGSimplificationPass());
    gFPM->doInitialization();
}

// parser

class ExprAST {
  public:
    virtual ~ExprAST(){};
    virtual Value* CodeGen() = 0;
};

std::unique_ptr<ExprAST> LogError(const std::string& str) {
    fprintf(stderr, "LogError: %s\n", str.data());
    return nullptr;
}

llvm::Value* LogErrorV(const std::string& str) {
    LogError(str);
    return nullptr;
}

class NumberExprAST : public ExprAST {
    double val_;

  public:
    NumberExprAST(double val) : val_(val){};
    virtual Value* CodeGen() {
        return llvm::ConstantFP::get(gContext, llvm::APFloat(val_));
    }
};

class VariableExprAST : public ExprAST {
    std::string name_;

  public:
    VariableExprAST(const std::string& name) : name_(name){};

    virtual Value* CodeGen() {
        auto iter = gNamedValues.find(name_);
        if (iter == end(gNamedValues)) {
            return LogErrorV("Unknown variable name " + name_);
        }
        return iter->second;
    }
};

class BinaryExprAST : public ExprAST {
    char op_;
    std::unique_ptr<ExprAST> lhs_, rhs_;

  public:
    BinaryExprAST(
        char op, std::unique_ptr<ExprAST> lhs, std::unique_ptr<ExprAST> rhs)
        : op_(op), lhs_(std::move(lhs)), rhs_(std::move(rhs)) {}

    virtual Value* CodeGen() {
        Value* lhs_value = lhs_->CodeGen();
        Value* rhs_value = rhs_->CodeGen();
        if (not lhs_value or not rhs_value) {
            return nullptr;
        }
        switch (op_) {
            case '+':
                return gBuilder.CreateFAdd(lhs_value, rhs_value, "addtmp");
            case '-':
                return gBuilder.CreateFSub(lhs_value, rhs_value, "minustmp");
            case '*':
                return gBuilder.CreateFMul(lhs_value, rhs_value, "multitmp");
            case '<':
                return gBuilder.CreateUIToFP(
                    gBuilder.CreateFCmpULT(lhs_value, rhs_value, "booltmp"),
                    llvm::Type::getDoubleTy(gContext));
            default:
                return LogErrorV(std::string("invalid binary operator ") + op_);
        }
    }
};

class CallExprAST : public ExprAST {
    std::string callee_;
    std::vector<std::unique_ptr<ExprAST>> args_;

  public:
    CallExprAST(
        const std::string& callee, std::vector<std::unique_ptr<ExprAST>> args)
        : callee_(callee), args_(std::move(args)){};
    virtual Value* CodeGen() {
        Function* callee = gModule->getFunction(callee_);
        if (not callee) {
            return LogErrorV("unknown function " + callee_ + " referenced");
        }
        if (callee->arg_size() != args_.size()) {
            return LogErrorV(
                "Incorrect argument passed, expect " +
                std::to_string(callee->arg_size()));
        }
        std::vector<Value*> args;
        for (auto& arg : args_) {
            Value* va = arg->CodeGen();
            if (not va) {
                return nullptr;
            }
            args.push_back(va);
        }
        return gBuilder.CreateCall(callee, args, "calltmp");
    }
};

class PrototypeAST {
    std::string name_;
    std::vector<std::string> args_;

  public:
    PrototypeAST(const std::string& name, std::vector<std::string> args)
        : name_(name), args_(std::move(args)) {}

    virtual Function* CodeGen() {
        std::vector<Type*> doubles(
            args_.size(), llvm::Type::getDoubleTy(gContext));
        llvm::FunctionType* ft =
            FunctionType::get(Type::getDoubleTy(gContext), doubles, false);
        llvm::Function* func = llvm::Function::Create(
            ft, Function::ExternalLinkage, name_, gModule.get());
        int i = 0;
        for (auto& arg : func->args()) {
            arg.setName(args_[i++]);
        }
        return func;
    }

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

    virtual Value* CodeGen() {
        Function* f = gModule->getFunction(proto_->GetName());
        if (not f) {
            f = proto_->CodeGen();
        }
        if (not f) {
            return nullptr;
        }
        if (not f->empty()) {
            return LogErrorV("Function redefinition of " + proto_->GetName());
        }
        llvm::BasicBlock* block =
            llvm::BasicBlock::Create(gContext, "entry", f);
        gBuilder.SetInsertPoint(block);

        gNamedValues.clear();
        for (auto& arg : f->args()) {
            gNamedValues[arg.getName()] = &arg;
        }

        if (Value* ret_val = body_->CodeGen()) {
            gBuilder.CreateRet(ret_val);
            llvm::verifyFunction(*f);

            gFPM->run(*f);

            return f;
        }
        f->eraseFromParent();
        return nullptr;
    }
};

class IfExprAST : public ExprAST {
    std::unique_ptr<ExprAST> cond_, then_, else_;

  public:
    IfExprAST(
        std::unique_ptr<ExprAST> cond, std::unique_ptr<ExprAST> then,
        std::unique_ptr<ExprAST> els)
        : cond_(std::move(cond)),
          then_(std::move(then)),
          else_(std::move(els)) {}

    Value* CodeGen() override {
        Value* condv = cond_->CodeGen();
        if (not condv)
            return nullptr;
        // On Not Equal
        condv = gBuilder.CreateFCmpONE(
            condv, ConstantFP::get(gContext, APFloat(0.0)), "ifcond");
        Function* the_function = gBuilder.GetInsertBlock()->getParent();
        BasicBlock* then_block =
            BasicBlock::Create(gContext, "then", the_function);
        BasicBlock* else_block = BasicBlock::Create(gContext, "else");
        BasicBlock* merge_block = BasicBlock::Create(gContext, "ifcont");
        gBuilder.CreateCondBr(condv, then_block, else_block);

        gBuilder.SetInsertPoint(then_block);
        Value* thenv = then_->CodeGen();
        if (not thenv) {
            return nullptr;
        }
        gBuilder.CreateBr(merge_block);
        then_block = gBuilder.GetInsertBlock();

        the_function->getBasicBlockList().push_back(else_block);
        gBuilder.SetInsertPoint(else_block);
        Value* elsev = else_->CodeGen();
        if (not elsev) {
            return nullptr;
        }
        gBuilder.CreateBr(merge_block);
        else_block = gBuilder.GetInsertBlock();

        the_function->getBasicBlockList().push_back(merge_block);
        gBuilder.SetInsertPoint(merge_block);
        llvm::PHINode* phi_node =
            gBuilder.CreatePHI(Type::getDoubleTy(gContext), 2, "iftmp");
        phi_node->addIncoming(thenv, then_block);
        phi_node->addIncoming(elsev, else_block);
        return phi_node;
    }
};

class ForExprAST : public ExprAST {
    std::string var_name_;
    std::unique_ptr<ExprAST> start_, end_, step_, body_;

  public:
    ForExprAST(
        const std::string& var_name, std::unique_ptr<ExprAST> start,
        std::unique_ptr<ExprAST> end, std::unique_ptr<ExprAST> step,
        std::unique_ptr<ExprAST> body)
        : var_name_(var_name),
          start_(std::move(start)),
          step_(std::move(step)),
          body_(std::move(body)) {}

    Value* CodeGen() override {
        // generate start value in original builder context
        Value* startv = start_->CodeGen();
        if (not startv) {
            return nullptr;
        }
        Function* the_function = gBuilder.GetInsertBlock()->getParent();
        // record pre_header_block
        BasicBlock* pre_header_block = gBuilder.GetInsertBlock();
        BasicBlock* loop_block =
            BasicBlock::Create(gContext, "loop", the_function);
        gBuilder.CreateBr(loop_block);
        gBuilder.SetInsertPoint(loop_block);
        PHINode* variable = gBuilder.CreatePHI(
            Type::getDoubleTy(gContext), 2, var_name_.data());
        variable->addIncoming(startv, pre_header_block);
        // create an inner block for the loop
        Value* oldval = gNamedValues[var_name_];
        gNamedValues[var_name_] = variable;
        if (not body_->CodeGen()) {
            return nullptr;
        }
        Value* stepv = nullptr;
        if (step_) {
            stepv = step_->CodeGen();
            if (not stepv) {
                return nullptr;
            }
        } else {
            stepv = ConstantFP::get(gContext, APFloat(1.0));
        }
        Value* next_value = gBuilder.CreateFAdd(variable, stepv, "nextvar");
        Value* endv = end_->CodeGen();
        if (not endv) {
            return nullptr;
        }
        endv = gBuilder.CreateFCmpONE(
            endv, ConstantFP::get(gContext, APFloat(0.0)), "loopcond");
        BasicBlock* loop_end_block = gBuilder.GetInsertBlock();
        BasicBlock* after_block =
            BasicBlock::Create(gContext, "afterloop", the_function);
        gBuilder.CreateCondBr(endv, loop_block, after_block);
        gBuilder.SetInsertPoint(after_block);
        variable->addIncoming(next_value, loop_end_block);
        // restore context
        if (oldval)
            gNamedValues[var_name_] = oldval;
        else
            gNamedValues.erase(var_name_);
        return Constant::getNullValue(Type::getDoubleTy(gContext));
    }
};

int gCurTok;
int GetNextToken() {
    return gCurTok = GetToken();
}

std::unique_ptr<PrototypeAST> LogErrorP(const std::string& str) {
    LogError(str);
    return nullptr;
}

std::unique_ptr<ExprAST> ParseExpression();
std::unique_ptr<ExprAST> ParseIfExpr();
std::unique_ptr<ExprAST> ParseForExpr();

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
        case TOK_IF:
            return ParseIfExpr();
        case TOK_FOR:
            return ParseForExpr();
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
    GetNextToken();  // consume 'def'
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
    GetNextToken();  // consume 'extern'
    return ParsePrototype();
}

/// toplevelexpr -> expression
std::unique_ptr<FunctionAST> ParseTopLevelExpr() {
    if (auto expr = ParseExpression()) {
        auto proto =
            std::make_unique<PrototypeAST>("", std::vector<std::string>());
        return std::make_unique<FunctionAST>(std::move(proto), std::move(expr));
    }
    return nullptr;
}

/// ifexpr -> 'if' expression 'then' expression 'else' expression
std::unique_ptr<ExprAST> ParseIfExpr() {
    GetNextToken();
    auto cond = ParseExpression();
    if (not cond)
        return nullptr;
    if (gCurTok != TOK_THEN) {
        return LogError("expect then");
    }
    GetNextToken();
    auto then = ParseExpression();
    if (not then) {
        return nullptr;
    }
    if (gCurTok != TOK_ELSE) {
        return LogError("expect else");
    }
    GetNextToken();
    auto els = ParseExpression();
    if (not els) {
        return nullptr;
    }
    return std::make_unique<IfExprAST>(
        std::move(cond), std::move(then), std::move(els));
}

std::unique_ptr<ExprAST> ParseForExpr() {
    GetNextToken();
    if (gCurTok != TOK_IDENTIFIER) {
        return LogError("expected identifier after for");
    }
    std::string id_name(gIdentifierStr);
    GetNextToken();
    if (gCurTok != '=')
        return LogError("expected '=' after for");
    GetNextToken();
    auto start = ParseExpression();
    if (not start) {
        return nullptr;
    }
    if (gCurTok != ',')
        return LogError("expected ',' after for start value");
    GetNextToken();
    auto end = ParseExpression();
    if (not end) {
        return nullptr;
    }
    std::unique_ptr<ExprAST> step;
    if (gCurTok == ',') {
        GetNextToken();
        step = ParseExpression();
        if (not step)
            return nullptr;
    }
    if (gCurTok != TOK_IN) {
        return LogError("expect 'in' after for");
    }
    GetNextToken();
    auto body = ParseExpression();
    if (not body) {
        return nullptr;
    }
    return std::make_unique<ForExprAST>(
        id_name, std::move(start), std::move(end), std::move(step),
        std::move(body));
}
//===----------------------------------------------------------------------===//
// Top-Level parsing and JIT Driver
//===----------------------------------------------------------------------===//

static void HandleDefinition() {
    if (auto FnAST = ParseDefinition()) {
        if (auto* FnIR = FnAST->CodeGen()) {
            fprintf(stderr, "Read function definition:");
            FnIR->print(errs());
            fprintf(stderr, "\n");
        }
    } else {
        // Skip token for error recovery.
        GetNextToken();
    }
}

static void HandleExtern() {
    if (auto ProtoAST = ParseExtern()) {
        if (auto* FnIR = ProtoAST->CodeGen()) {
            fprintf(stderr, "Read extern: ");
            FnIR->print(errs());
            fprintf(stderr, "\n");
        }
    } else {
        // Skip token for error recovery.
        GetNextToken();
    }
}

static void HandleTopLevelExpression() {
    // Evaluate a top-level expression into an anonymous function.
    if (auto FnAST = ParseTopLevelExpr()) {
        if (auto* FnIR = FnAST->CodeGen()) {
            fprintf(stderr, "Read top-level expression:");
            FnIR->print(errs());
            fprintf(stderr, "\n");
        }
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
            case ';':  // ignore top-level semicolons.
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
    gBinopPrecedence['*'] = 40;  // highest.

    // Prime the first token.
    fprintf(stderr, "ready> ");
    GetNextToken();

    InitializeModuleAndPassManager();

    // Run the main "interpreter loop" now.
    MainLoop();

    // Print out all of the generated code.
    gModule->print(errs(), nullptr);

    return 0;
}
