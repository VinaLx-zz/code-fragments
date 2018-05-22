#include "Parser.h"
#include <memory>

using vinalx::calc::Parser;
using vinalx::calc::Token;
using vinalx::calc::Expression;
using std::unique_ptr;

unique_ptr<Expression> Parser::Parse() {
    std::unique_ptr<Expression> expression = GetExpression();
    if (not expression) {
        throw SyntaxError("Expression");
    }
    if (cur_idx_ != kFinishIndex) {
        throw SyntaxError(tokens_[cur_idx_]);
    }
    return expression;
}

unique_ptr<Expression> Parser::GetExpression() {
    if (cur_idx_ == kFinishIndex) {
        throw SyntaxError("Expression");
    }
    std::unique_ptr<Expression> term = GetTerm();
    if (cur_idx_ == kFinishIndex) {
        return term;
    }
    if (tokens_[cur_idx_].type != Token::LOW_OPERATOR) {
        return term;
    }

    std::unique_ptr<Operation> expression;
    if (tokens_[cur_idx_].content == "+") {
        expression = std::make_unique<Addition>();
    } else if (tokens_[cur_idx_].content == "-") {
        expression = std::make_unique<Minus>();
    } else {
        assert(false and "Invalid Token !");
    }
    expression->right = std::move(term);

    --cur_idx_;
    expression->left = GetExpression();
    return std::move(expression);
}

unique_ptr<Expression> Parser::GetTerm() {
    if (cur_idx_ == kFinishIndex) {
        throw SyntaxError("Term");
    }
    std::unique_ptr<Expression> factor = GetFactor();
    if (cur_idx_ == kFinishIndex) {
        return factor;
    }
    if (tokens_[cur_idx_].type != Token::HIGH_OPERATOR) {
        return factor;
    }
    std::unique_ptr<Operation> expression;
    if (tokens_[cur_idx_].content == "*") {
        expression = std::make_unique<Multiplication>();
    } else if (tokens_[cur_idx_].content == "/") {
        expression = std::make_unique<Division>();
    } else {
        assert(false and "Invalid Token !");
    }

    expression->right = std::move(factor);

    --cur_idx_;
    expression->left = GetTerm();
    return std::move(expression);
}

unique_ptr<Expression> Parser::GetFactor() {
    if (cur_idx_ == kFinishIndex) {
        throw SyntaxError("Factor");
    }
    std::unique_ptr<Expression> expression;
    if (tokens_[cur_idx_].type == Token::RIGHT_PARENTH) {
        --cur_idx_;
        expression = GetExpression();
        if (tokens_[cur_idx_].type != Token::LEFT_PARENTH) {
            throw SyntaxError(tokens_[cur_idx_], Token::RIGHT_PARENTH);
        }
        --cur_idx_;
        return expression;
    }
    std::unique_ptr<Expression> number;
    if (tokens_[cur_idx_].type == Token::INTEGER or
        tokens_[cur_idx_].type == Token::FLOATING) {
        number = std::make_unique<Number>(stod(tokens_[cur_idx_].content));
    } else {
        throw SyntaxError(tokens_[cur_idx_], Token::INTEGER);
    }
    --cur_idx_;
    return number;
}
