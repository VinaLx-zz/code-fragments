#include "Scanner.h"
#include <cctype>
#include "Token.h"

using vinalx::calc::Scanner;
using vinalx::calc::Token;

Scanner::Scanner(const std::string& text) : text_(text), position_(0) {}

Scanner::Recorder::Recorder(int64_t start)
    : start_(start), latest_acc_(start), token_type_(Token::Type::ERROR) {}

void Scanner::Recorder::UpdateRecord(State state, int64_t position) {
    token_type_ = StateToToken(state);
    if (token_type_ != Token::ERROR) {
        latest_acc_ = position;
    }
}

Token Scanner::Recorder::CollectResult(
    const std::string& text, int64_t* new_position) {
    if (token_type_ == Token::ERROR) {
        throw InvalidToken(text.substr(start_, *new_position - start_), start_);
    }
    *new_position = latest_acc_;
    return Token(token_type_, text.substr(latest_acc_ - start_));
}

Token::Type Scanner::StateToToken(State state) {
    if (state == State::LOW_OPERATOR) {
        return Token::LOW_OPERATOR;
    }
    if (state == State::HIGH_OPERATOR) {
        return Token::HIGH_OPERATOR;
    }
    if (state == State::LEFT_PARENTH) {
        return Token::LEFT_PARENTH;
    }
    if (state == State::RIGHT_PARENTH) {
        return Token::RIGHT_PARENTH;
    }
    if (state == State::INTEGER_ACC) {
        return Token::INTEGER;
    }
    if (state == State::FLOAT_ACC) {
        return Token::FLOATING;
    }
    return Token::ERROR;
}

bool Scanner::IsValidChar(char c) {
    return std::isdigit(c) or std::isspace(c) or c == '+' or c == '-' or
           c == '*' or c == '/' or c == '(' or c == ')' or c == '.';
}

std::vector<Token> Scanner::Scan() {
    std::vector<Token> tokens;
    for (; position_ < text_.size();) {
        tokens.push_back(GetToken());
    }
    return tokens;
}

Token Scanner::GetToken() {
    State state = State::START;
    Recorder recorder(position_);
    for (; state != State::END;) {
        char next_char = NextChar();
        state = StateDispatch(state, next_char);
        recorder.UpdateRecord(state, position_);
    }
    return recorder.CollectResult(text_, &position_);
}

char Scanner::NextChar() {
    char next_char = text_[position_];
    if (not IsValidChar(next_char)) {
        throw InvalidChar(next_char, position_);
    }
    ++position_;
    return next_char;
}

Scanner::State Scanner::StateDispatch(State state, char next_char) {
    return state_handler_[state](next_char);
}

Scanner::State Scanner::StartHandler(char next_char) {
    if (next_char == '+' or next_char == '-') {
        return LOW_OPERATOR;
    }
    if (next_char == '*' or next_char == '/') {
        return HIGH_OPERATOR;
    }
    if (next_char == '(') {
        return LEFT_PARENTH;
    }
    if (next_char == ')') {
        return RIGHT_PARENTH;
    }
    if (next_char == '.') {
        return FLOAT_DOT;
    }
    if (isdigit(next_char)) {
        return INTEGER_ACC;
    }
    return END;
}

Scanner::State Scanner::SingleCharHandler(char next_char) {
    return END;
}

Scanner::State Scanner::IntAccHandler(char next_char) {
    if (next_char == '.') {
        return FLOAT_ACC;
    }
    if (isdigit(next_char)) {
        return INTEGER_ACC;
    }
    return END;
}

Scanner::State Scanner::FloatAccHandler(char next_char) {
    if (isdigit(next_char)) {
        return FLOAT_ACC;
    }
    return END;
}

Scanner::State Scanner::FloatDotHandler(char next_char) {
    if (isdigit(next_char)) {
        return FLOAT_ACC;
    }
    return END;
}
