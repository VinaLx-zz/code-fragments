#ifndef VINALX_CALC_PARSER_H_
#define VINALX_CALC_PARSER_H_

#include "Expression.h"
#include "Scanner.h"
#include "Token.h"

namespace vinalx {

namespace calc {

class Parser {
  public:
    Parser(std::vector<Token>&& tokens)
        : tokens_(std::move(tokens)), cur_idx_(tokens_.size() - 1) {}

    /**
     * parse the tokens to a expression tree
     */
    std::unique_ptr<Expression> Parse();

  private:
    static constexpr int64_t kFinishIndex = -1;

    int64_t NextIndex();
    int64_t PrevIndex();

    std::unique_ptr<Expression> GetExpression();
    std::unique_ptr<Expression> GetTerm();
    std::unique_ptr<Expression> GetFactor();

    std::vector<Token> tokens_;

    int64_t cur_idx_;
};

class SyntaxError : public std::logic_error {
  public:
    SyntaxError(const Token& token, Token::Type expected)
        : logic_error(MakeMessage(token, expected)) {}

    SyntaxError(const std::string& expected)
        : logic_error(MakeMessage(expected)) {}

    SyntaxError(const Token& token) : logic_error(MakeMessage(token)) {}

  private:
    static std::string MakeMessage(const Token& token, Token::Type expected) {
        return "SyntaxError: Invalid Token: " + token.content + ", expect " +
               Token::TypeString(expected);
    }

    static std::string MakeMessage(const std::string& expected) {
        return "SyntaxError: Expect " + expected;
    }

    static std::string MakeMessage(const Token& token) {
        return "SyntaxError: Invalid Token: " + token.content;
    }
};

}  // namespace calc

}  // namespace vinalx

#endif  // VINALX_CALC_PARSER_H_
