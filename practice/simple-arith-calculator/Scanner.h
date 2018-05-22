#ifndef VINALX_CALC_SCANNER_H_
#define VINALX_CALC_SCANNER_H_

#include <cassert>
#include <cstdint>
#include <stdexcept>
#include <string>
#include <vector>
#include "Token.h"

namespace vinalx {

namespace calc {

class Scanner {
  public:
    Scanner(const std::string& text);
    /**
     * turn a input string to the sequence of tokens
     * @param text  input of the scanner
     */
    std::vector<Token> Scan();

  private:
    enum State {
        START = 0,          // start state
        LOW_OPERATOR = 1,   // '+', '-'
        HIGH_OPERATOR = 2,  // '*', '/'
        LEFT_PARENTH = 3,   // '('
        RIGHT_PARENTH = 4,  // ')'
        INTEGER_ACC = 5,    // accept state for integer
        FLOAT_DOT = 6,      // a middle state to the FLOAT_ACC
        FLOAT_ACC = 7,      // accept state for floating point
        END = 8,  // end state, collect previous accept token or report error
    };

    class Recorder {
      public:
        Recorder(int64_t start);
        void UpdateRecord(State state, int64_t position);
        Token CollectResult(const std::string& text, int64_t* new_position);

      private:
        int64_t start_, latest_acc_;
        Token::Type token_type_ = Token::ERROR;
    };

    /**
     * receive the next character and getting the next state
     */
    using StateHandler = State (*)(char);

    /**
     * hanldler for the START state
     */
    static State StartHandler(char);

    /**
     * handler for the token that only have a single character
     * such as '+', '-'
     */
    static State SingleCharHandler(char);

    /**
     * handler for INTEGER_ACC
     */
    static State IntAccHandler(char);

    /**
     * handelr for FLOAT_ACC
     */
    static State FloatAccHandler(char);

    /**
     * handler for FLOAT_DOT
     */
    static State FloatDotHandler(char);

    /**
     * get next state according to current state and next character
     */
    static State StateDispatch(State state, char next_char);

    /**
     * convert a accepting state to corresponding token type
     * or Token::Type::Error otherwise
     */
    static Token::Type StateToToken(State state);

    /**
     * determine whether the character is valid for the scanner
     */
    static bool IsValidChar(char);

    static constexpr StateHandler state_handler_[] = {
        &StartHandler,      &SingleCharHandler, &SingleCharHandler,
        &SingleCharHandler, &SingleCharHandler, &IntAccHandler,
        &FloatAccHandler,   &FloatDotHandler,
    };

    /**
     * get next token of the text according to the position
     */
    Token GetToken();

    /**
     * get next char according to position
     */
    char NextChar();

    /**
     * the original text to scan
     */
    std::string text_;

    /**
     * current position of the scan procedure
     */
    int64_t position_;
};

class InvalidChar : public std::logic_error {
  public:
    InvalidChar(char c, size_t index) : logic_error(MakeMessage(c, index)) {}

  private:
    static std::string MakeMessage(char c, size_t index) {
        return std::string("InvaldChar: '") + c + "' at " +
               std::to_string(index);
    }
};

class InvalidToken : public std::logic_error {
  public:
    InvalidToken(const std::string& token, size_t index)
        : logic_error(MakeMessage(token, index)) {}

  private:
    static std::string MakeMessage(const std::string& token, size_t index) {
        return "InvalidToken: " + token + " at " + std::to_string(index);
    }
};

}  // namespace calc

}  // namespace vinalx

#endif  // VINAX_CALC_SCANNER_H_
