#ifndef VINALX_CALC_TOKEN_H_
#define VINALX_CALC_TOKEN_H_

#include <string>

namespace vinalx {

namespace calc {

struct Token {
    /**
     * type of tokens
     */
    enum Type {
        INTEGER,
        FLOATING,
        LOW_OPERATOR,
        HIGH_OPERATOR,
        LEFT_PARENTH,
        RIGHT_PARENTH,
        ERROR,
    };

    Token(Type type, const std::string& content)
        : content(content), type(type) {}

    /*
     * real content of the token
     */
    std::string content;

    /**
     * token type
     */
    Type type;

    static std::string TypeString(Type type) {
        static const char* type_string[] = {
            "Number",
            "Number",
            "+ or -",
            "* or /",
            "(",
            ")",
        };
        return type_string[type];
    }
};

}  // namespace calc

}  // namespace vinalx

#endif  // VINALX_CALC_TOKEN_H_
