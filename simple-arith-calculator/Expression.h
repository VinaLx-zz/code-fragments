#ifndef VINALX_CALC_EXPRESSION_H_
#define VINALX_CALC_EXPRESSION_H_

#include <memory>

namespace vinalx {

namespace calc {

struct Expression {
    virtual double Value() const = 0;
};

struct Operation : Expression {
    std::unique_ptr<Expression> left;
    std::unique_ptr<Expression> right;
};

struct Addition : Operation {
    virtual double Value() const override {
        return left->Value() + right->Value();
    }
};

struct Minus : Operation {
    virtual double Value() const override {
        return left->Value() - right->Value();
    }
};

struct Multiplication : Operation {
    virtual double Value() const override {
        return left->Value() * right->Value();
    }
};

struct Division: Operation {
    virtual double Value() const override {
        return left->Value() / right->Value();
    }
};

struct Number : Expression {
    Number(double number) : number_(number) {}

    virtual double Value() const override {
        return number_;
    }

  private:
    double number_;
};

}  // namespace calc

}  // namespace vinalx

#endif  // VINALX_CALC_EXPRESSION_H_
