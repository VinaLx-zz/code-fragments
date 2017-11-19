#include <string>

using std::string;

class Person {
  public:
    // "methods" are behaviors
    virtual void Kill(Person p) {
        if (p.weight < weight) {
            p.alive = false;
        }
    }
    virtual void Suicide() {
        alive = false;
    }
    // .. other "methods"
  private:
    // "fields" are properties
    double weight;
    bool alive;
    string name;
    // ... other "fields"
};

