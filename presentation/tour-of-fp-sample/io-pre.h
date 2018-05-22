#include <experimental/optional>
#include <functional>
#include <iostream>
#include <string>

using std::function;
using std::experimental::optional;

class DBConfig {};
class DBConnection {};

void WriteConfig(DBConfig config);
DBConfig ReadConfig();

DBConfig GetDefaultConfig();
void ResetConfig() {
    auto default_config = GetDefaultConfig();
    WriteConfig(default_config);
}

DBConnection GetConnectionFromConfig(DBConfig);
DBConnection GetDBConnection() {
    auto config = ReadConfig();
    return GetConnectionFromConfig(config);
}

DBConnection initDB() {
    // blablabla...
    ResetConfig();
    // blablabla...
    return GetDBConnection();
}

namespace io_monad {
// note: invalid code! only illustration purpose
template <typename A>
class IO {
  public:
    IO(function<A()> f) : closure_(f) {}

    A run() const {
        return closure_();
    }

    template <typename B>
    IO<B> fmap(function<B(A)> f) {
        return IO<B>([this, f] { return f(this->run()); });
    }

    template <typename B>
    IO<B> bind(function<IO<B>(A)> f) {
        return IO<B>([this, f] { return f(this->run()).run(); });
    }

  private:
    function<A()> closure_;
};

template <typename A>
IO<A> unit(const A& a) {
    return IO<A>([a]() { return a; });
}

IO<std::string> ReadLine() {
    return IO<std::string>([]() {
        std::string line;
        std::getline(std::cin, line);
        return line;
    });
}

IO<int> ReadInt() {
    return IO<int>([]() {
        int i;
        std::cin >> i;
        return i;
    });
}

IO<void> PrintLine(const std::string& line) {
    return IO<void>([line]() { std::cout << line << '\n'; });
}

IO<void> Echo() {
    return ReadLine().bind(
        [](const std::string& line) { return PrintLine(line); });
}

}  // namespace io_monad
