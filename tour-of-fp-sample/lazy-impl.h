#include <type_traits>
#include <utility>

template <class T, class Closure>
class Lazy {
  public:
    template <
        typename std::enable_if<
            std::is_same<std::result_of_t<Closure()>, T>::value, int>::type = 0>
    Lazy(Closure&& c) : closure_(std::move(c)) {}

    T result() {
        return closure_();
    }

  private:
    Closure closure_;
};

template <class Closure>
auto thunk(Closure&& c) {
    using Result = std::result_of_t<Closure()>;
    return Lazy<Result, Closure>(std::move(c));
}
