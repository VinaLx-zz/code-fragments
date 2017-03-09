#include <map>

template <typename A>
class Lazy;

template <typename Map, typename K, typename V>
V GetOrFallback(const Map& m, const K& k, Lazy<V> fb);

template <typename A>
class Stream {
    A head_;
    Lazy<Stream<A>> tail_;
};
