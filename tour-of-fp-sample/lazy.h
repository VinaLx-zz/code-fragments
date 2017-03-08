#include <map>

template <typename A>
class Lazy;

template <typename K, typename V>
V GetOrFallback(std::map<K, V> m, const K& k, Lazy<V> fb);

template <typename A>
class Stream {
    A head_;
    Lazy<Stream<A>> tail_;
};

