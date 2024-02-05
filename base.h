#pragma once

#include <algorithm>
#include <array>
#include <bitset>
#include <cassert>
#include <chrono>
#include <climits>
#include <forward_list>
#include <functional>
#include <iomanip>
#include <iostream>
#include <list>
#include <map>
#include <numeric>
#include <optional>
#include <queue>
#include <random>
#include <ranges>
#include <set>
#include <source_location>
#include <span>
#include <stack>
#include <stdexcept>
#include <string>
#include <string_view>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <typeinfo>
#include <type_traits>
#include <valarray>
#include <variant>
#include <vector>

#if __has_include(<ext/pb_ds/assoc_container.hpp>)
#include <ext/pb_ds/assoc_container.hpp>
#endif

namespace detail {

using namespace std;
using namespace chrono;

[[nodiscard]] constexpr auto trim(string_view path) noexcept -> string_view {
    for (size_t i = size(path); i >= 1; --i)
        if (path[i - 1] == '/' || path[i - 1] == '\\')
            return path.substr(i);
    return path;
}

auto log_prefix(ostream& os, source_location sl) -> ostream& {
    return os << "[" << trim(sl.file_name()) << ":(" << sl.line() << ":" << sl.column() << ")]: ";
}

#ifdef __SIZEOF_INT128__

auto operator>>(istream& is, __int128& x) -> istream& {
    string s;
    is >> s;
    x = 0;
    bool neg = false;
    int i = 0;
    if (s[i] == '-') {
        neg = true;
        ++i;
    }
    if (s[i] == '+')
        ++i;
    for (; i < ssize(s); ++i) {
        if (!isdigit(s[i]))
            throw runtime_error(string("Non-numeric character: ") + s[i]);
        x *= 10;
        x += s[i] - '0';
    }
    if (neg)
        x *= -1;
    return is;
}

auto operator<<(ostream& os, __int128 x) -> ostream& {
    if (x < 0) {
        os << "-";
        x *= -1;
    }
    string s;
    do {
        s.push_back('0' + x % 10);
        x /= 10;
    } while (x > 0);
    for (auto it = s.crbegin(); it != s.crend(); ++it)
        os << *it;
    return os;
}

#endif

#ifdef _GLIBCXX_USE_FLOAT128

auto operator>>(istream& is, __float128& x) -> istream& {
    long double d;
    is >> d;
    x = d;
    return is;
}

auto operator<<(ostream& os, __float128 x) -> ostream& {
    return os << static_cast<long double>(x);
}

#endif

using clock = high_resolution_clock;
using time_point = clock::time_point;

struct Timer {
    const char* name;
    source_location sl = source_location::current();
    time_point start = clock::now();

    template<class Period = ratio<1>>
    [[nodiscard]] constexpr auto get() const noexcept -> double {
        return static_cast<duration<double, Period>>(clock::now() - start).count();
    }

    ~Timer() {
#ifdef LOGGING
        log_prefix(cout, sl);
        cout << name << " " << to_string(get()) << " seconds\n";
#endif
    }
};

#define TIMER(name) ::detail::Timer name{#name}

TIMER(timer);

[[nodiscard]] constexpr auto splitmix64(uint64_t x) -> uint64_t {
    x += 0x9e3779b97f4a7c15;
    x = (x ^ (x >> 30)) * 0xbf58476d1ce4e5b9;
    x = (x ^ (x >> 27)) * 0x94d049bb133111eb;
    return x ^ (x >> 31);
}

class Hasher;

template<size_t N, class T>
[[nodiscard]] consteval auto static_array(const vector<T>& v) noexcept -> array<T, N> {
    array<T, N> a{};
    ranges::copy(v, a.begin());
    return a;
}

#define STATIC_ARRAY1(v) ::detail::static_array<::std::size(v)>(v)
#define STATIC_ARRAY2(v, n) ::detail::static_array<n>(v)
#define GET_MACRO(_1, _2, NAME, ...) NAME
#define EXPAND(x) x
#define STATIC_ARRAY(...) EXPAND(GET_MACRO(__VA_ARGS__, STATIC_ARRAY2, STATIC_ARRAY1)(__VA_ARGS__))

template<size_t N>
struct rank : rank<N - 1> {};

template<>
struct rank<0> {};

template<class T>
struct is_pair : false_type {};

template<class T1, class T2>
struct is_pair<pair<T1, T2>> : true_type {};

template<class T>
struct is_tuple : false_type {};

template<class... Types>
struct is_tuple<tuple<Types...>> : true_type {};

template<class T>
struct is_list : false_type {};

template<class T, size_t N>
struct is_list<array<T, N>> : true_type {};

template<class T, class Allocator>
struct is_list<vector<T, Allocator>> : true_type {};

template<class T, class Allocator>
struct is_list<deque<T, Allocator>> : true_type {};

template<class T, class Allocator>
struct is_list<forward_list<T, Allocator>> : true_type {};

template<class T, class Allocator>
struct is_list<list<T, Allocator>> : true_type {};

template<class T>
struct is_list<valarray<T>> : true_type {};

template<class T, size_t Extent>
struct is_list<span<T, Extent>> : true_type {};

template<class T>
struct is_set : false_type {};

template<class Key, class Compare, class Allocator>
struct is_set<set<Key, Compare, Allocator>> : true_type {};

template<class Key, class Compare, class Allocator>
struct is_set<multiset<Key, Compare, Allocator>> : true_type {};

template<class Key, class Hash, class KeyEqual, class Allocator>
struct is_set<unordered_set<Key, Hash, KeyEqual, Allocator>> : true_type {};

template<class Key, class Hash, class KeyEqual, class Allocator>
struct is_set<unordered_multiset<Key, Hash, KeyEqual, Allocator>> : true_type {};

template<class T>
struct is_map : false_type {};

template<class Key, class T, class Compare, class Allocator>
struct is_map<map<Key, T, Compare, Allocator>> : true_type {};

template<class Key, class T, class Compare, class Allocator>
struct is_map<multimap<Key, T, Compare, Allocator>> : true_type {};

template<class Key, class T, class Hash, class KeyEqual, class Allocator>
struct is_map<unordered_map<Key, T, Hash, KeyEqual, Allocator>> : true_type {};

template<class Key, class T, class Hash, class KeyEqual, class Allocator>
struct is_map<unordered_multimap<Key, T, Hash, KeyEqual, Allocator>> : true_type {};

template<class T>
struct is_adaptor : false_type {};

template<class T, class Container>
struct is_adaptor<stack<T, Container>> : true_type {};

template<class T, class Container>
struct is_adaptor<queue<T, Container>> : true_type {};

template<class T, class Container, class Compare>
struct is_adaptor<priority_queue<T, Container, Compare>> : true_type {};

#ifdef PB_DS_ASSOC_CNTNR_HPP

using namespace __gnu_pbds;

template<typename Key, typename Cmp_Fn = less<Key>>
using tree_set = tree<Key, null_type, Cmp_Fn, rb_tree_tag, tree_order_statistics_node_update>;

template<typename Key, typename Mapped, typename Cmp_Fn = less<Key>>
using tree_map = tree<Key, Mapped, Cmp_Fn, rb_tree_tag, tree_order_statistics_node_update>;

template<typename Key>
using hash_set = gp_hash_table<Key, null_type, Hasher>;

template<typename Key, typename Mapped>
using hash_map = gp_hash_table<Key, Mapped, Hasher>;

template<typename Key, typename Cmp_Fn, typename Tag, template<typename Const_Node_Iterator, typename Node_Iterator, typename Cmp_Fn_, typename Allocator_> class Node_Update, typename Allocator>
struct is_set<tree<Key, null_type, Cmp_Fn, Tag, Node_Update, Allocator>> : true_type {};

template<typename Key, typename Hash_Fn, typename Eq_Fn, typename Comp_Probe_Fn, typename Probe_Fn, typename Resize_Policy, bool Store_Hash, typename Allocator>
struct is_set<gp_hash_table<Key, null_type, Hash_Fn, Eq_Fn, Comp_Probe_Fn, Probe_Fn, Resize_Policy, Store_Hash, Allocator>> : true_type {};

template<typename Key, typename Cmp_Fn, typename Tag, template<typename Const_Node_Iterator, typename Node_Iterator, typename Cmp_Fn_, typename Allocator_> class Node_Update, typename Allocator>
struct is_map<tree<Key, null_type, Cmp_Fn, Tag, Node_Update, Allocator>> : false_type {};

template<typename Key, typename Hash_Fn, typename Eq_Fn, typename Comp_Probe_Fn, typename Probe_Fn, typename Resize_Policy, bool Store_Hash, typename Allocator>
struct is_map<gp_hash_table<Key, null_type, Hash_Fn, Eq_Fn, Comp_Probe_Fn, Probe_Fn, Resize_Policy, Store_Hash, Allocator>> : false_type {};

template<typename Key, typename Mapped, typename Cmp_Fn, typename Tag, template<typename Const_Node_Iterator, typename Node_Iterator, typename Cmp_Fn_, typename Allocator_> class Node_Update, typename Allocator>
struct is_map<tree<Key, Mapped, Cmp_Fn, Tag, Node_Update, Allocator>> : true_type {};

template<typename Key, typename Mapped, typename Hash_Fn, typename Eq_Fn, typename Comp_Probe_Fn, typename Probe_Fn, typename Resize_Policy, bool Store_Hash, typename Allocator>
struct is_map<gp_hash_table<Key, Mapped, Hash_Fn, Eq_Fn, Comp_Probe_Fn, Probe_Fn, Resize_Policy, Store_Hash, Allocator>> : true_type {};

#else

template<typename Key>
using hash_set = unordered_set<Key, Hasher>;

template<typename Key, typename Mapped>
using hash_map = unordered_map<Key, Mapped, Hasher>;

#endif

template<class T>
concept Pair = is_pair<decay_t<T>>::value;

template<class T>
concept Tuple = is_tuple<decay_t<T>>::value;

template<class T>
concept List = is_list<decay_t<T>>::value;

template<class T>
concept Set = is_set<decay_t<T>>::value;

template<class T>
concept Map = is_map<decay_t<T>>::value;

template<class T>
concept Adaptor = is_adaptor<decay_t<T>>::value;

template<Adaptor Adaptor>
[[nodiscard]] auto get_container(Adaptor&& a) noexcept -> auto&& {
    struct hack : decay_t<Adaptor> {
        [[nodiscard]] static auto get(Adaptor&& a) noexcept -> auto&& {
            return forward<Adaptor>(a).*&hack::c;
        }
    };
    return hack::get(forward<Adaptor>(a));
}

auto operator>>(istream& is, Pair auto& p) -> istream& {
    return is >> p.first >> p.second;
}

auto operator>>(istream& is, Tuple auto& t) -> istream& {
    apply([&](auto&... args) {
        (is >> ... >> args);
    }, t);
    return is;
}

auto operator>>(istream& is, List auto& l) -> istream& {
    for (auto& x : l)
        is >> x;
    return is;
}

struct Fmt {
    string_view open;
    string_view sep;
    string_view close;
};

struct DictFmt : Fmt {
    string_view key_val_sep;
};

class Printer {
public:
    Fmt tuple_fmt;
    Fmt list_fmt;
    DictFmt dict_fmt;

    auto print(ostream& os, const auto& x) const noexcept -> ostream& {
        return print(os, x, rank<1>());
    }

private:
    auto print(ostream& os, const auto& x, rank<0>) const noexcept -> ostream& {
        return os << x;
    }

    auto print(ostream& os, const Pair auto& p, rank<1>) const noexcept -> ostream& {
        os << tuple_fmt.open;
        print(os, p.first);
        os << tuple_fmt.sep;
        print(os, p.second);
        return os << tuple_fmt.close;
    }

    auto print(ostream& os, const Tuple auto& t, rank<1>) const noexcept -> ostream& {
        os << tuple_fmt.open;
        bool print_sep = false;
        auto print_impl = [&](const auto& x) {
            if (print_sep)
                os << tuple_fmt.sep;
            print(os, x);
            print_sep = true;
        };
        apply([&](const auto&... args) {
            (print_impl(args), ...);
        }, t);
        return os << tuple_fmt.close;
    }

    auto print(ostream& os, const List auto& l, rank<1>) const noexcept -> ostream& {
        os << list_fmt.open;
        bool print_sep = false;
        for (auto it = l.begin(); it != l.end(); ++it) {
            if (print_sep)
                os << list_fmt.sep;
            print(os, *it);
            print_sep = true;
        }
        return os << list_fmt.close;
    }

    auto print(ostream& os, const Set auto& s, rank<1>) const noexcept -> ostream& {
        os << dict_fmt.open;
        bool print_sep = false;
        for (auto it = s.begin(); it != s.end(); ++it) {
            if (print_sep)
                os << dict_fmt.sep;
            print(os, *it);
            print_sep = true;
        }
        return os << dict_fmt.close;
    }

    auto print(ostream& os, const Map auto& m, rank<1>) const noexcept -> ostream& {
        os << dict_fmt.open;
        bool print_sep = false;
        for (auto it = m.begin(); it != m.end(); ++it) {
            if (print_sep)
                os << dict_fmt.sep;
            print(os, it->first);
            os << dict_fmt.key_val_sep;
            print(os, it->second);
            print_sep = true;
        }
        return os << dict_fmt.close;
    }

    auto print(ostream& os, const Adaptor auto& a, rank<1>) const noexcept -> ostream& {
        return print(os, get_container(a));
    }
};

static constexpr DictFmt plain{"", " ", "", " "};
static constexpr Printer printer{plain, plain, plain};
static constexpr Printer pprinter{{"(", ", ", ")"}, {"[", ", ", "]"}, {"{", ", ", "}", ": "}};

template<class T>
concept PrettyPrintable = Pair<T> || Tuple<T> || List<T> || Set<T> || Map<T> || Adaptor<T>;

auto operator<<(ostream& os, const PrettyPrintable auto& pp) -> ostream& {
    return printer.print(os, pp);
}

class Hasher {
public:
    [[nodiscard]] auto operator()(const auto& x) const noexcept -> size_t {
        return hash(x, rank<1>());
    }

private:
    template<class T>
    [[nodiscard]] auto hash(const T& x, rank<0>) const noexcept -> size_t {
        return static_cast<size_t>(splitmix64(::std::hash<T>()(x) + timer.start.time_since_epoch().count()));
    }

    auto hash_combine(size_t& seed, const auto& v) const noexcept -> void {
        seed ^= operator()(v) + 0x9e3779b9 + (seed << 6) + (seed >> 2);
    }

    [[nodiscard]] auto hash(const Pair auto& p, rank<1>) const noexcept -> size_t {
        size_t seed = 0;
        hash_combine(seed, p.first);
        hash_combine(seed, p.second);
        return seed;
    }

    [[nodiscard]] auto hash(const Tuple auto& t, rank<1>) const noexcept -> size_t {
        size_t seed = 0;
        apply([&](const auto&... args) {
            (hash_combine(seed, args), ...);
        }, t);
        return seed;
    }

    [[nodiscard]] auto hash(const List auto& l, rank<1>) const noexcept -> size_t {
        size_t seed = 0;
        for (const auto& x : l)
            hash_combine(seed, x);
        return seed;
    }

    [[nodiscard]] auto hash(const Adaptor auto& a, rank<1>) const noexcept -> size_t {
        return operator()(get_container(a));
    }
};

[[nodiscard]] constexpr auto is_whitespace(char c) noexcept -> bool {
    switch (c) {
    case ' ':
    case '\n':
    case '\r':
    case '\f':
    case '\v':
    case '\t':
        return true;
    default:
        return false;
    }
}

[[nodiscard]] constexpr auto is_quote(char c) noexcept -> bool {
    return c == '\'' || c == '"';
}

[[nodiscard]] constexpr auto match_quote(string_view s, int i, char c) noexcept -> int {
    ++i;
    while (s[i] != c)
        ++i;
    return i;
}

[[nodiscard]] constexpr auto match_parenthesis(string_view s, int i) -> int {
    ++i;
    while (s[i] != ')') {
        if (is_quote(s[i]))
            i = match_quote(s, i, s[i]);
        else if (s[i] == '(')
            i = match_parenthesis(s, i);
        ++i;
    }
    return i;
}

[[nodiscard]] constexpr auto split_args(string_view s) -> vector<string_view> {
    vector<string_view> args;
    int i = -1;
    while (i < ssize(s)) {
        int j = i + 1;
        while (j < ssize(s) && s[j] != ',') {
            if (is_quote(s[j]))
                j = match_quote(s, j, s[j]);
            else if (s[j] == '(')
                j = match_parenthesis(s, j);
            ++j;
        }
        string_view arg = s.substr(i + 1, j - i - 1);
        while (is_whitespace(arg.front()))
            arg.remove_prefix(1);
        while (is_whitespace(arg.back()))
            arg.remove_suffix(1);
        args.push_back(arg);
        i = j;
    }
    return args;
}

template<size_t N>
auto log(ostream& os, source_location sl, const array<string_view, N>& arg_names, const auto&... args) -> ostream& {
    log_prefix(os, sl);
    int index = 0;
    auto log_pair = [&](const auto& x) {
        if (index)
            os << ", ";
        os << arg_names.at(index) << " = ";
        pprinter.print(os, x);
        ++index;
    };
    (log_pair(args), ...);
    return os;
}

#ifdef LOGGING
#define LOG(...) ::detail::log(::std::cout, source_location::current(), STATIC_ARRAY(::detail::split_args(#__VA_ARGS__)), __VA_ARGS__) << std::endl
#else
#define LOG(...)
#endif

namespace rng {

thread_local mt19937_64 mt(clock::now().time_since_epoch().count());

template<integral T>
[[nodiscard]] auto get(T a, T b) noexcept -> T {
    uniform_int_distribution<T> dist(a, b);
    return dist(mt);
}

template<integral T>
[[nodiscard]] auto get() noexcept -> T {
    uniform_int_distribution<T> dist(0, numeric_limits<T>::max());
    return dist(mt);
}

template<floating_point T>
[[nodiscard]] auto get(T a, T b) noexcept -> T {
    uniform_real_distribution<T> dist(a, b);
    return dist(mt);
}

template<floating_point T>
[[nodiscard]] auto get() noexcept -> T {
    uniform_real_distribution<T> dist(0, 1);
    return dist(mt);
}

}  // namespace rng

template<size_t D, class T>
struct vec {
    using type = std::vector<typename vec<D - 1, T>::type>;
};

template<class T>
struct vec<0, T> {
    using type = T;
};

template<size_t D, class T>
using Vec = typename vec<D, T>::type;

template<class T>
[[nodiscard]] constexpr auto make_vec(size_t dim, T&& init) noexcept -> auto {
    return vector(dim, init);
}

template<class... Args>
[[nodiscard]] constexpr auto make_vec(size_t dim, Args&&... args) noexcept -> auto {
    return vector(dim, make_vec(forward<Args>(args)...));
}

}  // namespace detail

using detail::operator>>;
using detail::operator<<;
using detail::timer;

#ifdef PB_DS_ASSOC_CNTNR_HPP

using detail::tree_set;
using detail::tree_map;

#endif

using detail::hash_set;
using detail::hash_map;
using detail::Pair;
using detail::Tuple;
using detail::List;
using detail::Set;
using detail::Map;
using detail::Adaptor;
using detail::get_container;
using detail::Printer;

namespace rng = detail::rng;

using detail::Vec;
using detail::make_vec;

static constexpr int INF = int(1e9);
static constexpr int64_t LLINF = int64_t(1e18);
static constexpr int64_t MOD = int64_t(1e9) + 7;

[[nodiscard]] constexpr auto mod(int64_t x, int64_t m = MOD) noexcept -> int64_t {
    int64_t tmp = x % m;
    return tmp >= 0 ? tmp : tmp + m;
}

[[nodiscard]] constexpr auto sq(int64_t x) noexcept -> int64_t {
    return x * x;
}

[[nodiscard]] constexpr auto sq(long double x) noexcept -> long double {
    return x * x;
}

[[nodiscard]] constexpr auto sgn(auto x) noexcept -> int {
    return (x > 0) - (x < 0);
}

template<std::integral T>
[[nodiscard]] constexpr auto floor_div(T a, T b) noexcept -> T {
    T d = a / b;
    T r = a % b;
    return d - (r ? (a < 0) ^ (b < 0) : 0);
}

template<std::integral T>
[[nodiscard]] constexpr auto ceil_div(T a, T b) noexcept -> T {
    T d = a / b;
    T r = a % b;
    return d + (r ? (a > 0) ^ (b < 0) : 0);
}

template<class T>
constexpr auto min_assign(T& lhs, const T& rhs) noexcept -> T& {
    if (lhs > rhs)
        lhs = rhs;
    return lhs;
}

template<class T>
constexpr auto max_assign(T& lhs, const T& rhs) noexcept -> T& {
    if (lhs < rhs)
        lhs = rhs;
    return lhs;
}
