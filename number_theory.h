#pragma once

#include "base.h"

namespace detail {

using namespace std;

template<integral T>
constexpr auto extended_euclid(T a, T b, T& x, T& y) noexcept -> T {
    pair<T, T> r{a, b};
    pair<T, T> s{1, 0};
    pair<T, T> t{0, 1};
    while (r.second) {
        T q = r.first / r.second;
        r = { r.second, r.first - q * r.second };
        s = { s.second, s.first - q * s.second };
        t = { t.second, t.first - q * t.second };
    }
    x = s.first;
    y = t.first;
    return r.first;
}

[[nodiscard]] auto trailing_zeros(int64_t n) noexcept -> int {
    if (!n)
        return 0;
#ifdef _MSC_VER
    unsigned long z = 0;
    _BitScanForward64(&z, n);
    return z;
#else
    return __builtin_ctzll(n);
#endif
}

[[nodiscard]] auto mod_mul(int64_t a, int64_t b, int64_t m = MOD) noexcept -> int64_t {
#ifdef _MSC_VER
    unsigned __int64 h, l, r;
    l = _umul128(llabs(a), llabs(b), &h);
    _udiv128(h, l, m, &r);
    return sgn(a) * sgn(b) >= 0 ? r : m - r;
#else
    int64_t r = a * static_cast<__int128>(b) % m;
    return r >= 0 ? r : r + m;
#endif
}
 
[[nodiscard]] auto mod_exp(int64_t base, int64_t exp, int64_t m = MOD) noexcept -> int64_t {
    if (!exp)
        return mod(1, m);
    int64_t result = mod(1, m);
    base = mod(base, m);
    while (exp > 0) {
        if (exp & 1)
            result = mod_mul(result, base, m);
        exp >>= 1;
        base = mod_mul(base, base, m);
    }
    return result;
}

[[nodiscard]] auto mod_inv(int64_t a, int64_t m = MOD) noexcept -> int64_t {
    return mod_exp(a, m - 2, m);
}

[[nodiscard]] auto mod_div(int64_t a, int64_t b, int64_t m = MOD) noexcept -> int64_t {
    return mod(a * mod_inv(b, m), m);
}

[[nodiscard]] auto factorial(size_t n, int64_t m = MOD) noexcept -> int64_t {
    static hash_map<int64_t, vector<int64_t>> memo;
    auto it = memo.find(m);
    if (it == memo.end())
        it = memo.insert({m, {mod(1, m)}}).first;
    auto& f = it->second;
    while (n >= f.size())
        f.push_back(mod(f.back() * f.size(), m));
    return f[n];
}

[[nodiscard]] constexpr auto legendre(int64_t n, int64_t p) noexcept -> int64_t {
    int64_t x = n / p;
    int64_t sum = 0;
    while (x) {
        sum += x;
        x /= p;
    }
    return sum;
}

[[nodiscard]] auto nck(size_t n, size_t k, int64_t p = MOD) noexcept -> int64_t {
    if (k > n || legendre(n, p) > legendre(k, p) + legendre(n - k, p))
        return 0;
    static hash_map<int64_t, vector<int64_t>> memo;
    auto it = memo.find(p);
    if (it == memo.end())
        it = memo.insert({p, {mod(1, p)}}).first;
    auto& f = it->second;
    while (n >= f.size()) {
        auto x = f.size();
        while (!(x % p))
            x /= p;
        f.push_back(mod(x * f.back(), p));
    }
    return mod_div(f[n], mod(f[k] * f[n - k], p), p);
}

[[nodiscard]] constexpr auto sieve(int n) noexcept -> vector<int> {
    vector<int> gpf(n + 1);
    gpf[0] = gpf[1] = -1;
    for (int i = 2; i <= n; i++) {
        if (gpf[i])
            continue;
        gpf[i] = i;
        for (int j = 2 * i; j <= n; j += i)
            gpf[j] = i;
    }
    return gpf;
}

#define STATIC_SIEVE(n) STATIC_ARRAY(nt::sieve(n), n + 1)

bool miller_test(int64_t n, int64_t d, int64_t r, int64_t a) {
    int64_t x = mod_exp(a, d, n);
    if (x == 1 || x == n - 1)
        return true;
    for (int i = 0; i < r - 1; i++) {
        x = mod_mul(x, x, n);
        if (x == n - 1)
            return true;
    }
    return false;
}

bool is_prime(int64_t n, span<const int> gpf = span<const int>()) {
    if (n < ssize(gpf))
        return gpf[n] == n;
    if (n < 2)
        return false;
    if (n < 4)
        return true;
    int64_t r = trailing_zeros(n - 1);
    int64_t d = (n - 1) >> r;
    static constexpr array BASES{2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37}; 
    for (int i = 0; i < ssize(BASES) && BASES[i] <= n - 2; i++)
        if (!miller_test(n, d, r, BASES[i]))
            return false;
    return true;
}
 
int64_t pollard_rho(int64_t n) {
    for (int64_t sum = 3; sum <= n; ++sum)
        for (int64_t c = 1; sum - c >= 2; ++c) {
            auto g = [c, n](int64_t x) { return mod(mod_mul(x, x, n) + c, n); };
            int64_t x = sum - c;
            int64_t y = x;
            do {
                x = g(x);
                y = g(g(y));
                int64_t p = gcd(llabs(x - y), n);
                if (p > 1 && p < n)
                    return p;
            } while (x != y);
        }
    throw std::invalid_argument("no divisor found");
}
 
hash_map<int64_t, int64_t> factorize(int64_t n, span<const int> gpf = span<const int>()) {
    if (n < ssize(gpf)) {
        hash_map<int64_t, int64_t> m;
        while (n > 1) {
            m[gpf[n]]++;
            n /= gpf[n];
        }
        return m;
    }

    int twos = trailing_zeros(n);
    vector<int64_t> factors(twos, 2), st{n >> twos};
    while (!st.empty() && st.back() > 1) {
        int64_t x = st.back();
        st.pop_back();
        if (is_prime(x))
            factors.push_back(x);
        else {
            int64_t p = pollard_rho(x);
            int64_t q = x / p;
            st.push_back(p);
            st.push_back(q);
        }
    }
    hash_map<int64_t, int64_t> m;
    for (const auto& factor : factors)
        ++m[factor];
    return m;
}

}  // namespace detail

using detail::extended_euclid;
using detail::trailing_zeros;
using detail::mod_mul;
using detail::mod_exp;
using detail::mod_inv;
using detail::mod_div;
using detail::factorial;
using detail::legendre;
using detail::nck;
using detail::sieve;
using detail::miller_test;
using detail::is_prime;
using detail::pollard_rho;
using detail::factorize;
