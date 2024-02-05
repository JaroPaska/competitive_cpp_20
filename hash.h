#include "base.h"
#include "number_theory.h"

namespace detail {

using namespace std;

struct Hash {
    static constexpr size_t cnt = 2;
    static constexpr array<long long, 3> abcs_{269, 271, 277};
    static constexpr array<long long, 2> mods_{1'000'000'000'000'000'003, 1'000'000'000'000'000'009};
    static vector<array<long long, cnt>> abc_pows_;
    static vector<array<long long, cnt>> inv_abc_pows_;
    array<long long, cnt> vals_{};
    int len_{};
    
    [[nodiscard]] static consteval auto confs() noexcept -> array<array<long long, 2>, cnt> {
        array<array<long long, 2>, cnt> a{};
        for (size_t k = 0; k < cnt; ++k) {
            size_t i = k / size(mods_);
            size_t j = k % size(mods_);
            a[k] = {abcs_[i], mods_[j]};
        }
        return a;
    }

    [[nodiscard]] static constexpr auto mod(size_t conf) noexcept -> long long {
        return confs()[conf][1];
    }

    [[nodiscard]] static auto init_pows() noexcept -> vector<array<long long, cnt>> {
        vector<array<long long, cnt>> v{{{}}, {{}}};
        for (size_t k = 0; k < cnt; ++k) {
            v[0][k] = 1;
            v[1][k] = confs()[k][0];
        }
        return v;
    }

    [[nodiscard]] static auto init_inv_pows() noexcept -> vector<array<long long, cnt>> {
        vector<array<long long, cnt>> v{{{}}, {{}}};
        for (size_t k = 0; k < cnt; ++k) {
            v[0][k] = 1;
            v[1][k] = mod_inv(confs()[k][0], mod(k));
        }
        return v;
    }

    [[nodiscard]] static auto pow(int exp, size_t conf) noexcept -> long long {
        for (auto i = ssize(abc_pows_); i <= exp; ++i) {
            array<long long, cnt> abc_pow{};
            for (int j = 0; j < cnt; ++j)
                abc_pow[j] = mod_mul(abc_pows_[1][j], abc_pows_[i - 1][j], mod(j));
            abc_pows_.push_back(abc_pow);
        }
        return abc_pows_[exp][conf];
    }

    [[nodiscard]] static auto inv_pow(int exp, size_t conf) noexcept -> long long {
        for (auto i = ssize(inv_abc_pows_); i <= exp; ++i) {
            array<long long, cnt> inv_abc_pow{};
            for (int j = 0; j < cnt; ++j)
                inv_abc_pow[j] = mod_mul(inv_abc_pows_[1][j], inv_abc_pows_[i - 1][j], mod(j));
            inv_abc_pows_.push_back(inv_abc_pow);
        }
        return inv_abc_pows_[exp][conf];
    }

    Hash() = default;

    Hash(char c) {
        for (size_t k = 0; k < cnt; ++k)
            vals_[k] = ::mod(c, mod(k));
        len_ = 1;
    }

    Hash(std::string_view s) {
        for (const auto& c : s)
            *this = this->append(c);
    }

    friend auto operator<=>(const Hash&, const Hash&) = default;

    [[nodiscard]] auto append(const Hash& r) const noexcept -> Hash {
        Hash h;
        for (size_t k = 0; k < cnt; ++k)
            h.vals_[k] = ::mod(mod_mul(vals_[k], pow(r.len_, k), mod(k)) + r.vals_[k], mod(k));
        h.len_ = len_ + r.len_;
        return h;
    }

    [[nodiscard]] auto pop(const Hash& r) const noexcept -> Hash {
        Hash h;
        for (size_t k = 0; k < cnt; ++k)
            h.vals_[k] = mod_mul(::mod(vals_[k] - r.vals_[k], mod(k)), inv_pow(r.len_, k), mod(k));
        h.len_ = len_ - r.len_;
        return h;
    }

    [[nodiscard]] auto popleft(const Hash& l) const noexcept -> Hash {
        Hash h;
        for (size_t k = 0; k < cnt; ++k)
            h.vals_[k] = ::mod(vals_[k] - mod_mul(l.vals_[k], pow(len_ - l.len_, k), mod(k)));
        h.len_ = len_ - l.len_;
        return h;
    }
};

auto operator<<(ostream& os, const Hash& h) -> ostream& {
    os << "{vals: ";
    pprinter.print(os, h.vals_);
    os << ", len: ";
    pprinter.print(os, h.len_);
    return os << "}";
}

vector<array<long long, Hash::cnt>> Hash::abc_pows_ = Hash::init_pows();
vector<array<long long, Hash::cnt>> Hash::inv_abc_pows_ = Hash::init_inv_pows();

}  // namespace detail

using detail::Hash;

namespace std {

template<>
struct hash<Hash> {
    [[nodiscard]] auto operator()(const Hash& h) const noexcept -> size_t {
        return detail::Hasher()(make_pair(h.vals_, h.len_));
    }
};

}  // namespace std