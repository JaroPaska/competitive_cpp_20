#include "base.h"
#include "number_theory.h"

namespace detail {

using namespace std;

class Hash {
public:


private:
    static constexpr size_t strength = 4;
    static constexpr array<long long, 2> abcs_{269, 271};
    static constexpr array<long long, 2> mods_{1'000'000'000'000'000'003, 1'000'000'000'000'000'009};
    static vector<array<long long, strength>> abc_pows_;
    array<long long, strength> values_;
    
    [[nodiscard]] static consteval auto confs() noexcept -> array<array<long long, 2>, strength> {
        array<array<long long, 2>, strength> a{};
        for (int i = 0; i < ssize(abcs_); ++i)
            for (int j = 0; j < ssize(mods_); ++j)
                a[i * ssize(mods_) + j] = {abcs_[i], mods_[j]};
        return a;
    }

    [[nodiscard]] static auto abc_pows(int exp) noexcept -> array<long long, strength> {
        for (int i = ssize(abc_pows_); i <= exp; ++i) {
            abc_pows_.push_back({});
            for (int j = 0; j < strength; ++j)
                abc_pows_[i][j] = mod(confs()[j][0] * abc_pows_[i - 1][j], confs()[j][1]);
        }
        return abc_pows_[exp];
    }
};

vector<array<long long, Hash::strength>> Hash::abc_pows_{{{1, 1, 1, 1}}};

}  // namespace detail

using detail::Hash;