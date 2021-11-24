#pragma once

#include <concepts>
#include <limits>
#include <stdexcept>

namespace sn {

namespace concepts {

template<typename T>
concept numeric = std::is_arithmetic_v<T>;

template<typename T>
concept is_unsigned = std::is_unsigned_v<T>;


template<typename F, typename T>
constexpr bool fits_in_v = std::numeric_limits<F>::max() >= std::numeric_limits<T>::max();

template<typename F, typename T>
constexpr bool conversion_safe_v = 
        (std::integral<F> == std::integral<T>)
        && (std::numeric_limits<T>::max() >= std::numeric_limits<F>::max()) 
        && (is_unsigned<F> || (std::numeric_limits<F>::min() >= std::numeric_limits<T>::min()));

template<typename F, typename T>
constexpr bool same_signidness_v = is_unsigned<F> == is_unsigned<T>;

template<typename T, template<typename...> typename V>
concept specialisation_of = requires(const T& val) {
    []<typename... Args>(const V<Args...>&){}(val);
};

}

namespace except {

class invalid_cast : public std::runtime_error {
public:
    using std::runtime_error::runtime_error;

};

}

namespace util {

template<concepts::numeric T, concepts::numeric F>
constexpr auto cast_is_invalid(F from) -> bool {
    if constexpr (concepts::conversion_safe_v<F, T>) {
        return false;
    } else if (from > std::numeric_limits<T>::max() || from < std::numeric_limits<T>::min()) {
        return false;
    } else {
        return true;
    }
}

template<typename Lhs, typename Rhs>
struct bigger {
    static constexpr auto value = std::numeric_limits<Lhs>::max() >= std::numeric_limits<Rhs>::max();
    using type = std::conditional_t<value, Lhs, Rhs>;
}; 


template<typename Lhs, typename Rhs>
using bigger_t = typename bigger<Lhs, Rhs>::type;

}




namespace detail {

template<concepts::numeric T>
class basic_strict_num {
public:
    using value_type = T;

    template<concepts::specialisation_of<basic_strict_num> Other>
    auto to() {
        using to_t = typename Other::value_type;
        if (!util::cast_is_invalid<to_t>(val_)) {
            return to_unchecked<Other>(*this);
        } else {
            throw except::invalid_cast("");
        }
    }

    template<concepts::specialisation_of<basic_strict_num> Other>
    auto to_unchecked() {
        return Other::from_unchecked(static_cast<typename Other::value_type>(val_));
    }

    template<std::convertible_to<T> V>
    static auto from_unchecked(V v) {
        return basic_strict_num(v, false);
    }

    basic_strict_num(T v) : val_{v} {}

    explicit operator T() {
        return val_;
    }

    template<typename RhsV>
        requires (concepts::same_signidness_v<value_type, RhsV>) 
    constexpr auto operator+(basic_strict_num<RhsV> rhs) -> basic_strict_num<util::bigger_t<value_type, RhsV>> {
        using retr_t = basic_strict_num<util::bigger_t<value_type, RhsV>>;
        using retr_v = typename retr_t::value_type;
        return retr_t{static_cast<retr_v>(val_) + static_cast<retr_v>(rhs.val_)};
    }



private:
    basic_strict_num(T v, bool) : val_{v} {}

    value_type val_;

};
}

using u32 = detail::basic_strict_num<std::uint32_t>;
using u64 = detail::basic_strict_num<std::uint64_t>;
using i32 = detail::basic_strict_num<std::int32_t>;
using i64 = detail::basic_strict_num<std::int64_t>;

}

