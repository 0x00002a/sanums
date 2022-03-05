#pragma once

#include <concepts>
#include <limits>
#include <stdexcept>
#include <bit>

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
concept conversion_safe = conversion_safe_v<F, T>;

template<typename F, typename T>
constexpr bool same_signidness_v = is_unsigned<F> == is_unsigned<T>;

template<typename T, template<typename...> typename V>
concept specialisation_of = requires(const T& val) {
    []<typename... Args>(const V<Args...>&){}(val);
};

template<typename F, typename To>
concept castable_to = requires(F f) {
    { static_cast<To>(f) } -> std::same_as<To>;
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
    } else {
        return (from > std::numeric_limits<T>::max() || from < std::numeric_limits<T>::min());
    }
}
template<typename To, typename From>
constexpr auto cast_maybe_valid() -> bool {
    return !(cast_is_invalid<To>(std::numeric_limits<From>::max()) || cast_is_invalid<To>(std::numeric_limits<From>::min()));
}

template<typename Lhs, typename Rhs>
struct bigger {
    static constexpr auto value = std::numeric_limits<Lhs>::max() >= std::numeric_limits<Rhs>::max();
    using type = std::conditional_t<value, Lhs, Rhs>;
};


template<typename Lhs, typename Rhs>
using bigger_t = typename bigger<Lhs, Rhs>::type;

template<template<typename> typename Wrap>
constexpr auto apply_bin(auto lhs, auto rhs, auto op) {
    using lhs_v = typename std::decay_t<decltype(lhs)>::value_type;
    using rhs_v = typename std::decay_t<decltype(rhs)>::value_type;
    using retr_v = bigger_t<lhs_v, rhs_v>;
    using retr_t = Wrap<retr_v>;
    return retr_t{op(static_cast<retr_v>(lhs), static_cast<retr_v>(rhs))};
}
}



namespace detail {

template<concepts::numeric T>
class basic_strict_num {
    template<concepts::numeric>
    friend class basic_strict_num;
public:
    using value_type = T;

    template<concepts::specialisation_of<basic_strict_num> Other>
    constexpr auto as() const {
        using to_t = typename Other::value_type;
        if (!util::cast_is_invalid<to_t>(val_)) {
            return as_unchecked<Other>();
        } else {
            throw except::invalid_cast("");
        }
    }

    template<concepts::specialisation_of<basic_strict_num> Other>
    constexpr auto as_unchecked() const {
        return Other::from_unchecked(static_cast<typename Other::value_type>(val_));
    }

    template<std::convertible_to<T> V>
    constexpr static auto from_unchecked(V v) noexcept {
        return basic_strict_num(v, false);
    }

    template<std::convertible_to<T> V>
    explicit constexpr basic_strict_num(V v) : val_{static_cast<T>(v)} {
        if constexpr (!concepts::conversion_safe_v<V, T>) {
            if (v != val_) {
                throw except::invalid_cast("cannot fit " + std::to_string(v) + " into size");
            }
        }
    }
    constexpr basic_strict_num(const basic_strict_num&) noexcept = default;
    constexpr basic_strict_num(basic_strict_num&&) noexcept = default;
    constexpr auto operator=(const basic_strict_num&) noexcept -> basic_strict_num& = default;
    constexpr auto operator=(basic_strict_num&&) noexcept -> basic_strict_num& = default;

    constexpr auto operator<=>(basic_strict_num other) noexcept {
        return val_ <=> other.val_;
    }
    constexpr auto operator==(basic_strict_num other) noexcept {
        return !(*this < other) && !(*this > other);
    }

    template<typename V>
        requires concepts::conversion_safe<T, V>
    constexpr explicit operator V() {
        return static_cast<V>(val_);
    }

#define OP_SAME_SIGN(op) \
    constexpr auto operator op (basic_strict_num rhs) noexcept { \
        return util::apply_bin<basic_strict_num>(*this, rhs, [](auto lhs, auto rhs) { return lhs op rhs; }); \
    }

    OP_SAME_SIGN(+)
    OP_SAME_SIGN(-)
    OP_SAME_SIGN(^)
    OP_SAME_SIGN(&)
    OP_SAME_SIGN(*)
    OP_SAME_SIGN(%)
    OP_SAME_SIGN(<<)
    OP_SAME_SIGN(>>)
#undef OP_SAME_SIGN

    template<typename RhsV>
    constexpr auto operator/(basic_strict_num rhs) {
        return util::apply_bin<basic_strict_num>(*this, rhs, [](auto lhs, auto rhs) { return lhs / rhs; });
    }

private:
    template<concepts::numeric By>
    friend constexpr auto operator<<(basic_strict_num f, By by) {
        f.val_ <<= by;
        return f;
    }
    template<concepts::specialisation_of<basic_strict_num> By>
    friend constexpr auto operator<<(basic_strict_num f, By by) {
        f.val_ <<= by.val_;
        return f;
    }

    constexpr basic_strict_num(T v, bool) noexcept : val_{v} {}

    value_type val_;

};

#define IMPL_EQ_OP(op) \
    template <typename Lhs, typename Rhs> \
    requires requires (Lhs l, Rhs r) { \
        { l op r } -> std::convertible_to<Lhs>; \
    } \
    constexpr auto operator op##= (Lhs& l, Rhs r) noexcept -> Lhs& { \
        l = l op r; \
        return l; \
    }

IMPL_EQ_OP(<<)
IMPL_EQ_OP(>>)
IMPL_EQ_OP(^)
IMPL_EQ_OP(&)
IMPL_EQ_OP(*)
IMPL_EQ_OP(/)
IMPL_EQ_OP(%)
IMPL_EQ_OP(-)
IMPL_EQ_OP(+)

#undef IMPL_EQ_OP

static_assert(concepts::conversion_safe_v<int, long int>);




}

using u8 = detail::basic_strict_num<std::uint8_t>;
using u16 = detail::basic_strict_num<std::uint16_t>;
using u32 = detail::basic_strict_num<std::uint32_t>;
using u64 = detail::basic_strict_num<std::uint64_t>;
using i32 = detail::basic_strict_num<std::int32_t>;
using i64 = detail::basic_strict_num<std::int64_t>;


}

