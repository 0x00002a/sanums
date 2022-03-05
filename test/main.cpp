
#include <sanums.hpp>
#include <functional>
#include <type_traits>

#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN
#include <doctest/doctest.h>

using namespace sn;

namespace sn::detail {

template<sn::concepts::specialisation_of<sn::detail::basic_strict_num> T>
auto operator<<(std::ostream& s, T lhs) -> std::ostream& {
    s << static_cast<typename T::value_type>(lhs);
    return s;
}
}

template<typename Func>
concept check_is_valid = requires(Func f) {
    std::invoke(f);
};

#define DO_CHECK(args, expr, msg, name, valid) static_assert((requires args expr) == valid, msg)


#define CHECK_VALID(args, expr, msg) DO_CHECK(args, expr, msg, tests_check_valid, true)
#define CHECK_INVALID(args, expr, msg) DO_CHECK(args, expr, msg, tests_check_invalid, false)

/*
template<typename T>
concept cast_test = requires(sn::u16 v) {
    sn::u16(127).to<sn::u8>();
};*/

CHECK_VALID((), { sn::u16(277).as<sn::u32>(); }, "to cast");

template<typename F, typename T>
concept check_as = requires(F v) {
    v.template as<T>();
};

static_assert(check_as<u32, u32>, "can convert to own type safely");

template<typename Lhs, typename Rhs>
concept adding_test = requires(Lhs lhs, Rhs rhs) {
    lhs + rhs;
};

template<typename Lhs, typename Rhs, typename Retr>
concept adding_test_retr = requires(Lhs lhs, Rhs rhs) {
    { lhs + rhs } -> std::same_as<Retr>;
};

static_assert(!adding_test<sn::i32, sn::u32>, "adding different signs is invalid");
static_assert(adding_test<sn::i32, sn::i32>, "adding different signs is valid");

static_assert(adding_test_retr<sn::i32, sn::i32, sn::i32>, "adding same width results in same type");

TEST_CASE("adding works") {
    sn::u32 l{32};
    sn::u8 r{7};
    CHECK(l + r.as<u32>() == sn::u32{32 + 7});
    l += u32{3};
    CHECK(l == u32{32 + 3});
}

TEST_CASE("shifting works") {
    sn::u32 l{65};
    l <<= 2;
    CHECK(l == sn::u32{65 << 2});
}

TEST_CASE("numeric conversions") {
    constexpr auto vnum = 277;
    u16 v{vnum};
    SUBCASE("checked as is safe for valid values") {
        CHECK(v.as<u64>() == u64{vnum});
    }
    SUBCASE("checked as throws for narrowing conversions") {
        CHECK_THROWS(v.as<u8>());
    }
}
