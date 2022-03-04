
#include <sanums.hpp>
#include <functional>
#include <type_traits>

#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN
#include <doctest/doctest.h>

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

#define DO_CHECK(args, expr, msg, name, valid) \
    template<typename T> \
    concept name = requires args expr; \
    static_assert(name <void> == valid, msg)


#define CHECK_VALID(args, expr, msg) DO_CHECK(args, expr, msg, tests_check_valid##__LINE__, true)
#define CHECK_INVALID(args, expr, msg) DO_CHECK(args, expr, msg, tests_check_invalid##__LINE__, false)

/*
template<typename T>
concept cast_test = requires(sn::u16 v) {
    sn::u16(127).to<sn::u8>();
};*/

CHECK_VALID((), { sn::u16(277).to<sn::u16>(); }, "to cast");



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
static_assert(adding_test_retr<sn::i64, sn::i32, sn::i64>, "adding different widths results in the larger type");

TEST_CASE("adding works") {
    sn::u32 l{32};
    sn::u8 r{7};
    CHECK(l + r == sn::u32{32 + 7});
}

TEST_CASE("shifting works") {
    sn::u32 l{65};
    l <<= 2;
    CHECK(l == sn::u32{65 << 2});
}
