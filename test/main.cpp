
#include <sanums.hpp>
#include <functional>
#include <type_traits>


template<typename Func>
concept check_is_valid = requires(Func f) {
    std::invoke(f);
};

#define DO_CHECK(expr, msg, name, valid) \
    template<typename T> \
    concept name = requires() expr
    //static_assert(name<int> == valid, msg)


#define CHECK_VALID(expr, msg) DO_CHECK(expr, msg, tests_check_valid##__LINE__, true)
    
#define CHECK_INVALID(expr, msg) DO_CHECK(expr, msg, tests_check_invalid##__LINE__, false)




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

int main() {

}

