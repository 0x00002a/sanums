
set(TEST_SRC main.cpp)

add_executable(tests ${TEST_SRC})


find_package(doctest REQUIRED)

target_link_libraries(tests sanums doctest::doctest)

sanums_add_flags(tests)

