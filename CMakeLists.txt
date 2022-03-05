cmake_minimum_required(VERSION 3.1)

project(sanums VERSION 0.1.0)

set(SRC src/sanums.hpp)
set(CMAKE_CXX_STANDARD 20)


function(sanums_add_flags TARGET)
    if (${CMAKE_CXX_COMPILER_ID} MATCHES "(GNU|AppleClang|Clang)")
        target_compile_options(${TARGET} PUBLIC -fdiagnostics-color=always -Werror -Wall -Wpedantic)
    endif()
endfunction()

add_library(sanums INTERFACE ${SRC})

target_include_directories(sanums INTERFACE src)

add_subdirectory(test)

