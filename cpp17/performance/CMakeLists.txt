cmake_minimum_required(VERSION 3.2)
include("cmake/HunterGate.cmake")
HunterGate(
    URL "https://github.com/ruslo/hunter/archive/v0.22.1.tar.gz"
    SHA1 "3d87bd3c7a54126cb0822a59d3789970c000ab19"
)

project(performance)

hunter_add_package(benchmark)
find_package(benchmark CONFIG REQUIRED)

add_executable(performance performance.cpp)
target_compile_options(performance PRIVATE -std=c++17 -Wall -Werror -pedantic -O3 -g -march=native  -fno-omit-frame-pointer -fsanitize=address -fsanitize-address-use-after-scope)
target_link_libraries(performance PRIVATE benchmark::benchmark -fsanitize=address)
