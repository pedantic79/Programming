cmake_minimum_required (VERSION 3.0.0)
project (performance)

add_executable(performance performance.cpp)
target_compile_options(performance PRIVATE -std=c++17 -Wall -Werror -pedantic -O3 -g -march=native  -fno-omit-frame-pointer -fsanitize=address -fsanitize-address-use-after-scope)
target_link_libraries(performance PRIVATE -fsanitize=address)
