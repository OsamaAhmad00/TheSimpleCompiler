cmake_minimum_required(VERSION 3.28)
project(TheSimpleCompiler)

set(CMAKE_CXX_STANDARD 23)

set(CMAKE_CXX_SCAN_FOR_MODULES OFF)

if(UNIX)
    set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -fsanitize=undefined")
    set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -fsanitize=address")
endif()

add_compile_options(-Wall -Werror -Wextra -Wpedantic -Wconversion -Wshadow)

add_executable(TheSimpleCompiler main.cpp)
