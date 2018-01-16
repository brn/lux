project(i6 CXX)
cmake_minimum_required(VERSION 2.8)
set(CMAKE_CXX_COMPILER ./tools/clang+llvm-5.0.1-final-x86_64-apple-darwin/bin/clang++)
set(CMAKE_CXX_FLAGS "-std=c++1y -Wall")
set(CMAKE_CXX_FLAGS_DEBUG "-g3 -O0 -pg")
set(CMAKE_CXX_FLAGS_RELEASE "-O2 -s -DNDEBUG -march=native")
include_directories(./tools/clang+llvm-5.0.1-final-x86_64-apple-darwin/include/c++/v1)
add_executable(i6
./src/main.cc
./src/heap.cc
./src/unicode.cc
./src/isolate.cc
./src/ast/ast.cc
./src/debug/stacktrace.cc)
target_link_libraries(i6)
