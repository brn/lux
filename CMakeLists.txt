project(lux CXX)
cmake_minimum_required(VERSION 2.8)

set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/build)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/build)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/build)

set(CMAKE_CXX_COMPILER ./tools/clang+llvm-5.0.1-final-x86_64-apple-darwin/bin/clang++)
set(CMAKE_CXX_FLAGS "-std=c++1y -Wall")
set(CMAKE_CXX_FLAGS_DEBUG "-g3 -O0 -pg")
set(CMAKE_CXX_FLAGS_RELEASE "-O2 -s -DNDEBUG -march=native")

if(CMAKE_BUILD_TYPE STREQUAL "Test")
  enable_testing()
  include_directories(./googletest/googletest/include/)
  link_directories(./googletest/build/googlemock/gtest)
  get_filename_component(unicode_texts_dir "./test/unicode_texts" REALPATH)
  add_definitions(-DUNICODE_TEXTS_DIR="${unicode_texts_dir}")
  add_definitions(-DDEBUG)
  add_definitions(-DLUX_TEST)
  add_definitions(-DPLATFORM_POSIX)

  add_executable(parser_test
    src/platform/os.cc
    src/node.cc
    src/isolate.cc
    src/heap.cc
    src/reporter.cc
    src/unicode.cc
    src/parser.cc
    src/debug/stacktrace.cc
    src/zone.cc
    test/cases/parser_test.cc)
  target_link_libraries(parser_test libgtest.a libgtest_main.a)

  add_executable(unicode_test
    src/platform/os.cc
    src/isolate.cc
    src/heap.cc
    src/unicode.cc
    src/debug/stacktrace.cc
    test/cases/unicode_test.cc)
  target_link_libraries(unicode_test libgtest.a libgtest_main.a)
endif()

include_directories(./tools/clang+llvm-5.0.1-final-x86_64-apple-darwin/include/c++/v1)
add_executable(lux
  src/platform/os.cc
  src/main.cc
  src/node.cc
  src/heap.cc
  src/reporter.cc
  src/unicode.cc
  src/isolate.cc
  src/debug/stacktrace.cc)
target_link_libraries(lux)
