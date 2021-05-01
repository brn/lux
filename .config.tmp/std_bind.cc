#include <functional>
#include <stdio.h>
template<typename T>
void test_holder(T v) {printf("ok!\n");};
                                int main(int argc, char** argv) {
                                     test_holder(std::bind(fopen, std::placeholders::_1, "rb"));

                                   }
