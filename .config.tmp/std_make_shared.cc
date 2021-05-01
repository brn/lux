#include <memory>
#include <stdio.h>
template<typename T>
void test_holder(T v) {printf("ok!\n");};
                                int main(int argc, char** argv) {
                                     test_holder(std::make_shared<char>(5));

                                   }
