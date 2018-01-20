// The MIT License (MIT)
//
// Copyright (c) Taketoshi Aono(brn)
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.


#ifndef SRC_PLATFORM_OS_H_
#define SRC_PLATFORM_OS_H_

#include <stdlib.h>

#include <errno.h>
#include <string>
#include <stdexcept>

#ifdef _WIN32
#define K_ERRNO _doserrno
#else
#define K_ERRNO errno
#endif

namespace lux {
template <typename T, typename Traits, typename Allocator>
void Strerror(std::basic_string<T, std::char_traits<Traits>, Allocator>* buf,
              int err);


void Printf(const char* format, ...);


template <typename T, typename Traits, typename Allocator>
void SPrintf(std::basic_string<T, std::char_traits<Traits>, Allocator>* buf,
             bool append, const char* format, ...);


template <typename T, typename Traits, typename Allocator>
void VSPrintf(std::basic_string<T, std::char_traits<Traits>, Allocator>* buf,
              bool append, const char* format, va_list args);


void VFPrintf(FILE* fp, const char* format, ...);


void FPrintf(FILE* fp, const char* format, ...);


FILE* FOpen(const char* filename, const char* mode);


size_t FRead(void* buffer,
             size_t buffer_size,
             size_t element_size,
             size_t count,
             FILE* fp);


void FClose(FILE* fp);


template <typename T, typename Traits, typename Allocator>
void GetEnv(std::basic_string<T, std::char_traits<Traits>, Allocator> *buf,
            const char* env);


bool Sleep(int nano_time);
int Utime(const char* path);
time_t Time(time_t* time);


template <typename T, typename Traits, typename Allocator>
int Asctime(std::basic_string<T, std::char_traits<Traits>, Allocator>* buf,
            tm* tm);
int LocalTime(tm* t, time_t* time);
void OnExit(void(*callback)());


template <typename T, typename Traits, typename Allocator>
void GetLastError(
    std::basic_string<T, std::char_traits<Traits>, Allocator>* buf);
FILE* POpen(const char* name, const char* mode);
void PClose(FILE* fp);
char* Strdup(const char* path);
void Strcpy(char* dest, const char* src, size_t length);

}  // namespace lux

#include "./os-inl.h"

#endif  // SRC_PLATFORM_OS_H_
