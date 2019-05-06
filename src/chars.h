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

#ifndef SRC_CHARS_H_
#define SRC_CHARS_H_

#include <vector>
#include "./utils.h"

namespace lux {

class Chars {
 public:
  LUX_INLINE static bool IsAscii(u32 c) { return c > 0 && c < 127; }

  LUX_INLINE static bool IsWhiteSpace(u32 c) {
    return IsAscii(c) &&
           (c == u32(0x09) || c == u32(0x0b) || c == u32(0x0c) ||
            c == u32(0x20) || c == u32(255) || c == u32(0x2028) ||
            c == u32(0x1680) || c == u32(0x180E) ||
            (c >= u32(0x2000) && c <= u32(0x200A)) || c == u32(0x2028) ||
            c == u32(0x2029) || c == u32(0x202F) || c == u32(0x205F) ||
            c == u32(0x3000));
  }

  LUX_INLINE static bool IsUtfSignature(u32 c) { return c == u32(0xFEFF); }

  LUX_INLINE static bool IsIdentifierStart(u32 value) {
    return (value >= 'A' && value <= 'Z') || (value >= 'a' && value <= 'z') ||
           value == 0x00AA || value == 0x00B5 || value == 0x00BA ||
           (value >= 0x00C0 && value <= 0x00D6) ||
           (value >= 0x00D8 && value <= 0x00F6) ||
           (value >= 0x00F8 && value <= 0x02C1) ||
           (value >= 0x02C6 && value <= 0x02D1) ||
           (value >= 0x02E0 && value <= 0x02E4) || value == 0x02EC ||
           value == 0x02EE || (value >= 0x0370 && value <= 0x0374) ||
           value == 0x0376 || value == 0x0377 ||
           (value >= 0x037A && value <= 0x037D) || value == 0x037F ||
           value == 0x0386 || (value >= 0x0388 && value <= 0x038A) ||
           value == 0x038C || (value >= 0x038E && value <= 0x03A1) ||
           (value >= 0x03A3 && value <= 0x03F5) ||
           (value >= 0x03F7 && value <= 0x0481) ||
           (value >= 0x048A && value <= 0x052F) ||
           (value >= 0x0531 && value <= 0x0556) || value == 0x0559 ||
           (value >= 0x0560 && value <= 0x0588) ||
           (value >= 0x05D0 && value <= 0x05EA) ||
           (value >= 0x05EF && value <= 0x05F2) ||
           (value >= 0x0620 && value <= 0x064A) || value == 0x066E ||
           value == 0x066F || (value >= 0x0671 && value <= 0x06D3) ||
           value == 0x06D5 || value == 0x06E5 || value == 0x06E6 ||
           value == 0x06EE || value == 0x06EF ||
           (value >= 0x06FA && value <= 0x06FC) || value == 0x06FF ||
           value == 0x0710 || (value >= 0x0712 && value <= 0x072F) ||
           (value >= 0x074D && value <= 0x07A5) || value == 0x07B1 ||
           (value >= 0x07CA && value <= 0x07EA) || value == 0x07F4 ||
           value == 0x07F5 || value == 0x07FA ||
           (value >= 0x0800 && value <= 0x0815) || value == 0x081A ||
           value == 0x0824 || value == 0x0828 ||
           (value >= 0x0840 && value <= 0x0858) ||
           (value >= 0x0860 && value <= 0x086A) ||
           (value >= 0x08A0 && value <= 0x08B4) ||
           (value >= 0x08B6 && value <= 0x08BD) ||
           (value >= 0x0904 && value <= 0x0939) || value == 0x093D ||
           value == 0x0950 || (value >= 0x0958 && value <= 0x0961) ||
           (value >= 0x0971 && value <= 0x0980) ||
           (value >= 0x0985 && value <= 0x098C) || value == 0x098F ||
           value == 0x0990 || (value >= 0x0993 && value <= 0x09A8) ||
           (value >= 0x09AA && value <= 0x09B0) || value == 0x09B2 ||
           (value >= 0x09B6 && value <= 0x09B9) || value == 0x09BD ||
           value == 0x09CE || value == 0x09DC || value == 0x09DD ||
           (value >= 0x09DF && value <= 0x09E1) || value == 0x09F0 ||
           value == 0x09F1 || value == 0x09FC ||
           (value >= 0x0A05 && value <= 0x0A0A) || value == 0x0A0F ||
           value == 0x0A10 || (value >= 0x0A13 && value <= 0x0A28) ||
           (value >= 0x0A2A && value <= 0x0A30) || value == 0x0A32 ||
           value == 0x0A33 || value == 0x0A35 || value == 0x0A36 ||
           value == 0x0A38 || value == 0x0A39 ||
           (value >= 0x0A59 && value <= 0x0A5C) || value == 0x0A5E ||
           (value >= 0x0A72 && value <= 0x0A74) ||
           (value >= 0x0A85 && value <= 0x0A8D) ||
           (value >= 0x0A8F && value <= 0x0A91) ||
           (value >= 0x0A93 && value <= 0x0AA8) ||
           (value >= 0x0AAA && value <= 0x0AB0) || value == 0x0AB2 ||
           value == 0x0AB3 || (value >= 0x0AB5 && value <= 0x0AB9) ||
           value == 0x0ABD || value == 0x0AD0 || value == 0x0AE0 ||
           value == 0x0AE1 || value == 0x0AF9 ||
           (value >= 0x0B05 && value <= 0x0B0C) || value == 0x0B0F ||
           value == 0x0B10 || (value >= 0x0B13 && value <= 0x0B28) ||
           (value >= 0x0B2A && value <= 0x0B30) || value == 0x0B32 ||
           value == 0x0B33 || (value >= 0x0B35 && value <= 0x0B39) ||
           value == 0x0B3D || value == 0x0B5C || value == 0x0B5D ||
           (value >= 0x0B5F && value <= 0x0B61) || value == 0x0B71 ||
           value == 0x0B83 || (value >= 0x0B85 && value <= 0x0B8A) ||
           (value >= 0x0B8E && value <= 0x0B90) ||
           (value >= 0x0B92 && value <= 0x0B95) || value == 0x0B99 ||
           value == 0x0B9A || value == 0x0B9C || value == 0x0B9E ||
           value == 0x0B9F || value == 0x0BA3 || value == 0x0BA4 ||
           (value >= 0x0BA8 && value <= 0x0BAA) ||
           (value >= 0x0BAE && value <= 0x0BB9) || value == 0x0BD0 ||
           (value >= 0x0C05 && value <= 0x0C0C) ||
           (value >= 0x0C0E && value <= 0x0C10) ||
           (value >= 0x0C12 && value <= 0x0C28) ||
           (value >= 0x0C2A && value <= 0x0C39) || value == 0x0C3D ||
           (value >= 0x0C58 && value <= 0x0C5A) || value == 0x0C60 ||
           value == 0x0C61 || value == 0x0C80 ||
           (value >= 0x0C85 && value <= 0x0C8C) ||
           (value >= 0x0C8E && value <= 0x0C90) ||
           (value >= 0x0C92 && value <= 0x0CA8) ||
           (value >= 0x0CAA && value <= 0x0CB3) ||
           (value >= 0x0CB5 && value <= 0x0CB9) || value == 0x0CBD ||
           value == 0x0CDE || value == 0x0CE0 || value == 0x0CE1 ||
           value == 0x0CF1 || value == 0x0CF2 ||
           (value >= 0x0D05 && value <= 0x0D0C) ||
           (value >= 0x0D0E && value <= 0x0D10) ||
           (value >= 0x0D12 && value <= 0x0D3A) || value == 0x0D3D ||
           value == 0x0D4E || (value >= 0x0D54 && value <= 0x0D56) ||
           (value >= 0x0D5F && value <= 0x0D61) ||
           (value >= 0x0D7A && value <= 0x0D7F) ||
           (value >= 0x0D85 && value <= 0x0D96) ||
           (value >= 0x0D9A && value <= 0x0DB1) ||
           (value >= 0x0DB3 && value <= 0x0DBB) || value == 0x0DBD ||
           (value >= 0x0DC0 && value <= 0x0DC6) ||
           (value >= 0x0E01 && value <= 0x0E30) || value == 0x0E32 ||
           value == 0x0E33 || (value >= 0x0E40 && value <= 0x0E46) ||
           value == 0x0E81 || value == 0x0E82 || value == 0x0E84 ||
           (value >= 0x0E86 && value <= 0x0E8A) ||
           (value >= 0x0E8C && value <= 0x0EA3) || value == 0x0EA5 ||
           (value >= 0x0EA7 && value <= 0x0EB0) || value == 0x0EB2 ||
           value == 0x0EB3 || value == 0x0EBD ||
           (value >= 0x0EC0 && value <= 0x0EC4) || value == 0x0EC6 ||
           (value >= 0x0EDC && value <= 0x0EDF) || value == 0x0F00 ||
           (value >= 0x0F40 && value <= 0x0F47) ||
           (value >= 0x0F49 && value <= 0x0F6C) ||
           (value >= 0x0F88 && value <= 0x0F8C) ||
           (value >= 0x1000 && value <= 0x102A) || value == 0x103F ||
           (value >= 0x1050 && value <= 0x1055) ||
           (value >= 0x105A && value <= 0x105D) || value == 0x1061 ||
           value == 0x1065 || value == 0x1066 ||
           (value >= 0x106E && value <= 0x1070) ||
           (value >= 0x1075 && value <= 0x1081) || value == 0x108E ||
           (value >= 0x10A0 && value <= 0x10C5) || value == 0x10C7 ||
           value == 0x10CD || (value >= 0x10D0 && value <= 0x10FA) ||
           (value >= 0x10FC && value <= 0x1248) ||
           (value >= 0x124A && value <= 0x124D) ||
           (value >= 0x1250 && value <= 0x1256) || value == 0x1258 ||
           (value >= 0x125A && value <= 0x125D) ||
           (value >= 0x1260 && value <= 0x1288) ||
           (value >= 0x128A && value <= 0x128D) ||
           (value >= 0x1290 && value <= 0x12B0) ||
           (value >= 0x12B2 && value <= 0x12B5) ||
           (value >= 0x12B8 && value <= 0x12BE) || value == 0x12C0 ||
           (value >= 0x12C2 && value <= 0x12C5) ||
           (value >= 0x12C8 && value <= 0x12D6) ||
           (value >= 0x12D8 && value <= 0x1310) ||
           (value >= 0x1312 && value <= 0x1315) ||
           (value >= 0x1318 && value <= 0x135A) ||
           (value >= 0x1380 && value <= 0x138F) ||
           (value >= 0x13A0 && value <= 0x13F5) ||
           (value >= 0x13F8 && value <= 0x13FD) ||
           (value >= 0x1401 && value <= 0x166C) ||
           (value >= 0x166F && value <= 0x167F) ||
           (value >= 0x1681 && value <= 0x169A) ||
           (value >= 0x16A0 && value <= 0x16EA) ||
           (value >= 0x16EE && value <= 0x16F8) ||
           (value >= 0x1700 && value <= 0x170C) ||
           (value >= 0x170E && value <= 0x1711) ||
           (value >= 0x1720 && value <= 0x1731) ||
           (value >= 0x1740 && value <= 0x1751) ||
           (value >= 0x1760 && value <= 0x176C) ||
           (value >= 0x176E && value <= 0x1770) ||
           (value >= 0x1780 && value <= 0x17B3) || value == 0x17D7 ||
           value == 0x17DC || (value >= 0x1820 && value <= 0x1878) ||
           (value >= 0x1880 && value <= 0x18A8) || value == 0x18AA ||
           (value >= 0x18B0 && value <= 0x18F5) ||
           (value >= 0x1900 && value <= 0x191E) ||
           (value >= 0x1950 && value <= 0x196D) ||
           (value >= 0x1970 && value <= 0x1974) ||
           (value >= 0x1980 && value <= 0x19AB) ||
           (value >= 0x19B0 && value <= 0x19C9) ||
           (value >= 0x1A00 && value <= 0x1A16) ||
           (value >= 0x1A20 && value <= 0x1A54) || value == 0x1AA7 ||
           (value >= 0x1B05 && value <= 0x1B33) ||
           (value >= 0x1B45 && value <= 0x1B4B) ||
           (value >= 0x1B83 && value <= 0x1BA0) || value == 0x1BAE ||
           value == 0x1BAF || (value >= 0x1BBA && value <= 0x1BE5) ||
           (value >= 0x1C00 && value <= 0x1C23) ||
           (value >= 0x1C4D && value <= 0x1C4F) ||
           (value >= 0x1C5A && value <= 0x1C7D) ||
           (value >= 0x1C80 && value <= 0x1C88) ||
           (value >= 0x1C90 && value <= 0x1CBA) ||
           (value >= 0x1CBD && value <= 0x1CBF) ||
           (value >= 0x1CE9 && value <= 0x1CEC) ||
           (value >= 0x1CEE && value <= 0x1CF3) || value == 0x1CF5 ||
           value == 0x1CF6 || value == 0x1CFA ||
           (value >= 0x1D00 && value <= 0x1DBF) ||
           (value >= 0x1E00 && value <= 0x1F15) ||
           (value >= 0x1F18 && value <= 0x1F1D) ||
           (value >= 0x1F20 && value <= 0x1F45) ||
           (value >= 0x1F48 && value <= 0x1F4D) ||
           (value >= 0x1F50 && value <= 0x1F57) || value == 0x1F59 ||
           value == 0x1F5B || value == 0x1F5D ||
           (value >= 0x1F5F && value <= 0x1F7D) ||
           (value >= 0x1F80 && value <= 0x1FB4) ||
           (value >= 0x1FB6 && value <= 0x1FBC) || value == 0x1FBE ||
           (value >= 0x1FC2 && value <= 0x1FC4) ||
           (value >= 0x1FC6 && value <= 0x1FCC) ||
           (value >= 0x1FD0 && value <= 0x1FD3) ||
           (value >= 0x1FD6 && value <= 0x1FDB) ||
           (value >= 0x1FE0 && value <= 0x1FEC) ||
           (value >= 0x1FF2 && value <= 0x1FF4) ||
           (value >= 0x1FF6 && value <= 0x1FFC) || value == 0x2071 ||
           value == 0x207F || (value >= 0x2090 && value <= 0x209C) ||
           value == 0x2102 || value == 0x2107 ||
           (value >= 0x210A && value <= 0x2113) || value == 0x2115 ||
           (value >= 0x2118 && value <= 0x211D) || value == 0x2124 ||
           value == 0x2126 || value == 0x2128 ||
           (value >= 0x212A && value <= 0x2139) ||
           (value >= 0x213C && value <= 0x213F) ||
           (value >= 0x2145 && value <= 0x2149) || value == 0x214E ||
           (value >= 0x2160 && value <= 0x2188) ||
           (value >= 0x2C00 && value <= 0x2C2E) ||
           (value >= 0x2C30 && value <= 0x2C5E) ||
           (value >= 0x2C60 && value <= 0x2CE4) ||
           (value >= 0x2CEB && value <= 0x2CEE) || value == 0x2CF2 ||
           value == 0x2CF3 || (value >= 0x2D00 && value <= 0x2D25) ||
           value == 0x2D27 || value == 0x2D2D ||
           (value >= 0x2D30 && value <= 0x2D67) || value == 0x2D6F ||
           (value >= 0x2D80 && value <= 0x2D96) ||
           (value >= 0x2DA0 && value <= 0x2DA6) ||
           (value >= 0x2DA8 && value <= 0x2DAE) ||
           (value >= 0x2DB0 && value <= 0x2DB6) ||
           (value >= 0x2DB8 && value <= 0x2DBE) ||
           (value >= 0x2DC0 && value <= 0x2DC6) ||
           (value >= 0x2DC8 && value <= 0x2DCE) ||
           (value >= 0x2DD0 && value <= 0x2DD6) ||
           (value >= 0x2DD8 && value <= 0x2DDE) ||
           (value >= 0x3005 && value <= 0x3007) ||
           (value >= 0x3021 && value <= 0x3029) ||
           (value >= 0x3031 && value <= 0x3035) ||
           (value >= 0x3038 && value <= 0x303C) ||
           (value >= 0x3041 && value <= 0x3096) ||
           (value >= 0x309B && value <= 0x309F) ||
           (value >= 0x30A1 && value <= 0x30FA) ||
           (value >= 0x30FC && value <= 0x30FF) ||
           (value >= 0x3105 && value <= 0x312F) ||
           (value >= 0x3131 && value <= 0x318E) ||
           (value >= 0x31A0 && value <= 0x31BA) ||
           (value >= 0x31F0 && value <= 0x31FF) ||
           (value >= 0x3400 && value <= 0x4DB5) ||
           (value >= 0x4E00 && value <= 0x9FEF) ||
           (value >= 0xA000 && value <= 0xA48C) ||
           (value >= 0xA4D0 && value <= 0xA4FD) ||
           (value >= 0xA500 && value <= 0xA60C) ||
           (value >= 0xA610 && value <= 0xA61F) || value == 0xA62A ||
           value == 0xA62B || (value >= 0xA640 && value <= 0xA66E) ||
           (value >= 0xA67F && value <= 0xA69D) ||
           (value >= 0xA6A0 && value <= 0xA6EF) ||
           (value >= 0xA717 && value <= 0xA71F) ||
           (value >= 0xA722 && value <= 0xA788) ||
           (value >= 0xA78B && value <= 0xA7BF) ||
           (value >= 0xA7C2 && value <= 0xA7C6) ||
           (value >= 0xA7F7 && value <= 0xA801) ||
           (value >= 0xA803 && value <= 0xA805) ||
           (value >= 0xA807 && value <= 0xA80A) ||
           (value >= 0xA80C && value <= 0xA822) ||
           (value >= 0xA840 && value <= 0xA873) ||
           (value >= 0xA882 && value <= 0xA8B3) ||
           (value >= 0xA8F2 && value <= 0xA8F7) || value == 0xA8FB ||
           value == 0xA8FD || value == 0xA8FE ||
           (value >= 0xA90A && value <= 0xA925) ||
           (value >= 0xA930 && value <= 0xA946) ||
           (value >= 0xA960 && value <= 0xA97C) ||
           (value >= 0xA984 && value <= 0xA9B2) || value == 0xA9CF ||
           (value >= 0xA9E0 && value <= 0xA9E4) ||
           (value >= 0xA9E6 && value <= 0xA9EF) ||
           (value >= 0xA9FA && value <= 0xA9FE) ||
           (value >= 0xAA00 && value <= 0xAA28) ||
           (value >= 0xAA40 && value <= 0xAA42) ||
           (value >= 0xAA44 && value <= 0xAA4B) ||
           (value >= 0xAA60 && value <= 0xAA76) || value == 0xAA7A ||
           (value >= 0xAA7E && value <= 0xAAAF) || value == 0xAAB1 ||
           value == 0xAAB5 || value == 0xAAB6 ||
           (value >= 0xAAB9 && value <= 0xAABD) || value == 0xAAC0 ||
           value == 0xAAC2 || (value >= 0xAADB && value <= 0xAADD) ||
           (value >= 0xAAE0 && value <= 0xAAEA) ||
           (value >= 0xAAF2 && value <= 0xAAF4) ||
           (value >= 0xAB01 && value <= 0xAB06) ||
           (value >= 0xAB09 && value <= 0xAB0E) ||
           (value >= 0xAB11 && value <= 0xAB16) ||
           (value >= 0xAB20 && value <= 0xAB26) ||
           (value >= 0xAB28 && value <= 0xAB2E) ||
           (value >= 0xAB30 && value <= 0xAB5A) ||
           (value >= 0xAB5C && value <= 0xAB67) ||
           (value >= 0xAB70 && value <= 0xABE2) ||
           (value >= 0xAC00 && value <= 0xD7A3) ||
           (value >= 0xD7B0 && value <= 0xD7C6) ||
           (value >= 0xD7CB && value <= 0xD7FB) ||
           (value >= 0xF900 && value <= 0xFA6D) ||
           (value >= 0xFA70 && value <= 0xFAD9) ||
           (value >= 0xFB00 && value <= 0xFB06) ||
           (value >= 0xFB13 && value <= 0xFB17) || value == 0xFB1D ||
           (value >= 0xFB1F && value <= 0xFB28) ||
           (value >= 0xFB2A && value <= 0xFB36) ||
           (value >= 0xFB38 && value <= 0xFB3C) || value == 0xFB3E ||
           value == 0xFB40 || value == 0xFB41 || value == 0xFB43 ||
           value == 0xFB44 || (value >= 0xFB46 && value <= 0xFBB1) ||
           (value >= 0xFBD3 && value <= 0xFD3D) ||
           (value >= 0xFD50 && value <= 0xFD8F) ||
           (value >= 0xFD92 && value <= 0xFDC7) ||
           (value >= 0xFDF0 && value <= 0xFDFB) ||
           (value >= 0xFE70 && value <= 0xFE74) ||
           (value >= 0xFE76 && value <= 0xFEFC) ||
           (value >= 0xFF21 && value <= 0xFF3A) ||
           (value >= 0xFF41 && value <= 0xFF5A) ||
           (value >= 0xFF66 && value <= 0xFFBE) ||
           (value >= 0xFFC2 && value <= 0xFFC7) ||
           (value >= 0xFFCA && value <= 0xFFCF) ||
           (value >= 0xFFD2 && value <= 0xFFD7) ||
           (value >= 0xFFDA && value <= 0xFFDC) ||
           (value >= 0x00010000 && value <= 0x0001000B) ||
           (value >= 0x0001000D && value <= 0x00010026) ||
           (value >= 0x00010028 && value <= 0x0001003A) ||
           value == 0x0001003C || value == 0x0001003D ||
           (value >= 0x0001003F && value <= 0x0001004D) ||
           (value >= 0x00010050 && value <= 0x0001005D) ||
           (value >= 0x00010080 && value <= 0x000100FA) ||
           (value >= 0x00010140 && value <= 0x00010174) ||
           (value >= 0x00010280 && value <= 0x0001029C) ||
           (value >= 0x000102A0 && value <= 0x000102D0) ||
           (value >= 0x00010300 && value <= 0x0001031F) ||
           (value >= 0x0001032D && value <= 0x0001034A) ||
           (value >= 0x00010350 && value <= 0x00010375) ||
           (value >= 0x00010380 && value <= 0x0001039D) ||
           (value >= 0x000103A0 && value <= 0x000103C3) ||
           (value >= 0x000103C8 && value <= 0x000103CF) ||
           (value >= 0x000103D1 && value <= 0x000103D5) ||
           (value >= 0x00010400 && value <= 0x0001049D) ||
           (value >= 0x000104B0 && value <= 0x000104D3) ||
           (value >= 0x000104D8 && value <= 0x000104FB) ||
           (value >= 0x00010500 && value <= 0x00010527) ||
           (value >= 0x00010530 && value <= 0x00010563) ||
           (value >= 0x00010600 && value <= 0x00010736) ||
           (value >= 0x00010740 && value <= 0x00010755) ||
           (value >= 0x00010760 && value <= 0x00010767) ||
           (value >= 0x00010800 && value <= 0x00010805) ||
           value == 0x00010808 ||
           (value >= 0x0001080A && value <= 0x00010835) ||
           value == 0x00010837 || value == 0x00010838 || value == 0x0001083C ||
           (value >= 0x0001083F && value <= 0x00010855) ||
           (value >= 0x00010860 && value <= 0x00010876) ||
           (value >= 0x00010880 && value <= 0x0001089E) ||
           (value >= 0x000108E0 && value <= 0x000108F2) ||
           value == 0x000108F4 || value == 0x000108F5 ||
           (value >= 0x00010900 && value <= 0x00010915) ||
           (value >= 0x00010920 && value <= 0x00010939) ||
           (value >= 0x00010980 && value <= 0x000109B7) ||
           value == 0x000109BE || value == 0x000109BF || value == 0x00010A00 ||
           (value >= 0x00010A10 && value <= 0x00010A13) ||
           (value >= 0x00010A15 && value <= 0x00010A17) ||
           (value >= 0x00010A19 && value <= 0x00010A35) ||
           (value >= 0x00010A60 && value <= 0x00010A7C) ||
           (value >= 0x00010A80 && value <= 0x00010A9C) ||
           (value >= 0x00010AC0 && value <= 0x00010AC7) ||
           (value >= 0x00010AC9 && value <= 0x00010AE4) ||
           (value >= 0x00010B00 && value <= 0x00010B35) ||
           (value >= 0x00010B40 && value <= 0x00010B55) ||
           (value >= 0x00010B60 && value <= 0x00010B72) ||
           (value >= 0x00010B80 && value <= 0x00010B91) ||
           (value >= 0x00010C00 && value <= 0x00010C48) ||
           (value >= 0x00010C80 && value <= 0x00010CB2) ||
           (value >= 0x00010CC0 && value <= 0x00010CF2) ||
           (value >= 0x00010D00 && value <= 0x00010D23) ||
           (value >= 0x00010F00 && value <= 0x00010F1C) ||
           value == 0x00010F27 ||
           (value >= 0x00010F30 && value <= 0x00010F45) ||
           (value >= 0x00010FE0 && value <= 0x00010FF6) ||
           (value >= 0x00011003 && value <= 0x00011037) ||
           (value >= 0x00011083 && value <= 0x000110AF) ||
           (value >= 0x000110D0 && value <= 0x000110E8) ||
           (value >= 0x00011103 && value <= 0x00011126) ||
           value == 0x00011144 ||
           (value >= 0x00011150 && value <= 0x00011172) ||
           value == 0x00011176 ||
           (value >= 0x00011183 && value <= 0x000111B2) ||
           (value >= 0x000111C1 && value <= 0x000111C4) ||
           value == 0x000111DA || value == 0x000111DC ||
           (value >= 0x00011200 && value <= 0x00011211) ||
           (value >= 0x00011213 && value <= 0x0001122B) ||
           (value >= 0x00011280 && value <= 0x00011286) ||
           value == 0x00011288 ||
           (value >= 0x0001128A && value <= 0x0001128D) ||
           (value >= 0x0001128F && value <= 0x0001129D) ||
           (value >= 0x0001129F && value <= 0x000112A8) ||
           (value >= 0x000112B0 && value <= 0x000112DE) ||
           (value >= 0x00011305 && value <= 0x0001130C) ||
           value == 0x0001130F || value == 0x00011310 ||
           (value >= 0x00011313 && value <= 0x00011328) ||
           (value >= 0x0001132A && value <= 0x00011330) ||
           value == 0x00011332 || value == 0x00011333 ||
           (value >= 0x00011335 && value <= 0x00011339) ||
           value == 0x0001133D || value == 0x00011350 ||
           (value >= 0x0001135D && value <= 0x00011361) ||
           (value >= 0x00011400 && value <= 0x00011434) ||
           (value >= 0x00011447 && value <= 0x0001144A) ||
           value == 0x0001145F ||
           (value >= 0x00011480 && value <= 0x000114AF) ||
           value == 0x000114C4 || value == 0x000114C5 || value == 0x000114C7 ||
           (value >= 0x00011580 && value <= 0x000115AE) ||
           (value >= 0x000115D8 && value <= 0x000115DB) ||
           (value >= 0x00011600 && value <= 0x0001162F) ||
           value == 0x00011644 ||
           (value >= 0x00011680 && value <= 0x000116AA) ||
           value == 0x000116B8 ||
           (value >= 0x00011700 && value <= 0x0001171A) ||
           (value >= 0x00011800 && value <= 0x0001182B) ||
           (value >= 0x000118A0 && value <= 0x000118DF) ||
           value == 0x000118FF ||
           (value >= 0x000119A0 && value <= 0x000119A7) ||
           (value >= 0x000119AA && value <= 0x000119D0) ||
           value == 0x000119E1 || value == 0x000119E3 || value == 0x00011A00 ||
           (value >= 0x00011A0B && value <= 0x00011A32) ||
           value == 0x00011A3A || value == 0x00011A50 ||
           (value >= 0x00011A5C && value <= 0x00011A89) ||
           value == 0x00011A9D ||
           (value >= 0x00011AC0 && value <= 0x00011AF8) ||
           (value >= 0x00011C00 && value <= 0x00011C08) ||
           (value >= 0x00011C0A && value <= 0x00011C2E) ||
           value == 0x00011C40 ||
           (value >= 0x00011C72 && value <= 0x00011C8F) ||
           (value >= 0x00011D00 && value <= 0x00011D06) ||
           value == 0x00011D08 || value == 0x00011D09 ||
           (value >= 0x00011D0B && value <= 0x00011D30) ||
           value == 0x00011D46 ||
           (value >= 0x00011D60 && value <= 0x00011D65) ||
           value == 0x00011D67 || value == 0x00011D68 ||
           (value >= 0x00011D6A && value <= 0x00011D89) ||
           value == 0x00011D98 ||
           (value >= 0x00011EE0 && value <= 0x00011EF2) ||
           (value >= 0x00012000 && value <= 0x00012399) ||
           (value >= 0x00012400 && value <= 0x0001246E) ||
           (value >= 0x00012480 && value <= 0x00012543) ||
           (value >= 0x00013000 && value <= 0x0001342E) ||
           (value >= 0x00014400 && value <= 0x00014646) ||
           (value >= 0x00016800 && value <= 0x00016A38) ||
           (value >= 0x00016A40 && value <= 0x00016A5E) ||
           (value >= 0x00016AD0 && value <= 0x00016AED) ||
           (value >= 0x00016B00 && value <= 0x00016B2F) ||
           (value >= 0x00016B40 && value <= 0x00016B43) ||
           (value >= 0x00016B63 && value <= 0x00016B77) ||
           (value >= 0x00016B7D && value <= 0x00016B8F) ||
           (value >= 0x00016E40 && value <= 0x00016E7F) ||
           (value >= 0x00016F00 && value <= 0x00016F4A) ||
           value == 0x00016F50 ||
           (value >= 0x00016F93 && value <= 0x00016F9F) ||
           value == 0x00016FE0 || value == 0x00016FE1 || value == 0x00016FE3 ||
           (value >= 0x00017000 && value <= 0x000187F7) ||
           (value >= 0x00018800 && value <= 0x00018AF2) ||
           (value >= 0x0001B000 && value <= 0x0001B11E) ||
           (value >= 0x0001B150 && value <= 0x0001B152) ||
           (value >= 0x0001B164 && value <= 0x0001B167) ||
           (value >= 0x0001B170 && value <= 0x0001B2FB) ||
           (value >= 0x0001BC00 && value <= 0x0001BC6A) ||
           (value >= 0x0001BC70 && value <= 0x0001BC7C) ||
           (value >= 0x0001BC80 && value <= 0x0001BC88) ||
           (value >= 0x0001BC90 && value <= 0x0001BC99) ||
           (value >= 0x0001D400 && value <= 0x0001D454) ||
           (value >= 0x0001D456 && value <= 0x0001D49C) ||
           value == 0x0001D49E || value == 0x0001D49F || value == 0x0001D4A2 ||
           value == 0x0001D4A5 || value == 0x0001D4A6 ||
           (value >= 0x0001D4A9 && value <= 0x0001D4AC) ||
           (value >= 0x0001D4AE && value <= 0x0001D4B9) ||
           value == 0x0001D4BB ||
           (value >= 0x0001D4BD && value <= 0x0001D4C3) ||
           (value >= 0x0001D4C5 && value <= 0x0001D505) ||
           (value >= 0x0001D507 && value <= 0x0001D50A) ||
           (value >= 0x0001D50D && value <= 0x0001D514) ||
           (value >= 0x0001D516 && value <= 0x0001D51C) ||
           (value >= 0x0001D51E && value <= 0x0001D539) ||
           (value >= 0x0001D53B && value <= 0x0001D53E) ||
           (value >= 0x0001D540 && value <= 0x0001D544) ||
           value == 0x0001D546 ||
           (value >= 0x0001D54A && value <= 0x0001D550) ||
           (value >= 0x0001D552 && value <= 0x0001D6A5) ||
           (value >= 0x0001D6A8 && value <= 0x0001D6C0) ||
           (value >= 0x0001D6C2 && value <= 0x0001D6DA) ||
           (value >= 0x0001D6DC && value <= 0x0001D6FA) ||
           (value >= 0x0001D6FC && value <= 0x0001D714) ||
           (value >= 0x0001D716 && value <= 0x0001D734) ||
           (value >= 0x0001D736 && value <= 0x0001D74E) ||
           (value >= 0x0001D750 && value <= 0x0001D76E) ||
           (value >= 0x0001D770 && value <= 0x0001D788) ||
           (value >= 0x0001D78A && value <= 0x0001D7A8) ||
           (value >= 0x0001D7AA && value <= 0x0001D7C2) ||
           (value >= 0x0001D7C4 && value <= 0x0001D7CB) ||
           (value >= 0x0001E100 && value <= 0x0001E12C) ||
           (value >= 0x0001E137 && value <= 0x0001E13D) ||
           value == 0x0001E14E ||
           (value >= 0x0001E2C0 && value <= 0x0001E2EB) ||
           (value >= 0x0001E800 && value <= 0x0001E8C4) ||
           (value >= 0x0001E900 && value <= 0x0001E943) ||
           value == 0x0001E94B ||
           (value >= 0x0001EE00 && value <= 0x0001EE03) ||
           (value >= 0x0001EE05 && value <= 0x0001EE1F) ||
           value == 0x0001EE21 || value == 0x0001EE22 || value == 0x0001EE24 ||
           value == 0x0001EE27 ||
           (value >= 0x0001EE29 && value <= 0x0001EE32) ||
           (value >= 0x0001EE34 && value <= 0x0001EE37) ||
           value == 0x0001EE39 || value == 0x0001EE3B || value == 0x0001EE42 ||
           value == 0x0001EE47 || value == 0x0001EE49 || value == 0x0001EE4B ||
           (value >= 0x0001EE4D && value <= 0x0001EE4F) ||
           value == 0x0001EE51 || value == 0x0001EE52 || value == 0x0001EE54 ||
           value == 0x0001EE57 || value == 0x0001EE59 || value == 0x0001EE5B ||
           value == 0x0001EE5D || value == 0x0001EE5F || value == 0x0001EE61 ||
           value == 0x0001EE62 || value == 0x0001EE64 ||
           (value >= 0x0001EE67 && value <= 0x0001EE6A) ||
           (value >= 0x0001EE6C && value <= 0x0001EE72) ||
           (value >= 0x0001EE74 && value <= 0x0001EE77) ||
           (value >= 0x0001EE79 && value <= 0x0001EE7C) ||
           value == 0x0001EE7E ||
           (value >= 0x0001EE80 && value <= 0x0001EE89) ||
           (value >= 0x0001EE8B && value <= 0x0001EE9B) ||
           (value >= 0x0001EEA1 && value <= 0x0001EEA3) ||
           (value >= 0x0001EEA5 && value <= 0x0001EEA9) ||
           (value >= 0x0001EEAB && value <= 0x0001EEBB) ||
           (value >= 0x00020000 && value <= 0x0002A6D6) ||
           (value >= 0x0002A700 && value <= 0x0002B734) ||
           (value >= 0x0002B740 && value <= 0x0002B81D) ||
           (value >= 0x0002B820 && value <= 0x0002CEA1) ||
           (value >= 0x0002CEB0 && value <= 0x0002EBE0) ||
           (value >= 0x0002F800 && value <= 0x0002FA1D) || value == 0x0149 ||
           value == '_' || value == '$' || value == 92;
  }

  LUX_INLINE static bool IsIdentifierPart(u32 value, bool is_unicode) {
    return IsIdentifierStart(value) || (value >= '0' && value <= '9') ||
           value == 0x00B7 || value == 0x00B7 ||
           (value >= 0x0300 && value <= 0x0374) ||
           (value >= 0x0386 && value <= 0x038A) ||
           (value >= 0x0483 && value <= 0x0487) ||
           (value >= 0x0591 && value <= 0x05BD) || value == 0x05BF ||
           value == 0x05C1 || value == 0x05C2 || value == 0x05C4 ||
           value == 0x05C5 || value == 0x05C7 ||
           (value >= 0x0610 && value <= 0x061A) ||
           (value >= 0x0620 && value <= 0x0669) ||
           (value >= 0x066E && value <= 0x06D3) ||
           (value >= 0x06D5 && value <= 0x06DC) ||
           (value >= 0x06DF && value <= 0x06E8) ||
           (value >= 0x06EA && value <= 0x06FC) ||
           (value >= 0x0710 && value <= 0x074A) ||
           (value >= 0x074D && value <= 0x07B1) ||
           (value >= 0x07C0 && value <= 0x07F5) || value == 0x07FD ||
           (value >= 0x0800 && value <= 0x082D) ||
           (value >= 0x0840 && value <= 0x085B) ||
           (value >= 0x08D3 && value <= 0x08E1) ||
           (value >= 0x08E3 && value <= 0x0963) ||
           (value >= 0x0966 && value <= 0x096F) ||
           (value >= 0x0971 && value <= 0x0983) ||
           (value >= 0x09BC && value <= 0x09C4) || value == 0x09C7 ||
           value == 0x09C8 || (value >= 0x09CB && value <= 0x09CE) ||
           value == 0x09D7 || (value >= 0x09DF && value <= 0x09E3) ||
           (value >= 0x09E6 && value <= 0x09F1) || value == 0x09FE ||
           (value >= 0x0A01 && value <= 0x0A03) || value == 0x0A3C ||
           (value >= 0x0A3E && value <= 0x0A42) || value == 0x0A47 ||
           value == 0x0A48 || (value >= 0x0A4B && value <= 0x0A4D) ||
           value == 0x0A51 || (value >= 0x0A66 && value <= 0x0A75) ||
           (value >= 0x0A81 && value <= 0x0A83) ||
           (value >= 0x0ABC && value <= 0x0AC5) ||
           (value >= 0x0AC7 && value <= 0x0AC9) ||
           (value >= 0x0ACB && value <= 0x0ACD) ||
           (value >= 0x0AE0 && value <= 0x0AE3) ||
           (value >= 0x0AE6 && value <= 0x0AEF) ||
           (value >= 0x0AF9 && value <= 0x0AFF) ||
           (value >= 0x0B01 && value <= 0x0B03) ||
           (value >= 0x0B3C && value <= 0x0B44) || value == 0x0B47 ||
           value == 0x0B48 || (value >= 0x0B4B && value <= 0x0B4D) ||
           value == 0x0B56 || value == 0x0B57 ||
           (value >= 0x0B5F && value <= 0x0B63) ||
           (value >= 0x0B66 && value <= 0x0B6F) || value == 0x0B82 ||
           (value >= 0x0BBE && value <= 0x0BC2) ||
           (value >= 0x0BC6 && value <= 0x0BC8) ||
           (value >= 0x0BCA && value <= 0x0BCD) || value == 0x0BD7 ||
           (value >= 0x0BE6 && value <= 0x0BEF) ||
           (value >= 0x0C00 && value <= 0x0C0C) ||
           (value >= 0x0C3D && value <= 0x0C44) ||
           (value >= 0x0C46 && value <= 0x0C48) ||
           (value >= 0x0C4A && value <= 0x0C4D) || value == 0x0C55 ||
           value == 0x0C56 || (value >= 0x0C60 && value <= 0x0C63) ||
           (value >= 0x0C66 && value <= 0x0C6F) ||
           (value >= 0x0C80 && value <= 0x0C83) ||
           (value >= 0x0CBC && value <= 0x0CC4) ||
           (value >= 0x0CC6 && value <= 0x0CC8) ||
           (value >= 0x0CCA && value <= 0x0CCD) || value == 0x0CD5 ||
           value == 0x0CD6 || (value >= 0x0CE0 && value <= 0x0CE3) ||
           (value >= 0x0CE6 && value <= 0x0CEF) ||
           (value >= 0x0D00 && value <= 0x0D03) ||
           (value >= 0x0D12 && value <= 0x0D44) ||
           (value >= 0x0D46 && value <= 0x0D48) ||
           (value >= 0x0D4A && value <= 0x0D4E) ||
           (value >= 0x0D54 && value <= 0x0D57) ||
           (value >= 0x0D5F && value <= 0x0D63) ||
           (value >= 0x0D66 && value <= 0x0D6F) || value == 0x0D82 ||
           value == 0x0D83 || value == 0x0DCA ||
           (value >= 0x0DCF && value <= 0x0DD4) || value == 0x0DD6 ||
           (value >= 0x0DD8 && value <= 0x0DDF) ||
           (value >= 0x0DE6 && value <= 0x0DEF) || value == 0x0DF2 ||
           value == 0x0DF3 || (value >= 0x0E01 && value <= 0x0E3A) ||
           (value >= 0x0E40 && value <= 0x0E4E) ||
           (value >= 0x0E50 && value <= 0x0E59) ||
           (value >= 0x0EA7 && value <= 0x0EBD) ||
           (value >= 0x0EC8 && value <= 0x0ECD) ||
           (value >= 0x0ED0 && value <= 0x0ED9) || value == 0x0F18 ||
           value == 0x0F19 || (value >= 0x0F20 && value <= 0x0F29) ||
           value == 0x0F35 || value == 0x0F37 || value == 0x0F39 ||
           (value >= 0x0F3E && value <= 0x0F47) ||
           (value >= 0x0F71 && value <= 0x0F84) ||
           (value >= 0x0F86 && value <= 0x0F97) ||
           (value >= 0x0F99 && value <= 0x0FBC) || value == 0x0FC6 ||
           (value >= 0x1000 && value <= 0x1049) ||
           (value >= 0x1050 && value <= 0x109D) ||
           (value >= 0x135D && value <= 0x135F) ||
           (value >= 0x1369 && value <= 0x1371) ||
           (value >= 0x170E && value <= 0x1714) ||
           (value >= 0x1720 && value <= 0x1734) ||
           (value >= 0x1740 && value <= 0x1753) || value == 0x1772 ||
           value == 0x1773 || (value >= 0x1780 && value <= 0x17D3) ||
           value == 0x17DD || (value >= 0x17E0 && value <= 0x17E9) ||
           (value >= 0x180B && value <= 0x180D) ||
           (value >= 0x1810 && value <= 0x1819) ||
           (value >= 0x1880 && value <= 0x18AA) ||
           (value >= 0x1920 && value <= 0x192B) ||
           (value >= 0x1930 && value <= 0x193B) ||
           (value >= 0x1946 && value <= 0x196D) ||
           (value >= 0x19D0 && value <= 0x19DA) ||
           (value >= 0x1A00 && value <= 0x1A1B) ||
           (value >= 0x1A20 && value <= 0x1A5E) ||
           (value >= 0x1A60 && value <= 0x1A7C) ||
           (value >= 0x1A7F && value <= 0x1A89) ||
           (value >= 0x1A90 && value <= 0x1A99) ||
           (value >= 0x1AB0 && value <= 0x1ABD) ||
           (value >= 0x1B00 && value <= 0x1B4B) ||
           (value >= 0x1B50 && value <= 0x1B59) ||
           (value >= 0x1B6B && value <= 0x1B73) ||
           (value >= 0x1B80 && value <= 0x1BF3) ||
           (value >= 0x1C00 && value <= 0x1C37) ||
           (value >= 0x1C40 && value <= 0x1C49) ||
           (value >= 0x1C4D && value <= 0x1C7D) ||
           (value >= 0x1CD0 && value <= 0x1CD2) ||
           (value >= 0x1CD4 && value <= 0x1CFA) ||
           (value >= 0x1D00 && value <= 0x1DF9) ||
           (value >= 0x1DFB && value <= 0x1F15) || value == 0x203F ||
           value == 0x2040 || value == 0x2054 ||
           (value >= 0x20D0 && value <= 0x20DC) || value == 0x20E1 ||
           (value >= 0x20E5 && value <= 0x20F0) ||
           (value >= 0x2CEB && value <= 0x2CF3) ||
           (value >= 0x2D7F && value <= 0x2D96) ||
           (value >= 0x2DE0 && value <= 0x2DFF) ||
           (value >= 0x3021 && value <= 0x302F) ||
           (value >= 0x3099 && value <= 0x309F) ||
           (value >= 0xA610 && value <= 0xA62B) ||
           (value >= 0xA640 && value <= 0xA66F) ||
           (value >= 0xA674 && value <= 0xA67D) ||
           (value >= 0xA67F && value <= 0xA6F1) ||
           (value >= 0xA7F7 && value <= 0xA827) ||
           (value >= 0xA880 && value <= 0xA8C5) ||
           (value >= 0xA8D0 && value <= 0xA8D9) ||
           (value >= 0xA8E0 && value <= 0xA8F7) ||
           (value >= 0xA8FD && value <= 0xA92D) ||
           (value >= 0xA930 && value <= 0xA953)

           || (value >= 0xA980 && value <= 0xA9C0) ||
           (value >= 0xA9CF && value <= 0xA9D9) ||
           (value >= 0xA9E0 && value <= 0xA9FE) ||
           (value >= 0xAA00 && value <= 0xAA36) ||
           (value >= 0xAA40 && value <= 0xAA4D) ||
           (value >= 0xAA50 && value <= 0xAA59) ||
           (value >= 0xAA7A && value <= 0xAAC2) ||
           (value >= 0xAAE0 && value <= 0xAAEF) ||
           (value >= 0xAAF2 && value <= 0xAAF6) ||
           (value >= 0xAB70 && value <= 0xABEA) || value == 0xABEC ||
           value == 0xABED || (value >= 0xABF0 && value <= 0xABF9) ||
           (value >= 0xFB1D && value <= 0xFB28) ||
           (value >= 0xFE00 && value <= 0xFE0F) ||
           (value >= 0xFE20 && value <= 0xFE2F) || value == 0xFE33 ||
           value == 0xFE34 || (value >= 0xFE4D && value <= 0xFE4F) ||
           (value >= 0xFF10 && value <= 0xFF19) || value == 0xFF3F ||
           value == 0x000101FD || value == 0x000102E0 ||
           (value >= 0x00010350 && value <= 0x0001037A) ||
           (value >= 0x000104A0 && value <= 0x000104A9) ||
           (value >= 0x00010A00 && value <= 0x00010A03) ||
           value == 0x00010A05 || value == 0x00010A06 ||
           (value >= 0x00010A0C && value <= 0x00010A13) ||
           (value >= 0x00010A38 && value <= 0x00010A3A) ||
           value == 0x00010A3F ||
           (value >= 0x00010AC9 && value <= 0x00010AE6) ||
           (value >= 0x00010D00 && value <= 0x00010D27) ||
           (value >= 0x00010D30 && value <= 0x00010D39) ||
           (value >= 0x00010F30 && value <= 0x00010F50) ||
           (value >= 0x00011000 && value <= 0x00011046) ||
           (value >= 0x00011066 && value <= 0x0001106F) ||
           (value >= 0x0001107F && value <= 0x000110BA) ||
           (value >= 0x000110F0 && value <= 0x000110F9) ||
           (value >= 0x00011100 && value <= 0x00011134) ||
           (value >= 0x00011136 && value <= 0x0001113F) ||
           (value >= 0x00011144 && value <= 0x00011146) ||
           (value >= 0x00011150 && value <= 0x00011173) ||
           (value >= 0x00011180 && value <= 0x000111C4) ||
           (value >= 0x000111C9 && value <= 0x000111CC) ||
           (value >= 0x000111D0 && value <= 0x000111DA) ||
           (value >= 0x00011213 && value <= 0x00011237) ||
           value == 0x0001123E ||
           (value >= 0x000112B0 && value <= 0x000112EA) ||
           (value >= 0x000112F0 && value <= 0x000112F9) ||
           (value >= 0x00011300 && value <= 0x00011303) ||
           (value >= 0x0001133B && value <= 0x00011344) ||
           value == 0x00011347 || value == 0x00011348 ||
           (value >= 0x0001134B && value <= 0x0001134D) ||
           value == 0x00011357 ||
           (value >= 0x0001135D && value <= 0x00011363) ||
           (value >= 0x00011366 && value <= 0x0001136C) ||
           (value >= 0x00011370 && value <= 0x00011374) ||
           (value >= 0x00011400 && value <= 0x0001144A) ||
           (value >= 0x00011450 && value <= 0x00011459) ||
           value == 0x0001145E ||
           (value >= 0x00011480 && value <= 0x000114C5) ||
           (value >= 0x000114D0 && value <= 0x000114D9) ||
           (value >= 0x00011580 && value <= 0x000115B5) ||
           (value >= 0x000115B8 && value <= 0x000115C0) ||
           (value >= 0x000115D8 && value <= 0x000115DD) ||
           (value >= 0x00011600 && value <= 0x00011640) ||
           (value >= 0x00011650 && value <= 0x00011659) ||
           (value >= 0x00011680 && value <= 0x000116B8) ||
           (value >= 0x000116C0 && value <= 0x000116C9) ||
           (value >= 0x0001171D && value <= 0x0001172B) ||
           (value >= 0x00011730 && value <= 0x00011739) ||
           (value >= 0x00011800 && value <= 0x0001183A) ||
           (value >= 0x000118A0 && value <= 0x000118E9) ||
           (value >= 0x000119AA && value <= 0x000119D7) ||
           (value >= 0x000119DA && value <= 0x000119E1) ||
           value == 0x000119E4 ||
           (value >= 0x00011A00 && value <= 0x00011A3E) ||
           value == 0x00011A47 ||
           (value >= 0x00011A50 && value <= 0x00011A99) ||
           (value >= 0x00011C0A && value <= 0x00011C36) ||
           (value >= 0x00011C38 && value <= 0x00011C40) ||
           (value >= 0x00011C50 && value <= 0x00011C59) ||
           (value >= 0x00011C92 && value <= 0x00011CA7) ||
           (value >= 0x00011CA9 && value <= 0x00011CB6) ||
           (value >= 0x00011D0B && value <= 0x00011D36) ||
           value == 0x00011D3A || value == 0x00011D3C || value == 0x00011D3D ||
           (value >= 0x00011D3F && value <= 0x00011D47) ||
           (value >= 0x00011D50 && value <= 0x00011D59) ||
           (value >= 0x00011D6A && value <= 0x00011D8E) ||
           value == 0x00011D90 || value == 0x00011D91 ||
           (value >= 0x00011D93 && value <= 0x00011D98) ||
           (value >= 0x00011DA0 && value <= 0x00011DA9) ||
           (value >= 0x00011EE0 && value <= 0x00011EF6) ||
           (value >= 0x00016A60 && value <= 0x00016A69) ||
           (value >= 0x00016AF0 && value <= 0x00016AF4) ||
           (value >= 0x00016B00 && value <= 0x00016B36) ||
           (value >= 0x00016B50 && value <= 0x00016B59) ||
           (value >= 0x00016F4F && value <= 0x00016F87) ||
           (value >= 0x00016F8F && value <= 0x00016F9F) ||
           value == 0x0001BC9D || value == 0x0001BC9E ||
           (value >= 0x0001D165 && value <= 0x0001D169) ||
           (value >= 0x0001D16D && value <= 0x0001D172) ||
           (value >= 0x0001D17B && value <= 0x0001D182) ||
           (value >= 0x0001D185 && value <= 0x0001D18B) ||
           (value >= 0x0001D1AA && value <= 0x0001D1AD) ||
           (value >= 0x0001D242 && value <= 0x0001D244) ||
           (value >= 0x0001D7CE && value <= 0x0001D7FF) ||
           (value >= 0x0001DA00 && value <= 0x0001DA36) ||
           (value >= 0x0001DA3B && value <= 0x0001DA6C) ||
           value == 0x0001DA75 || value == 0x0001DA84 ||
           (value >= 0x0001DA9B && value <= 0x0001DA9F) ||
           (value >= 0x0001DAA1 && value <= 0x0001DAAF) ||
           (value >= 0x0001E000 && value <= 0x0001E006) ||
           (value >= 0x0001E008 && value <= 0x0001E018) ||
           (value >= 0x0001E01B && value <= 0x0001E021) ||
           value == 0x0001E023 || value == 0x0001E024 ||
           (value >= 0x0001E026 && value <= 0x0001E02A) ||
           (value >= 0x0001E130 && value <= 0x0001E13D) ||
           (value >= 0x0001E140 && value <= 0x0001E149) ||
           (value >= 0x0001E2C0 && value <= 0x0001E2F9) ||
           (value >= 0x0001E8D0 && value <= 0x0001E8D6) ||
           (value >= 0x0001E900 && value <= 0x0001E94B) ||
           (value >= 0x0001E950 && value <= 0x0001E959) ||
           (value >= 0x000E0100 && value <= 0x000E01EF) ||
           (is_unicode && IsHexDigit(value));
  }

  LUX_INLINE static bool IsDecimalDigit(u32 value) {
    return (value >= 48 && value <= 57);
  }

  LUX_INLINE static bool IsHexDigit(u32 value) {
    return (value >= '0' && value <= '9') || (value >= 'a' && value <= 'z') ||
           (value >= 'A' && value <= 'Z');
  }

  LUX_INLINE static bool IsBinaryDigit(u32 value) {
    return (value >= '0' && value <= '1');
  }

  LUX_INLINE static bool IsOctalDigit(u32 value) {
    return (value >= '0' && value <= '7');
  }

  LUX_INLINE static bool IsStartUnicodeEscapeSequence(u32 u) {
    return u == 'u';
  }

  LUX_INLINE static bool IsStartAsciiEscapeSequence(u32 u) { return u == 'x'; }

  LUX_INLINE static bool IsStartEscapeSequence(u32 u) {
    return IsStartUnicodeEscapeSequence(u) || IsStartAsciiEscapeSequence(u);
  }

  static u32 ToHexValue(u32 uchar);
};

}  // namespace lux

#endif  // SRC_CHARS_H_
