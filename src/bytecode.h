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

#ifndef SRC_BYTECODE_H_
#define SRC_BYTECODE_H_

#include <string>
#include <vector>
#include "./unicode.h"
#include "./utils.h"
#include "./zone.h"
#include "./objects/jsobject.h"

namespace lux {
class Isolate;
class BytecodeExecutable;
// Based on Lua bytecode.
/*===========================================================================
  We assume that instructions are unsigned 32-bit integers.
  All instructions have an opcode in the first 7 bits.
  Instructions can have the following formats:

        3 3 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0
        1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
iABC    |k|     C(8)    | |     B(8)    | |     A(8)    | |   Op(7)   |
iABx    |            Bx(17)             | |     A(8)    | |   Op(7)   |
iAsBx   |           sBx (signed)(17)    | |     A(8)    | |   Op(7)   |
iAx     |                       Ax(25)                  | |   Op(7)   |
iksJ    |k|                     sJ(24)                  | |   Op(7)   |

  A signed argument is represented in excess K: the represented value is
  the written unsigned value minus K, where K is half the maximum for the
  corresponding unsigned argument.
===========================================================================*/

struct BytecodeFormat {
  enum Type {
    kOpsJk,
    kOpAx,
    kOpAsBx,
    kOpABx,
    kOpABCk
  };
};

#define BYTECODE_NUM(index, Format)             \
  (index << 3) | BytecodeFormat::Format

#define BYTECODE_LIST(A)                                                \
  /*=== OpsJk ===*/                                                     \
  A(1 << 3, Jmp, 1, BytecodeLabel*) /* Jump if Acc == true to offset */ \
  /* Jump if Acc == true to offset */                                   \
  A(2 << 3, JmpIfTrue, 1, BytecodeLabel*)                               \
  /* Jump if Acc == false to offset */                                  \
  A(3 << 3, JmpIfFalse, 1, BytecodeLabel*)                              \
  /*=== OpAx ===*/                                                      \
  A(BYTECODE_NUM(1, kOpAx), Return, 0)                                  \
  A(BYTECODE_NUM(2, kOpAx), Comment, 1, const char*)                    \
  /* Store Constant to Acc */                                           \
  A(BYTECODE_NUM(3, kOpAx), ConstantA, 1, uint16_t)                     \
  /* Call Fast builtin property */                                      \
  A(BYTECODE_NUM(4, kOpAx), CallFastPropertyA, 1, FastProperty)         \
  /*=== kOpAsBx ===*/                                                   \
  /*=== kOpABx ===*/                                                    \
  /* Store Imm to Reg */                                                \
  A(BYTECODE_NUM(1, kOpABx), ImmI8, 1, int8_t, RegisterRef*)            \
  /* Store Imm to Reg */                                                \
  A(BYTECODE_NUM(2, kOpABx), ImmI32, 1, int32_t, RegisterRef*)          \
  /* Store Constant to Reg */                                           \
  A(BYTECODE_NUM(4, kOpABx), ConstantR, 2, uint16_t, RegisterRef*)      \
  /* Store Acc to Reg */                                                \
  A(BYTECODE_NUM(5, kOpABx), StoreAR, 1, RegisterRef*)                  \
  /* Load Reg to Acc */                                                 \
  A(BYTECODE_NUM(6, kOpABx), LoadRA, 1, RegisterRef*)                   \
  /* Load Acc index to Reg */                                           \
  A(BYTECODE_NUM(7, kOpABx), LoadAIxR, 2, RegisterRef*, RegisterRef*)   \
  /* Compare Reg Integer with Reg */                                    \
  A(BYTECODE_NUM(8, kOpABx), ICmpRR, 2, RegisterRef*, RegisterRef*)     \
  /* Compare Acc Integer with Reg */                                    \
  A(BYTECODE_NUM(9, kOpABx), ICmpAR, 1, RegisterRef*)                   \
  /* Compare Reg1 > Reg2 to Acc. */                                     \
  A(BYTECODE_NUM(10, kOpABx), ICmpGTRRA, 2, RegisterRef*, RegisterRef*) \
  /* Compare Reg1 < Reg2 to Acc. */                                     \
  A(BYTECODE_NUM(11, kOpABx), ICmpLTRRA, 2, RegisterRef*, RegisterRef*) \
  A(BYTECODE_NUM(12, kOpABx), Inc, 1, RegisterRef*)  /* Increment Reg. */ \
  A(BYTECODE_NUM(13, kOpABx), Dec, 1, RegisterRef*)  /* Increment Reg. */ \
  /* Add Reg and out Acc. */                                            \
  A(BYTECODE_NUM(14, kOpABx), Add, 2, RegisterRef*, RegisterRef*)       \
  /* Sub Reg and out Acc. */                                            \
  A(BYTECODE_NUM(15, kOpABx), Sub, 2, RegisterRef*, RegisterRef*)       \
  /* Mul Reg and out Acc. */                                            \
  A(BYTECODE_NUM(16, kOpABx), Mul, 2, RegisterRef*, RegisterRef*)       \
  /* Div Reg and out Acc. */                                            \
  A(BYTECODE_NUM(17, kOpABx), Div, 2, RegisterRef*, RegisterRef*)       \
  /*=== kOpABCk ===*/                                                   \
  /* Load Reg index to Reg */                                           \
  A(BYTECODE_NUM(1, kOpABCk), LoadRIxR, 3,                              \
    RegisterRef*, RegisterRef*, RegisterRef*)

enum class AddressingMode: uint8_t {
  kPointer,
  kImmediate,
};

enum class FastProperty: uint8_t {
  kLength
};

class RegisterRef {
 public:
  explicit RegisterRef(AddressingMode mode
                       = AddressingMode::kImmediate)
      : mode_(mode), register_id_(-1) {}

  LUX_GETTER(AddressingMode, mode, mode_)
  LUX_CONST_PROPERTY(uint8_t, id, register_id_)
  bool has_id() const {
    return register_id_ != -1;
  }

 private:
  AddressingMode mode_;
  int32_t register_id_;
};

class RegisterAllocator {
 public:
  enum Type {
    kParameter1,
    kParameter2,
    kParameter3,
    kParameter4,
    kParameter5,
    kParameter6,
    kParameter7,
    kParameter8,
    kParameter9,
    kParameter10,
    kImmRange = 128
  };

  RegisterAllocator() {
    use_1.assign(1023);
  }

  static RegisterRef Parameter(int id) {
    auto r = RegisterRef();
    r.set_id(id);
    return r;
  }

  int32_t use(AddressingMode mode) {
    bool imm = mode == AddressingMode::kImmediate;
    auto ret = DoUse(imm? &use_1: &addr_1);
    if (ret == -1) {
      ret = DoUse(imm? &use_2: &addr_2);
      if (ret > -1) {
        if (imm) {
          ret += 64;
        } else {
          ret += 192;
        }
        return ret;
      }
    }

    if (!imm) {
      ret += 128;
    }
    return ret;
  }

  void unuse(int32_t index) {
    if (index < 64) {
      use_1.unset(index);
    } else if (index < 128) {
      use_2.unset(index - 64);
    } else if (index < 192) {
      addr_1.unset(index - 128);
    } else {
      addr_1.unset(index - 192);
    }
  }

 private:
  int32_t DoUse(Bitset<uint64_t>* use) {
    if (use->is_full()) {
      return -1;
    }
    auto index = use->RightMostEmptySlot();
    use->set(index);
    return index;
  }

  Bitset<uint64_t> use_1;
  Bitset<uint64_t> use_2;
  Bitset<uint64_t> addr_1;
  Bitset<uint64_t> addr_2;
};

class BytecodeConstantArray:
      public GenericFixedArray<
  Object*, BytecodeConstantArray, kPointerSize> {
};

struct BytecodeSize {
  enum Type {
    SIZE_C = 8,
    SIZE_Cx = (SIZE_C + 1),
    SIZE_B = 8,
    SIZE_Bx = (SIZE_Cx + SIZE_B),
    SIZE_A = 8,
    SIZE_Ax = (SIZE_Cx + SIZE_B + SIZE_A),
    SIZE_sJ = (SIZE_C + SIZE_B + SIZE_A),
    MASK_sJ = 0x7fffff80,
    MASK_k = 1 << 31,
    SIZE_OP = 7,
    MASK_OP = 0x7F,
    POS_OP = 0,
    POS_A = (POS_OP + SIZE_OP),
    POS_B = (POS_A + SIZE_A),
    POS_C = (POS_B + SIZE_B),
    POS_k = (POS_C + SIZE_C),
    POS_Bx = POS_B,
    POS_Ax = POS_A,
    POS_sJ = POS_A,
  };
};

class Bytecode {
  friend class BytecodeUpdater;
 public:
  enum Instruction {
#define BYTECODE_DECL(index, Bytecode, num, ...) k##Bytecode = index,
    BYTECODE_LIST(BYTECODE_DECL)
#undef BYTECODE_DECL
  };

  explicit Bytecode(uint32_t b)
      : bytecode_(b) {}

  Instruction instruction() const {
    return static_cast<Instruction>(
        bytecode_ & BytecodeSize::MASK_OP);
  }

  BytecodeFormat::Type format() const {
    return static_cast<BytecodeFormat::Type>(
        bytecode_ & 0x3);
  }

  int32_t sJ() const {
    auto ret = (bytecode_ >> BytecodeSize::POS_sJ) & 0xFFFFFF;
    if (k()) {
      return ret | 0xFF000000;
    }
    return ret;
  }

  uint32_t Ax() const {
    return (bytecode_ >> BytecodeSize::POS_Ax);
  }

  uint8_t A() const {
    return (bytecode_ >> BytecodeSize::POS_A) & 0xFF;
  }

  uint8_t B() const {
    return (bytecode_ >> BytecodeSize::POS_B) & 0xFF;
  }

  uint8_t C() const {
    return (bytecode_ >> BytecodeSize::POS_C) & 0xFF;
  }

  uint8_t k() const {
    return (bytecode_ >> BytecodeSize::POS_k);
  }

  uint32_t Bx() const {
    return (bytecode_ >> BytecodeSize::POS_Bx);
  }

#ifdef DEBUG
  std::string ToString(BytecodeConstantArray* pool) const {
    std::stringstream st;
    if (instruction() == Bytecode::kComment) {
      auto v = pool->at(Ax());
      INVALIDATE(v->IsHeapObject());
      JSString::Utf8String u8str(JSString::Cast(v));
      st << ";;" << u8str.value();
      return st.str();
    }

    switch (instruction()) {
#define BYTECODE_CASE(index, A, n, ...)               \
      case k##A: {                                    \
        size_t len = sizeof(#A) - 1;                  \
        st << #A << ToStringField(len);               \
        auto ret = st.str();                          \
        return ret;                                   \
      }
      BYTECODE_LIST(BYTECODE_CASE)
#undef BYTECODE_CASE
      default:
        UNREACHABLE();
      return "";
    }
  }

  std::string ToStringField(size_t len) const {
    std::stringstream st;
    std::string indent(19 - len, ' ');
    switch (format()) {
      case BytecodeFormat::kOpsJk:
        st << indent << "sJ = " << sJ();
        return st.str();
      case BytecodeFormat::kOpAx:
        st << indent << "Ax = " << +Ax();
        return st.str();
      case BytecodeFormat::kOpAsBx:
      case BytecodeFormat::kOpABx:
        st << indent << "A  = " << +A()
           << "  " << "Bx = " << +Bx();
        return st.str();
      case BytecodeFormat::kOpABCk:
        st << indent << "A = " << +A()
           << "  " << "B = " << +B()
           << "  " << "C = " << +C()
           << "  " << "k = " << +k();
        return st.str();
      default:
        UNREACHABLE();
    }
  }
#endif

 private:
  uint32_t bytecode_;
};

class BytecodeUpdater {
 public:
  explicit BytecodeUpdater(Bytecode bytecode)
      : bytecode_(bytecode) {}
  void UpdateSJ(int32_t sj) {
    bytecode_.bytecode_ =
      (bytecode_.bytecode_ & (~BytecodeSize::MASK_sJ))
      | (sj << BytecodeSize::POS_sJ);
    bytecode_.bytecode_ |= (sj < 0? (1 << BytecodeSize::POS_k): 0);
  }
  Bytecode bytecode() const {
    return bytecode_;
  }
 private:
  Bytecode bytecode_;
};

template <uint8_t format>
class BytecodeFactory {};

template <>
class BytecodeFactory<BytecodeFormat::kOpABCk> {
 public:
  static Bytecode New(
      Bytecode::Instruction op, uint8_t a, uint8_t b, uint8_t c, uint8_t k) {
    //           3 3 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0
    //           1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
    // OpABCk    |k|     C(8)    | |     B(8)    | |     A(8)    | |   Op(7)   |
    uint32_t f = (k << BytecodeSize::POS_k)
      | (c << BytecodeSize::POS_C)
      | (b << BytecodeSize::POS_B)
      | (a << BytecodeSize::POS_A) | op;
    return Bytecode(f);
  }
};

template <>
class BytecodeFactory<BytecodeFormat::kOpABx> {
 public:
  static Bytecode New(Bytecode::Instruction op, uint8_t a, uint32_t bx) {
    //          3 3 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0
    //          1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
    // OpABx    |            Bx(17)             | |     A(8)    | |   Op(7)   |
    uint32_t f = (bx << BytecodeSize::POS_Bx)
      | (a << BytecodeSize::POS_A) | op;
    return Bytecode(f);
  }
};

template <>
class BytecodeFactory<BytecodeFormat::kOpAsBx> {
 public:
  static Bytecode New(Bytecode::Instruction op, uint8_t a, int32_t sbx) {
    uint32_t f = (sbx << BytecodeSize::POS_Bx)
      | (a << BytecodeSize::POS_A) | op;
    return Bytecode(f);
  }
};

template <>
class BytecodeFactory<BytecodeFormat::kOpAx> {
 public:
  static Bytecode New(Bytecode::Instruction op, uint32_t ax) {
    uint32_t f = (ax << BytecodeSize::POS_Ax) | op;
    return Bytecode(f);
  }
};

template <>
class BytecodeFactory<BytecodeFormat::kOpsJk> {
 public:
  static Bytecode New(Bytecode::Instruction op, int32_t sj, uint8_t k) {
    uint32_t f = (k << BytecodeSize::POS_k)
      | (sj << BytecodeSize::POS_sJ) | op;
    return Bytecode(f);
  }
};

class BytecodeNode: public Zone {
 public:
  explicit BytecodeNode(Bytecode bytecode)
      : bytecode_(bytecode),
        jmp_(nullptr),
        next_(nullptr) {}

  explicit BytecodeNode(Bytecode bytecode,
                        BytecodeNode* jmp)
      : bytecode_(bytecode),
        jmp_(jmp),
        next_(nullptr) {}

  LUX_CONST_PROPERTY(uint32_t, offset, offset_)
  LUX_CONST_PROPERTY(BytecodeNode*, jmp, jmp_)
  LUX_CONST_PROPERTY(Bytecode, bytecode, bytecode_)
  LUX_CONST_PROPERTY(BytecodeNode*, next, next_)

 private:
  uint32_t offset_;
  Bytecode bytecode_;
  BytecodeNode* jmp_;
  BytecodeNode* next_;
};

class BytecodeLabel {
 public:
  using NodeList = std::vector<BytecodeNode*>;
  using iterator = NodeList::iterator;

  BytecodeLabel()
      : jmps_(nullptr) {}

  LUX_PROPERTY(BytecodeNode*, to, to_)

  LUX_PROPERTY(NodeList*, jmps, jmps_)

  LUX_PROPERTY(BytecodeNode*, bound_node, bound_node_)

  void AddFrom(BytecodeNode* from) {
    if (jmps_) {
      INVALIDATE(bound_node_ != nullptr);
      jmps_->push_back(from);
      from->set_jmp(bound_node_);
    } else {
      from_.push_back(from);
    }
  }

  iterator begin() {
    return from_.begin();
  }

  iterator end() {
    return from_.end();
  }

 private:
  NodeList from_;
  NodeList* jmps_;
  BytecodeNode* bound_node_;
  BytecodeNode* to_;
};

class BytecodeArrayWriter;

class BytecodeArray:
      public GenericFixedArray<
  Bytecode, BytecodeArray, sizeof(Bytecode)> {
 public:
#ifdef DEBUG
  std::string ToString(BytecodeConstantArray* constant_pool) {
    std::stringstream st;
    auto max = std::to_string(length()).size();
    for (auto i = 0; i < length(); i++) {
      auto size = std::to_string(i).size();
      std::string indent((max - size) + 1, ' ');
      st << (i > 0? "\n": "") << indent <<  i << ": "
         << at(i).ToString(constant_pool);
    }
    return st.str();
  }
#endif
};

class BytecodeConstantNode: public Zone {
 public:
  explicit BytecodeConstantNode(Object* obj)
      : ptr_(obj), next_(nullptr) {}
  LUX_CONST_PROPERTY(BytecodeConstantNode*, next, next_);
  LUX_CONST_PROPERTY(Object*, ptr, ptr_);
 private:
  Object* ptr_;
  BytecodeConstantNode* next_;
};

class BytecodeArrayWriter {
 public:
  explicit BytecodeArrayWriter(Isolate* isolate,
                               ZoneAllocator* zone_alloc)
      : current_offset_(0),
        constant_length_(0),
        isolate_(isolate),
        constant_list_(nullptr),
        constant_top_(nullptr),
        bytecode_top_(nullptr),
        bytecode_list_(nullptr),
        zone_allocator_(zone_alloc){}

  int32_t EmitConstant(BytecodeConstantNode* node);
  void Emit(BytecodeNode* bytecode);
  void EmitJump(BytecodeLabel* label, BytecodeNode* bytecode);
  LUX_GETTER(BytecodeNode*, last_bytecode, bytecode_list_)
  Handle<BytecodeExecutable> Flush();
  void Bind(BytecodeLabel* label);

 private:
  void Append(BytecodeNode* node);
  LUX_INLINE ZoneAllocator* zone() const {
    return zone_allocator_;
  }
  int32_t current_offset_;
  int32_t constant_length_;
  std::vector<BytecodeNode*> jmps_;
  Isolate* isolate_;
  BytecodeConstantNode* constant_list_;
  BytecodeConstantNode* constant_top_;
  BytecodeNode* bytecode_top_;
  BytecodeNode* bytecode_list_;
  ZoneAllocator* zone_allocator_;
};

class BytecodeBuilder;
class BytecodeScope {
 public:
  explicit BytecodeScope(BytecodeBuilder* builder);

  BytecodeNode* start() {
    return start_;
  }

  BytecodeNode* end();

 private:
  BytecodeNode* start_;
  BytecodeBuilder* builder_;
};

class BytecodeBuilder {
 public:
  explicit BytecodeBuilder(Isolate* isolate,
                           ZoneAllocator* zone_allocator)
      : isolate_(isolate), zone_allocator_(zone_allocator) {
    bytecode_array_writer_(isolate, zone_allocator);
  }

#define BYTECODE_BUILDER_DEF(index, Bytecode, _, ...)  \
  BytecodeNode* Bytecode(__VA_ARGS__);
  BYTECODE_LIST(BYTECODE_BUILDER_DEF)
#undef BYTECODE_BUILDER_DEF

  void Branch(RegisterRef* reg,
              BytecodeLabel* then_jmp,
              BytecodeLabel* else_jmp);

  void BranchA(BytecodeLabel* then_jmp, BytecodeLabel* else_jmp);

  uint32_t StringConstant(JSString* str, int32_t index = -1);

  LUX_INLINE Handle<BytecodeExecutable> flush() {
    return bytecode_array_writer_->Flush();
  }

  LUX_INLINE void Bind(BytecodeLabel* label) {
    bytecode_array_writer_->Bind(label);
  }

  LUX_INLINE int32_t allocate_regiseter(AddressingMode mode) {
    return register_allocator_.use(mode);
  }

  LUX_INLINE void free_regiseter(uint32_t r) {
    register_allocator_.unuse(r);
  }

  LUX_INLINE BytecodeNode* last_bytecode() {
    return bytecode_array_writer_->last_bytecode();
  }

 private:
  ZoneAllocator* zone() const {
    return zone_allocator_;
  }

  Isolate* isolate_;
  ZoneAllocator* zone_allocator_;
  LazyInitializer<BytecodeArrayWriter> bytecode_array_writer_;
  RegisterAllocator register_allocator_;
};
}  // namespace lux

#endif  // SRC_BYTECODE_H_
