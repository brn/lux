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
//
// Bytecode
//
// ==============================================================
//
//                    +-------------------------------------------+
// Opecode            |                   1byte                   |
//                    +-------------------------------------------+
//                    +-------------------------------------------+
// Register           |                   1byte                   |
//                    +-------------------------------------------+
//                    +-------------------------------------------+
// Next Register      ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Min 0b Max 8b
//
// ============================ Format ==========================
//          +---------------------------------------------+
//          | instruction | H | G | F | E | D | C | B | A |
//          +---------------------------------------------+
//          |             |  Dx   |   Cx  |   Bx  |   Ax  |
//          +---------------------------------------------+
//          |             |      Ebx      |      Eax      |
//          +---------------------------------------------+
//          |             |              Rax              |
//          +----------------------------------------------
//
// ============================ Layout ==========================
//          +-------------+
// None     | instruction |
//          +-------------+
//
//          +-----------------+
// Short1   | instruction | A |
//          +-----------------+
//
//          +---------------------+
// Short2   | instruction | B | A |
//          +---------------------+
//
//          +-------------------------+
// Short3   | instruction | C | B | A |
//          +-------------------------+
//
//          +---------------------+
// Double1  | instruction |   Ax  |
//          +---------------------+
//
//          +-------------------------+
// Double2  | instruction | C |   Ax  |
//          +-------------------------+
//
//          +---------------------------------------------+
// Word1    | instruction |            Rax                |
//          +---------------------------------------------+
//
//          +-----------------------------+
// Wide1    | instruction |      Eax      |
//          +-----------------------------+
//
//          +---------------------------------+
// Wide2    | instruction | E |      Eax      |
//          +---------------------------------+
//

enum class BytecodeLayout: uint8_t {
  kNone,
  kShort1,
  kShort2,
  kShort3,
  kDouble1,
  kDouble2,
  kWord1,
  kWide1,
  kWide2,
};

struct BytecodeOperand {
  uint8_t shift;
  uint64_t mask;
};

#define BYTECODE_LIST_WITHOUT_RETURN(A)                                 \
  /*=========================== Short1 2 ===========================*/  \
  /* Store Acc to Reg */                                                \
  A(StoreAR, Short1, 2, 1, RegisterRef*)                                \
  /* Load Reg to Acc */                                                 \
  A(LoadRA, Short1, 2, 1, RegisterRef*)                                 \
  /* Compare Acc Integer with Reg */                                    \
  A(ICmpAR, Short1, 2, 1, RegisterRef*)                                 \
  /* Call Fast builtin property */                                      \
  A(CallFastPropertyA, Short1, 2, 1, FastProperty)                      \
  /* Compare Reg1 < Reg2 to Acc. */                                     \
  A(Inc, Short1, 2, 1, RegisterRef*)  /* Increment Reg. */              \
  A(Dec, Short1, 2, 1, RegisterRef*)  /* Increment Reg. */              \
  /*=========================== Short2 3 ===========================*/  \
  /* Store Imm to Reg */                                                \
  A(ImmI8, Short2, 3, 2, int8_t, RegisterRef*)                          \
  /* Load Acc index to Reg */                                           \
  A(LoadAIxR, Short2, 3, 2, RegisterRef*, RegisterRef*)                 \
  /* Compare Reg Integer with Reg */                                    \
  A(ICmpRR, Short2, 3, 2, RegisterRef*, RegisterRef*)                   \
  /* Compare Reg1 > Reg2 to Acc. */                                     \
  A(ICmpGTRRA, Short2, 3, 2, RegisterRef*, RegisterRef*)                \
  /* Add Reg and out Acc. */                                            \
  A(Add, Short2, 3, 2, RegisterRef*, RegisterRef*)                      \
  /* Sub Reg and out Acc. */                                            \
  A(Sub, Short2, 3, 2, RegisterRef*, RegisterRef*)                      \
  /* Mul Reg and out Acc. */                                            \
  A(Mul, Short2, 3, 2, RegisterRef*, RegisterRef*)                      \
  /* Div Reg and out Acc. */                                            \
  A(Div, Short2, 3, 2, RegisterRef*, RegisterRef*)                      \
  /*=========================== Short3 ===========================*/    \
  /* Load Reg index to Reg */                                           \
  A(LoadRIxR, Short3, 4, 3,                                             \
    RegisterRef*, RegisterRef*, RegisterRef*)                           \
  /*=========================== Word ===========================*/      \
  A(Comment, Word1, (kPointerSize + 1), 1, const char*)                 \
  /*=========================== Double1 ===========================*/   \
  /* Store Constant to Acc */                                           \
  A(ConstantA, Double1, 3, 1, uint16_t)                                 \
  /*=========================== Double2 ===========================*/   \
  /* Store Constant to Reg */                                           \
  A(ConstantR, Short2, 4, 2, uint16_t, RegisterRef*)                    \
  /*=========================== Wide1 ===========================*/     \
  A(Jmp, Wide1, 5, 1, BytecodeLabel*) /* Jump if Acc == true to offset */ \
  /* Jump if Acc == true to offset */                                   \
  A(JmpIfTrue, Wide1, 5, 1, BytecodeLabel*)                             \
  /* Jump if Acc == false to offset */                                  \
  A(JmpIfFalse, Wide1, 5, 1, BytecodeLabel*)                            \
  /*=========================== Wide2 ===========================*/     \
  /* Store Imm to Reg */                                                \
  A(ImmI32, Wide2, 6, 2, int32_t, RegisterRef*)

#define BYTECODE_LIST(A)                                            \
  /*=========================== None ===========================*/  \
  A(Return, None, 0, 0)                                             \
  BYTECODE_LIST_WITHOUT_RETURN(A)

enum class Bytecode: uint8_t {
#define BYTECODE_DEF(Name, Layout, size, num, ...) k##Name,
  BYTECODE_LIST(BYTECODE_DEF)
#undef BYTECODE_DEF
  kLastSentinel__,
  kExit,
};

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

class BytecodeFetcher;
class BytecodeArray:
      public GenericFixedArray<
  uint8_t, BytecodeArray, sizeof(uint8_t)> {
 public:
#ifdef DEBUG
  std::string ToString(BytecodeConstantArray* constant_pool);
#endif
};

class BytecodeFetcher {
 public:
  explicit BytecodeFetcher(BytecodeArray* array)
      : pc_(0), array_(array) {}

  inline Bytecode FetchBytecode() {
    return static_cast<Bytecode>(FetchBytecodeAsInt());
  }

  inline uint8_t FetchBytecodeAsInt() {
    return array_->at(pc_++);
  }

  inline int8_t FetchNextShortOperand() {
    return array_->at(pc_++);
  }

  inline int16_t FetchNextDoubleOperand() {
    auto a = array_->at(pc_++);
    auto b = array_->at(pc_++);
    return ((b << 8) | a);
  }

  inline int32_t FetchNextWideOperand() {
    auto a = array_->at(pc_++);
    auto b = array_->at(pc_++);
    auto c = array_->at(pc_++);
    auto d = array_->at(pc_++);
    return ((d << 24) | (c << 16) | (b << 8) | a);
  }

  inline int64_t FetchNextWordOperand() {
    auto a = static_cast<uint64_t>(array_->at(pc_++));
    auto b = static_cast<uint64_t>(array_->at(pc_++));
    auto c = static_cast<uint64_t>(array_->at(pc_++));
    auto d = static_cast<uint64_t>(array_->at(pc_++));
    auto e = static_cast<uint64_t>(array_->at(pc_++));
    auto f = static_cast<uint64_t>(array_->at(pc_++));
    auto g = static_cast<uint64_t>(array_->at(pc_++));
    auto h = static_cast<uint64_t>(array_->at(pc_++));
    return (a | (b << 8) | (c << 16) | (d << 24)
            | (e << 32) | (f << 40) | (g << 48)
            | (h << 56));
  }

#ifdef PLATFORM_64BIT
  template <typename T>
  inline T FetchNextPtrOperand() {
    return reinterpret_cast<T>(FetchNextWordOperand());
  }
#else
  template <typename T>
  inline T FetchNextPtrOperand() {
    return reinterpret_cast<T>(FetchNextWideOperand());
  }
#endif

  inline void UpdatePC(uint32_t jmp) {
    pc_ = jmp;
  }

  inline bool HasMore() const {
    return pc_ <= array_->length() - 3;
  }

  inline uint32_t pc() const {
    return pc_;
  }

 private:
  uint32_t pc_;
  BytecodeArray* array_;
};

class BytecodeUtil {
 public:
  static const BytecodeOperand kOperandA;
  static const BytecodeOperand kOperandB;
  static const BytecodeOperand kOperandC;
  static const BytecodeOperand kOperandD;
  static const BytecodeOperand kOperandE;
  static const BytecodeOperand kOperandF;
  static const BytecodeOperand kOperandG;
  static const BytecodeOperand kOperandH;
  static const BytecodeOperand kOperandAx;
  static const BytecodeOperand kOperandBx;
  static const BytecodeOperand kOperandCx;
  static const BytecodeOperand kOperandDx;
  static const BytecodeOperand kOperandEax;
  static const BytecodeOperand kOperandEbx;
  static const BytecodeOperand kOperandRax;

  inline static BytecodeLayout layout(Bytecode b) {
    return kLayout[static_cast<uint8_t>(b)];
  }

  inline static uint8_t size(Bytecode b) {
    return kSize[static_cast<uint8_t>(b)];
  }

  inline static uint64_t EncodeOperand(const BytecodeOperand& operand,
                                       uint64_t value) {
    return value << operand.shift;
  }

#ifdef DEBUG
  static const char* ToStringOpecode(Bytecode bc) {
    static std::array<
      const char*,
      (static_cast<uint8_t>(Bytecode::kExit) + 1)> kOpecodeStr = {{
#define BYTECODE_CASE(Name, Layout, size, n, ...) #Name,
        BYTECODE_LIST(BYTECODE_CASE)
#undef BYTECODE_CASE
        "Sentinel",
        "Exit"
      }};
    return kOpecodeStr[static_cast<uint8_t>(bc)];
  }

  static std::string ToString(BytecodeFetcher* fetcher);

  static std::string ToStringField(Bytecode bc,
                                   BytecodeFetcher* fetcher,
                                   size_t len);
#endif

 private:
  static const std::array<
   BytecodeLayout,
   static_cast<uint8_t>(Bytecode::kLastSentinel__)> kLayout;

  static const std::array<
   uint8_t,
    static_cast<uint8_t>(Bytecode::kLastSentinel__)> kSize;
};

class BytecodeNode: public Zone {
 public:
  explicit BytecodeNode(Bytecode bytecode, int64_t operands)
      : operands_(operands),
        bytecode_(bytecode),
        jmp_(nullptr),
        next_(nullptr) {}

  explicit BytecodeNode(Bytecode bytecode,
                        BytecodeNode* jmp)
      : operands_(0),
        bytecode_(bytecode),
        jmp_(jmp),
        next_(nullptr) {}

  explicit BytecodeNode(Bytecode bytecode)
      : operands_(0),
        bytecode_(bytecode),
        jmp_(nullptr),
        next_(nullptr) {}

  LUX_CONST_PROPERTY(uint32_t, offset, offset_)
  LUX_CONST_PROPERTY(BytecodeNode*, jmp, jmp_)
  LUX_CONST_PROPERTY(Bytecode, bytecode, bytecode_)
  LUX_CONST_PROPERTY(BytecodeNode*, next, next_)
  LUX_CONST_PROPERTY(int64_t, operands, operands_)

  inline BytecodeLayout layout() const {
    return BytecodeUtil::layout(bytecode_);
  }

  inline uint8_t DecodeOperand(const BytecodeOperand& operand) {
    return (operands_ >> operand.shift) & operand.mask;
  }

 private:
  size_t offset_;
  int64_t operands_;
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
        zone_allocator_(zone_alloc) {}

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

#define BYTECODE_BUILDER_DEF(Bytecode, Layout, size, num, ...) \
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
