#pragma once

#include <cassert>
#include <cstddef>
#include <cstdint>


namespace jac::cfg::tless {


enum class Tag {
    Int,
    Float64,
    Bool,
    Undefined,
    Null,
    String,
    Object,
    Symbol,
    BigInt,
    Exception,
    Uninitialized,
    Closure,
    Other,
};

#define JAC_TLESS_OPCODE_TABLE(X) \
    X(CreateLocal,       1,         1)  /* bool -> slot<a> */ \
    X(CreateUndefined,   0,         1)  /* -> undefined */ \
    X(Copy,              1,         1)  /* a -> b */ \
    X(BoolNot,           1,         1)  /* a -> bool */ \
    X(BitNot,            1,         3)  /* int32 -> int32, ex, ex? */ \
    X(UnPlus,            1,         3)  /* a -> number, ex, ex? */ \
    X(UnMinus,           1,         3)  /* a -> a, ex, ex? */ \
    X(Load,              1,         4)  /* slot<a> -> a, slot<a>, ex, ex? */ \
    X(Dup,               1,         2)  /* a -> a a */ \
    X(Kill,              1,         0)  /* a -> */ \
    X(CreateGlobalSlot,  2,         1)  /* string int32 -> slot<any> */ \
    X(GetGlobalRef,      1,         1)  /* string -> slot<any> */ \
    X(Add,               2,         3)  /* a a -> a, ex, ex? */ \
    X(Sub,               2,         3)  /* a a -> a, ex, ex? */ \
    X(Mul,               2,         3)  /* a a -> a, ex, ex? */ \
    X(Div,               2,         3)  /* a a -> a, ex, ex? */ \
    X(Rem,               2,         3)  /* a a -> a, ex, ex? */ \
    X(Pow,               2,         3)  /* float64 float64 -> float64, ex, ex? */ \
    X(LShift,            2,         3)  /* int32 int32 -> int32, ex, ex? */ \
    X(RShift,            2,         3)  /* int32 int32 -> int32, ex, ex? */ \
    X(URShift,           2,         3)  /* int32 int32 -> int32, ex, ex? */ \
    X(BitAnd,            2,         3)  /* int32 int32 -> int32, ex, ex? */ \
    X(BitOr,             2,         3)  /* int32 int32 -> int32, ex, ex? */ \
    X(BitXor,            2,         3)  /* int32 int32 -> int32, ex, ex? */ \
    X(Eq,                2,         3)  /* a a -> bool, ex, ex? */ \
    X(Neq,               2,         3)  /* a a -> bool, ex, ex? */ \
    X(StrictEq,          2,         1)  /* a a -> bool*/ \
    X(StrictNeq,         2,         1)  /* a a -> bool*/ \
    X(Gt,                2,         3)  /* a a -> bool, ex, ex? */ \
    X(Gte,               2,         3)  /* a a -> bool, ex, ex? */ \
    X(Lt,                2,         3)  /* a a -> bool, ex, ex? */ \
    X(Lte,               2,         3)  /* a a -> bool, ex, ex? */ \
    X(GetMember,         2,         3)  /* a b -> any, ex, ex? */ \
    X(SetMember,         3,         2)  /* parent id val -> ex, ex? */ \
    X(Store,             2,         3)  /* a slot<a> -> slot<a>, ex, ex? */ \
    X(Call,              VARIADIC,  3)  /* func args... -> ret, ex, ex? */ \
    X(CallMethod,        VARIADIC,  3)  /* this method args... -> ret, ex, ex? */ \
    X(Construct,         VARIADIC,  3)  /* ctor args... -> obj, ex, ex? */ \
    X(Await,             1,         3)  /* awaitable -> res, ex, ex? */ \
    X(MakeClosure,       VARIADIC,  1)  /* code slot... -> closure */ \
    /* slow-path intrinsics */ \
    X(ToPrimitive,       1,         3)  /* v -> v, ex, ex? */ \
    X(StringConcat,      2,         3)  /* primitive primitive -> string, ex, ex? */ \
    X(AddSlow,           2,         3)  /* v v -> v, ex, ex? */ \
    X(SubSlow,           2,         3)  /* v v -> v, ex, ex? */ \
    X(MulSlow,           2,         3)  /* v v -> v, ex, ex? */ \
    /* Low level */ \
    X(GetTag,            1,         2)  /* v -> v, rawTag */ \
    X(CmpEqTag,          2,         1)  /* rawTag rawTag -> rawBool */ \
    X(UnboxI32,          1,         1)  /* v -> rawI32 */ \
    X(UnboxF64,          1,         1)  /* v -> rawF64 */ \
    X(BoxI32,            1,         1)  /* rawI32 -> v */ \
    X(BoxF64,            1,         1)  /* rawF64 -> v */ \
    X(AddI32,            2,         2)  /* rawI32 rawI32 -> rawI32, rawBool(inexact) */ \
    X(SubI32,            2,         2)  /* rawI32 rawI32 -> rawI32, rawBool(inexact) */ \
    X(MulI32,            2,         2)  /* rawI32 rawI32 -> rawI32, rawBool(inexact) */ \
    X(AddF64,            2,         1)  /* rawF64 rawF64 -> rawF64 */ \
    X(SubF64,            2,         1)  /* rawF64 rawF64 -> rawF64 */ \
    X(MulF64,            2,         1)  /* rawF64 rawF64 -> rawF64 */ \
    X(I32ToF64,          1,         1)  /* rawI32 -> rawF64 */

enum class Opcode {
#define JAC_TLESS_DECLARE_OPCODE(name, ...) name,
    JAC_TLESS_OPCODE_TABLE(JAC_TLESS_DECLARE_OPCODE)
#undef JAC_TLESS_DECLARE_OPCODE
};

inline constexpr size_t OPCODE_COUNT = 0
#define JAC_TLESS_COUNT_OPCODE(name, ...) + 1
    JAC_TLESS_OPCODE_TABLE(JAC_TLESS_COUNT_OPCODE)
#undef JAC_TLESS_COUNT_OPCODE
;



}  // namespace jac::cfg
