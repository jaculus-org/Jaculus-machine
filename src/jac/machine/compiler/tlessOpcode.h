#pragma once

#include <cassert>


namespace jac::cfg::tless {


enum class Opcode {
    // Nullary
    CreateLocal,      // bool isConst -> slot<a>
    CreateUndefined,  // -> undefined

    // Unary
    Copy,             // a -> b
    BoolNot,          // a -> bool                     (ToBoolean never runs user code: cannot throw)
    BitNot,           // int32 -> int32, ex, ex?
    UnPlus,           // a -> number, ex, ex?
    UnMinus,          // a -> a, ex, ex?
    Load,             // slot<a> -> a, slot<a>, ex, ex?
    Dup,              // a -> a a
    Kill,             // a ->
    CreateGlobalSlot, // string bool -> slot<any>
    GetArgRef,        // int32 -> slot<any>
    GetClosureRef,    // int32 -> slot<any>
    GetGlobalRef,     // string -> slot<any>

    // Binary
    Add,              // a a -> a, ex, ex?
    Sub,              // a a -> a, ex, ex?
    Mul,              // a a -> a, ex, ex?
    Div,              // a a -> a, ex, ex?
    Rem,              // a a -> a, ex, ex?
    Pow,              // float64 float64 -> float64, ex, ex?
    LShift,           // int32 int32 -> int32, ex, ex?
    RShift,           // int32 int32 -> int32, ex, ex?
    URShift,          // int32 int32 -> int32, ex, ex?
    BitAnd,           // int32 int32 -> int32, ex, ex?
    BitOr,            // int32 int32 -> int32, ex, ex?
    BitXor,           // int32 int32 -> int32, ex, ex?
    Eq,               // a a -> bool, ex, ex?
    Neq,              // a a -> bool, ex, ex?
    StrictEq,         // a a -> bool                     (never invokes user code: cannot throw)
    StrictNeq,        // a a -> bool                     (never invokes user code: cannot throw)
    Gt,               // a a -> bool, ex, ex?
    Gte,              // a a -> bool, ex, ex?
    Lt,               // a a -> bool, ex, ex?
    Lte,              // a a -> bool, ex, ex?
    GetMember,        // a b -> any, ex, ex?           (a: object | any)

    // Ternary
    SetMember,        // parent id val -> ex, ex?      (id: StringConst, parent: object | any)
    Store,            // a slot<a> -> slot<a>, ex, ex?

    // Variadic
    Call,             // func args... -> ret, ex, ex?
    CallMethod,       // this method args... -> ret, ex, ex?
    Construct,        // ctor args...      -> obj, ex, ex?
    Await,            // awaitable -> res, ex, ex?
    MakeClosure,      // code slot... -> closure...
};


}  // namespace jac::cfg
