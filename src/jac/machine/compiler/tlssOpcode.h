#pragma once

#include <cassert>


namespace jac::cfg::tless {


enum class Opcode {
    // Nullary
    CreateSlot,       // -> slot<a>
    CreateUndefined,  // -> undefined

    // Unary
    Copy,             // a -> b
    BoolNot,          // a -> bool
    BitNot,           // int32 -> int32
    UnPlus,           // a -> number
    UnMinus,          // a -> a
    Load,             // slot<a> -> a, slot<a>
    Dup,              // a -> a a
    Kill,             // a ->
    CreateGlobalSlot, // string -> slot<a>
    GetArgRef,        // int32 -> slot<any>
    GetClosureRef,    // int32 -> slot<any>
    GetGlobalRef,     // string -> slot<any>

    // Binary
    Add,              // a a -> a
    Sub,              // a a -> a
    Mul,              // a a -> a
    Div,              // a a -> a
    Rem,              // a a -> a
    Pow,              // float64 float64 -> float64
    LShift,           // int32 int32 -> int32
    RShift,           // int32 int32 -> int32
    URShift,          // int32 int32 -> int32
    BitAnd,           // int32 int32 -> int32
    BitOr,            // int32 int32 -> int32
    BitXor,           // int32 int32 -> int32
    Eq,               // a a -> bool
    Neq,              // a a -> bool
    Gt,               // a a -> bool
    Gte,              // a a -> bool
    Lt,               // a a -> bool
    Lte,              // a a -> bool
    GetMember,        // a b -> any        (a: object | any)

    // Ternary
    SetMember,        // parent id val -> void  (id: StringConst, parent: object | any)
    Store,            // a slot<a> -> slot<a>

    // Variadic
    Call,             // func args... -> ret except
    CallMethod,       // this ident args... -> ret except
    Construct,        // ctor args...      -> obj except
    MakeClosure,      // code slot... -> closure...
};


}  // namespace jac::cfg
