#pragma once

#include <cassert>
#include <cmath>
#include <cstdint>
#include <cstdlib>
#include <map>
#include <memory>
#include <type_traits>
#include <unordered_map>
#include <utility>

#include <jac/machine/compiler/cfg.h>
#include <jac/machine/compiler/quickjsOps.h>
#include <jac/util.h>
#include <quickjs.h>

#include "types.h"


namespace jac::cfg::interp {


template<typename Id>
struct RefCounter {
    struct ScreamingCount {
        int value = 1;

        void increment() {
            assert(value > 0 && "Reference count already zero");
            value++;
        }

        void decrement() {
            assert(value > 0 && "Reference count already zero");
            value--;
        }

        int32_t get() const {
            return value;
        }

        ~ScreamingCount() {
            assert(value == 0 && "Reference count not zero");
        }
    };

    std::shared_ptr<ScreamingCount> count;

    RefCounter() = delete;
    RefCounter(std::shared_ptr<ScreamingCount> c) : count(c) {}
    static RefCounter create() {
        return RefCounter(std::make_shared<ScreamingCount>());
    }

    void increment() { count->increment(); }
    void decrement() { count->decrement(); }
};


struct Int32 {
    int32_t value;
    RefCounter<Int32> refCounter;

    Int32(int32_t v) : value(v), refCounter(RefCounter<Int32>::create()) {}

    int32_t& operator*() {
        return value;
    }

    void _dup(JSContext*) {
        refCounter.increment();
    }

    void _free(JSContext*) {
        refCounter.decrement();
    }
};

struct Float64 {
    double value;
    RefCounter<Float64> refCounter;

    Float64(double v) : value(v), refCounter(RefCounter<Float64>::create()) {}

    double& operator*() {
        return value;
    }

    void _dup(JSContext*) {
        refCounter.increment();
    }

    void _free(JSContext*) {
        refCounter.decrement();
    }
};


struct Boolean {
    bool value;
    RefCounter<Boolean> refCounter;

    Boolean(bool v) : value(v), refCounter(RefCounter<Boolean>::create()) {}

    bool& operator*() {
        return value;
    }

    void _dup(JSContext*) {
        refCounter.increment();
    }

    void _free(JSContext*) {
        refCounter.decrement();
    }
};

struct ObjectPtr {
    using JSObject = void;

    JSObject* value;

    ObjectPtr(JSObject* v) : value(v) {}

    JSObject*& operator*() {
        return value;
    }

    void _dup(JSContext* ctx) {
        auto obj = JS_MKPTR(JS_TAG_OBJECT, value);
        JS_DupValue(ctx, obj);
    }

    void _free(JSContext* ctx) {
        auto val = JS_MKPTR(JS_TAG_OBJECT, value);
        JS_FreeValue(ctx, val);
    }
};

struct StringConst {
    std::string const* value;

    StringConst(const std::string& v) : value(&v) {}

    const std::string& operator*() {
        return *value;
    }

    void _dup(JSContext*) {
        // no op
    }

    void _free(JSContext*) {
        // no op
    }
};

struct Any {
    JSValue value;

    Any(JSValue v) : value(v) {}

    JSValue& operator*() {
        return value;
    }

    void _dup(JSContext* ctx) {
        JS_DupValue(ctx, value);
    }

    void _free(JSContext* ctx) {
        JS_FreeValue(ctx, value);
    }
};


template<typename T>
concept I32Able = std::is_same_v<T, Int32> || std::is_same_v<T, Boolean>;

template<typename L>
concept DoubleAble = std::is_same_v<L, Float64> || I32Able<L>;

template<typename T>
struct ConvertVisitor {};

template<>
struct ConvertVisitor<Int32> {
    JSContext* ctx;

    Int32 operator()(Int32 v) {
        return v;
    }
    Int32 operator()(DoubleAble auto v) {
        return static_cast<int32_t>(*v);
    }
    Int32 operator()(Any v) {
        int32_t res;
        if (JS_ToInt32(ctx, &res, *v) < 0) {
            throw std::runtime_error("Invalid conversion to Int32");
        }
        return res;
    }
    Int32 operator()(auto) {
        throw std::runtime_error("Invalid conversion to Int32");
    }
};

template<>
struct ConvertVisitor<Float64> {
    JSContext* ctx;

    Float64 operator()(Float64 v) {
        return v;
    }
    Float64 operator()(DoubleAble auto v) {
        return static_cast<double>(*v);
    }
    Float64 operator()(Any v) {
        double res;
        if (JS_ToFloat64(ctx, &res, *v) < 0) {
            throw std::runtime_error("Invalid conversion to Float64");
        }
        return res;
    }
    Float64 operator()(auto) {
        throw std::runtime_error("Invalid conversion to Float64");
    }
};

template<>
struct ConvertVisitor<Boolean> {
    JSContext* ctx;

    Boolean operator()(Boolean v) {
        return v;
    }
    Boolean operator()(DoubleAble auto v) {
        return *v != 0;
    }
    Boolean operator()(Any v) {
        int32_t res = JS_ToBool(ctx, *v);
        if (res < 0) {
            throw std::runtime_error("Invalid conversion to Bool");
        }
        return res != 0;
    }
    Boolean operator()(auto) {
        throw std::runtime_error("Invalid conversion to Bool");
    }
};

template<>
struct ConvertVisitor<ObjectPtr> {
    JSContext* ctx;

    ObjectPtr operator()(ObjectPtr v) {
        return v;
    }
    ObjectPtr operator()(Any v) {
        if (JS_VALUE_GET_TAG(*v) == JS_TAG_OBJECT) {
            JS_DupValue(ctx, *v);
            return JS_VALUE_GET_PTR(*v);
        }
        throw std::runtime_error("Invalid conversion to ObjectPtr");
    }
    ObjectPtr operator()(auto) {
        throw std::runtime_error("Invalid conversion to Object");
    }
};

template<>
struct ConvertVisitor<StringConst> {
    JSContext* ctx;

    StringConst operator()(StringConst v) {
        return v;
    }
    StringConst operator()(auto) {
        throw std::runtime_error("Invalid conversion to StringConst");
    }
};

template<>
struct ConvertVisitor<Any> {
    JSContext* ctx;

    Any operator()(Any v) {
        return v;
    }
    Any operator()(ObjectPtr v) {
        JSValue val = JS_MKPTR(JS_TAG_OBJECT, *v);
        JS_DupValue(ctx, val);
        return val;
    }
    Any operator()(Int32 v) {
        return JS_NewInt32(ctx, *v);
    }
    Any operator()(Float64 v) {
        return JS_NewFloat64(ctx, *v);
    }
    Any operator()(Boolean v) {
        return JS_NewBool(ctx, *v);
    }
    Any operator()(StringConst v) {
        return JS_NewString(ctx, (*v).c_str());
    }
    Any operator()(auto) {
        throw std::runtime_error("Invalid conversion to JSValue");
    }
};

using RegVal = std::variant<Int32, Float64, Boolean, ObjectPtr, StringConst, Any>;

template<typename T>
T convert(JSContext* ctx, RegVal v) {
    return std::visit(ConvertVisitor<T>{ctx}, v);
}

inline RegVal convert(JSContext* ctx, RegVal v, ValueType type) {
    switch (type) {
        case ValueType::I32:
            return convert<Int32>(ctx, v);
        case ValueType::F64:
            return convert<Float64>(ctx, v);
        case ValueType::Bool:
            return convert<Boolean>(ctx, v);
        case ValueType::Object:
            return convert<ObjectPtr>(ctx, v);
        case ValueType::Any:
            return convert<Any>(ctx, v);
        case ValueType::StringConst:
            throw std::runtime_error("Impossible conversion to StringConst");
        case ValueType::Void:
            throw std::runtime_error("Invalid type");
    }
    abort();
}


struct WrapAnyVisitor {
    JSContext* ctx;

    Any operator()(Any v) {
        return v;
    }
    Any operator()(ObjectPtr v) {
        JSValue val = JS_MKPTR(JS_TAG_OBJECT, *v);
        return val;
    }
    Any operator()(Int32 v) {
        return JS_NewInt32(ctx, *v);
    }
    Any operator()(Float64 v) {
        return JS_NewFloat64(ctx, *v);
    }
    Any operator()(Boolean v) {
        return JS_NewBool(ctx, *v);
    }
    Any operator()(auto) {
        throw std::runtime_error("Invalid conversion to JSValue");
    }
};

inline Any wrapAny(JSContext* ctx, RegVal v) {
    return std::visit(WrapAnyVisitor{ctx}, v);
}


inline RegVal unwrapAny(JSContext* ctx, Any v, ValueType type) {
    switch (type) {
        case ValueType::I32: {
            if (JS_VALUE_GET_TAG(*v) != JS_TAG_INT) { throw std::runtime_error("Not an Int32"); }
            return Int32{ JS_VALUE_GET_INT(*v) };
        }
        case ValueType::F64: {
            if (JS_VALUE_GET_TAG(*v) != JS_TAG_FLOAT64) { throw std::runtime_error("Not a Float64"); }
            return Float64{ JS_VALUE_GET_FLOAT64(*v) };
        }
        case ValueType::Bool: {
            if (JS_VALUE_GET_TAG(*v) != JS_TAG_BOOL) { throw std::runtime_error("Not a Bool"); }
            return Boolean{ static_cast<bool>(JS_VALUE_GET_INT(*v)) };
        }
        case ValueType::Object: {
            if (JS_VALUE_GET_TAG(*v) != JS_TAG_OBJECT) { throw std::runtime_error("Not an Object"); }
            return ObjectPtr{ JS_VALUE_GET_PTR(*v) };
        }
        case ValueType::Any: {
            return Any{ v };
        }
        default:
            throw std::runtime_error("Invalid type");
    }
}


namespace detail {

    template<typename T>
    concept NotAny = !std::is_same_v<T, JSValue>;

    template<typename T>
    struct plus;
    template<>
    struct plus<JSValue> {
        JSContext* ctx;
        JSValue operator()(JSValue a, JSValue b) {
            int32_t err;
            return jac::quickjs_ops::add(ctx, a, b, &err);
        }
    };
    template<typename T>
        requires NotAny<T>
    struct plus<T> {
        JSContext* ctx;
        decltype(auto) operator()(T a, T b) {
            return std::plus<T>{}(a, b);
        }
    };

    template<typename T>
    struct minus;
    template<>
    struct minus<JSValue> {
        JSContext* ctx;
        JSValue operator()(JSValue a, JSValue b) {
            int32_t err;
            return jac::quickjs_ops::sub(ctx, a, b, &err);
        }
    };
    template<typename T>
        requires NotAny<T>
    struct minus<T> {
        JSContext* ctx;
        decltype(auto) operator()(T a, T b) {
            return std::minus<T>{}(a, b);
        }
    };

    template<typename T>
    struct multiplies;
    template<>
    struct multiplies<JSValue> {
        JSContext* ctx;
        JSValue operator()(JSValue a, JSValue b) {
            int32_t err;
            return jac::quickjs_ops::mul(ctx, a, b, &err);
        }
    };
    template<typename T>
        requires NotAny<T>
    struct multiplies<T> {
        JSContext* ctx;
        decltype(auto) operator()(T a, T b) {
            return std::multiplies<T>{}(a, b);
        }
    };

    template<typename T>
    struct divides;
    template<>
    struct divides<JSValue> {
        JSContext* ctx;
        JSValue operator()(JSValue a, JSValue b) {
            int32_t err;
            return jac::quickjs_ops::div(ctx, a, b, &err);
        }
    };
    template<>
    struct divides<int32_t> {
        JSContext* ctx;
        decltype(auto) operator()(int32_t a, int32_t b) {
            if (b == 0) {
                throw std::runtime_error("Division by zero");
            }
            return a / b;
        }
    };
    template<typename T>
        requires NotAny<T>
    struct divides<T> {
        JSContext* ctx;
        decltype(auto) operator()(T a, T b) {
            return std::divides<T>{}(a, b);
        }
    };

    template<typename T>
    struct exponentiates;
    template<>
    struct exponentiates<JSValue> {
        JSContext* ctx;
        JSValue operator()(JSValue a, JSValue b) {
            int32_t err;
            return jac::quickjs_ops::pow(ctx, a, b, &err);
        }
    };

    template<>
    struct exponentiates<double> {
        JSContext* ctx;
        decltype(auto) operator()(double a, double b) {
            return std::pow(a, b);
        }
    };

    template<>
    struct exponentiates<int32_t> {
        JSContext* ctx;
        decltype(auto) operator()(int32_t a, int32_t b) {
            int32_t res = 1;
            while (b) {
                if (b & 1) {
                    res *= a;
                }
                a *= a;
                b >>= 1;
            }
            return res;
        }
    };

    template<typename T>
        requires NotAny<T>
    struct exponentiates<T> {
        JSContext* ctx;
        T operator()(T, T) {
            throw std::runtime_error("Invalid exponentiation arguments");
        }
    };


    template<typename T>
    struct equal_to;
    template<>
    struct equal_to<JSValue> {
        JSContext* ctx;
        decltype(auto) operator()(JSValue a, JSValue b) {
            return jac::quickjs_ops::equal(ctx, a, b, nullptr);
        }
    };

    template<typename T>
        requires NotAny<T>
    struct equal_to<T> {
        JSContext* ctx;
        decltype(auto) operator()(T a, T b) {
            return std::equal_to<T>{}(a, b);
        }
    };

    template<typename T>
    struct not_equal_to;
    template<>
    struct not_equal_to<JSValue> {
        JSContext* ctx;
        decltype(auto) operator()(JSValue a, JSValue b) {
            return !jac::quickjs_ops::equal(ctx, a, b, nullptr);
        }
    };

    template<typename T>
        requires NotAny<T>
    struct not_equal_to<T> {
        JSContext* ctx;
        decltype(auto) operator()(T a, T b) {
            return std::not_equal_to<T>{}(a, b);
        }
    };

    template<typename T>
    struct greater;
    template<>
    struct greater<JSValue> {
        JSContext* ctx;
        decltype(auto) operator()(JSValue a, JSValue b) {
            return jac::quickjs_ops::greater(ctx, a, b, nullptr);
        }
    };

    template<typename T>
        requires NotAny<T>
    struct greater<T> {
        JSContext* ctx;
        decltype(auto) operator()(T a, T b) {
            return std::greater<T>{}(a, b);
        }
    };

    template<typename T>
    struct greater_equal;
    template<>
    struct greater_equal<JSValue> {
        JSContext* ctx;
        decltype(auto) operator()(JSValue a, JSValue b) {
            return jac::quickjs_ops::greaterEq(ctx, a, b, nullptr);
        }
    };

    template<typename T>
        requires NotAny<T>
    struct greater_equal<T> {
        JSContext* ctx;
        decltype(auto) operator()(T a, T b) {
            return std::greater_equal<T>{}(a, b);
        }
    };

    template<typename T>
    struct less;
    template<>
    struct less<JSValue> {
        JSContext* ctx;
        decltype(auto) operator()(JSValue a, JSValue b) {
            return jac::quickjs_ops::less(ctx, a, b, nullptr);
        }
    };

    template<typename T>
        requires NotAny<T>
    struct less<T> {
        JSContext* ctx;
        decltype(auto) operator()(T a, T b) {
            return std::less<T>{}(a, b);
        }
    };

    template<typename T>
    struct less_equal;
    template<>
    struct less_equal<JSValue> {
        JSContext* ctx;
        decltype(auto) operator()(JSValue a, JSValue b) {
            return jac::quickjs_ops::lessEq(ctx, a, b, nullptr);
        }
    };

    template<typename T>
        requires NotAny<T>
    struct less_equal<T> {
        JSContext* ctx;
        decltype(auto) operator()(T a, T b) {
            return std::less_equal<T>{}(a, b);
        }
    };

    template<typename T>
    struct bit_and {
        JSContext* ctx;
        decltype(auto) operator()(T a, T b) {
            return std::bit_and<T>{}(a, b);
        }
    };

    template<typename T>
    struct bit_or {
        JSContext* ctx;
        decltype(auto) operator()(T a, T b) {
            return std::bit_or<T>{}(a, b);
        }
    };

    template<typename T>
    struct bit_xor {
        JSContext* ctx;
        decltype(auto) operator()(T a, T b) {
            return std::bit_xor<T>{}(a, b);
        }
    };


    struct lshift {
        Int32 operator()(int32_t a, uint32_t b) {
            return a << b;
        }
    };

    struct rshift {
        Int32 operator()(int32_t a, uint32_t b) {
            return a >> b;
        }
    };

    struct urshift {
        Int32 operator()(int32_t a, uint32_t b) {
            return static_cast<uint32_t>(a) >> b;
        }
    };

    struct unplus {
        Int32 operator()(I32Able auto a) {
            return +(*a);
        }
        Float64 operator()(DoubleAble auto a) {
            return +(*a);
        }
        Boolean operator()(Boolean a) {
            return +(*a);
        }
        Float64 operator()(auto) {
            throw std::runtime_error("Invalid type");
        }
    };

    struct unminus {
        Int32 operator()(I32Able auto a) {
            return -*a;
        }
        Float64 operator()(DoubleAble auto a) {
            return -*a;
        }
        Float64 operator()(auto) {
            throw std::runtime_error("Invalid type");
        }
    };

    struct GetMbrVisitor {
        JSContext* ctx;

        RegVal operator()(JSValue val, Any id) {
            JSAtom atom = JS_ValueToAtom(ctx, *id);
            if (atom == JS_ATOM_NULL) {
                throw std::runtime_error("Invalid member access");
            }
            JSValue res = JS_GetProperty(ctx, val, atom);
            JS_FreeAtom(ctx, atom);
            return Any{ res };
        }
        RegVal operator()(JSValue val, StringConst id) {
            JSValue res = JS_GetPropertyStr(ctx, val, (*id).c_str());
            return Any{ res };
        }
        RegVal operator()(JSValue val, Int32 id) {
            JSValue res = JS_GetPropertyUint32(ctx, val, *id);
            return Any{ res };
        }

        RegVal operator()(Any val, auto id) {
            return this->operator()(*val, id);
        }

        RegVal operator()(ObjectPtr val, auto id) {
            return this->operator()(JS_MKPTR(JS_TAG_OBJECT, *val), id);
        }

        RegVal operator()(const auto&, const auto&) {
            throw std::runtime_error("Not implemented (get member access)");
        }
    };

    struct SetMbrVisitor {
        JSContext* ctx;

        int operator()(Any id, JSValue val, JSValue parent) {
            JSAtom atom = JS_ValueToAtom(ctx, *id);
            if (atom == JS_ATOM_NULL) {
                throw std::runtime_error("Invalid member access");
            }
            JS_DupValue(ctx, val);
            auto res = JS_SetProperty(ctx, parent, atom, val);
            JS_FreeAtom(ctx, atom);
            return res;
        }
        int operator()(StringConst id, JSValue val, JSValue parent) {
            JS_DupValue(ctx, val);
            auto res = JS_SetPropertyStr(ctx, parent, (*id).c_str(), val);
            return res;
        }
        int operator()(Int32 id, JSValue val, JSValue parent) {
            JS_DupValue(ctx, val);
            auto res = JS_SetPropertyUint32(ctx, parent, *id, val);
            return res;
        }


        int operator()(auto id, Int32 val, JSValue parent) {
            return this->operator()(id, JS_MKVAL(JS_TAG_INT, *val), parent);
        }
        int operator()(auto id, Float64 val, JSValue parent) {
            JSValue v;
            v.tag = JS_TAG_FLOAT64;
            v.u.float64 = *val;
            return this->operator()(id, v, parent);
        }
        int operator()(auto id, Boolean val, JSValue parent) {
            return this->operator()(id, JS_MKVAL(JS_TAG_BOOL, *val), parent);
        }
        int operator()(auto id, ObjectPtr val, JSValue parent) {
            return this->operator()(id, JS_MKPTR(JS_TAG_OBJECT, *val), parent);
        }
        int operator()(auto id, Any val, JSValue parent) {
            return this->operator()(id, *val, parent);
        }


        int operator()(auto id, auto val, ObjectPtr parent) {
            return this->operator()(id, val, JS_MKPTR(JS_TAG_OBJECT, *parent));
        }
        int operator()(auto id, auto val, Any parent) {
            return this->operator()(id, val, *parent);
        }

        int operator()(const auto&, const auto&, const auto&) {
            throw std::runtime_error("Not implemented (set member access)");
        }
    };

}  // namespace detail


inline bool checkType(const RegVal& val, ValueType type) {
    switch (type) {
        case ValueType::I32:
            return std::holds_alternative<Int32>(val);
        case ValueType::F64:
            return std::holds_alternative<Float64>(val);
        case ValueType::Bool:
            return std::holds_alternative<Boolean>(val);
        case ValueType::Object:
            return std::holds_alternative<ObjectPtr>(val);
        case ValueType::Any:
            return std::holds_alternative<Any>(val);
        case ValueType::StringConst:
            return std::holds_alternative<StringConst>(val);
        case ValueType::Void:
            return false;
    }
    abort();
}


class CFGInterpreter {
    std::map<std::string, CompFn>& _compiledHolder;

    std::unordered_map<int, RegVal> registers;
    std::vector<RegVal> freeStack;

    template<template<typename> class Op, typename Com, typename Res>
    void evalBinop(JSContext* ctx, const Operation& op) {
        setReg(op.res.id, Res{Op<std::decay_t<decltype(*std::declval<Com>())>>{ctx}(
            *std::get<Com>(getReg(op.a.id)),
            *std::get<Com>(getReg(op.b.id))
        )});
    }

    template<template<typename> class Op>
    void evalBinopResType(JSContext* ctx, const Operation& op) {
        switch (op.res.type) {
            case ValueType::I32:
                evalBinop<Op, Int32, Int32>(ctx, op);
                break;
            case ValueType::F64:
                evalBinop<Op, Float64, Float64>(ctx, op);
                break;
            case ValueType::Bool:
                evalBinop<Op, Boolean, Boolean>(ctx, op);
                break;
            case ValueType::Any:
                evalBinop<Op, Any, Any>(ctx, op);
                break;
            default:
                throw std::runtime_error("Not implemented (result type)");
        }
    }

    template<template<typename> class Op>
    void evalRelational(JSContext* ctx, const Operation& op) {
        switch (commonUpcast(op.a.type, op.b.type)) {
            case ValueType::I32:
                evalBinop<Op, Int32, Boolean>(ctx, op);
                break;
            case ValueType::F64:
                evalBinop<Op, Float64, Boolean>(ctx, op);
                break;
            case ValueType::Bool:
                evalBinop<Op, Boolean, Boolean>(ctx, op);
                break;
            case ValueType::Any:
                evalBinop<Op, Any, Boolean>(ctx, op);
                break;
            default:
                throw std::runtime_error("Not implemented (common upcast)");
        }
    }

    void evalDup(JSContext* ctx, const Operation& op) {
        auto val = getReg(op.a.id);
        std::visit([ctx](auto& v) { v._dup(ctx); }, val);
    }

    void evalPushFree(JSContext*, const Operation& op) {
        freeStack.push_back(getReg(op.a.id));
    }

    void evalRem(JSContext* ctx, const Operation& op) {
        if (op.res.type == ValueType::I32) {
            if (*std::get<Int32>(getReg(op.b.id)) == 0) {
                throw std::runtime_error("Division by zero");
            }
            setReg(op.res.id, Int32{ *std::get<Int32>(getReg(op.a.id)) % *std::get<Int32>(getReg(op.b.id)) });
        }
        else if (op.res.type == ValueType::F64) {
            setReg(op.res.id, Float64{ std::fmod(*std::get<Float64>(getReg(op.a.id)), *std::get<Float64>(getReg(op.b.id))) });
        }
        else if (op.res.type == ValueType::Any) {
            int32_t err;
            setReg(op.res.id, Any{ jac::quickjs_ops::rem(ctx, *std::get<Any>(getReg(op.a.id)), *std::get<Any>(getReg(op.b.id)), &err) });
        }
        else {
            throw std::runtime_error("Invalid result type");
        }
    }

    template<typename Op>
    void evalBitShift(JSContext*, const Operation& op) {
        if (op.res.type != ValueType::I32) {
            throw std::runtime_error("Invalid result type");
        }
        setReg(op.res.id, Op{}(  // XXX: polish
            *std::get<Int32>(getReg(op.a.id)),
            static_cast<uint32_t>(*std::get<Int32>(getReg(op.b.id)))
        ));
    }

    void evalSet(JSContext* ctx, const Operation& op) {
        setReg(op.res.id, convert(ctx, getReg(op.a.id), op.res.type));
    }

    template<typename Op>
    void evalUnop(JSContext*, const Operation& op) {
        switch (op.res.type) {
            case ValueType::I32:
                setReg(op.res.id, Op{}(std::get<Int32>(getReg(op.a.id))));
                break;
            case ValueType::F64:
                setReg(op.res.id, Op{}(std::get<Float64>(getReg(op.a.id))));
                break;
            case ValueType::Bool:
                setReg(op.res.id, Op{}(std::get<Boolean>(getReg(op.a.id))));
                break;
            default:
                throw std::runtime_error("Not implemented (unop)");
        }
    }

    void evalGetMember(JSContext* ctx, const Operation op) {
        auto res = std::visit(detail::GetMbrVisitor{ ctx }, getReg(op.a.id), getReg(op.b.id));
        setReg(op.res.id, res);
    }

    void evalSetMember(JSContext* ctx, const Operation op) {
        auto res = std::visit(detail::SetMbrVisitor{ ctx }, getReg(op.a.id), getReg(op.b.id), getReg(op.res.id));
        if (res < 0) {
            throw std::runtime_error("Failed to set member");
        }
    }

    void evalBoolNot(JSContext*, const Operation& op) {
        setReg(op.res.id, Boolean{ !*std::get<Boolean>(getReg(op.a.id)) });
    }

    void evalCallNative(JSContext* ctx, const Call& call) {
        auto it = _compiledHolder.find(std::get<Identifier>(call.obj));
        if (it == _compiledHolder.end()) {
            throw std::runtime_error("Function not found");
        }

        std::vector<JSValue> args;
        args.reserve(call.args.size());
        for (auto& arg : call.args) {
            args.push_back(*wrapAny(ctx, getReg(arg.id)));
        }

        auto& fn = it->second;

        cfg::interp::CFGInterpreter interp(_compiledHolder);
        JSValue res = interp.run(fn.fn, ctx, JS_UNDEFINED, args.size(), args.data());

        if (call.res.type == ValueType::Void) {
            JS_FreeValue(ctx, res);
            return;
        }
        setReg(call.res.id, unwrapAny(ctx, Any{ res }, call.res.type));
    }

    void evalCallObj(JSContext* ctx, const Call& call) {
        auto obj = getReg(std::get<Reg>(call.obj).id);

        std::vector<JSValue> args;
        args.reserve(call.args.size() - 1);
        for (size_t i = 1; i < call.args.size(); ++i) {
            args.push_back(*wrapAny(ctx, getReg(call.args[i].id)));
        }

        JSValue thisVal;
        if (call.args[0].type == ValueType::Void) {
            thisVal = JS_UNDEFINED;
        }
        else {
            thisVal = *wrapAny(ctx, getReg(call.args[0].id));
        }

        JSValue res;
        if (!call.isConstructor) {
            if (std::holds_alternative<Any>(obj)) {
                res = JS_Call(ctx, *std::get<Any>(obj), thisVal, args.size(), args.data());
            }
            else if (std::holds_alternative<ObjectPtr>(obj)) {
                JSValue objVal = JS_MKPTR(JS_TAG_OBJECT, *std::get<ObjectPtr>(obj));
                res = JS_Call(ctx, objVal, thisVal, args.size(), args.data());
            }
            else {
                throw std::runtime_error("Invalid call operand type");
            }
        }
        else {
            if (std::holds_alternative<Any>(obj)) {
                res = JS_CallConstructor(ctx, *std::get<Any>(obj), args.size(), args.data());
            }
            else if (std::holds_alternative<ObjectPtr>(obj)) {
                JSValue objVal = JS_MKPTR(JS_TAG_OBJECT, *std::get<ObjectPtr>(obj));
                res = JS_CallConstructor(ctx, objVal, args.size(), args.data());
            }
            else {
                throw std::runtime_error("Invalid call operand type");
            }
        }
        setReg(call.res.id, Any{ res });
    }
public:
    CFGInterpreter(std::map<std::string, CompFn>& compiledHolder) : _compiledHolder(compiledHolder) {}

    void evalOperation(JSContext* ctx, const Operation& op) {
        if (op.a.type != ValueType::Void) {
            assert(checkType(getReg(op.a.id), op.a.type) && "Invalid operand type");
        }
        if (op.b.type != ValueType::Void) {
            assert(checkType(getReg(op.b.id), op.b.type) && "Invalid operand type");
        }
        switch (op.op) {
            case Opcode::Add: evalBinopResType<detail::plus>(ctx, op); break;
            case Opcode::Sub: evalBinopResType<detail::minus>(ctx, op); break;
            case Opcode::Mul: evalBinopResType<detail::multiplies>(ctx, op); break;
            case Opcode::Div: evalBinopResType<detail::divides>(ctx, op); break;
            case Opcode::Pow: evalBinopResType<detail::exponentiates>(ctx, op); break;
            case Opcode::Eq: evalRelational<detail::equal_to>(ctx, op); break;
            case Opcode::Neq: evalRelational<detail::not_equal_to>(ctx, op); break;
            case Opcode::Gt: evalRelational<detail::greater>(ctx, op); break;
            case Opcode::Gte: evalRelational<detail::greater_equal>(ctx, op); break;
            case Opcode::Lt: evalRelational<detail::less>(ctx, op); break;
            case Opcode::Lte: evalRelational<detail::less_equal>(ctx, op); break;
            case Opcode::BitAnd: evalBinop<detail::bit_and, Int32, Int32>(ctx, op); break;
            case Opcode::BitOr: evalBinop<detail::bit_or, Int32, Int32>(ctx, op); break;
            case Opcode::BitXor: evalBinop<detail::bit_xor, Int32, Int32>(ctx, op); break;
            case Opcode::Dup: evalDup(ctx, op); break;
            case Opcode::PushUnref: evalPushFree(ctx, op); break;
            case Opcode::Rem: evalRem(ctx, op); break;
            case Opcode::LShift: evalBitShift<detail::lshift>(ctx, op); break;
            case Opcode::RShift: evalBitShift<detail::rshift>(ctx, op); break;
            case Opcode::URShift: evalBitShift<detail::urshift>(ctx, op); break;
            case Opcode::Set: evalSet(ctx, op); break;
            case Opcode::UnMinus: evalUnop<detail::unminus>(ctx, op); break;
            case Opcode::UnPlus: evalUnop<detail::unplus>(ctx, op); break;
            case Opcode::GetMember: evalGetMember(ctx, op); break;
            case Opcode::SetMember: evalSetMember(ctx, op); break;
            case Opcode::BoolNot: evalBoolNot(ctx, op); break;
            case Opcode::BitNot:
                throw std::runtime_error("Not implemented (operation) " + std::to_string(static_cast<int>(op.op)));
        }
        if (op.res.type != ValueType::Void) {
            assert(checkType(getReg(op.res.id), op.res.type) && "Invalid result type");
        }
    }

    void constInit(JSContext*, const ConstInit& init) {
        struct visitor {
            RegVal operator()(int32_t value) {
                return Int32{ value };
            }
            RegVal operator()(double value) {
                return Float64{ value };
            }
            RegVal operator()(bool value) {
                return Boolean{ value };
            }
            RegVal operator()(const std::string& value) {
                return StringConst{ value };
            }
        };

        setReg(init.id, std::visit(visitor{}, init.value));
    }

    void evalInstruction(JSContext* ctx, const Instruction& s) {
        struct visitor {
            CFGInterpreter& self;
            JSContext* ctx;

            void operator()(const Operation& op) {
                self.evalOperation(ctx, op);
            }
            void operator()(const ConstInit& init) {
                self.constInit(ctx, init);
            }
            void operator()(const Call& call) {
                if (call.isNative()) {
                    self.evalCallNative(ctx, call);
                }
                else {
                    self.evalCallObj(ctx, call);
                }
            }
        };

        std::visit(visitor{ *this, ctx }, s.op);
    }

    RegVal getReg(int id) {
        return registers.at(id);
    }

    void setReg(int id, RegVal value) {
        if (registers.contains(id)) {
            if (registers.at(id).index() != value.index()) {
                throw std::runtime_error("Cannot change type of register");
            }
            registers.at(id) = value;
        }
        else {
            registers.emplace(id, value);
        }
    }

    JSValue run(const Function& func, JSContext* ctx, [[maybe_unused]] JSValueConst this_val, int argc, JSValueConst* argv) {
        registers.clear();
        freeStack.clear();

        std::vector<RegVal> argsHolder;
        argsHolder.reserve(func.args.size());

        Defer d([&] {
            for (auto& val : freeStack) {
                std::visit([ctx](auto& v) { v._free(ctx); }, val);
            }
            for (auto& val : argsHolder) {
                std::visit([ctx](auto& v) { v._free(ctx); }, val);
            }
        });


        for (size_t i = 0; i < func.args.size(); i++) {
            if (i >= static_cast<size_t>(argc)) {
                throw std::runtime_error("Not enough arguments");
            }
            auto set = [&](int id, auto value) {
                setReg(id, value);
                if constexpr (!std::is_same_v<decltype(value), Any> && !std::is_same_v<decltype(value), ObjectPtr>) {
                    argsHolder.push_back(value);
                }
            };
            switch (func.args[i].type) {
                case ValueType::I32: {
                    int32_t value;
                    JS_ToInt32(ctx, &value, argv[i]);
                    set(func.args[i].id, Int32{ value });
                } break;
                case ValueType::F64: {
                    double value;
                    JS_ToFloat64(ctx, &value, argv[i]);
                    set(func.args[i].id, Float64{ value });
                } break;
                case ValueType::Bool: {
                    auto val = JS_ToBool(ctx, argv[i]);
                    if (val < 0) {
                        throw std::runtime_error("Invalid conversion to Bool");
                    }
                    set(func.args[i].id, Boolean{ val != 0 });
                } break;
                case ValueType::Object: {
                    set(func.args[i].id, ObjectPtr{ JS_VALUE_GET_PTR(argv[i]) });
                } break;
                case ValueType::Any: {
                    set(func.args[i].id, Any{ argv[i] });
                } break;
                default:
                    throw std::runtime_error("Not implemented (arg type)");
            }
        }

        BasicBlockPtr activeBlock = func.entry;
        while (activeBlock) {
            for (auto& instruction : activeBlock->instructions) {
                evalInstruction(ctx, instruction);
            }

            switch (activeBlock->jump.type) {
                case Terminator::Jump:
                    activeBlock = activeBlock->jump.target;
                    break;
                case Terminator::Branch:
                    if (*std::get<Boolean>(getReg(activeBlock->jump.value.id))) {
                        activeBlock = activeBlock->jump.target;
                    }
                    else {
                        activeBlock = activeBlock->jump.other;
                    }
                    break;
                case Terminator::Return:
                    if (activeBlock->jump.value.type == ValueType::Void) {
                        return JS_UNDEFINED;
                    }
                    else {
                        auto val = getReg(activeBlock->jump.value.id);
                        switch (activeBlock->jump.value.type) {
                            case ValueType::I32: {
                                if (auto valPtr = std::get_if<Int32>(&val)) {
                                    auto res = *convert<Any>(ctx, val);
                                    valPtr->_free(ctx);
                                    return res;
                                }
                                throw std::runtime_error("Invalid return type");
                            }
                            case ValueType::F64: {
                                if (auto valPtr = std::get_if<Float64>(&val)) {
                                    auto res = *convert<Any>(ctx, val);
                                    valPtr->_free(ctx);
                                    return res;
                                }
                                throw std::runtime_error("Invalid return type");
                            }
                            case ValueType::Bool: {
                                if (auto valPtr = std::get_if<Boolean>(&val)) {
                                    auto res = *convert<Any>(ctx, val);
                                    valPtr->_free(ctx);
                                    return res;
                                }
                                throw std::runtime_error("Invalid return type");
                            }
                            case ValueType::Object:
                                if (auto valPtr = std::get_if<ObjectPtr>(&val)) {
                                    auto res = *convert<Any>(ctx, val);
                                    valPtr->_free(ctx);
                                    return res;
                                }
                                throw std::runtime_error("Invalid return type");
                            case ValueType::Any:
                                if (!std::holds_alternative<Any>(val)) {
                                    throw std::runtime_error("Invalid return type");
                                }
                                return *std::get<Any>(val);
                            case ValueType::StringConst:
                                throw std::runtime_error("Not implemented (return StringConst)");
                            case ValueType::Void:
                                throw std::runtime_error("Invalid return type");
                        }
                    }
                    abort();
                case Terminator::Throw: {
                    auto val = getReg(activeBlock->jump.value.id);
                    if (auto valPtr = std::get_if<Any>(&val)) {
                        JS_Throw(ctx, **valPtr);
                        return JS_EXCEPTION;
                    }
                    throw std::runtime_error("Invalid throw type");
                }
                case Terminator::None:
                    throw std::runtime_error("Invalid jump (None)");
            }
        }

        abort();
    }
};


}  // namespace jac::cfg::interp
