#pragma once

#include "jac/machine/context.h"
#include <cassert>
#include <cmath>
#include <cstdint>
#include <cstdlib>
#include <deque>
#include <iostream>
#include <map>
#include <memory>
#include <optional>
#include <span>
#include <string>
#include <type_traits>
#include <utility>
#include <variant>
#include <vector>

#include <quickjs.h>

#include <jac/machine/compiler/quickjsOps.h>
#include <jac/machine/compiler/tlessCfg.h>
#include <jac/machine/compiler/tlessBodies.h>
#include <jac/machine/functionFactory.h>
#include <jac/util.h>


namespace jac::cfg::tless::interp {

#ifdef JAC_TLESS_INTERP_TRACE
inline constexpr bool traceEnabled = JAC_TLESS_INTERP_TRACE != 0;
#else
inline constexpr bool traceEnabled = false;
#endif


struct Slot;


struct Code {
    cfg::tless::Function* code;
    std::shared_ptr<cfg::tless::Function> root;
};


struct Closure {
    Code code;
    std::vector<Slot> capturedVars;
};


constexpr Tag mapNormalizedQuickJsTag(int normalizedTag) {
    switch (normalizedTag) {
        case JS_TAG_INT: return Tag::Int;
        case JS_TAG_FLOAT64: return Tag::Float64;
        case JS_TAG_BOOL: return Tag::Bool;
        case JS_TAG_UNDEFINED: return Tag::Undefined;
        case JS_TAG_NULL: return Tag::Null;
        case JS_TAG_STRING:
        case JS_TAG_STRING_ROPE: return Tag::String;
        case JS_TAG_OBJECT: return Tag::Object;
        case JS_TAG_SYMBOL: return Tag::Symbol;
        case JS_TAG_BIG_INT:
        case JS_TAG_SHORT_BIG_INT: return Tag::BigInt;
        case JS_TAG_EXCEPTION: return Tag::Exception;
        case JS_TAG_UNINITIALIZED: return Tag::Uninitialized;
        default: return Tag::Other;
    }
}


struct Value {
    using Content = std::variant<JSValue, Closure>;

    Content val;

    template<typename T>
        requires std::is_constructible_v<Content, T&&>
    explicit Value(T&& val_) : val(std::forward<T>(val_)) {}

    void free(JSContext* ctx) {
        if (std::holds_alternative<JSValue>(val)) {
            JS_FreeValue(ctx, std::get<JSValue>(val));
        }
    }

    Value dup(JSContext* ctx) const {
        if (std::holds_alternative<JSValue>(val)) {
            return Value(JS_DupValue(ctx, std::get<JSValue>(val)));
        }
        return *this;
    }

    JSValue toJSValue(JSContext* ctx) const;
};


inline Tag getTag(const Value& value) {
    if (std::holds_alternative<JSValue>(value.val)) {
        return mapNormalizedQuickJsTag(JS_VALUE_GET_NORM_TAG(std::get<JSValue>(value.val)));
    }
    return Tag::Closure;
}


struct SlotCell {
    virtual ~SlotCell() = default;
    virtual Value load() = 0;
    virtual void store(Value newVal) = 0;
    virtual bool isGlobal() const { return false; }

    bool isConst() const { return _isConst; }
    bool isInitialized() const { return _initialized; }
    void markInitialized() { _initialized = true; }

protected:
    bool _isConst = false;
    bool _initialized = false;
};


struct LocalSlotCell : public SlotCell {
    JSContext* ctx;
    Value val;

    LocalSlotCell(JSContext* ctx_, Value val_, bool isConst_ = false) : ctx(ctx_), val(std::move(val_)) { _isConst = isConst_; }
    ~LocalSlotCell() override { val.free(ctx); }

    Value load() override {
        if (std::holds_alternative<JSValue>(val.val)
            && JS_IsUninitialized(std::get<JSValue>(val.val))) {
            JS_ThrowReferenceError(ctx, "Cannot access uninitialized variable");
            return Value(JS_EXCEPTION);
        }
        return val.dup(ctx);
    }

    void store(Value newVal) override {
        val.free(ctx);
        val = std::move(newVal);
    }

    LocalSlotCell(const LocalSlotCell&) = delete;
    LocalSlotCell& operator=(const LocalSlotCell&) = delete;
    LocalSlotCell(LocalSlotCell&& other) = delete;
    LocalSlotCell& operator=(LocalSlotCell&& other) = delete;
};


struct GlobalSlotCell : public SlotCell {
    JSContext* ctx;
    std::string name;

    GlobalSlotCell(JSContext* ctx_, std::string name_, bool isConst_ = false) : ctx(ctx_), name(std::move(name_)) { _isConst = isConst_; }

    bool isGlobal() const override { return true; }

    Value load() override {
        JSValue globalObj = JS_GetGlobalObject(ctx);
        JSAtom atom = JS_NewAtom(ctx, name.c_str());
        int has = JS_HasProperty(ctx, globalObj, atom);
        JS_FreeAtom(ctx, atom);
        if (has <= 0) {
            JS_FreeValue(ctx, globalObj);
            if (has < 0) {
                assert(false && "Exception during global variable has-check");
            }
            JS_ThrowReferenceError(ctx, "%s is not defined", name.c_str());
            return Value(JS_EXCEPTION);
        }
        JSValue prop = JS_GetPropertyStr(ctx, globalObj, name.c_str());
        JS_FreeValue(ctx, globalObj);
        if (JS_IsException(prop)) {
            assert(false && "Exception during global variable load");
        }
        return Value(prop);
    }

    void store(Value newVal) override {
        JSValue globalObj = JS_GetGlobalObject(ctx);
        if (!std::holds_alternative<JSValue>(newVal.val)) {
            JS_FreeValue(ctx, globalObj);
            assert(false && "Global variables can only store JS values");
        }

        JSValue value = std::get<JSValue>(newVal.val);
        if (JS_SetPropertyStr(ctx, globalObj, name.c_str(), value) < 0) {
            JS_FreeValue(ctx, value);
            JS_FreeValue(ctx, globalObj);
            assert(false && "Exception during global variable store: ");
        }
        JS_FreeValue(ctx, globalObj);
    }
};


struct Slot {
    std::shared_ptr<SlotCell> val;

    Value load() {
        return val->load();
    }

    void store(Value newVal) {
        val->store(std::move(newVal));
    }

    static Slot make(JSContext* ctx, Value val, bool isConst = false) {
        return Slot{ std::make_shared<LocalSlotCell>(ctx, std::move(val), isConst) };
    }

    static Slot makeGlobal(JSContext* ctx, std::string name, bool isConst = false) {
        return Slot{ std::make_shared<GlobalSlotCell>(ctx, std::move(name), isConst) };
    }
};


struct Unset {};
struct Used {};


struct RawValue {
    std::variant<int32_t, double, bool, Tag, Code> v;

    explicit RawValue(auto value) : v(std::move(value)) {}
};


struct RegContent {
    using Content = std::variant<Unset, Used, Value, Slot, RawValue>;

    Content content;

    Content takeContent() {
        Content old = std::move(content);
        content = Used{};
        return old;
    }

    Slot getSlot() const {
        assert(std::holds_alternative<Slot>(content) && "Expected slot");
        return std::get<Slot>(content);
    }

    Value getValue() const {
        assert(std::holds_alternative<Value>(content) && "Expected value");
        return std::get<Value>(content);
    }

    Value takeValue() {
        assert(std::holds_alternative<Value>(content) && "Expected value");
        Value value = std::move(std::get<Value>(content));
        content = Used{};
        return value;
    }

    const RawValue& getRaw() const {
        assert(std::holds_alternative<RawValue>(content) && "Expected raw value");
        return std::get<RawValue>(content);
    }

    template<typename T>
    bool isRawType() const {
        return isRaw() && std::holds_alternative<T>(getRaw().v);
    }

    template<typename T>
    const T& getRawAs() const {
        assert(isRawType<T>() && "Unexpected raw value kind");
        return std::get<T>(getRaw().v);
    }

    bool isUnset() const { return std::holds_alternative<Unset>(content); }
    bool isUsed() const { return std::holds_alternative<Used>(content); }
    bool isSlot() const { return std::holds_alternative<Slot>(content); }
    bool isValue() const { return std::holds_alternative<Value>(content); }
    bool isRaw() const { return std::holds_alternative<RawValue>(content); }

    bool isI32() const { return isRawType<int32_t>(); }
    bool isF64() const { return isRawType<double>(); }
    bool isBool() const { return isRawType<bool>(); }
    bool isTag() const { return isRawType<Tag>(); }

    const int32_t& getI32() const { return getRawAs<int32_t>(); }
    const double& getF64() const { return getRawAs<double>(); }
    const bool& getBool() const { return getRawAs<bool>(); }
    const Tag& getTag() const { return getRawAs<Tag>(); }
};


enum class RunState {
    Runnable,
    Suspended,
    Finished,
};


inline void freeRegContent(JSContext* ctx, const RegContent& content) {
    if (content.isValue()) {
        content.getValue().free(ctx);
    }
}


inline void freeOperands(JSContext* ctx, std::vector<RegContent>& operands) {
    for (auto& operand : operands) {
        freeRegContent(ctx, operand);
    }
    operands.clear();
}


struct Operands {
    std::vector<RegContent> v;

    Operands() = default;
    explicit Operands(std::vector<RegContent> v_) : v(std::move(v_)) {}
    Operands(Operands&& other) noexcept : v(std::move(other.v)) { other.v.clear(); }
    Operands& operator=(Operands&& other) noexcept {
        v = std::move(other.v);
        other.v.clear();
        return *this;
    }
    Operands(const Operands&) = delete;
    Operands& operator=(const Operands&) = delete;
    ~Operands() { assert(v.empty() && "Operands dropped unconsumed"); }

    std::vector<RegContent> take() { return std::move(v); }
};


using Callee = std::variant<const Function*, Closure>;


struct Exited {
    Operands args;
};


struct Awaiting {
    JSValue awaitable;

    explicit Awaiting(JSValue awaitable_) : awaitable(awaitable_) {}
    Awaiting(Awaiting&& other) noexcept : awaitable(other.awaitable) { other.awaitable = JS_UNDEFINED; }
    Awaiting& operator=(Awaiting&& other) noexcept {
        awaitable = other.awaitable;
        other.awaitable = JS_UNDEFINED;
        return *this;
    }
    Awaiting(const Awaiting&) = delete;
    Awaiting& operator=(const Awaiting&) = delete;
    ~Awaiting() { assert(JS_IsUndefined(awaitable) && "Awaitable dropped unconsumed"); }

    JSValue take() {
        JSValue value = awaitable;
        awaitable = JS_UNDEFINED;
        return value;
    }
};


struct Calls {
    Callee callee;
    Operands args;
};


using ExecResult = std::variant<Exited, Awaiting, Calls>;


struct Frame {
    JSContext* ctx;
    std::shared_ptr<Function> root;  // keeps func's constant pool alive
    const Function& func;

    BasicBlockPtr block;
    size_t pc = 0;
    std::map<int, RegContent> regs;
    RunState state = RunState::Runnable;

    Frame(JSContext* ctx_, std::shared_ptr<Function> root_, const Function& func_) : ctx(ctx_), root(std::move(root_)), func(func_), block(func_.entry) {}

    ~Frame() {
#ifndef NDEBUG
        if (state == RunState::Finished) {
            for (const auto& [_, content] : regs) {
                assert((content.isUnset() || content.isUsed()) && "Finished frame left a register unconsumed");
            }
        }
#endif
        for (const auto& [_, content] : regs) {
            freeRegContent(ctx, content);
        }
    }

    Frame(const Frame&) = delete;
    Frame& operator=(const Frame&) = delete;
    Frame(Frame&&) = delete;
    Frame& operator=(Frame&&) = delete;

    RegContent getReg(const Reg& reg) {
        assert(reg.id() != 0 && "Use of register 0 (invalid)");
        auto it = regs.find(reg.id());
        assert(it != regs.end() && "Reading uninitialized register");
        assert(!it->second.isUnset() && "Reading uninitialized register");
        assert(!it->second.isUsed() && "Double use of register");
        return RegContent{ it->second.takeContent() };
    }

    void setReg(const Reg& reg, RegContent cont) {
        assert(reg.id() != 0 && "Use of register 0 (invalid)");
        assert((!regs.contains(reg.id()) || regs.at(reg.id()).isUnset() || regs.at(reg.id()).isUsed()) && "Double assignment of register");
        regs[reg.id()] = std::move(cont);
    }

    RegContent getPoolConst(int index) {
        assert(index >= 0 && static_cast<size_t>(index) < func.constPool.size());
        return std::visit([&](const auto& value) -> RegContent {
            if constexpr (std::is_same_v<std::decay_t<decltype(value)>, std::unique_ptr<Function>>) {
                return RegContent{ RawValue{ Code{ value.get(), root } } };
            }
            assert(false && "Invalid constant type in pool");
            return RegContent{ Used{} };
        }, func.constPool[index].value);
    }
};


inline void printState(const Frame& frame) {
    std::cout << "  Register state:\n";

    auto valStr = [](const Value& value) -> std::string {
        if (std::holds_alternative<JSValue>(value.val)) {
            return "<js-value tag=" + std::to_string(JS_VALUE_GET_NORM_TAG(std::get<JSValue>(value.val))) + ">";
        }
        return "<closure>";
    };

    auto tagStr = [](Tag tag) -> const char* {
        switch (tag) {
            case Tag::Int: return "Int";
            case Tag::Float64: return "Float64";
            case Tag::Bool: return "Bool";
            case Tag::Undefined: return "Undefined";
            case Tag::Null: return "Null";
            case Tag::String: return "String";
            case Tag::Object: return "Object";
            case Tag::Symbol: return "Symbol";
            case Tag::BigInt: return "BigInt";
            case Tag::Exception: return "Exception";
            case Tag::Uninitialized: return "Uninitialized";
            case Tag::Closure: return "Closure";
            case Tag::Other: return "Other";
        }
        return "Unknown";
    };

    auto rawStr = [&](const RawValue& raw) -> std::string {
        return std::visit([&](const auto& value) -> std::string {
            using T = std::decay_t<decltype(value)>;
            if constexpr (std::is_same_v<T, int32_t>) {
                return "i32:" + std::to_string(value);
            }
            else if constexpr (std::is_same_v<T, double>) {
                return "f64:" + std::to_string(value);
            }
            else if constexpr (std::is_same_v<T, bool>) {
                return std::string("bool:") + (value ? "true" : "false");
            }
            else if constexpr (std::is_same_v<T, Tag>) {
                return std::string("tag:") + tagStr(value);
            }
            else if constexpr (std::is_same_v<T, Code>) {
                return "<code>";
            }
        }, raw.v);
    };

    for (const auto& [id, content] : frame.regs) {
        std::cout << "    _" << id << ": " << std::flush;
        if (content.isUnset()) {
            std::cout << "Unset" << std::endl;
        }
        else if (content.isUsed()) {
            std::cout << "Used" << std::endl;
        }
        else if (content.isValue()) {
            std::cout << "Value(" << std::flush << valStr(content.getValue()) << ")" << std::endl;
        }
        else if (content.isSlot()) {
            const auto slot = content.getSlot();
            std::cout << "Slot(" << (slot.val->isGlobal() ? "global" : "local") << ", " << slot.val.get() << ")" << std::endl;
        }
        else if (content.isRaw()) {
            std::cout << "Raw(" << rawStr(content.getRaw()) << ")" << std::endl;
        }
    }
}


inline void evalConstInit(Frame& f, const ConstInit& init) {
    JSContext* ctx = f.ctx;
    RegContent content = std::visit(overloaded{
        [&](int32_t value) -> RegContent { return RegContent{ Value(JS_NewInt32(ctx, value)) }; },
        [&](double value) -> RegContent { return RegContent{ Value(JS_NewFloat64(ctx, value)) }; },
        [&](bool value) -> RegContent { return RegContent{ Value(JS_NewBool(ctx, value)) }; },
        [&](const std::string& value) -> RegContent { return RegContent{ Value(JS_NewString(ctx, value.c_str())) }; },
        [&](PoolConst value) -> RegContent { return f.getPoolConst(value.id); },
        [](RawI32Const value) -> RegContent { return RegContent{ RawValue{ value.v } }; },
        [](RawF64Const value) -> RegContent { return RegContent{ RawValue{ value.v } }; },
        [](RawBoolConst value) -> RegContent { return RegContent{ RawValue{ value.v } }; },
        [](RawTagConst value) -> RegContent { return RegContent{ RawValue{ value.v } }; }
    }, init.value);
    f.setReg(init.reg, std::move(content));
}


inline void moveRegs(Frame& f, const std::vector<Reg>& from, const std::vector<Reg>& to) {
    assert(from.size() == to.size());
    for (size_t i = 0; i < from.size(); i++) {
        f.setReg(to[i], f.getReg(from[i]));
    }
}


// nullopt to continue, value to pause execution
inline std::optional<ExecResult> evalOperation(Frame& f, const Operation& op) {
    JSContext* ctx = f.ctx;
    auto takeJSValue = [ctx](RegContent& content) { return content.takeValue().toJSValue(ctx); };

    std::vector<RegContent> args;
    for (const auto& arg : op.args) {
        args.push_back(f.getReg(arg));
    }

    std::vector<RegContent> res;
    switch (op.op) {
        case Opcode::CreateLocal: {
            JSValue isConstVal = takeJSValue(args[0]);
            bool isConst = JS_ToBool(ctx, isConstVal);
            JS_FreeValue(ctx, isConstVal);
            res.emplace_back(Slot::make(ctx, Value(JS_UNINITIALIZED), isConst));
        } break;
        case Opcode::CreateUndefined: res.emplace_back(Value(JS_UNDEFINED)); break;
        case Opcode::BitNot: {
            int32_t operand;
            JSValue operandVal = takeJSValue(args[0]);
            int conversionResult = JS_ToInt32(ctx, &operand, operandVal);
            JS_FreeValue(ctx, operandVal);
            if (conversionResult) {
                JSValue exc = JS_GetException(ctx);
                res.emplace_back(Value(JS_UNDEFINED));
                res.emplace_back(Value(exc));
                res.emplace_back(RawValue{ true });
            }
            else {
                res.emplace_back(Value(JS_NewInt32(ctx, ~operand)));
                res.emplace_back(Value(JS_UNDEFINED));
                res.emplace_back(RawValue{ false });
            }
        } break;
        case Opcode::UnPlus: {
            int32_t exception = 0;
            JSValue operand = takeJSValue(args[0]);
            JSValue num = quickjs_ops::toNumber(ctx, operand, &exception);
            JS_FreeValue(ctx, operand);
            if (exception) {
                JSValue exc = JS_GetException(ctx);
                res.emplace_back(Value(JS_UNDEFINED));
                res.emplace_back(Value(exc));
                res.emplace_back(RawValue{ true });
            }
            else {
                res.emplace_back(Value(num));
                res.emplace_back(Value(JS_UNDEFINED));
                res.emplace_back(RawValue{ false });
            }
        } break;
        case Opcode::UnMinus: {
            int32_t exception = 0;
            JSValue operand = takeJSValue(args[0]);
            JSValue num = quickjs_ops::toNumber(ctx, operand, &exception);
            JS_FreeValue(ctx, operand);
            if (exception) {
                JSValue exc = JS_GetException(ctx);
                res.emplace_back(Value(JS_UNDEFINED));
                res.emplace_back(Value(exc));
                res.emplace_back(RawValue{ true });
                break;
            }
            switch (JS_VALUE_GET_NORM_TAG(num)) {
                case JS_TAG_INT: res.emplace_back(Value(JS_NewInt32(ctx, -JS_VALUE_GET_INT(num)))); break;
                case JS_TAG_FLOAT64: res.emplace_back(Value(JS_NewFloat64(ctx, -JS_VALUE_GET_FLOAT64(num)))); break;
                default: assert(false && "Unexpected type after conversion to number"); break;
            }
            JS_FreeValue(ctx, num);
            res.emplace_back(Value(JS_UNDEFINED));
            res.emplace_back(RawValue{ false });
        } break;
        case Opcode::Load: {
            auto slot = args[0].getSlot();
            Value val = slot.load();
            if (std::holds_alternative<JSValue>(val.val) && JS_IsException(std::get<JSValue>(val.val))) {
                JSValue exc = JS_GetException(ctx);
                res.emplace_back(Value(JS_UNDEFINED));
                res.emplace_back(slot);
                res.emplace_back(Value(exc));
                res.emplace_back(RawValue{ true });
            }
            else {
                res.emplace_back(std::move(val));
                res.emplace_back(slot);
                res.emplace_back(Value(JS_UNDEFINED));
                res.emplace_back(RawValue{ false });
            }
        } break;
        case Opcode::Dup:
            if (args[0].isSlot()) {
                auto slot = args[0].getSlot();
                res.emplace_back(slot);
                res.emplace_back(slot);
            }
            else if (args[0].isValue()) {
                auto value = args[0].takeValue();
                res.emplace_back(value);
                res.emplace_back(value.dup(ctx));
            }
            else {
                auto raw = args[0].getRaw();
                res.emplace_back(raw);
                res.emplace_back(std::move(raw));
            }
            break;
        case Opcode::Kill:
            if (args[0].isValue()) {
                auto value = args[0].takeValue();
                value.free(ctx);
            }
            break;
        case Opcode::CreateGlobalSlot: {
            JSValue ident = takeJSValue(args[0]);
            if (!JS_IsString(ident)) {
                assert(false && "Exception during CreateGlobalSlot (identifier is not a string)");
            }
            size_t len;
            const char* name = JS_ToCStringLen(ctx, &len, ident);
            if (name == nullptr) {
                assert(false && "Exception during CreateGlobalSlot (failed to convert identifier to C string)");
            }
            std::string nameStr(name, len);
            JS_FreeCString(ctx, name);
            JS_FreeValue(ctx, ident);

            JSValue kindVal = takeJSValue(args[1]);
            int32_t kind = 0;
            JS_ToInt32(ctx, &kind, kindVal);
            JS_FreeValue(ctx, kindVal);
            bool isConst = (kind == 2);
            res.emplace_back(Slot::makeGlobal(ctx, nameStr, isConst));
        } break;
        case Opcode::GetGlobalRef: {
            JSValue ident = takeJSValue(args[0]);
            if (!JS_IsString(ident)) {
                assert(false && "Exception during GetGlobalRef (identifier is not a string)");
            }
            size_t len;
            const char* name = JS_ToCStringLen(ctx, &len, ident);
            if (name == nullptr) {
                assert(false && "Exception during GetGlobalRef (failed to convert identifier to C string)");
            }
            std::string nameStr(name, len);
            JS_FreeCString(ctx, name);
            JS_FreeValue(ctx, ident);
            res.emplace_back(Slot::makeGlobal(ctx, nameStr));
        } break;
        case Opcode::Div: {
            int32_t exception = 0;
            JSValue lhs = takeJSValue(args[0]);
            JSValue rhs = takeJSValue(args[1]);
            JSValue result = quickjs_ops::div(ctx, lhs, rhs, &exception);
            JS_FreeValue(ctx, lhs);
            JS_FreeValue(ctx, rhs);
            if (exception) {
                JSValue exc = JS_GetException(ctx);
                res.emplace_back(Value(JS_UNDEFINED));
                res.emplace_back(Value(exc));
                res.emplace_back(RawValue{ true });
            }
            else {
                res.emplace_back(Value(result));
                res.emplace_back(Value(JS_UNDEFINED));
                res.emplace_back(RawValue{ false });
            }
        } break;
        case Opcode::Rem: {
            int32_t exception = 0;
            JSValue lhs = takeJSValue(args[0]);
            JSValue rhs = takeJSValue(args[1]);
            JSValue result = quickjs_ops::rem(ctx, lhs, rhs, &exception);
            JS_FreeValue(ctx, lhs);
            JS_FreeValue(ctx, rhs);
            if (exception) {
                JSValue exc = JS_GetException(ctx);
                res.emplace_back(Value(JS_UNDEFINED));
                res.emplace_back(Value(exc));
                res.emplace_back(RawValue{ true });
            }
            else {
                res.emplace_back(Value(result));
                res.emplace_back(Value(JS_UNDEFINED));
                res.emplace_back(RawValue{ false });
            }
        } break;
        case Opcode::Pow: {
            int32_t exception = 0;
            JSValue lhs = takeJSValue(args[0]);
            JSValue rhs = takeJSValue(args[1]);
            JSValue result = quickjs_ops::pow(ctx, lhs, rhs, &exception);
            JS_FreeValue(ctx, lhs);
            JS_FreeValue(ctx, rhs);
            if (exception) {
                JSValue exc = JS_GetException(ctx);
                res.emplace_back(Value(JS_UNDEFINED));
                res.emplace_back(Value(exc));
                res.emplace_back(RawValue{ true });
            }
            else {
                res.emplace_back(Value(result));
                res.emplace_back(Value(JS_UNDEFINED));
                res.emplace_back(RawValue{ false });
            }
        } break;
        case Opcode::LShift: case Opcode::RShift: case Opcode::URShift:
        case Opcode::BitAnd: case Opcode::BitOr: case Opcode::BitXor: {
            int32_t lhs;
            int32_t rhs;
            JSValue lhsVal = takeJSValue(args[0]);
            JSValue rhsVal = takeJSValue(args[1]);
            int conversionResult = JS_ToInt32(ctx, &lhs, lhsVal);
            if (!conversionResult) {
                conversionResult = JS_ToInt32(ctx, &rhs, rhsVal);
            }
            JS_FreeValue(ctx, lhsVal);
            JS_FreeValue(ctx, rhsVal);
            if (conversionResult) {
                JSValue exc = JS_GetException(ctx);
                res.emplace_back(Value(JS_UNDEFINED));
                res.emplace_back(Value(exc));
                res.emplace_back(RawValue{ true });
                break;
            }
            switch (op.op) {
                case Opcode::LShift:   res.emplace_back(Value(JS_NewInt32(ctx, lhs << (static_cast<uint32_t>(rhs) % 32)))); break;
                case Opcode::RShift:   res.emplace_back(Value(JS_NewInt32(ctx, lhs >> (static_cast<uint32_t>(rhs) % 32)))); break;
                case Opcode::URShift:  res.emplace_back(Value(JS_NewUint32(ctx, static_cast<uint32_t>(lhs) >> (static_cast<uint32_t>(rhs) % 32)))); break;
                case Opcode::BitAnd:   res.emplace_back(Value(JS_NewInt32(ctx, lhs & rhs))); break;
                case Opcode::BitOr:    res.emplace_back(Value(JS_NewInt32(ctx, lhs | rhs))); break;
                case Opcode::BitXor:   res.emplace_back(Value(JS_NewInt32(ctx, lhs ^ rhs))); break;
                default: assert(false); break;
            }
            res.emplace_back(Value(JS_UNDEFINED));
            res.emplace_back(RawValue{ false });
        } break;
        case Opcode::Eq: {
            int32_t exception = 0;
            JSValue lhs = takeJSValue(args[0]);
            JSValue rhs = takeJSValue(args[1]);
            bool result = quickjs_ops::equal(ctx, lhs, rhs, &exception);
            JS_FreeValue(ctx, lhs);
            JS_FreeValue(ctx, rhs);
            if (exception) {
                JSValue exc = JS_GetException(ctx);
                res.emplace_back(Value(JS_UNDEFINED));
                res.emplace_back(Value(exc));
                res.emplace_back(RawValue{ true });
            }
            else {
                res.emplace_back(Value(JS_NewBool(ctx, result)));
                res.emplace_back(Value(JS_UNDEFINED));
                res.emplace_back(RawValue{ false });
            }
        } break;
        case Opcode::Neq: {
            int32_t exception = 0;
            JSValue lhs = takeJSValue(args[0]);
            JSValue rhs = takeJSValue(args[1]);
            bool result = quickjs_ops::equal(ctx, lhs, rhs, &exception);
            JS_FreeValue(ctx, lhs);
            JS_FreeValue(ctx, rhs);
            if (exception) {
                JSValue exc = JS_GetException(ctx);
                res.emplace_back(Value(JS_UNDEFINED));
                res.emplace_back(Value(exc));
                res.emplace_back(RawValue{ true });
            }
            else {
                res.emplace_back(Value(JS_NewBool(ctx, !result)));
                res.emplace_back(Value(JS_UNDEFINED));
                res.emplace_back(RawValue{ false });
            }
        } break;
        case Opcode::BoolNot: {
            JSValue operand = takeJSValue(args[0]);
            res.emplace_back(Value(JS_NewBool(ctx, !JS_ToBool(ctx, operand))));
            JS_FreeValue(ctx, operand);
        } break;
        case Opcode::StrictEq:
        case Opcode::StrictNeq: {
            int32_t exception = 0;
            JSValue lhs = takeJSValue(args[0]);
            JSValue rhs = takeJSValue(args[1]);
            bool result = quickjs_ops::strictEqual(ctx, lhs, rhs, &exception);
            JS_FreeValue(ctx, lhs);
            JS_FreeValue(ctx, rhs);
            res.emplace_back(Value(JS_NewBool(ctx, op.op == Opcode::StrictEq ? result : !result)));
        } break;
        case Opcode::Gt:
        case Opcode::Gte:
        case Opcode::Lt:
        case Opcode::Lte: {
            int32_t exception = 0;
            JSValue lhs = takeJSValue(args[0]);
            JSValue rhs = takeJSValue(args[1]);
            bool result = false;
            switch (op.op) {
                case Opcode::Gt:  result = quickjs_ops::greater(ctx, lhs, rhs, &exception); break;
                case Opcode::Gte: result = quickjs_ops::greaterEq(ctx, lhs, rhs, &exception); break;
                case Opcode::Lt:  result = quickjs_ops::less(ctx, lhs, rhs, &exception); break;
                case Opcode::Lte: result = quickjs_ops::lessEq(ctx, lhs, rhs, &exception); break;
                default: assert(false); break;
            }
            JS_FreeValue(ctx, lhs);
            JS_FreeValue(ctx, rhs);
            if (exception) {
                res.emplace_back(Value(JS_UNDEFINED));
                res.emplace_back(Value(JS_GetException(ctx)));
                res.emplace_back(RawValue{ true });
            }
            else {
                res.emplace_back(Value(JS_NewBool(ctx, result)));
                res.emplace_back(Value(JS_UNDEFINED));
                res.emplace_back(RawValue{ false });
            }
        } break;
        case Opcode::GetMember: {
            JSValue obj = takeJSValue(args[0]);
            JSValue id = takeJSValue(args[1]);
            JSAtom atom = JS_ValueToAtom(ctx, id);
            JSValue prop = JS_GetProperty(ctx, obj, atom);
            JS_FreeValue(ctx, obj);
            JS_FreeValue(ctx, id);
            JS_FreeAtom(ctx, atom);
            if (JS_IsException(prop)) {
                JSValue exc = JS_GetException(ctx);
                res.emplace_back(Value(JS_UNDEFINED));
                res.emplace_back(Value(exc));
                res.emplace_back(RawValue{ true });
            }
            else {
                res.emplace_back(Value(prop));
                res.emplace_back(Value(JS_UNDEFINED));
                res.emplace_back(RawValue{ false });
            }
        } break;
        case Opcode::Store: {
            auto val = args[0].takeValue();
            auto slot = args[1].getSlot();
            if (slot.val->isConst() && slot.val->isInitialized()) {
                val.free(ctx);
                JS_ThrowTypeError(ctx, "assignment to constant variable");
                JSValue exc = JS_GetException(ctx);
                res.emplace_back(slot);
                res.emplace_back(Value(exc));
                res.emplace_back(RawValue{ true });
                break;
            }
            if (slot.val->isGlobal() && !std::holds_alternative<JSValue>(val.val)) {
                val = Value(val.toJSValue(ctx));
            }
            slot.store(val);
            slot.val->markInitialized();
            res.emplace_back(slot);
            res.emplace_back(Value(JS_UNDEFINED));
            res.emplace_back(RawValue{ false });
        } break;
        case Opcode::SetMember: {
            JSValue obj = takeJSValue(args[0]);
            JSValue id = takeJSValue(args[1]);
            JSValue val = takeJSValue(args[2]);
            JSAtom atom = JS_ValueToAtom(ctx, id);
            JS_FreeValue(ctx, id);
            JSValue exVal = JS_UNDEFINED;
            bool hadEx = false;
            if (JS_SetProperty(ctx, obj, atom, val) < 0) {
                exVal = JS_GetException(ctx);
                hadEx = true;
            }
            JS_FreeValue(ctx, obj);
            JS_FreeAtom(ctx, atom);
            res.emplace_back(Value(exVal));
            res.emplace_back(RawValue{ hadEx });
        } break;
        case Opcode::Call: {
            assert(args[0].isValue() && "Call target must be value or closure");
            if (std::holds_alternative<Closure>(args[0].getValue().val)) {
                Closure closure = std::get<Closure>(args[0].takeValue().val);
                std::vector<RegContent> callArgs;
                for (size_t i = 1; i < args.size(); i++) {
                    callArgs.push_back(RegContent{ args[i].takeContent() });
                }
                f.state = RunState::Suspended;
                return ExecResult{ Calls{
                    Callee{ std::move(closure) },
                    Operands{ std::move(callArgs) }
                }};
            }
            JSValue resVal = JS_UNDEFINED;
            JSValue exVal = JS_UNDEFINED;
            bool hadEx = false;
            JSValue jsFn = takeJSValue(args[0]);
            std::vector<JSValue> callArgs;
            for (size_t i = 1; i < args.size(); i++) {
                callArgs.push_back(takeJSValue(args[i]));
            }
            resVal = JS_Call(ctx, jsFn, JS_UNDEFINED, callArgs.size(), callArgs.data());
            JS_FreeValue(ctx, jsFn);
            if (JS_IsException(resVal)) {
                resVal = JS_UNDEFINED;
                exVal = JS_GetException(ctx);
                hadEx = true;
            }
            for (auto arg : callArgs) {
                JS_FreeValue(ctx, arg);
            }
            res.emplace_back(Value(resVal));
            res.emplace_back(Value(exVal));
            res.emplace_back(RawValue{ hadEx });
        } break;
        case Opcode::CallMethod: {
            JSValue obj = takeJSValue(args[0]);
            JSValue method = takeJSValue(args[1]);
            JSValue resVal = JS_UNDEFINED;
            JSValue exVal = JS_UNDEFINED;
            bool hadEx = false;
            if (JS_IsException(method)) {
                resVal = JS_UNDEFINED;
                exVal = JS_GetException(ctx);
                hadEx = true;
            }
            else {
                std::vector<JSValue> callArgs;
                for (size_t i = 2; i < args.size(); i++) {
                    callArgs.push_back(takeJSValue(args[i]));
                }
                resVal = JS_Call(ctx, method, obj, callArgs.size(), callArgs.data());
                for (auto arg : callArgs) {
                    JS_FreeValue(ctx, arg);
                }
                if (JS_IsException(resVal)) {
                    resVal = JS_UNDEFINED;
                    exVal = JS_GetException(ctx);
                    hadEx = true;
                }
            }
            JS_FreeValue(ctx, obj);
            JS_FreeValue(ctx, method);
            res.emplace_back(Value(resVal));
            res.emplace_back(Value(exVal));
            res.emplace_back(RawValue{ hadEx });
        } break;
        case Opcode::Construct: {
            JSValue ctor = takeJSValue(args[0]);
            std::vector<JSValue> callArgs;
            for (size_t i = 1; i < args.size(); i++) {
                callArgs.push_back(takeJSValue(args[i]));
            }
            JSValue resVal = JS_CallConstructor(ctx, ctor, callArgs.size(), callArgs.data());
            JSValue exVal = JS_UNDEFINED;
            bool hadEx = false;
            JS_FreeValue(ctx, ctor);
            for (auto arg : callArgs) {
                JS_FreeValue(ctx, arg);
            }
            if (JS_IsException(resVal)) {
                resVal = JS_UNDEFINED;
                exVal = JS_GetException(ctx);
                hadEx = true;
            }
            res.emplace_back(Value(resVal));
            res.emplace_back(Value(exVal));
            res.emplace_back(RawValue{ hadEx });
        } break;
        case Opcode::Await: {
            assert(f.func.isAsync && "Await executed in non-async function");
            JSValue awaitable = takeJSValue(args[0]);
            freeOperands(ctx, args);
            f.state = RunState::Suspended;
            return ExecResult{ Awaiting{ awaitable } };
        }
        case Opcode::MakeClosure: {
            auto code = args[0].getRawAs<Code>();
            std::vector<Slot> capturedVars;
            for (size_t i = 1; i < args.size(); i++) {
                capturedVars.push_back(args[i].getSlot());
            }
            res.emplace_back(Value{ Closure{ code, std::move(capturedVars) } });
        } break;
        case Opcode::ToPrimitive: {
            JSValue value = takeJSValue(args[0]);
            int32_t exception = 0;
            JSValue result = quickjs_ops::toPrimitive(ctx, value, &exception);
            JS_FreeValue(ctx, value);
            if (exception) {
                res.emplace_back(Value(JS_UNDEFINED));
                res.emplace_back(Value(JS_GetException(ctx)));
                res.emplace_back(RawValue{ true });
            }
            else {
                res.emplace_back(Value(result));
                res.emplace_back(Value(JS_UNDEFINED));
                res.emplace_back(RawValue{ false });
            }
        } break;
        case Opcode::StringConcat: {
            JSValue lhs = takeJSValue(args[0]);
            JSValue rhs = takeJSValue(args[1]);
            assert((JS_IsString(lhs) || JS_IsString(rhs)) && "StringConcat requires a string operand");
            int32_t exception = 0;
            JSValue result = quickjs_ops::add(ctx, lhs, rhs, &exception);
            JS_FreeValue(ctx, lhs);
            JS_FreeValue(ctx, rhs);
            if (exception) {
                res.emplace_back(Value(JS_UNDEFINED));
                res.emplace_back(Value(JS_GetException(ctx)));
                res.emplace_back(RawValue{ true });
            }
            else {
                res.emplace_back(Value(result));
                res.emplace_back(Value(JS_UNDEFINED));
                res.emplace_back(RawValue{ false });
            }
        } break;
        case Opcode::AddSlow:
        case Opcode::SubSlow:
        case Opcode::MulSlow: {
            JSValue lhs = takeJSValue(args[0]);
            JSValue rhs = takeJSValue(args[1]);
            int32_t exception = 0;
            JSValue result = JS_UNDEFINED;
            switch (op.op) {
                case Opcode::AddSlow:
                    result = quickjs_ops::add(ctx, lhs, rhs, &exception);
                    break;
                case Opcode::SubSlow:
                    result = quickjs_ops::sub(ctx, lhs, rhs, &exception);
                    break;
                case Opcode::MulSlow:
                    result = quickjs_ops::mul(ctx, lhs, rhs, &exception);
                    break;
                default:
                    assert(false);
                    break;
            }
            JS_FreeValue(ctx, lhs);
            JS_FreeValue(ctx, rhs);
            if (exception) {
                res.emplace_back(Value(JS_UNDEFINED));
                res.emplace_back(Value(JS_GetException(ctx)));
                res.emplace_back(RawValue{ true });
            }
            else {
                res.emplace_back(Value(result));
                res.emplace_back(Value(JS_UNDEFINED));
                res.emplace_back(RawValue{ false });
            }
        } break;
        case Opcode::GetTag: {
            Tag tag = getTag(args[0].getValue());
            res.emplace_back(args[0].takeContent());
            res.emplace_back(RawValue{ tag });
        } break;
        case Opcode::CmpEqTag: {
            Tag lhs = args[0].getTag();
            Tag rhs = args[1].getTag();
            res.emplace_back(RawValue{ lhs == rhs });
        } break;
        case Opcode::UnboxI32: {
            JSValue operand = takeJSValue(args[0]);
            assert(JS_VALUE_GET_NORM_TAG(operand) == JS_TAG_INT && "UnboxI32 requires value with runtime tag Int");
            int32_t payload = JS_VALUE_GET_INT(operand);
            JS_FreeValue(ctx, operand);
            res.emplace_back(RawValue{ payload });
        } break;
        case Opcode::UnboxF64: {
            JSValue operand = takeJSValue(args[0]);
            assert(JS_VALUE_GET_NORM_TAG(operand) == JS_TAG_FLOAT64 && "UnboxF64 requires value with runtime tag Float64");
            double payload = JS_VALUE_GET_FLOAT64(operand);
            JS_FreeValue(ctx, operand);
            res.emplace_back(RawValue{ payload });
        } break;
        case Opcode::BoxI32: {
            int32_t payload = args[0].getI32();
            res.emplace_back(Value(JS_NewInt32(ctx, payload)));
        } break;
        case Opcode::BoxF64: {
            double payload = args[0].getF64();
            res.emplace_back(Value(__JS_NewFloat64(ctx, payload)));
        } break;
        case Opcode::AddI32: {
            int32_t lhs = args[0].getI32();
            int32_t rhs = args[1].getI32();
            int64_t r64 = static_cast<int64_t>(lhs) + static_cast<int64_t>(rhs);
            int32_t result = static_cast<int32_t>(r64);
            res.emplace_back(RawValue{ result });
            res.emplace_back(RawValue{ result != r64 });
        } break;
        case Opcode::SubI32: {
            int32_t lhs = args[0].getI32();
            int32_t rhs = args[1].getI32();
            int64_t r64 = static_cast<int64_t>(lhs) - static_cast<int64_t>(rhs);
            int32_t result = static_cast<int32_t>(r64);
            res.emplace_back(RawValue{ result });
            res.emplace_back(RawValue{ result != r64 });
        } break;
        case Opcode::MulI32: {
            int32_t lhs = args[0].getI32();
            int32_t rhs = args[1].getI32();
            int64_t r64 = static_cast<int64_t>(lhs) * static_cast<int64_t>(rhs);
            int32_t result = static_cast<int32_t>(r64);
            res.emplace_back(RawValue{ result });
            res.emplace_back(RawValue{ result != r64 || (r64 == 0 && (lhs | rhs) < 0) });
        } break;
        case Opcode::AddF64: {
            double lhs = args[0].getF64();
            double rhs = args[1].getF64();
            res.emplace_back(RawValue{ lhs + rhs });
        } break;
        case Opcode::SubF64: {
            double lhs = args[0].getF64();
            double rhs = args[1].getF64();
            res.emplace_back(RawValue{ lhs - rhs });
        } break;
        case Opcode::MulF64: {
            double lhs = args[0].getF64();
            double rhs = args[1].getF64();
            res.emplace_back(RawValue{ lhs * rhs });
        } break;
        case Opcode::I32ToF64: {
            int32_t payload = args[0].getI32();
            res.emplace_back(RawValue{ static_cast<double>(payload) });
        } break;
        case Opcode::Add:
        case Opcode::Sub:
        case Opcode::Mul:
        {
            const Function* body = opcodeBody(op.op);
            assert(body);
            f.state = RunState::Suspended;
            return ExecResult{ Calls{ Callee{ body }, Operands{ std::move(args) } } };
        }
    }

    freeOperands(ctx, args);
    for (size_t i = 0; i < op.res.size(); i++) {
        f.setReg(op.res[i], std::move(res[i]));
    }
    return std::nullopt;
}


inline ExecResult execute(Frame& f) {
    assert(f.state == RunState::Runnable && "Executing a frame that is not runnable");
    while (true) {
        for (; f.pc < f.block->instructions.size(); f.pc++) {
            const auto& insn = f.block->instructions[f.pc];
            if constexpr (traceEnabled) {
                std::cout << "  Executing instruction " << f.pc << "\n";
                printState(f);
            }
            if (insn->isOperation()) {
                if (auto pause = evalOperation(f, insn->asOperation())) {
                    return std::move(*pause);
                }
            }
            else {
                evalConstInit(f, insn->asConstInit());
            }
        }
        if constexpr (traceEnabled) {
            std::cout << "  Terminator\n";
            printState(f);
        }
        const auto& terminator = f.block->terminator;
        switch (terminator.type) {
            case Terminator::Type::Jump:
                moveRegs(f, terminator.args, terminator.target->args);
                f.block = terminator.target;
                f.pc = 0;
                break;
            case Terminator::Type::Branch: {
                auto condContent = f.getReg(terminator.value);
                bool condition;
                if (condContent.isBool()) {
                    condition = condContent.getBool();
                }
                else {
                    assert(condContent.isValue() && "Branch condition must be a raw bool or a value");
                    JSValue condVal = condContent.takeValue().toJSValue(f.ctx);
                    condition = JS_ToBool(f.ctx, condVal);
                    JS_FreeValue(f.ctx, condVal);
                }
                BasicBlockPtr target = condition ? terminator.target : terminator.other;
                moveRegs(f, terminator.args, target->args);
                f.block = target;
                f.pc = 0;
            } break;
            case Terminator::Type::Exit: {
                std::vector<RegContent> out;
                out.reserve(terminator.args.size());
                for (const auto& arg : terminator.args) {
                    out.push_back(f.getReg(arg));
                }
                f.state = RunState::Finished;
                return ExecResult{ Exited{ Operands{ std::move(out) } } };
            }
            case Terminator::Type::None:
                assert(false && "Invalid terminator");
                break;
        }
    }
}


inline void deliverResults(Frame& f, std::vector<RegContent>& values) {
    assert(f.state == RunState::Suspended && "Delivering results to a running frame");
    const auto& op = f.block->instructions[f.pc]->asOperation();
    assert(values.size() == op.res.size() && "Result count must match the paused operation");
    for (size_t i = 0; i < values.size(); i++) {
        assert(!op.res[i].void_() && "Delivered result cannot target a void register");
        f.setReg(op.res[i], std::move(values[i]));
    }
    values.clear();
    f.pc++;
    f.state = RunState::Runnable;
}


inline void deliverAwait(Frame& f, JSValue value, bool rejected) {
    std::vector<RegContent> values;
    values.emplace_back(Value(rejected ? JS_UNDEFINED : value));
    values.emplace_back(Value(rejected ? value : JS_UNDEFINED));
    values.emplace_back(RawValue{ rejected });
    deliverResults(f, values);
}


inline JSValue startAsync(JSContext* ctx, const Function& func, std::shared_ptr<Function> root, std::vector<Value> argValues, std::span<const Slot> capturedVars);


struct Interp {
    JSContext* ctx;
    std::list<Frame> stack;

    explicit Interp(JSContext* ctx_) : ctx(ctx_) {}

    Frame& top() { return stack.back(); }

    void pushClosureFrame(const Function& func, std::shared_ptr<Function> root, std::vector<Value> argValues, std::span<const Slot> capturedVars) {
        assert(capturedVars.size() == func.closureCount);
        assert(func.entry->args.size() == func.argCount + func.closureCount);
        stack.emplace_back(ctx, std::move(root), func);
        Frame& f = stack.back();
        for (size_t i = 0; i < func.argCount; i++) {
            Value arg = i < argValues.size() ? std::move(argValues[i]) : Value(JS_UNDEFINED);
            f.setReg(func.entry->args[i], RegContent{ Slot::make(ctx, std::move(arg)) });
        }
        for (size_t i = func.argCount; i < argValues.size(); i++) {
            argValues[i].free(ctx);
        }
        for (size_t i = 0; i < capturedVars.size(); i++) {
            f.setReg(func.entry->args[func.argCount + i], RegContent{ capturedVars[i] });
        }
    }

    void pushBodyFrame(const Function& body, std::vector<RegContent> operands) {
        assert(body.entry && "Body must have an entry block");
        assert(operands.size() == body.entry->args.size() && "Body operand count must match its entry arity");
        stack.emplace_back(ctx, nullptr, body);
        Frame& f = stack.back();
        for (size_t i = 0; i < operands.size(); i++) {
            f.setReg(body.entry->args[i], std::move(operands[i]));
        }
    }

    void pushCallee(Calls& call) {
        if (const Function* const* body = std::get_if<const Function*>(&call.callee)) {
            pushBodyFrame(**body, call.args.take());
            return;
        }

        const Closure& closure = std::get<Closure>(call.callee);
        std::vector<RegContent> operands = call.args.take();
        std::vector<Value> argValues;
        argValues.reserve(operands.size());
        for (auto& operand : operands) {
            argValues.push_back(operand.takeValue());
        }

        const Function& func = *closure.code.code;
        if (!func.isAsync) {
            pushClosureFrame(func, closure.code.root, std::move(argValues), closure.capturedVars);
            return;
        }

        JSValue promise = startAsync(ctx, func, closure.code.root, std::move(argValues), closure.capturedVars);
        std::vector<RegContent> results;
        if (JS_IsException(promise)) {
            results.emplace_back(Value(JS_UNDEFINED));
            results.emplace_back(Value(JS_GetException(ctx)));
            results.emplace_back(RawValue{ true });
        }
        else {
            results.emplace_back(Value(promise));
            results.emplace_back(Value(JS_UNDEFINED));
            results.emplace_back(RawValue{ false });
        }
        deliverResults(stack.back(), results);
    }

    std::variant<Exited, Awaiting> run() {
        while (true) {
            ExecResult result = execute(stack.back());
            if (auto* exited = std::get_if<Exited>(&result)) {
                if (stack.size() == 1) {
                    return std::move(*exited);
                }
                std::vector<RegContent> out = exited->args.take();
                stack.pop_back();
                deliverResults(stack.back(), out);
            }
            else if (auto* awaiting = std::get_if<Awaiting>(&result)) {
                assert(stack.size() == 1 && "Await suspended a nested frame");
                return std::move(*awaiting);
            }
            else {
                pushCallee(std::get<Calls>(result));
            }
        }
    }
};


struct AwaitResult {
    JSValue value;
    bool rejected;
};


struct AsyncTask : std::enable_shared_from_this<AsyncTask> {
    Interp interp;
    JSValue resolve = JS_UNDEFINED;
    JSValue reject = JS_UNDEFINED;

    explicit AsyncTask(JSContext* ctx) : interp(ctx) {}

    ~AsyncTask() {
        JS_FreeValue(interp.ctx, resolve);
        JS_FreeValue(interp.ctx, reject);
    }

    void step(std::optional<AwaitResult> settled = std::nullopt) {
        while (true) {
            if (settled) {
                deliverAwait(interp.top(), settled->value, settled->rejected);
                settled.reset();
            }
            auto result = interp.run();
            if (auto* exited = std::get_if<Exited>(&result)) {
                settle(exited->args.take());
                return;
            }
            JSValue awaitable = std::get<Awaiting>(result).take();
            if (attachAwaitHandlers(awaitable)) {
                return;
            }
            settled = AwaitResult{ JS_GetException(interp.ctx), true };
        }
    }

    void settle(std::vector<RegContent> exitArgs) {
        JSContext* ctx = interp.ctx;
        assert(exitArgs.size() == 3 && "JS function Exit must carry (res, ex, hadEx)");
        bool hadException = exitArgs[2].getBool();
        JSValue value = exitArgs[hadException ? 1 : 0].takeValue().toJSValue(ctx);
        freeRegContent(ctx, exitArgs[hadException ? 0 : 1]);

        JSValue callback = JS_DupValue(ctx, hadException ? reject : resolve);
        JSValue result = JS_Call(ctx, callback, JS_UNDEFINED, 1, &value);
        JS_FreeValue(ctx, value);
        JS_FreeValue(ctx, callback);
        if (JS_IsException(result)) {
            JS_FreeValue(ctx, JS_GetException(ctx));
        }
        JS_FreeValue(ctx, result);
    }

    JSValue normalizeAwaitable(JSValue awaitable) {
        JSContext* ctx = interp.ctx;
        JSValue global = JS_GetGlobalObject(ctx);
        JSValue promiseCtor = JS_GetPropertyStr(ctx, global, "Promise");
        JS_FreeValue(ctx, global);
        JSValue resolveFn = JS_GetPropertyStr(ctx, promiseCtor, "resolve");
        JSValue normalized = JS_Call(ctx, resolveFn, promiseCtor, 1, &awaitable);
        JS_FreeValue(ctx, resolveFn);
        JS_FreeValue(ctx, promiseCtor);
        JS_FreeValue(ctx, awaitable);
        return normalized;
    }

    bool attachAwaitHandlers(JSValue awaitable) {
        JSContext* ctx = interp.ctx;
        JSValue normalized = normalizeAwaitable(awaitable);
        if (JS_IsException(normalized)) {
            return false;
        }
        FunctionFactory ff{ ContextRef(ctx) };
        auto self = shared_from_this();
        auto onFulfilled = ff.newFunction([self, ctx](ValueWeak settled) {
            self->step(AwaitResult{ JS_DupValue(ctx, settled.getVal()), false });
        });
        auto onRejected = ff.newFunction([self, ctx](ValueWeak rejected) {
            self->step(AwaitResult{ JS_DupValue(ctx, rejected.getVal()), true });
        });
        JSValue handlers[] = {
            JS_DupValue(ctx, onFulfilled.getVal()),
            JS_DupValue(ctx, onRejected.getVal())
        };
        JSValue thenFn = JS_GetPropertyStr(ctx, normalized, "then");
        JSValue result = JS_Call(ctx, thenFn, normalized, 2, handlers);
        JS_FreeValue(ctx, handlers[0]);
        JS_FreeValue(ctx, handlers[1]);
        JS_FreeValue(ctx, thenFn);
        JS_FreeValue(ctx, normalized);
        if (JS_IsException(result)) {
            JS_FreeValue(ctx, result);
            return false;
        }
        JS_FreeValue(ctx, result);
        return true;
    }
};


inline JSValue startAsync(JSContext* ctx, const Function& func, std::shared_ptr<Function> root,
                          std::vector<Value> argValues, std::span<const Slot> capturedVars) {
    auto [promise, resolve, reject] = jac::Promise::create(ContextRef(ctx));
    auto task = std::make_shared<AsyncTask>(ctx);
    task->interp.pushClosureFrame(func, std::move(root), std::move(argValues), capturedVars);
    task->resolve = resolve.loot().second;
    task->reject = reject.loot().second;
    task->step();
    return promise.loot().second;
}


inline std::vector<Value> dupArgs(JSContext* ctx, int argc, JSValueConst* argv) {
    std::vector<Value> values;
    for (int i = 0; i < argc; i++) {
        values.push_back(Value(JS_DupValue(ctx, argv[i])));
    }
    return values;
}


inline JSValue finishJs(JSContext* ctx, std::vector<RegContent> exitArgs) {
    assert(exitArgs.size() == 3 && "JS function Exit must carry (res, ex, hadEx)");
    bool hadException = exitArgs[2].getBool();
    JSValue value = exitArgs[hadException ? 1 : 0].takeValue().toJSValue(ctx);
    freeRegContent(ctx, exitArgs[hadException ? 0 : 1]);
    if (hadException) {
        JS_Throw(ctx, value);
        return JS_EXCEPTION;
    }
    return value;
}


inline JSValue runSync(JSContext* ctx, const Function& func, std::shared_ptr<Function> root,
                       JSValueConst thisVal, int argc, JSValueConst* argv, std::span<const Slot> capturedVars = {}) {
    Interp interp(ctx);
    interp.pushClosureFrame(func, std::move(root), dupArgs(ctx, argc, argv), capturedVars);
    auto result = interp.run();
    assert(std::holds_alternative<Exited>(result) && "Synchronous function suspended on await");
    return finishJs(ctx, std::get<Exited>(result).args.take());
}


inline JSValue runAsync(JSContext* ctx, const Function& func, std::shared_ptr<Function> root,
                        JSValueConst thisVal, int argc, JSValueConst* argv, std::span<const Slot> capturedVars = {}) {
    return startAsync(ctx, func, std::move(root), dupArgs(ctx, argc, argv), capturedVars);
}


struct ClosureWrapper {
    Closure c;

    jac::Value operator()(ContextRef ctx, ValueWeak thisVal, std::vector<ValueWeak> args) {
        std::vector<JSValue> jsArgs;
        jsArgs.reserve(args.size());
        for (ValueWeak& arg : args) {
            jsArgs.push_back(arg.getVal());
        }

        if (!c.code.code->isAsync) {
            JSValue res = runSync(ctx, *c.code.code, c.code.root, thisVal.getVal(), jsArgs.size(), jsArgs.data(), c.capturedVars);
            return jac::Value(ctx, res);
        }

        JSValue res = runAsync(ctx, *c.code.code, c.code.root, thisVal.getVal(), jsArgs.size(), jsArgs.data(), c.capturedVars);
        return jac::Value(ctx, res);
    }

    static JSValue mkJs(ContextRef ctx, Closure c) {
        ClosureWrapper w(std::move(c));
        jac::FunctionFactory ff(ctx);
        jac::Value v = ff.newFunctionThisVariadic(std::move(w));
        return v.loot().second;
    }
};


inline JSValue Value::toJSValue(JSContext* ctx) const {
    if (std::holds_alternative<JSValue>(val)) {
        return std::get<JSValue>(val);
    }
    return ClosureWrapper::mkJs(ctx, std::get<Closure>(val));
}


}  // namespace jac::cfg::tless::interp
