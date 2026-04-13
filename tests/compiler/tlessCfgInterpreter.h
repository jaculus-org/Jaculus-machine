#pragma once

#include "jac/machine/context.h"
#include <cassert>
#include <cmath>
#include <cstdint>
#include <cstdlib>
#include <map>
#include <memory>
#include <span>
#include <utility>

#include <quickjs.h>

#include <jac/machine/compiler/quickjsOps.h>
#include <jac/machine/compiler/tlessCfg.h>
#include <jac/machine/functionFactory.h>
#include <jac/util.h>
#include <variant>


namespace jac::cfg::tless::interp {


struct Slot;


struct Code {
    cfg::tless::Function* code;
};


struct Closure {
    Code code;
    std::vector<Slot> capturedVars;
};


struct ClosureWrapper {
    Closure c;
    jac::Value operator()(ContextRef ctx, ValueWeak thisVal, std::vector<ValueWeak> args);

    static JSValue mkJs(ContextRef ctx, Closure c) {
        ClosureWrapper w(std::move(c));

        jac::FunctionFactory ff(ctx);
        jac::Value v = ff.newFunctionThisVariadic(std::move(w));
        return v.loot().second;
    }
};


struct Value {
    std::variant<JSValue, Closure, Code> val;

    explicit Value(auto val_) : val(val_) {}

    void free(JSContext* ctx) {
        if (std::holds_alternative<JSValue>(val)) {
            JS_FreeValue(ctx, std::get<JSValue>(val));
        }
    }

    Value dup(JSContext* ctx) const {
        if (std::holds_alternative<JSValue>(val)) {
            return Value(JS_DupValue(ctx, std::get<JSValue>(val)));
        }
        else {
            return *this;
        }
    }

    JSValue toJSValue(JSContext* ctx) const {
        if (std::holds_alternative<Closure>(val)) {
            return ClosureWrapper::mkJs(ctx, std::get<Closure>(val));
        }
        if (std::holds_alternative<JSValue>(val)) {
            return std::get<JSValue>(val);
        }
        throw std::runtime_error("Value cannot be converted to JS");
    }
};


struct SlotCell {
    virtual ~SlotCell() = default;
    virtual Value load() = 0;
    virtual void store(Value newVal) = 0;
};


struct LocalSlotCell : public SlotCell {
    JSContext* ctx;
    Value val;

    LocalSlotCell(JSContext* ctx_, Value val_) : ctx(ctx_), val(val_) {}
    ~LocalSlotCell() override { val.free(ctx); }

    Value load() override {
        val.dup(ctx);
        return val;
    }

    void store(Value newVal) override {
        val.free(ctx);
        val = newVal;
    }

    LocalSlotCell(const LocalSlotCell&) = delete;
    LocalSlotCell& operator=(const LocalSlotCell&) = delete;
    LocalSlotCell(LocalSlotCell&& other) = delete;
    LocalSlotCell& operator=(LocalSlotCell&& other) = delete;
};


struct GlobalSlotCell : public SlotCell {
    JSContext* ctx;
    std::string name;

    GlobalSlotCell(JSContext* ctx_, std::string name_) : ctx(ctx_), name(std::move(name_)) {}

    Value load() override {
        JSValue globalObj = JS_GetGlobalObject(ctx);
        JSValue prop = JS_GetPropertyStr(ctx, globalObj, name.c_str());
        JS_FreeValue(ctx, globalObj);
        if (JS_IsException(prop)) {
            throw std::runtime_error("Exception during global variable load: " + name);
        }
        return Value(prop);
    }

    void store(Value newVal) override {
        JSValue globalObj = JS_GetGlobalObject(ctx);
        if (!std::holds_alternative<JSValue>(newVal.val)) {
            JS_FreeValue(ctx, globalObj);
            throw std::runtime_error("Global variables can only store JS values");
        }

        JSValue value = std::get<JSValue>(newVal.val);
        if (JS_SetPropertyStr(ctx, globalObj, name.c_str(), value) < 0) {
            JS_FreeValue(ctx, value);
            JS_FreeValue(ctx, globalObj);
            throw std::runtime_error("Exception during global variable store: " + name);
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
        val->store(newVal);
    }

    static Slot make(JSContext* ctx, Value val) {
        return Slot{ std::make_shared<LocalSlotCell>(ctx, val) };
    }

    static Slot makeGlobal(JSContext* ctx, std::string name) {
        return Slot{ std::make_shared<GlobalSlotCell>(ctx, std::move(name)) };
    }
};


struct Used {};

struct RegContent {
    std::variant<Used, Value, Slot> content;

    Slot getSlot() const {
        assert(std::holds_alternative<Slot>(content) && "Expected slot");
        return std::get<Slot>(content);
    }

    Value getValue() const {
        assert(std::holds_alternative<Value>(content) && "Expected value");
        return std::get<Value>(content);
    }

    bool isSlot() const {
        return std::holds_alternative<Slot>(content);
    }

    bool isValue() const {
        return std::holds_alternative<Value>(content);
    }
};


struct Interpreter {
    JSContext* ctx;
    std::map<int, RegContent> regs;
    const Function& func;
    int currentArgc = 0;
    JSValueConst* currentArgv = nullptr;
    std::span<Slot> currentClosureArgs;
    std::map<int, Slot> argSlots;

    void freeRegContent(const RegContent& content) {
        if (content.isValue()) {
            content.getValue().free(ctx);
        }
    }

    void printState() const {
        std::cout << "  Register state:\n";

        auto valStr = [&](const Value& value) -> std::string {
            if (std::holds_alternative<JSValue>(value.val)) {
                auto str = JS_ToCString(ctx, std::get<JSValue>(value.val));
                std::string strCopy(str);
                JS_FreeCString(ctx, str);
                return strCopy;
            }
            else if (std::holds_alternative<Closure>(value.val)) {
                return "<closure>";
            }
            else if (std::holds_alternative<Code>(value.val)) {
                return "<code>";
            }
            else {
                return "<unknown>";
            }
        };

        for (const auto& [ id, content ] : regs) {
            std::cout << "    _" << id << ": " << std::flush;
            if (std::holds_alternative<Used>(content.content)) {
                std::cout << "Used" << std::endl;
            }
            else if (content.isValue()) {
                std::cout << "Value(" << std::flush << valStr(content.getValue()) << ")" << std::endl;
            }
            else if (content.isSlot()) {
                auto val = content.getSlot().load();
                auto str = valStr(val);
                val.free(ctx);
                std::cout << "Slot(" << content.getSlot().val.get() << ", " << str << ")" << std::endl;
            }
        }
    }

    RegContent getReg(int id) {
        assert(id != 0 && "Use of register 0 (invalid)");
        auto it = regs.find(id);
        assert(it != regs.end() && "Reading uninitialized register");

        RegContent old = { Used{} };
        std::swap(old, it->second);

        assert(!std::holds_alternative<Used>(old.content) && "Double use of register");
        return old;
    }

    void setReg(int id, RegContent cont) {
        assert(id != 0 && "Use of register 0 (invalid)");
        assert((!regs.contains(id) || std::holds_alternative<Used>(regs.at(id).content)) && "Double assignment of register");
        regs[id] = std::move(cont);
    }

    RegContent getPoolConst(int index) {
        assert(index >= 0 && static_cast<size_t>(index) < func.constPool.size());
        return std::visit([&](const auto& value) -> RegContent {
            if constexpr (std::is_same_v<std::decay_t<decltype(value)>, std::unique_ptr<Function>>) {
                return RegContent{ Value{ Code{ value.get() } } };
            }
            assert(false && "Invalid constant type in pool");
            return RegContent{ Used{} };
        }, func.constPool[index].value);

    }

    void evalConstInit(const ConstInit& init) {
        JSValue jsVal;
        if (std::holds_alternative<int32_t>(init.value)) {
            jsVal = JS_NewInt32(ctx, std::get<int32_t>(init.value));
        }
        else if (std::holds_alternative<double>(init.value)) {
            jsVal = JS_NewFloat64(ctx, std::get<double>(init.value));
        }
        else if (std::holds_alternative<bool>(init.value)) {
            jsVal = JS_NewBool(ctx, std::get<bool>(init.value));
        }
        else if (std::holds_alternative<std::string>(init.value)) {
            jsVal = JS_NewString(ctx, std::get<std::string>(init.value).c_str());
        }
        else if (std::holds_alternative<PoolConst>(init.value)) {
            setReg(init.reg.id(), { getPoolConst(std::get<PoolConst>(init.value).id) });
            return;
        }
        else {
            assert(false && "Invalid constant type");
            return;
        }
        setReg(init.reg.id(), { Value(jsVal) });
    }

    void moveRegs(const std::vector<Reg>& from, const std::vector<Reg>& to) {
        assert(from.size() == to.size());
        for (size_t i = 0; i < from.size(); i++) {
            setReg(to[i].id(), getReg(from[i].id()));
        }
    }

    void evalOperation(const Operation& op) {
        std::vector<RegContent> args;
        for (const auto& arg : op.args) {
            args.push_back(getReg(arg.id()));
        }

        std::vector<RegContent> res;
        switch (op.op) {
            case Opcode::CreateSlot:
                res.emplace_back(Slot::make(ctx, Value(JS_UNINITIALIZED)));
                break;
            case Opcode::CreateUndefined:
                res.emplace_back(Value(JS_UNDEFINED));
                break;
            case Opcode::Copy:
                assert(false && "Opcode::Copy not implemented");
                break;
            case Opcode::BoolNot: {
                    res.emplace_back(Value(JS_NewBool(ctx, !JS_ToBool(ctx, args[0].getValue().toJSValue(ctx)))));
            } break;
            case Opcode::BitNot: {
                int32_t operand;
                if (JS_ToInt32(ctx, &operand, args[0].getValue().toJSValue(ctx))) {
                    throw std::runtime_error("Exception during bitwise NOT (failed to convert operand to int32)");
                }
                res.emplace_back(Value(JS_NewInt32(ctx, ~operand)));
            } break;
            case Opcode::UnPlus: {
                int32_t exception = 0;
                res.emplace_back(Value(quickjs_ops::toNumber(ctx, args[0].getValue().toJSValue(ctx), &exception)));
                if (exception) { throw std::runtime_error("Exception during unary plus"); }
            } break;
            case Opcode::UnMinus: {
                int32_t exception = 0;
                JSValue num = quickjs_ops::toNumber(ctx, args[0].getValue().toJSValue(ctx), &exception);
                if (exception) { throw std::runtime_error("Exception during unary minus (failed to convert operand to number)"); }
                switch (JS_VALUE_GET_TAG(num)) {
                    case JS_TAG_INT: res.emplace_back(Value(JS_NewInt32(ctx, -JS_VALUE_GET_INT(num)))); break;
                    case JS_TAG_FLOAT64: res.emplace_back(Value(JS_NewFloat64(ctx, -JS_VALUE_GET_FLOAT64(num)))); break;
                    default: assert(false && "Unexpected type after conversion to number"); break;
                }
            } break;
            case Opcode::Load: {
                auto slot = args[0].getSlot();
                res.emplace_back(slot.load());
                res.emplace_back(std::move(slot));
            } break;
            case Opcode::Dup:
                if (args[0].isSlot()) {
                    auto slot = args[0].getSlot();
                    res.emplace_back(slot);
                    res.emplace_back(slot);
                }
                else {
                    res.emplace_back(args[0].getValue());
                    res.emplace_back(args[0].getValue().dup(ctx));
                }
                break;
            case Opcode::Kill:
                freeRegContent(args[0]);
                break;
            case Opcode::GetArgRef: {
                int32_t argIndex;
                if (JS_ToInt32(ctx, &argIndex, args[0].getValue().toJSValue(ctx))) {
                    throw std::runtime_error("Exception during GetArgRef (failed to convert index to int32)");
                }
                if (argIndex < 0 || argIndex >= currentArgc) {
                    throw std::runtime_error("GetArgRef argument index out of bounds");
                }
                auto it = argSlots.find(argIndex);
                if (it == argSlots.end()) {
                    it = argSlots.emplace(argIndex, Slot::make(ctx, Value(JS_DupValue(ctx, currentArgv[argIndex])))).first;
                }
                res.emplace_back(it->second);
            } break;
            case Opcode::GetClosureRef: {
                int32_t closureIndex;
                if (JS_ToInt32(ctx, &closureIndex, args[0].getValue().toJSValue(ctx))) {
                    throw std::runtime_error("Exception during GetClosureRef (failed to convert index to int32)");
                }
                if (closureIndex < 0 || static_cast<size_t>(closureIndex) >= currentClosureArgs.size()) {
                    throw std::runtime_error("GetClosureRef closure index out of bounds");
                }
                res.emplace_back(currentClosureArgs[closureIndex]);
            } break;
            case Opcode::CreateGlobalSlot:
            case Opcode::GetGlobalRef: {
                auto ident = args[0].getValue().toJSValue(ctx);
                if (!JS_IsString(ident)) {
                    throw std::runtime_error("Exception during CreateGlobalSlot (identifier is not a string)");
                }
                size_t len;
                const char* name = JS_ToCStringLen(ctx, &len, ident);
                if (name == nullptr) {
                    throw std::runtime_error("Exception during CreateGlobalSlot (failed to convert identifier to C string)");
                }
                std::string nameStr(name, len);
                JS_FreeCString(ctx, name);
                JS_FreeValue(ctx, ident);
                res.emplace_back(Slot::makeGlobal(ctx, nameStr));
            } break;
            case Opcode::Add: {
                int32_t exception = 0;
                res.emplace_back(Value(quickjs_ops::add(ctx, args[0].getValue().toJSValue(ctx), args[1].getValue().toJSValue(ctx), &exception)));
                if (exception) { throw std::runtime_error("Exception during addition"); }
            } break;
            case Opcode::Sub: {
                int32_t exception = 0;
                res.emplace_back(Value(quickjs_ops::sub(ctx, args[0].getValue().toJSValue(ctx), args[1].getValue().toJSValue(ctx), &exception)));
                if (exception) { throw std::runtime_error("Exception during subtraction"); }
            } break;
            case Opcode::Mul: {
                int32_t exception = 0;
                res.emplace_back(Value(quickjs_ops::mul(ctx, args[0].getValue().toJSValue(ctx), args[1].getValue().toJSValue(ctx), &exception)));
                if (exception) { throw std::runtime_error("Exception during multiplication"); }
            } break;
            case Opcode::Div: {
                int32_t exception = 0;
                res.emplace_back(Value(quickjs_ops::div(ctx, args[0].getValue().toJSValue(ctx), args[1].getValue().toJSValue(ctx), &exception)));
                if (exception) { throw std::runtime_error("Exception during division"); }
            } break;
            case Opcode::Rem: {
                int32_t exception = 0;
                res.emplace_back(Value(quickjs_ops::rem(ctx, args[0].getValue().toJSValue(ctx), args[1].getValue().toJSValue(ctx), &exception)));
                if (exception) { throw std::runtime_error("Exception during remainder"); }
            } break;
            case Opcode::Pow: {
                int32_t exception = 0;
                res.emplace_back(Value(quickjs_ops::pow(ctx, args[0].getValue().toJSValue(ctx), args[1].getValue().toJSValue(ctx), &exception)));
                if (exception) { throw std::runtime_error("Exception during exponentiation"); }
            } break;
            case Opcode::LShift: case Opcode::RShift: case Opcode::URShift:
            case Opcode::BitAnd: case Opcode::BitOr: case Opcode::BitXor: {
                int32_t lhs;
                int32_t rhs;
                if (JS_ToInt32(ctx, &lhs, args[0].getValue().toJSValue(ctx)) || JS_ToInt32(ctx, &rhs, args[1].getValue().toJSValue(ctx))) {
                    throw std::runtime_error("Exception during bitwise operation (failed to convert operand to int32)");
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
            } break;
            case Opcode::Eq: {
                int32_t exception = 0;
                res.emplace_back(Value(JS_NewBool(ctx, quickjs_ops::equal(ctx, args[0].getValue().toJSValue(ctx), args[1].getValue().toJSValue(ctx), &exception))));
                if (exception) { throw std::runtime_error("Exception during equality comparison"); }
            } break;
            case Opcode::Neq: {
                int32_t exception = 0;
                res.emplace_back(Value(JS_NewBool(ctx, !quickjs_ops::equal(ctx, args[0].getValue().toJSValue(ctx), args[1].getValue().toJSValue(ctx), &exception))));
                if (exception) { throw std::runtime_error("Exception during inequality comparison"); }
            } break;
            case Opcode::Gt: {
                int32_t exception = 0;
                res.emplace_back(Value(JS_NewBool(ctx, quickjs_ops::greater(ctx, args[0].getValue().toJSValue(ctx), args[1].getValue().toJSValue(ctx), &exception))));
                if (exception) { throw std::runtime_error("Exception during greater-than comparison"); }
            } break;
            case Opcode::Gte: {
                int32_t exception = 0;
                res.emplace_back(Value(JS_NewBool(ctx, quickjs_ops::greaterEq(ctx, args[0].getValue().toJSValue(ctx), args[1].getValue().toJSValue(ctx), &exception))));
                if (exception) { throw std::runtime_error("Exception during greater-than-or-equal comparison"); }
            } break;
            case Opcode::Lt: {
                int32_t exception = 0;
                res.emplace_back(Value(JS_NewBool(ctx, quickjs_ops::less(ctx, args[0].getValue().toJSValue(ctx), args[1].getValue().toJSValue(ctx), &exception))));
                if (exception) { throw std::runtime_error("Exception during less-than comparison"); }
            } break;
            case Opcode::Lte: {
                int32_t exception = 0;
                res.emplace_back(Value(JS_NewBool(ctx, quickjs_ops::lessEq(ctx, args[0].getValue().toJSValue(ctx), args[1].getValue().toJSValue(ctx), &exception))));
                if (exception) { throw std::runtime_error("Exception during less-than-or-equal comparison"); }
            } break;
            case Opcode::GetMember: {
                JSValue obj = args[0].getValue().toJSValue(ctx);
                JSValue id = args[1].getValue().toJSValue(ctx);
                JSAtom atom = JS_ValueToAtom(ctx, id);
                JSValue prop = JS_GetProperty(ctx, obj, atom);
                JS_FreeValue(ctx, obj);
                JS_FreeValue(ctx, id);
                JS_FreeAtom(ctx, atom);
                if (JS_IsException(prop)) {
                    throw std::runtime_error("Exception during GetMember");
                }
                res.emplace_back(Value(prop));
            } break;
            case Opcode::Store: {
                auto val = args[0].getValue();
                auto slot = args[1].getSlot();
                slot.store(val);
                res.emplace_back(slot);
            } break;
            case Opcode::SetMember: {
                JSValue obj = args[0].getValue().toJSValue(ctx);
                JSValue id = args[1].getValue().toJSValue(ctx);
                JSValue val = args[2].getValue().toJSValue(ctx);
                JSAtom atom = JS_ValueToAtom(ctx, id);
                JS_FreeValue(ctx, id);
                if (JS_SetProperty(ctx, obj, atom, val) < 0) {
                    throw std::runtime_error("Exception during SetMember");
                }
                JS_FreeValue(ctx, obj);
                JS_FreeAtom(ctx, atom);
            } break;
            case Opcode::Call: {
                auto fn = args[0].getValue();
                JSValue thisVal = args[1].getValue().toJSValue(ctx);
                std::vector<JSValue> callArgs;
                for (size_t i = 2; i < args.size(); i++) {
                    callArgs.push_back(args[i].getValue().toJSValue(ctx));
                }
                if (std::holds_alternative<JSValue>(fn.val)) {
                    JSValue jsFn = fn.toJSValue(ctx);
                    JSValue resVal = JS_Call(ctx, jsFn, thisVal, callArgs.size(), callArgs.data());
                    JS_FreeValue(ctx, jsFn);
                    JS_FreeValue(ctx, thisVal);
                    for (auto arg : callArgs) {
                        JS_FreeValue(ctx, arg);
                    }
                    if (JS_IsException(resVal)) {
                        throw std::runtime_error("Exception during function call");
                    }
                    res.emplace_back(Value(resVal));
                }
                else if (std::holds_alternative<Closure>(fn.val)) {
                    auto closure = std::get<Closure>(fn.val);
                    cfg::tless::interp::Interpreter interp(ctx, *closure.code.code);
                    JSValue resVal = interp.run(thisVal, callArgs.size(), callArgs.data(), closure.capturedVars);
                    JS_FreeValue(ctx, thisVal);
                    for (auto arg : callArgs) {
                        JS_FreeValue(ctx, arg);
                    }
                    if (JS_IsException(resVal)) {
                        throw std::runtime_error("Exception during closure call");
                    }
                    res.emplace_back(Value(resVal));
                }
                else {
                    assert(false && "Call target must be value or closure");
                }
            } break;
            case Opcode::Construct: {
                JSValue ctor = args[0].getValue().toJSValue(ctx);
                std::vector<JSValue> callArgs;
                for (size_t i = 1; i < args.size(); i++) {
                    callArgs.push_back(args[i].getValue().toJSValue(ctx));
                }
                JSValue resVal = JS_CallConstructor(ctx, ctor, callArgs.size(), callArgs.data());
                JS_FreeValue(ctx, ctor);
                for (auto arg : callArgs) {
                    JS_FreeValue(ctx, arg);
                }
                if (JS_IsException(resVal)) {
                    throw std::runtime_error("Exception during constructor call");
                }
                res.emplace_back(Value(resVal));
            } break;
            case Opcode::MakeClosure: {
                auto code = std::get<Code>(args[0].getValue().val).code;
                std::vector<Slot> capturedVars;
                for (size_t i = 1; i < args.size(); i++) {
                    capturedVars.push_back(args[i].getSlot());
                }
                std::variant<JSValue, Closure, Code> closureValue{ std::in_place_type<Closure>, Closure{ code, std::move(capturedVars) } };  // NOLINT
                res.emplace_back(Value{ std::move(closureValue) });
            } break;
            }

        for (size_t i = 0; i < op.res.size(); i++) {
            setReg(op.res[i].id(), std::move(res[i]));
        }
    }

    JSValue run(JSValueConst thisVal, int argc, JSValueConst* argv, std::span<Slot> closureArgs = {}) {
        Defer _{
            [&] {
                for (const auto& [ id, content ] : regs) {
                    assert(std::holds_alternative<Used>(content.content));
                    freeRegContent(content);
                }
                regs.clear();
                argSlots.clear();
            }
        };
        currentArgc = argc;
        (void)thisVal;
        currentArgv = argv;
        currentClosureArgs = closureArgs;

        BasicBlockPtr currentBlock = func.entry;
        while (true) {
            int i = 0;
            for (const auto& insn : currentBlock->instructions) {
                std::cout << "  Executing instruction " << i++ << "\n";
                printState();
                if (insn->isOperation()) {
                    evalOperation(insn->asOperation());
                }
                else {
                    evalConstInit(insn->asConstInit());
                }
            }

            std::cout << "  Terminator" << "\n";
            printState();

            switch (currentBlock->terminator.type) {
                case Terminator::Type::Jump:
                    moveRegs(currentBlock->terminator.args, currentBlock->terminator.target->args);
                    currentBlock = currentBlock->terminator.target;
                    break;
                case Terminator::Type::Branch: {
                    auto cond = getReg(currentBlock->terminator.value.id()).getValue();
                    if (JS_ToBool(ctx, cond.toJSValue(ctx))) {
                        moveRegs(currentBlock->terminator.args, currentBlock->terminator.target->args);
                        currentBlock = currentBlock->terminator.target;
                    }
                    else {
                        moveRegs(currentBlock->terminator.args, currentBlock->terminator.other->args);
                        currentBlock = currentBlock->terminator.other;
                    }
                } break;
                case Terminator::Type::Return:
                    if (currentBlock->terminator.value.id() == 0) {
                        return JS_UNDEFINED;
                    }
                    else {
                        return getReg(currentBlock->terminator.value.id()).getValue().toJSValue(ctx);
                    }
                    break;
                case Terminator::Type::Throw: {
                    auto exception = getReg(currentBlock->terminator.value.id()).getValue();
                    JS_Throw(ctx, exception.toJSValue(ctx));
                    return JS_EXCEPTION;
                } break;
                case Terminator::Type::None: {
                    assert(false && "Invalid terminator");
                } break;
            }
        }

        return JS_UNDEFINED;
    }

    Interpreter(JSContext* ctx_, const Function& func_) : ctx(ctx_), func(func_) {}
};


inline jac::Value ClosureWrapper::operator()(ContextRef ctx, ValueWeak thisVal, std::vector<ValueWeak> args) {
    std::vector<JSValue> jsArgs;
    jsArgs.reserve(args.size());
    for (ValueWeak& arg : args) {
        jsArgs.push_back(arg.getVal());
    }

    cfg::tless::interp::Interpreter interp(ctx, *(c.code.code));
    JSValue res = interp.run(thisVal.getVal(), jsArgs.size(), jsArgs.data(), c.capturedVars);
    return jac::Value(ctx, res);
}


}  // namespace jac::cfg::tless::interp
