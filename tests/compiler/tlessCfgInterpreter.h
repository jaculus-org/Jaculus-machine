#pragma once

#include "jac/machine/context.h"
#include <cassert>
#include <cstdint>
#include <cstdlib>
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
#include <jac/machine/functionFactory.h>
#include <jac/util.h>


namespace jac::cfg::tless::interp {


struct Slot;


struct Code {
    cfg::tless::Function* code;
    std::shared_ptr<cfg::tless::Function> root;
};


struct Closure {
    Code code;
    std::vector<Slot> capturedVars;
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
        return *this;
    }

    template<typename Machine>
    JSValue toJSValue(JSContext* ctx) const;
};


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

    Value takeValue() {
        assert(std::holds_alternative<Value>(content) && "Expected value");
        Value value = std::move(std::get<Value>(content));
        content = Used{};
        return value;
    }

    bool isSlot() const {
        return std::holds_alternative<Slot>(content);
    }

    bool isValue() const {
        return std::holds_alternative<Value>(content);
    }
};


enum class FrameState {
    Runnable,
    Suspended,
    Completed,
    Failed,
};


struct Completion {
    JSValue value = JS_UNDEFINED;
    JSValue exception = JS_UNDEFINED;
    bool hadException = false;

    void free(JSContext* ctx) {
        JS_FreeValue(ctx, value);
        JS_FreeValue(ctx, exception);
        value = JS_UNDEFINED;
        exception = JS_UNDEFINED;
    }
};


struct AsyncPromiseState {
    JSValue resolve = JS_UNDEFINED;
    JSValue reject = JS_UNDEFINED;

    void free(JSContext* ctx) {
        JS_FreeValue(ctx, resolve);
        JS_FreeValue(ctx, reject);
        resolve = JS_UNDEFINED;
        reject = JS_UNDEFINED;
    }
};


template<typename Machine>
struct Frame : std::enable_shared_from_this<Frame<Machine>> {
    JSContext* ctx;
    Machine& machine;
    std::shared_ptr<Function> functionRoot;
    const Function& func;

    BasicBlockPtr currentBlock = nullptr;
    size_t instructionIndex = 0;
    std::map<int, RegContent> regs;
    std::map<int, Slot> argSlots;
    std::vector<JSValue> ownedArgs;
    std::vector<Slot> ownedClosureArgs;
    std::span<Slot> currentClosureArgs;
    int currentArgc = 0;
    JSValueConst* currentArgv = nullptr;
    FrameState state = FrameState::Runnable;
    Completion completion;
    std::optional<AsyncPromiseState> asyncPromise;
    std::array<Reg, 3> pendingAwaitRes{};

    Frame(JSContext* ctx_, Machine& machine_, std::shared_ptr<Function> root_, const Function& func_)
        : ctx(ctx_), machine(machine_), functionRoot(std::move(root_)), func(func_) {}

    ~Frame() {
        cleanupFrame();
        completion.free(ctx);
    }

    static bool isTerminalState(FrameState s) {
        return s == FrameState::Completed || s == FrameState::Failed;
    }

    void freeRegContent(const RegContent& content) {
        if (content.isValue()) {
            content.getValue().free(ctx);
        }
    }

    JSValue takeJSValue(RegContent& content) {
        return content.takeValue().template toJSValue<Machine>(ctx);
    }

    void freeArguments(const std::vector<RegContent>& args) {
        for (const auto& arg : args) {
            freeRegContent(arg);
        }
    }

    void cleanupFrame() {
        for (const auto& [id, content] : regs) {
            (void)id;
            freeRegContent(content);
        }
        regs.clear();
        argSlots.clear();
        for (auto arg : ownedArgs) {
            JS_FreeValue(ctx, arg);
        }
        ownedArgs.clear();
        ownedClosureArgs.clear();
        currentClosureArgs = {};
        currentArgc = 0;
        currentArgv = nullptr;
        if (asyncPromise) {
            asyncPromise->free(ctx);
            asyncPromise.reset();
        }
    }

    void printState() const {
        std::cout << "  Register state:\n";

        auto valStr = [&](const Value& value) -> std::string {
            if (std::holds_alternative<JSValue>(value.val)) {
                auto str = JS_ToCString(ctx, std::get<JSValue>(value.val));
                std::string strCopy(str ? str : "<invalid string>");
                JS_FreeCString(ctx, str);
                return strCopy;
            }
            if (std::holds_alternative<Closure>(value.val)) {
                return "<closure>";
            }
            if (std::holds_alternative<Code>(value.val)) {
                return "<code>";
            }
            return "<unknown>";
        };

        for (const auto& [id, content] : regs) {
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
                return RegContent{ Value{ Code{ value.get(), functionRoot } } };
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

    void settleAsyncFrame() {
        if (!asyncPromise) {
            return;
        }
        JSValue callback = completion.hadException
            ? JS_DupValue(ctx, asyncPromise->reject)
            : JS_DupValue(ctx, asyncPromise->resolve);
        JSValue arg = completion.hadException
            ? JS_DupValue(ctx, completion.exception)
            : JS_DupValue(ctx, completion.value);
        JSValue result = JS_Call(ctx, callback, JS_UNDEFINED, 1, &arg);
        JS_FreeValue(ctx, arg);
        JS_FreeValue(ctx, callback);
        if (JS_IsException(result)) {
            JSValue exc = JS_GetException(ctx);
            if (!completion.hadException) {
                completion.free(ctx);
            }
            completion.hadException = true;
            completion.exception = exc;
        }
        JS_FreeValue(ctx, result);
    }

    JSValue normalizeAwaitable(JSValue awaitable) {
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

    void resumeAwait(JSValue value, bool rejected) {
        if (state != FrameState::Suspended) {
            assert(false && "Cannot resume await on non-suspended frame");
        }
        setReg(pendingAwaitRes[0].id(), RegContent{ Value(rejected ? JS_UNDEFINED : value) });
        setReg(pendingAwaitRes[1].id(), RegContent{ Value(rejected ? value : JS_UNDEFINED) });
        setReg(pendingAwaitRes[2].id(), RegContent{ Value(JS_NewBool(ctx, rejected)) });
        state = FrameState::Runnable;
        runFrame();
    }

    bool attachAwaitHandlers(JSValue awaitable) {
        JSValue normalized = normalizeAwaitable(awaitable);
        if (JS_IsException(normalized)) {
            return false;
        }
        FunctionFactory ff{ ContextRef(ctx) };
        JSContext* callbackCtx = ctx;
        auto self = this->shared_from_this();
        auto onFulfilled = ff.newFunction([self, callbackCtx](ValueWeak settled) {
            JSValue settledVal = JS_DupValue(callbackCtx, settled.getVal());
            self->resumeAwait(settledVal, false);
        });
        auto onRejected = ff.newFunction([self, callbackCtx](ValueWeak rejected) {
            JSValue rejectedVal = JS_DupValue(callbackCtx, rejected.getVal());
            self->resumeAwait(rejectedVal, true);
        });
        JSValue handlers[] = { JS_DupValue(ctx, onFulfilled.getVal()), JS_DupValue(ctx, onRejected.getVal()) };
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

    void initArgs(JSValueConst thisVal, int argc, JSValueConst* argv, std::span<Slot> closureArgs = {}) {
        (void)thisVal;
        currentBlock = func.entry;
        instructionIndex = 0;
        state = FrameState::Runnable;
        completion = {};
        size_t nargs = func.argCount;
        ownedArgs.reserve(nargs);
        for (size_t i = 0; i < nargs; i++) {
            if (static_cast<int>(i) < argc) {
                ownedArgs.push_back(JS_DupValue(ctx, argv[i]));
            } else {
                ownedArgs.push_back(JS_UNDEFINED);
            }
        }
        currentArgc = nargs;
        currentArgv = ownedArgs.data();
        ownedClosureArgs.assign(closureArgs.begin(), closureArgs.end());
        currentClosureArgs = std::span<Slot>(ownedClosureArgs.data(), ownedClosureArgs.size());
    }

    void completeFrame(JSValue value, bool hadException) {
        if (isTerminalState(state)) {
            assert(false && "Frame completed more than once");
        }
        completion.hadException = hadException;
        if (hadException) {
            state = FrameState::Failed;
            completion.exception = value;
        }
        else {
            state = FrameState::Completed;
            completion.value = value;
        }
        settleAsyncFrame();
    }

    bool evalOperation(const Operation& op) {
        std::vector<RegContent> args;
        for (const auto& arg : op.args) {
            args.push_back(getReg(arg.id()));
        }

        std::vector<RegContent> res;
        switch (op.op) {
            case Opcode::CreateLocal: {
                JSValue isConstVal = takeJSValue(args[0]);
                bool isConst = JS_ToBool(ctx, isConstVal);
                JS_FreeValue(ctx, isConstVal);
                res.emplace_back(Slot::make(ctx, Value(JS_UNINITIALIZED), isConst));
            } break;
            case Opcode::CreateUndefined:
                res.emplace_back(Value(JS_UNDEFINED));
                break;
            case Opcode::Copy:
                assert(false && "Opcode::Copy not implemented");
                break;
            case Opcode::BoolNot: {
                JSValue operand = takeJSValue(args[0]);
                res.emplace_back(Value(JS_NewBool(ctx, !JS_ToBool(ctx, operand))));
                JS_FreeValue(ctx, operand);
            } break;
            case Opcode::BitNot: {
                int32_t operand;
                JSValue operandVal = takeJSValue(args[0]);
                int conversionResult = JS_ToInt32(ctx, &operand, operandVal);
                JS_FreeValue(ctx, operandVal);
                if (conversionResult) {
                    JSValue exc = JS_GetException(ctx);
                    res.emplace_back(Value(JS_UNDEFINED));
                    res.emplace_back(Value(exc));
                    res.emplace_back(Value(JS_NewBool(ctx, 1)));
                } else {
                    res.emplace_back(Value(JS_NewInt32(ctx, ~operand)));
                    res.emplace_back(Value(JS_UNDEFINED));
                    res.emplace_back(Value(JS_NewBool(ctx, 0)));
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
                    res.emplace_back(Value(JS_NewBool(ctx, 1)));
                } else {
                    res.emplace_back(Value(num));
                    res.emplace_back(Value(JS_UNDEFINED));
                    res.emplace_back(Value(JS_NewBool(ctx, 0)));
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
                    res.emplace_back(Value(JS_NewBool(ctx, 1)));
                    break;
                }
                switch (JS_VALUE_GET_TAG(num)) {
                    case JS_TAG_INT: res.emplace_back(Value(JS_NewInt32(ctx, -JS_VALUE_GET_INT(num)))); break;
                    case JS_TAG_FLOAT64: res.emplace_back(Value(JS_NewFloat64(ctx, -JS_VALUE_GET_FLOAT64(num)))); break;
                    default: assert(false && "Unexpected type after conversion to number"); break;
                }
                JS_FreeValue(ctx, num);
                res.emplace_back(Value(JS_UNDEFINED));
                res.emplace_back(Value(JS_NewBool(ctx, 0)));
            } break;
            case Opcode::Load: {
                auto slot = args[0].getSlot();
                Value val = slot.load();
                if (std::holds_alternative<JSValue>(val.val) && JS_IsException(std::get<JSValue>(val.val))) {
                    JSValue exc = JS_GetException(ctx);
                    res.emplace_back(Value(JS_UNDEFINED));
                    res.emplace_back(slot);
                    res.emplace_back(Value(exc));
                    res.emplace_back(Value(JS_NewBool(ctx, 1)));
                } else {
                    res.emplace_back(std::move(val));
                    res.emplace_back(slot);
                    res.emplace_back(Value(JS_UNDEFINED));
                    res.emplace_back(Value(JS_NewBool(ctx, 0)));
                }
            } break;
            case Opcode::Dup:
                if (args[0].isSlot()) {
                    auto slot = args[0].getSlot();
                    res.emplace_back(slot);
                    res.emplace_back(slot);
                }
                else {
                    auto value = args[0].takeValue();
                    res.emplace_back(value);
                    res.emplace_back(value.dup(ctx));
                }
                break;
            case Opcode::Kill:
                if (args[0].isValue()) {
                    auto value = args[0].takeValue();
                    value.free(ctx);
                }
                break;
            case Opcode::GetArgRef: {
                int32_t argIndex;
                JSValue argIndexVal = takeJSValue(args[0]);
                int conversionResult = JS_ToInt32(ctx, &argIndex, argIndexVal);
                JS_FreeValue(ctx, argIndexVal);
                if (conversionResult) {
                    assert(false && "Exception during GetArgRef (failed to convert index to int32)");
                }
                if (argIndex < 0 || argIndex >= currentArgc) {
                    assert(false && "GetArgRef argument index out of bounds");
                }
                auto it = argSlots.find(argIndex);
                if (it == argSlots.end()) {
                    it = argSlots.emplace(argIndex, Slot::make(ctx, Value(JS_DupValue(ctx, currentArgv[argIndex])))).first;
                }
                res.emplace_back(it->second);
            } break;
            case Opcode::GetClosureRef: {
                int32_t closureIndex;
                JSValue closureIndexVal = takeJSValue(args[0]);
                int conversionResult = JS_ToInt32(ctx, &closureIndex, closureIndexVal);
                JS_FreeValue(ctx, closureIndexVal);
                if (conversionResult) {
                    assert(false && "Exception during GetClosureRef (failed to convert index to int32)");
                }
                if (closureIndex < 0 || static_cast<size_t>(closureIndex) >= currentClosureArgs.size()) {
                    assert(false && "GetClosureRef closure index out of bounds");
                }
                res.emplace_back(currentClosureArgs[closureIndex]);
            } break;
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
            case Opcode::Add: {
                int32_t exception = 0;
                JSValue lhs = takeJSValue(args[0]);
                JSValue rhs = takeJSValue(args[1]);
                JSValue result = quickjs_ops::add(ctx, lhs, rhs, &exception);
                JS_FreeValue(ctx, lhs);
                JS_FreeValue(ctx, rhs);
                if (exception) {
                    JSValue exc = JS_GetException(ctx);
                    res.emplace_back(Value(JS_UNDEFINED));
                    res.emplace_back(Value(exc));
                    res.emplace_back(Value(JS_NewBool(ctx, 1)));
                } else {
                    res.emplace_back(Value(result));
                    res.emplace_back(Value(JS_UNDEFINED));
                    res.emplace_back(Value(JS_NewBool(ctx, 0)));
                }
            } break;
            case Opcode::Sub: {
                int32_t exception = 0;
                JSValue lhs = takeJSValue(args[0]);
                JSValue rhs = takeJSValue(args[1]);
                JSValue result = quickjs_ops::sub(ctx, lhs, rhs, &exception);
                JS_FreeValue(ctx, lhs);
                JS_FreeValue(ctx, rhs);
                if (exception) {
                    JSValue exc = JS_GetException(ctx);
                    res.emplace_back(Value(JS_UNDEFINED));
                    res.emplace_back(Value(exc));
                    res.emplace_back(Value(JS_NewBool(ctx, 1)));
                } else {
                    res.emplace_back(Value(result));
                    res.emplace_back(Value(JS_UNDEFINED));
                    res.emplace_back(Value(JS_NewBool(ctx, 0)));
                }
            } break;
            case Opcode::Mul: {
                int32_t exception = 0;
                JSValue lhs = takeJSValue(args[0]);
                JSValue rhs = takeJSValue(args[1]);
                JSValue result = quickjs_ops::mul(ctx, lhs, rhs, &exception);
                JS_FreeValue(ctx, lhs);
                JS_FreeValue(ctx, rhs);
                if (exception) {
                    JSValue exc = JS_GetException(ctx);
                    res.emplace_back(Value(JS_UNDEFINED));
                    res.emplace_back(Value(exc));
                    res.emplace_back(Value(JS_NewBool(ctx, 1)));
                } else {
                    res.emplace_back(Value(result));
                    res.emplace_back(Value(JS_UNDEFINED));
                    res.emplace_back(Value(JS_NewBool(ctx, 0)));
                }
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
                    res.emplace_back(Value(JS_NewBool(ctx, 1)));
                } else {
                    res.emplace_back(Value(result));
                    res.emplace_back(Value(JS_UNDEFINED));
                    res.emplace_back(Value(JS_NewBool(ctx, 0)));
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
                    res.emplace_back(Value(JS_NewBool(ctx, 1)));
                } else {
                    res.emplace_back(Value(result));
                    res.emplace_back(Value(JS_UNDEFINED));
                    res.emplace_back(Value(JS_NewBool(ctx, 0)));
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
                    res.emplace_back(Value(JS_NewBool(ctx, 1)));
                } else {
                    res.emplace_back(Value(result));
                    res.emplace_back(Value(JS_UNDEFINED));
                    res.emplace_back(Value(JS_NewBool(ctx, 0)));
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
                    res.emplace_back(Value(JS_NewBool(ctx, 1)));
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
                res.emplace_back(Value(JS_NewBool(ctx, 0)));
            } break;
            case Opcode::StrictEq: {
                // Strict equality never invokes user code, so it cannot throw.
                int32_t exception = 0;
                JSValue lhs = takeJSValue(args[0]);
                JSValue rhs = takeJSValue(args[1]);
                bool result = quickjs_ops::strictEqual(ctx, lhs, rhs, &exception);
                JS_FreeValue(ctx, lhs);
                JS_FreeValue(ctx, rhs);
                res.emplace_back(Value(JS_NewBool(ctx, result)));
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
                    res.emplace_back(Value(JS_NewBool(ctx, 1)));
                } else {
                    res.emplace_back(Value(JS_NewBool(ctx, result)));
                    res.emplace_back(Value(JS_UNDEFINED));
                    res.emplace_back(Value(JS_NewBool(ctx, 0)));
                }
            } break;
            case Opcode::StrictNeq: {
                // Strict inequality never invokes user code, so it cannot throw.
                int32_t exception = 0;
                JSValue lhs = takeJSValue(args[0]);
                JSValue rhs = takeJSValue(args[1]);
                bool result = quickjs_ops::strictEqual(ctx, lhs, rhs, &exception);
                JS_FreeValue(ctx, lhs);
                JS_FreeValue(ctx, rhs);
                res.emplace_back(Value(JS_NewBool(ctx, !result)));
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
                    res.emplace_back(Value(JS_NewBool(ctx, 1)));
                } else {
                    res.emplace_back(Value(JS_NewBool(ctx, !result)));
                    res.emplace_back(Value(JS_UNDEFINED));
                    res.emplace_back(Value(JS_NewBool(ctx, 0)));
                }
            } break;
            case Opcode::Gt: {
                int32_t exception = 0;
                JSValue lhs = takeJSValue(args[0]);
                JSValue rhs = takeJSValue(args[1]);
                bool result = quickjs_ops::greater(ctx, lhs, rhs, &exception);
                JS_FreeValue(ctx, lhs);
                JS_FreeValue(ctx, rhs);
                if (exception) {
                    JSValue exc = JS_GetException(ctx);
                    res.emplace_back(Value(JS_UNDEFINED));
                    res.emplace_back(Value(exc));
                    res.emplace_back(Value(JS_NewBool(ctx, 1)));
                } else {
                    res.emplace_back(Value(JS_NewBool(ctx, result)));
                    res.emplace_back(Value(JS_UNDEFINED));
                    res.emplace_back(Value(JS_NewBool(ctx, 0)));
                }
            } break;
            case Opcode::Gte: {
                int32_t exception = 0;
                JSValue lhs = takeJSValue(args[0]);
                JSValue rhs = takeJSValue(args[1]);
                bool result = quickjs_ops::greaterEq(ctx, lhs, rhs, &exception);
                JS_FreeValue(ctx, lhs);
                JS_FreeValue(ctx, rhs);
                if (exception) {
                    JSValue exc = JS_GetException(ctx);
                    res.emplace_back(Value(JS_UNDEFINED));
                    res.emplace_back(Value(exc));
                    res.emplace_back(Value(JS_NewBool(ctx, 1)));
                } else {
                    res.emplace_back(Value(JS_NewBool(ctx, result)));
                    res.emplace_back(Value(JS_UNDEFINED));
                    res.emplace_back(Value(JS_NewBool(ctx, 0)));
                }
            } break;
            case Opcode::Lt: {
                int32_t exception = 0;
                JSValue lhs = takeJSValue(args[0]);
                JSValue rhs = takeJSValue(args[1]);
                bool result = quickjs_ops::less(ctx, lhs, rhs, &exception);
                JS_FreeValue(ctx, lhs);
                JS_FreeValue(ctx, rhs);
                if (exception) {
                    JSValue exc = JS_GetException(ctx);
                    res.emplace_back(Value(JS_UNDEFINED));
                    res.emplace_back(Value(exc));
                    res.emplace_back(Value(JS_NewBool(ctx, 1)));
                } else {
                    res.emplace_back(Value(JS_NewBool(ctx, result)));
                    res.emplace_back(Value(JS_UNDEFINED));
                    res.emplace_back(Value(JS_NewBool(ctx, 0)));
                }
            } break;
            case Opcode::Lte: {
                int32_t exception = 0;
                JSValue lhs = takeJSValue(args[0]);
                JSValue rhs = takeJSValue(args[1]);
                bool result = quickjs_ops::lessEq(ctx, lhs, rhs, &exception);
                JS_FreeValue(ctx, lhs);
                JS_FreeValue(ctx, rhs);
                if (exception) {
                    JSValue exc = JS_GetException(ctx);
                    res.emplace_back(Value(JS_UNDEFINED));
                    res.emplace_back(Value(exc));
                    res.emplace_back(Value(JS_NewBool(ctx, 1)));
                } else {
                    res.emplace_back(Value(JS_NewBool(ctx, result)));
                    res.emplace_back(Value(JS_UNDEFINED));
                    res.emplace_back(Value(JS_NewBool(ctx, 0)));
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
                    res.emplace_back(Value(JS_NewBool(ctx, 1)));
                } else {
                    res.emplace_back(Value(prop));
                    res.emplace_back(Value(JS_UNDEFINED));
                    res.emplace_back(Value(JS_NewBool(ctx, 0)));
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
                    res.emplace_back(Value(JS_NewBool(ctx, 1)));
                    break;
                }
                if (slot.val->isGlobal() && !std::holds_alternative<JSValue>(val.val)) {
                    val = Value(val.toJSValue<Machine>(ctx));
                }
                slot.store(val);
                slot.val->markInitialized();
                res.emplace_back(slot);
                res.emplace_back(Value(JS_UNDEFINED));
                res.emplace_back(Value(JS_NewBool(ctx, 0)));
            } break;
            case Opcode::SetMember: {
                JSValue obj = takeJSValue(args[0]);
                JSValue id = takeJSValue(args[1]);
                JSValue val = takeJSValue(args[2]);
                JSAtom atom = JS_ValueToAtom(ctx, id);
                JS_FreeValue(ctx, id);
                JSValue exVal = JS_UNDEFINED;
                JSValue hadEx = JS_NewBool(ctx, 0);
                if (JS_SetProperty(ctx, obj, atom, val) < 0) {
                    exVal = JS_GetException(ctx);
                    hadEx = JS_NewBool(ctx, 1);
                }
                JS_FreeValue(ctx, obj);
                JS_FreeAtom(ctx, atom);
                res.emplace_back(Value(exVal));
                res.emplace_back(Value(hadEx));
            } break;
            case Opcode::Call: {
                JSValue resVal = JS_UNDEFINED;
                JSValue exVal = JS_UNDEFINED;
                JSValue hadEx = JS_NewBool(ctx, 0);
                Value fn = args[0].takeValue();
                std::vector<JSValue> callArgs;
                for (size_t i = 1; i < args.size(); i++) {
                    callArgs.push_back(takeJSValue(args[i]));
                }
                if (std::holds_alternative<JSValue>(fn.val)) {
                    JSValue jsFn = fn.toJSValue<Machine>(ctx);
                    resVal = JS_Call(ctx, jsFn, JS_UNDEFINED, callArgs.size(), callArgs.data());
                    JS_FreeValue(ctx, jsFn);
                    if (JS_IsException(resVal)) {
                        resVal = JS_UNDEFINED;
                        exVal = JS_GetException(ctx);
                        hadEx = JS_NewBool(ctx, 1);
                    }
                }
                else if (std::holds_alternative<Closure>(fn.val)) {
                    Closure closure = std::get<Closure>(fn.val);
                    if (!closure.code.code->isAsync) {
                        resVal = Frame<Machine>::runSync(ctx, machine, *closure.code.code, closure.code.root, JS_UNDEFINED, callArgs.size(), callArgs.data(), closure.capturedVars);
                    }
                    else {
                        resVal = Frame<Machine>::runAsync(ctx, machine, *closure.code.code, closure.code.root, JS_UNDEFINED, callArgs.size(), callArgs.data(), closure.capturedVars);
                    }
                    if (JS_IsException(resVal)) {
                        resVal = JS_UNDEFINED;
                        exVal = JS_GetException(ctx);
                        hadEx = JS_NewBool(ctx, 1);
                    }
                }
                else {
                    assert(false && "Call target must be value or closure");
                }
                for (auto arg : callArgs) {
                    JS_FreeValue(ctx, arg);
                }
                res.emplace_back(Value(resVal));
                res.emplace_back(Value(exVal));
                res.emplace_back(Value(hadEx));
            } break;
            case Opcode::CallMethod: {
                JSValue obj = takeJSValue(args[0]);
                JSValue method = takeJSValue(args[1]);
                JSValue resVal = JS_UNDEFINED;
                JSValue exVal = JS_UNDEFINED;
                JSValue hadEx = JS_NewBool(ctx, 0);
                if (JS_IsException(method)) {
                    resVal = JS_UNDEFINED;
                    exVal = JS_GetException(ctx);
                    hadEx = JS_NewBool(ctx, 1);
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
                        hadEx = JS_NewBool(ctx, 1);
                    }
                }
                JS_FreeValue(ctx, obj);
                JS_FreeValue(ctx, method);
                res.emplace_back(Value(resVal));
                res.emplace_back(Value(exVal));
                res.emplace_back(Value(hadEx));
            } break;
            case Opcode::Construct: {
                JSValue ctor = takeJSValue(args[0]);
                std::vector<JSValue> callArgs;
                for (size_t i = 1; i < args.size(); i++) {
                    callArgs.push_back(takeJSValue(args[i]));
                }
                JSValue resVal = JS_CallConstructor(ctx, ctor, callArgs.size(), callArgs.data());
                JSValue exVal = JS_UNDEFINED;
                JSValue hadEx = JS_NewBool(ctx, 0);
                JS_FreeValue(ctx, ctor);
                for (auto arg : callArgs) {
                    JS_FreeValue(ctx, arg);
                }
                if (JS_IsException(resVal)) {
                    resVal = JS_UNDEFINED;
                    exVal = JS_GetException(ctx);
                    hadEx = JS_NewBool(ctx, 1);
                }
                res.emplace_back(Value(resVal));
                res.emplace_back(Value(exVal));
                res.emplace_back(Value(hadEx));
            } break;
            case Opcode::Await: {
                assert(func.isAsync && "Await executed in non-async function");
                JSValue awaitable = takeJSValue(args[0]);
                if (!attachAwaitHandlers(awaitable)) {
                    JSValue exVal = JS_GetException(ctx);
                    res.emplace_back(Value(JS_UNDEFINED));
                    res.emplace_back(Value(exVal));
                    res.emplace_back(Value(JS_NewBool(ctx, 1)));
                    break;
                }
                pendingAwaitRes = { op.res[0], op.res[1], op.res[2] };
                instructionIndex++;
                state = FrameState::Suspended;
                freeArguments(args);
                return false;
            } break;
            case Opcode::MakeClosure: {
                auto code = std::get<Code>(args[0].takeValue().val);
                std::vector<Slot> capturedVars;
                for (size_t i = 1; i < args.size(); i++) {
                    capturedVars.push_back(args[i].getSlot());
                }
                res.emplace_back(Value{ Closure{ code, std::move(capturedVars) } });
            } break;
        }

        freeArguments(args);
        for (size_t i = 0; i < op.res.size(); i++) {
            setReg(op.res[i].id(), std::move(res[i]));
        }
        return true;
    }

    void runFrame() {
        if (isTerminalState(state)) {
            return;
        }
        while (true) {
            for (; instructionIndex < currentBlock->instructions.size(); instructionIndex++) {
                const auto& insn = currentBlock->instructions[instructionIndex];
                std::cout << "  Executing instruction " << instructionIndex << "\n";
                printState();
                if (insn->isOperation()) {
                    if (!evalOperation(insn->asOperation())) {
                        return;
                    }
                }
                else {
                    evalConstInit(insn->asConstInit());
                }
            }
            std::cout << "  Terminator\n";
            printState();
            switch (currentBlock->terminator.type) {
                case Terminator::Type::Jump:
                    moveRegs(currentBlock->terminator.args, currentBlock->terminator.target->args);
                    currentBlock = currentBlock->terminator.target;
                    instructionIndex = 0;
                    break;
                case Terminator::Type::Branch: {
                    Value cond = getReg(currentBlock->terminator.value.id()).getValue();
                    JSValue condVal = cond.toJSValue<Machine>(ctx);
                    bool condition = JS_ToBool(ctx, condVal);
                    JS_FreeValue(ctx, condVal);
                    if (condition) {
                        moveRegs(currentBlock->terminator.args, currentBlock->terminator.target->args);
                        currentBlock = currentBlock->terminator.target;
                    }
                    else {
                        moveRegs(currentBlock->terminator.args, currentBlock->terminator.other->args);
                        currentBlock = currentBlock->terminator.other;
                    }
                    instructionIndex = 0;
                } break;
                case Terminator::Type::Return:
                    if (currentBlock->terminator.value.id() == 0) {
                        completeFrame(JS_UNDEFINED, false);
                    }
                    else {
                        Value retVal = getReg(currentBlock->terminator.value.id()).getValue();
                        completeFrame(retVal.toJSValue<Machine>(ctx), false);
                    }
                    return;
                case Terminator::Type::Throw: {
                    Value exception = getReg(currentBlock->terminator.value.id()).getValue();
                    completeFrame(exception.toJSValue<Machine>(ctx), true);
                    return;
                } break;
                case Terminator::Type::None:
                    assert(false && "Invalid terminator");
                    break;
            }
        }
    }

    JSValue finishResult() {
        if (!isTerminalState(state)) {
            assert(false && "Cannot finish incomplete frame");
        }
        if (state == FrameState::Failed) {
            JS_Throw(ctx, JS_DupValue(ctx, completion.exception));
            return JS_EXCEPTION;
        }
        return JS_DupValue(ctx, completion.value);
    }

    static JSValue runSync(JSContext* ctx, Machine& machine, const Function& func, std::shared_ptr<Function> root, JSValueConst thisVal, int argc, JSValueConst* argv, std::span<Slot> closureArgs = {}) {
        auto frame = std::make_shared<Frame>(ctx, machine, std::move(root), func);
        frame->initArgs(thisVal, argc, argv, closureArgs);
        frame->runFrame();
        return frame->finishResult();
    }

    static JSValue runAsync(JSContext* ctx, Machine& machine, const Function& func, std::shared_ptr<Function> root, JSValueConst thisVal, int argc, JSValueConst* argv, std::span<Slot> closureArgs = {}) {
        auto [promise, resolve, reject] = jac::Promise::create(ContextRef(ctx));
        auto frame = std::make_shared<Frame>(ctx, machine, std::move(root), func);
        frame->initArgs(thisVal, argc, argv, closureArgs);
        frame->asyncPromise = AsyncPromiseState{ resolve.loot().second, reject.loot().second };
        frame->runFrame();
        return promise.loot().second;
    }
};


template<typename Machine>
struct ClosureWrapper {
    Closure c;

    jac::Value operator()(ContextRef ctx, ValueWeak thisVal, std::vector<ValueWeak> args) {
        std::vector<JSValue> jsArgs;
        jsArgs.reserve(args.size());
        for (ValueWeak& arg : args) {
            jsArgs.push_back(arg.getVal());
        }

        Machine& machine = *static_cast<Machine*>(JS_GetContextOpaque(ctx));

        if (!c.code.code->isAsync) {
            JSValue res = Frame<Machine>::runSync(ctx, machine, *c.code.code, c.code.root, thisVal.getVal(), jsArgs.size(), jsArgs.data(), c.capturedVars);
            return jac::Value(ctx, res);
        }

        JSValue res = Frame<Machine>::runAsync(ctx, machine, *c.code.code, c.code.root, thisVal.getVal(), jsArgs.size(), jsArgs.data(), c.capturedVars);
        return jac::Value(ctx, res);
    }

    static JSValue mkJs(ContextRef ctx, Closure c) {
        ClosureWrapper w(std::move(c));
        jac::FunctionFactory ff(ctx);
        jac::Value v = ff.newFunctionThisVariadic(std::move(w));
        return v.loot().second;
    }
};


template<typename Machine>
inline JSValue Value::toJSValue(JSContext* ctx) const {
    if (std::holds_alternative<JSValue>(val)) {
        return std::get<JSValue>(val);
    }
    if (std::holds_alternative<Closure>(val)) {
        return ClosureWrapper<Machine>::mkJs(ctx, std::get<Closure>(val));
    }
    assert(false && "Value cannot be converted to JS");
    return JS_UNDEFINED;
}


}  // namespace jac::cfg::tless::interp
