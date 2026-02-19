

# File machine.h

[**File List**](files.md) **>** [**jac**](dir_256037ad7d0c306238e2bc4f945d341d.md) **>** [**machine**](dir_10e7d6e7bc593e38e57ffe1bab5ed259.md) **>** [**machine.h**](machine_8h.md)

[Go to the documentation of this file](machine_8h.md)


```C++
#pragma once

#include <quickjs.h>

#include <chrono>
#include <functional>
#include <stdexcept>
#include <string>
#include <unordered_map>
#include <vector>

#include "values.h"


namespace jac {


template<class Base, template<class> class... MFeatures>
struct ComposeMachine;

template<class Base, template<class> class FirstFeature, template<class> class... MFeatures>
struct ComposeMachine<Base, FirstFeature, MFeatures...> : public ComposeMachine<FirstFeature<Base>, MFeatures...> {};

template<class Base>
struct ComposeMachine<Base> : public Base {};


enum class EvalFlags : int {
    Global = JS_EVAL_TYPE_GLOBAL,
    Module = JS_EVAL_TYPE_MODULE,

    Strict = JS_EVAL_FLAG_STRICT,
    CompileOnly = JS_EVAL_FLAG_COMPILE_ONLY,
    BacktraceBarrier = JS_EVAL_FLAG_BACKTRACE_BARRIER
};

inline constexpr EvalFlags operator|(EvalFlags a, EvalFlags b) {
    int res = static_cast<int>(a) | static_cast<int>(b);
    if (res & JS_EVAL_TYPE_GLOBAL && res & JS_EVAL_TYPE_MODULE) {
        throw std::runtime_error("Cannot use both global and module eval flags");
    }
    return static_cast<EvalFlags>(res);
}

inline constexpr EvalFlags operator&(EvalFlags a, EvalFlags b) {
    return static_cast<EvalFlags>(static_cast<int>(a) & static_cast<int>(b));
}


class MachineBase;


class Module {
    ContextRef _ctx;
    JSModuleDef *_def;

    std::vector<std::tuple<std::string, Value>> exports;

    inline MachineBase& base() {
        return *static_cast<MachineBase*>(JS_GetContextOpaque(_ctx));
    }

    static inline MachineBase& base(ContextRef ctx) {
        return *static_cast<MachineBase*>(JS_GetContextOpaque(ctx));
    }
public:
    Module(ContextRef ctx, std::string name);
    Module& operator=(const Module&) = delete;
    Module(const Module&) = delete;
    Module& operator=(Module&& other) {
        _ctx = other._ctx;
        _def = other._def;
        exports = std::move(other.exports);
        other._def = nullptr;
        return *this;
    }
    Module(Module&& other): _ctx(other._ctx), _def(other._def), exports(std::move(other.exports)) {
        other._def = nullptr;
    }

    void addExport(std::string name, Value val);

    JSModuleDef *get() {
        return _def;
    }
};


class MachineBase {
private:
    std::unordered_map<JSModuleDef*, Module> _modules;
    Module& findModule(JSModuleDef* m);

    bool _interrupt = false;

    std::chrono::milliseconds _watchdogTimeout = std::chrono::milliseconds(0);
    std::chrono::time_point<std::chrono::steady_clock> _watchdogNext;
    std::function<bool()> _wathdogCallback;
    const JSMallocFunctions* mallocFunctions = nullptr;

    JSRuntime* _runtime = nullptr;
    ContextRef _context = nullptr;
public:
    JSRuntime* runtime() {
        return _runtime;
    }

    ContextRef context() {
        return _context;
    }

    void initialize();

    MachineBase() = default;
    MachineBase(const MachineBase&) = delete;
    MachineBase(MachineBase&&) = delete;
    MachineBase& operator=(const MachineBase&) = delete;
    MachineBase& operator=(MachineBase&&) = delete;

    virtual ~MachineBase() {
        _modules.clear();
        if (_context) {
            JS_FreeContext(_context);
        }
        if (_runtime) {
            JS_FreeRuntime(_runtime);
        }
    }

    Value eval(std::string code, std::string filename, EvalFlags flags = EvalFlags::Global);

    Module& newModule(std::string name);

    void interruptRuntime() {
        _interrupt = true;
    }

    void resetWatchdog() {
        _watchdogNext = std::chrono::steady_clock::now() + _watchdogTimeout;
    }

    void setWatchdogTimeout(std::chrono::milliseconds timeout) {
        _watchdogTimeout = timeout;
    }

    void setWatchdogHandler(std::function<bool()> callback) {
        _wathdogCallback = callback;
    }

    void setMallocFunctions(const JSMallocFunctions* fns) {
        mallocFunctions = fns;
    }

    friend class Module;
};


} // namespace jac
```


