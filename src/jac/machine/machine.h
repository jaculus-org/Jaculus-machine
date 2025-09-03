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


/**
 * @brief A wrapper around JSModuleDef that allows for easy exporting of values
 */
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
    /**
     * @brief Create a new module in the given context. Should not be called
     * directly, use MachineBase::newModule instead
     *
     * @param ctx context to work in
     * @param name name of the module
     */
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

    /**
     * @brief Add a value to the module's exports
     *
     * @param name name of the export
     * @param val the value to export
     */
    void addExport(std::string name, Value val);

    /**
     * @brief Get the underlying JSModuleDef* for this module
     *
     * @return JSModuleDef*
     */
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
    /**
     * @brief Get the JSRuntime* for this machine
     *
     * @return The JSRuntime*
     */
    JSRuntime* runtime() {
        return _runtime;
    }

    /**
     * @brief Get the ContextRef for this machine
     *
     * @return The ContextRef
     */
    ContextRef context() {
        return _context;
    }

    /**
     * @brief Initialize the machine. Should be called after machine configuration
     * is done and before any interaction with the javascript engine.
     * Other MFeatures in the Machine stack can implement this method to
     * initialize themselves but the first call should be Next::initialize().
     */
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

    /**
     * @brief Evaluate a string containing javascript code
     * @note If the evaluation mode is EvalFlags::Module, the result will be a Promise
     *
     * @param code the code to evaluate
     * @param filename filename to use for the code. Used for error reporting
     * @param flags flags to evaluate the code with
     * @return Result of the evaluation
     */
    Value eval(std::string code, std::string filename, EvalFlags flags = EvalFlags::Global);

    /**
     * @brief Create a new module in the machine
     *
     * @param name name of the module
     * @return Reference to the new module
     */
    Module& newModule(std::string name);

    /**
     * @brief Interrupt running javascript code. Execution will be thrown
     * in the javascript as an InterruptError.
     */
    void interruptRuntime() {
        _interrupt = true;
    }

    /**
     * @brief Reset the watchdog timer. This should be called periodically
     * to prevent the watchdog from triggering.
     */
    void resetWatchdog() {
        _watchdogNext = std::chrono::steady_clock::now() + _watchdogTimeout;
    }

    /**
     * @brief Set the watchdog timeout. If the timeout is zero, the watchdog
     * is disabled. Otherwise, the watchdog will be called when the timeout
     * has passed since the last reset.
     *
     * @param timeout watchdog timeout
     */
    void setWatchdogTimeout(std::chrono::milliseconds timeout) {
        _watchdogTimeout = timeout;
    }

    /**
     * @brief Set the watchdog callback. The callback will be called when the
     * watchdog timeout has passed since the last reset. If the callback returns
     * true, the runtime will be interrupted.
     *
     * @param callback callback to call
     */
    void setWatchdogHandler(std::function<bool()> callback) {
        _wathdogCallback = callback;
    }

    /**
     * @brief Set custom malloc functions for the QuickJS engine.
     * If the value is nullptr, the default malloc functions will be used.
     *
     * @param fns malloc functions to use
     */
    void setMallocFunctions(const JSMallocFunctions* fns) {
        mallocFunctions = fns;
    }

    friend class Module;
};


} // namespace jac
