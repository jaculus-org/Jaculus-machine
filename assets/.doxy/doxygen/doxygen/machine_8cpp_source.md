

# File machine.cpp

[**File List**](files.md) **>** [**jac**](dir_256037ad7d0c306238e2bc4f945d341d.md) **>** [**machine**](dir_10e7d6e7bc593e38e57ffe1bab5ed259.md) **>** [**machine.cpp**](machine_8cpp.md)

[Go to the documentation of this file](machine_8cpp.md)


```C++
#include "machine.h"

#include <cstring>


namespace jac {


static int checkModuleAttributes(JSContext* ctx, void* /*opaque*/, JSValueConst attributes) {
    if (JS_IsUndefined(attributes)) {
        return 0;
    }

    JSPropertyEnum* tab = nullptr;
    uint32_t len = 0;
    if (JS_GetOwnPropertyNames(ctx, &tab, &len, attributes, JS_GPN_ENUM_ONLY | JS_GPN_STRING_MASK)) {
        return -1;
    }

    int ret = 0;
    for (uint32_t i = 0; i < len; ++i) {
        size_t nameLen = 0;
        const char* name = JS_AtomToCStringLen(ctx, &nameLen, tab[i].atom);
        if (!name) {
            ret = -1;
            break;
        }
        if (!(nameLen == 4 && std::memcmp(name, "type", nameLen) == 0)) {
            JS_ThrowTypeError(ctx, "import attribute '%s' is not supported", name);
            ret = -1;
        }
        JS_FreeCString(ctx, name);
        if (ret) {
            break;
        }
    }

    JS_FreePropertyEnum(ctx, tab, len);
    return ret;
}


Module::Module(ContextRef ctx, std::string name) : _ctx(ctx), _exports(Object::create(ctx)) {
    _def = JS_NewCModule(ctx, name.c_str(), [](JSContext* context, JSModuleDef* def) noexcept -> int {
        JSValue carrier = JS_GetModulePrivateValue(context, def);

        int ret = 0;
        try {
            // if no error has occurred, should contain an object
            if (JS_IsObject(carrier)) {
                Object exports(context, carrier);  // takes ownership of the ref
                for (Atom& key : exports.getOwnPropertyNames()) {
                    Value val = exports.get<Value>(key);
                    if (JS_SetModuleExport(context, def, key.toString().c_str(), val.loot().second) != 0) {
                        ret = -1;
                    }
                }
            } else {
                JS_FreeValue(context, carrier);
            }
        } catch (...) {
            ret = -1;
        }

        JS_SetModulePrivateValue(context, def, JS_UNDEFINED);
        return ret;
    });
    if (!_def) {
        throw std::runtime_error("JS_NewCModule failed");
    }
}

void Module::addExport(std::string name, Value val) {
    if (JS_AddModuleExport(_ctx, _def, name.c_str()) != 0) {
        throw Exception::create(Exception::Type::Error, "failed to add module export '" + name + "'");
    }
    _exports.set(name, val);
}

void Module::finalize() {
    JS_SetModulePrivateValue(_ctx, _def, _exports.loot().second);
}

void MachineBase::initialize() {
    // last in stack

    if (mallocFunctions) {
        _runtime = JS_NewRuntime2(mallocFunctions, nullptr);
    }
    else {
        _runtime = JS_NewRuntime();
    }
    _context = JS_NewContext(_runtime);

    JS_SetContextOpaque(_context, this);
    JS_SetInterruptHandler(_runtime, [](JSRuntime*, void* opaque) noexcept {
        MachineBase& base = *static_cast<MachineBase*>(opaque);
        if (base._interrupt) {
            base._interrupt = false;
            return 1;
        }
        if (base._watchdogTimeout.count() > 0) {
            auto now = std::chrono::steady_clock::now();
            if (now > base._watchdogNext) {
                base._watchdogNext = now + base._watchdogTimeout;
                if (!base._wathdogCallback || base._wathdogCallback()) {
                    return 1;
                }
            }
        }
        return 0;
    }, this);

    JS_SetModuleLoaderFunc2(_runtime, nullptr, loadModule, checkModuleAttributes, this);
}

Value MachineBase::eval(std::string code, std::string filename, EvalFlags flags /*= EvalFlags::Global*/) {
    resetWatchdog();
    Value bytecode(_context, JS_Eval(_context, code.c_str(), code.size(), filename.c_str(), static_cast<int>(flags | EvalFlags::CompileOnly)));
    if (static_cast<int>(flags & EvalFlags::CompileOnly) != 0) {
        return bytecode;
    }
    code = "";
    resetWatchdog();
    return Value(_context, JS_EvalFunction(_context, bytecode.loot().second));
}

void MachineBase::newModule(std::string name, ModuleBuilder builder) {
    auto [it, inserted] = _moduleBuilders.emplace(std::move(name), std::move(builder));
    if (!inserted) {
        throw std::runtime_error("module already defined: " + it->first);
    }
}

JSModuleDef* MachineBase::loadModule(JSContext* ctx, const char* name, void* opaque, JSValueConst attributes) {
    auto& self = *static_cast<MachineBase*>(opaque);

    auto it = self._moduleBuilders.find(name);
    if (it != self._moduleBuilders.end()) {
        JSModuleDef* def = nullptr;
        try {
            Module mdl(self._context, name);
            def = mdl.get();
            it->second(mdl);
            mdl.finalize();
            return mdl.get();
        }
        catch (...) {
            if (def) {
                JS_FreeValue(ctx, JS_MKPTR(JS_TAG_MODULE, def));
            }
            try {
                throw;
            }
            catch (Exception& e) {
                e.throwJS(ctx);
            }
            catch (std::exception& e) {
                Exception::create(Exception::Type::InternalError, e.what()).throwJS(ctx);
            }
            catch (...) {
                Exception::create(Exception::Type::InternalError, "unknown error").throwJS(ctx);
            }
            return nullptr;
        }
    }

    if (self._fileModuleLoader) {
        return self._fileModuleLoader(ctx, name, attributes);
    }

    JS_ThrowReferenceError(ctx, "could not load module '%s'", name);
    return nullptr;
}


} // namespace jac
```


