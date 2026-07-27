

# File simpleModuleLoaderFeature.h

[**File List**](files.md) **>** [**features**](dir_6f95e06b732314161804ab1ef73c9681.md) **>** [**simpleModuleLoaderFeature.h**](simpleModuleLoaderFeature_8h.md)

[Go to the documentation of this file](simpleModuleLoaderFeature_8h.md)


```C++
#pragma once

#include <jac/machine/machine.h>

#include <jac/features/util/moduleLoader.h>


namespace jac {


template<class Next>
class SimpleModuleLoaderFeature : public Next {
private:
    static JSModuleDef *moduleLoaderCbk(JSContext* ctx, const char *module_name, void *_self, JSValueConst attributes) {
        auto &self = *static_cast<SimpleModuleLoaderFeature<Next>*>(_self);

        std::string filename = module_name;

        std::string buffer;
        try {
            buffer = self.fs.loadCode(filename);
        } catch (jac::Exception &e) {
            e.throwJS(ctx);
            return nullptr;
        }

        self.resetWatchdog();

        int jsonType = moduleAttributesJsonType(ctx, attributes);
        if (jsonType > 0 || filename.ends_with(".json")) {
            JSValue val = JS_ParseJSON2(ctx, buffer.c_str(), buffer.size(), module_name,
                                        jsonType == 2 ? JS_PARSE_JSON_EXT : 0);
            if (JS_IsException(val)) {
                return nullptr;
            }
            return createJsonModule(ctx, module_name, val);
        }

        // compile and return module
        JSValue val = JS_Eval(ctx, buffer.c_str(), buffer.size(), module_name,
                              JS_EVAL_TYPE_MODULE | JS_EVAL_FLAG_COMPILE_ONLY);
        if (JS_IsException(val)) {
            return nullptr;
        }

        auto mdl = static_cast<JSModuleDef*>(JS_VALUE_GET_PTR(val));

        Object meta(ctx, JS_GetImportMeta(ctx, mdl));
        meta.set("url", filename);
        meta.set("main", false);

        JS_FreeValue(ctx, val);
        return mdl;
    }

public:
    Value evalFile(std::string path_) {
        auto buffer = this->fs.loadCode(path_);

        Value val = this->eval(std::move(buffer), path_, EvalFlags::Module);
        return val;
    }

    void evalFileWithEventLoop(std::string path_) {
        Value promise = this->evalFile(path_);
        this->evalWithEventLoopCommon(promise);
    }

    void initialize() {
        Next::initialize();

        this->setFileModuleLoader([this](JSContext* ctx, const char* name, JSValueConst attributes) {
            return moduleLoaderCbk(ctx, name, this, attributes);
        });
    }
};


} // namespace jac
```


