#pragma once

#include <jac/machine/machine.h>


namespace jac {


template<class Next>
class ModuleLoaderFeature : public Next {
private:
    static JSModuleDef *moduleLoaderCbk(JSContext* ctx, const char *module_name, void *_self) {
        auto &self = *static_cast<ModuleLoaderFeature<Next>*>(_self);

        std::string filename = module_name;

        std::string buffer;
        try {
            buffer = self.fs.loadCode(filename);
        } catch (jac::Exception &e) {
            e.throwJS(ctx);
            return nullptr;
        }

        // compile and return module
        self.resetWatchdog();
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
    /**
     * @brief Evaluate a file
     *
     * @param path_ Path to the file
     * @return A promise that will be resolved when the module ends, and rejected if
     *         the module throws an exception.
     */
    Value evalFile(std::string path_) {
        auto buffer = this->fs.loadCode(path_);

        Value val = this->eval(std::move(buffer), path_, EvalFlags::Module);
        return val;
    }

    /**
     * @brief Evaluate a file and run the event loop until the program exits
     *        or throws an exception.
     *
     * @param path_ Path to the file
     */
    void evalFileWithEventLoop(std::string path_) {
        Value promise = this->evalFile(path_);
        this->evalWithEventLoopCommon(promise);
    }

    void initialize() {
        Next::initialize();

        JS_SetModuleLoaderFunc(this->runtime(), nullptr, moduleLoaderCbk, this);
    }
};


} // namespace jac
