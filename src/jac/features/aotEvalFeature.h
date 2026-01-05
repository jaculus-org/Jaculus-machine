#pragma once

#include <jac/features/evalFeature.h>
#include <jac/machine/compiler/opcode.h>
#include <jac/machine/context.h>
#include <jac/machine/functionFactory.h>
#include <jac/machine/internal/declarations.h>
#include <jac/machine/machine.h>
#include <jac/util.h>

#include <jac/machine/compiler/ast.h>
#include <jac/machine/compiler/ast2cfg.h>
#include <jac/machine/compiler/cfg.h>
#include <jac/machine/compiler/cfg2mir.h>
#include <jac/machine/compiler/cfgUtil.h>
#include <jac/machine/compiler/scanner.h>
#include <jac/machine/compiler/traverseFuncs.h>

#include <mir-gen.h>
#include <mir.h>
#include <quickjs.h>

#include <algorithm>
#include <cstdint>
#include <cstdio>
#include <iostream>
#include <list>
#include <map>
#include <sstream>
#include <stdexcept>
#include <string>
#include <string_view>
#include <utility>
#include <vector>


namespace jac {


struct CompFn {
    std::string name;
    std::string alias;
    std::string_view code;
    std::optional<Function> jsFn;
};

namespace detail {

using CompiledFnSignature = JSValue(*)(int64_t, JSValue*, JSValue*);

struct CompiledCallable {
    CompiledFnSignature callerPtr;

    Value operator()(ContextRef ctx, ValueWeak /* this */, std::vector<ValueWeak> args) {
        std::vector<JSValue> jsArgs;
        for (ValueWeak& arg : args) {
            jsArgs.push_back(arg.getVal());
        }

        JSValue res;
        callerPtr(jsArgs.size(), jsArgs.data(), &res);
        return Value(ctx, res);
    }
};


} // namespace detail


template<class Next>
class AotEvalFeature : public EvalFeature<Next> {
    std::map<std::string, CompFn> compiledHolder;
    std::vector<MIR_context_t> mirContexts;

    cfg::mir_emit::RuntimeContext rtCtx;

    std::string alias(const void* ptr) {
        return "__jac_aot_func_" + std::to_string(reinterpret_cast<uint64_t>(ptr));  // NOLINT
    }

    jac::ast::ScriptPtr parseScript(std::string_view js) {
        bool hadError = false;
        std::vector<std::string> reports;
        jac::lex::Scanner scanner(js, [&hadError, &reports](int line, int col, const std::string& msg) {
            hadError = true;
            reports.push_back("Lex error: " + msg + " at " + std::to_string(line) + ":" + std::to_string(col));
        });

        if (hadError) {
            for (const auto& report : reports) {
                std::cerr << report << '\n';
            }
            throw std::runtime_error("Lex error");
        }

        auto tokens = scanner.scan();

        jac::ast::ParserState state(tokens);

        auto script = jac::ast::parseScript(state);
        if (!script || !state.isEnd()) {
            lex::Token errorToken = state.getErrorToken();
            std::cerr << "Parse error: " << state.getErrorMessage()
                      << " at " << errorToken.line << ":" << errorToken.column << '\n';
            throw std::runtime_error("Parse error");
        }

        return script;
    }

    std::list<std::pair<jac::cfg::Function, std::string_view>> transformFunctions(const jac::ast::traverse::Functions& functions) {
        std::list<std::pair<jac::cfg::Function, std::string_view>> result;
        std::map<cfg::Identifier, cfg::SignaturePtr> signatures;
        for (const auto& astFunc : functions.functions) {
            if (!astFunc->name()) {
                continue;
            }
            auto sig = jac::cfg::getSignature(*astFunc);
            if (sig) {
                signatures.emplace(astFunc->name()->name, sig);
            }
        }

        for (const auto& astFunc : functions.functions) {
            if (!astFunc->name() || !signatures.contains(astFunc->name()->name)) {
                continue;
            }
            auto sig = signatures.at(astFunc->name()->name);
            auto cfgFuncEm = jac::cfg::ast2cfg(*astFunc, sig, signatures);

            auto cfgFunc = cfgFuncEm.output();
            jac::cfg::removeEmptyBlocks(cfgFunc);
            result.emplace_back(std::move(cfgFunc), astFunc->code);
        }

        return result;
    }

    std::string placeStubs(std::string_view js, const std::list<CompFn*>& compiledFunctions) {
        std::stringstream newJs;
        auto begin = js.begin();
        for (const auto& fn : compiledFunctions) {
            newJs << std::string_view(begin, fn->code.begin());

            newJs << "const ";
            newJs << fn->name;
            newJs << " = ";
            newJs << fn->alias;
            newJs << "; /* compiled from: ";
            newJs << fn->code;
            newJs << " */";

            begin = fn->code.end();
        }

        newJs << std::string_view(begin, js.end());

        return newJs.str();
    }

    std::list<CompFn*> compile(std::list<std::pair<jac::cfg::Function, std::string_view>> cfgFunctions) {
        std::list<CompFn*> results;

        MIR_context_t ctx = MIR_init();
        MIR_module_t mod = MIR_new_module(ctx, "aot");
        std::vector<MIR_item_t> funcs;
        std::vector<MIR_item_t> callers;

        {
            Defer d([&] {
                MIR_finish_module(ctx);
            });

            mirContexts.push_back(ctx);

            cfg::mir_emit::Builtins builtins = jac::cfg::mir_emit::generateBuiltins(ctx, &rtCtx);

            std::map<std::string, std::pair<MIR_item_t, MIR_item_t>> prototypes;
            for (auto& [cfgFunc, code] : cfgFunctions) {
                auto proto = cfg::mir_emit::getPrototype(ctx, cfgFunc);
                auto forward = MIR_new_forward(ctx, cfgFunc.name().c_str());
                prototypes.emplace(cfgFunc.name(), std::make_pair(proto, forward));
            }

            for (auto& [cfgFunc, code] : cfgFunctions) {
                funcs.push_back(jac::cfg::mir_emit::compile(ctx, prototypes, cfgFunc, builtins));
            }

            for (auto& [cfgFunc, code] : cfgFunctions) {
                auto proto = prototypes.at(cfgFunc.name()).first;
                auto fun = prototypes.at(cfgFunc.name()).second;
                auto caller = jac::cfg::mir_emit::compileCaller(ctx, builtins, cfgFunc, fun, proto);
                callers.push_back(caller);
            }
        }

        MIR_load_module(ctx, mod);

        MIR_gen_init(ctx, 1);
        MIR_link(ctx, MIR_set_gen_interface, nullptr);

        FunctionFactory ff(this->context());

        size_t i = 0;
        for (auto& [cfgFunc, code] : cfgFunctions) {
            auto aliasName = alias(cfgFunc.entry);
            auto name = cfgFunc.name();
            auto compFn = CompFn{ name, aliasName, code, {} };

            auto [it, _] = compiledHolder.emplace(name, std::move(compFn));

            void* ptr = MIR_gen(ctx, 0, callers[i]);
            i++;

            // the "value" of "undefined" is ignored by QuickJS so we can abuse it to pass arbitrary data
            JSValue ptrWrap = JS_MKPTR(JS_TAG_UNDEFINED, ptr);
            JSCFunctionData* funWrap = +[](JSContext *, JSValueConst, int argc, JSValueConst *argv, int magic, JSValue *func_data) -> JSValue {
                auto caller = reinterpret_cast<detail::CompiledFnSignature>(func_data->u.ptr);  // NOLINT
                JSValue res;
                caller(argc, argv, &res);
                return res;
            };
            JSValue jsFn = JS_NewCFunctionData(this->context(), funWrap, 0, 0, 1, &ptrWrap);

            it->second.jsFn = Function(this->context(), jsFn);

            results.push_back(&it->second);
        }

        MIR_gen_finish(ctx);

        return results;
    }

    std::string tryAot(std::string_view js) {
        jac::ast::ScriptPtr script = parseScript(js);

        jac::ast::traverse::Functions astFunctions;
        jac::ast::traverse::funcs(*script, astFunctions);

        auto simple = transformFunctions(astFunctions);

        auto compiledFunctions = compile(std::move(simple));

        compiledFunctions.sort([](const CompFn* a, const CompFn* b) {
            return a->code.begin() < b->code.begin();
        });

        auto newJS = placeStubs(js, compiledFunctions);

        // define the compiled functions on globalThis
        jac::Object global = this->context().getGlobalObject();

        for (auto& fn : compiledFunctions) {
            assert(fn->jsFn);
            global.defineProperty(fn->alias, *(fn->jsFn));
        }

        return newJS;
    }
public:

    void initialize() {
        Next::initialize();
        rtCtx.ctx = this->context();
    }

    /**
     * @brief Evaluate a string containing javascript code, while compiling some
     * parts to native code
     *
     * @param code the code to evaluate
     * @param filename filename to use for the code. Used for error reporting
     * @param flags flags to evaluate the code with
     * @return Result of the evaluation
     */
    Value eval(std::string code, std::string filename, EvalFlags flags = EvalFlags::Global) {
        try {
            code = tryAot(code);
        }
        catch (const cfg::IRGenError& e) {
            throw jac::Exception::create(jac::Exception::Type::SyntaxError, "AOT compilation error: " + std::string(e.what()));
        }

        return EvalFeature<Next>::eval(std::move(code), filename, flags);
    }

    ~AotEvalFeature() {
        for (auto& ctx : mirContexts) {
            MIR_finish(ctx);
        }
    }
};


} // namespace jac
