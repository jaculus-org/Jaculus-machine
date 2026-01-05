#pragma once

#include <jac/features/evalFeature.h>
#include <jac/machine/functionFactory.h>
#include <jac/machine/machine.h>

#include <jac/machine/compiler/ast.h>
#include <jac/machine/compiler/ast2cfg.h>
#include <jac/machine/compiler/cfg.h>
#include <jac/machine/compiler/cfgUtil.h>
#include <jac/machine/compiler/scanner.h>
#include <jac/machine/compiler/traverseFuncs.h>

#include "cfgInterpreter.h"
#include "types.h"

#include <list>

#include <algorithm>
#include <cstdint>
#include <cstdio>
#include <iostream>
#include <sstream>
#include <stdexcept>
#include <string>
#include <string_view>
#include <utility>
#include <vector>


namespace jac {


namespace detail {


struct CompiledCallable {
    cfg::Function* fn;
    std::map<std::string, CompFn>* compiledHolder;

    Value operator()(ContextRef ctx, ValueWeak thisVal, std::vector<ValueWeak> args) {
        std::vector<JSValue> jsArgs;
        jsArgs.reserve(args.size());
        for (ValueWeak& arg : args) {
            jsArgs.push_back(arg.getVal());
        }

        cfg::interp::CFGInterpreter interp(*compiledHolder);
        JSValue res = interp.run(*fn, ctx, thisVal.getVal(), jsArgs.size(), jsArgs.data());
        return Value(ctx, res);
    }
};


} // namespace detail


template<class Next>
class AotEvalFeature : public EvalFeature<Next> {
    std::map<std::string, CompFn> compiledHolder;

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

        for (auto& [cfgFunc, code] : cfgFunctions) {
            auto aliasName = alias(cfgFunc.entry);
            auto name = cfgFunc.name();
            auto compFn = CompFn{ std::move(cfgFunc), name, aliasName, code, {} };

            auto [it, _] = compiledHolder.emplace(name, std::move(compFn));

            auto jsFn = detail::CompiledCallable{ &it->second.fn, &compiledHolder };

            FunctionFactory ff(this->context());
            Function jsFnObj = ff.newFunctionThisVariadic(std::move(jsFn));

            it->second.jsFn = jsFnObj;

            results.push_back(&it->second);
        }

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

        // define the compiled functions in global scope
        jac::Object global = this->context().getGlobalObject();

        for (auto& fn : compiledFunctions) {
            assert(fn->jsFn);
            global.defineProperty(fn->alias, *(fn->jsFn));
        }

        return newJS;
    }
public:

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
};


} // namespace jac
