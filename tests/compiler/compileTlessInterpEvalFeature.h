#pragma once

#include <jac/features/evalFeature.h>
#include <jac/machine/functionFactory.h>
#include <jac/machine/machine.h>

#include <jac/machine/compiler/ast.h>
#include <jac/machine/compiler/tlessAst2cfg.h>
#include <jac/machine/compiler/tlessCfg.h>
#include <jac/machine/compiler/tlessCfgUtil.h>
#include <jac/machine/compiler/scanner.h>
#include <jac/machine/compiler/traverseFuncs.h>

#include "tlessCfgInterpreter.h"

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


template<class Next>
class AotEvalFeature : public EvalFeature<Next> {

    jac::ast::ModulePtr parseModule(std::string_view js) {
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

        auto mod = jac::ast::parseModule(state);
        if (!mod || !state.isEnd()) {
            lex::Token errorToken = state.getErrorToken();
            std::cerr << "Parse error: " << state.getErrorMessage()
                      << " at " << errorToken.line << ":" << errorToken.column << '\n';
            throw std::runtime_error("Parse error");
        }

        return mod;
    }

    jac::cfg::tless::Function tryAot(std::string_view js) {
        jac::ast::ModulePtr mod = parseModule(js);
        jac::ast::hoistModule(*mod);

        auto em = jac::cfg::tless::ast2cfg(*mod);
        auto func = em.output();

        return func;
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
        assert(flags | EvalFlags::Module);  // XXX: only module code can be AOT compiled for now

        std::optional<jac::cfg::tless::Function> func;
        try {
            func.emplace(tryAot(code));
        }
        catch (const cfg::tless::IRGenError& e) {
            throw jac::Exception::create(jac::Exception::Type::SyntaxError, "AOT compilation error: " + std::string(e.what()));
        }

        if (func) {
            cfg::tless::interp::Interpreter interp(this->context(), *func);
            JSValue resVal = interp.run(JS_UNDEFINED, 0, nullptr);
            if (JS_IsException(resVal)) {
                throw std::runtime_error("Exception during interpretation");
            }
            return Value(this->context(), resVal);
        }
        else {
            return EvalFeature<Next>::eval(std::move(code), filename, flags);
        }
    }
};


} // namespace jac
