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

#include <cstdint>
#include <iostream>
#include <memory>
#include <stdexcept>
#include <string>
#include <string_view>
#include <utility>
#include <vector>


namespace jac {


template<class Next>
class TlessInterpEvalFeature : public EvalFeature<Next> {

    std::vector<jac::lex::Token> scan(std::string_view js) {
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

        return scanner.scan();
    }

    jac::cfg::tless::Function tryAot(std::string_view js, bool isModule) {
        auto tokens = scan(js);
        jac::ast::ParserState state(tokens);

        if (isModule) {
            auto mod = jac::ast::parseModule(state);
            if (!mod || !state.isEnd()) {
                jac::lex::Token errorToken = state.getErrorToken();
                std::cerr << "Parse error: " << state.getErrorMessage()
                          << " at " << errorToken.line << ":" << errorToken.column << '\n';
                throw std::runtime_error("Parse error");
            }
            jac::ast::hoistModule(*mod);
            return jac::cfg::tless::ast2cfg(*mod).output();
        }

        auto script = jac::ast::parseScript(state);
        if (!script || !state.isEnd()) {
            jac::lex::Token errorToken = state.getErrorToken();
            std::cerr << "Parse error: " << state.getErrorMessage()
                      << " at " << errorToken.line << ":" << errorToken.column << '\n';
            throw std::runtime_error("Parse error");
        }
        jac::ast::hoistScript(*script);
        return jac::cfg::tless::ast2cfg(*script).output();
    }
public:

    std::shared_ptr<cfg::tless::Function> compileTless(std::string_view code, EvalFlags flags = EvalFlags::Global) {
        bool isModule = (flags & EvalFlags::Module) == EvalFlags::Module;
        try {
            auto func = tryAot(code, isModule);
            cfg::tless::removeUnreachableBlocks(func);
            return std::make_shared<cfg::tless::Function>(std::move(func));
        }
        catch (const cfg::tless::IRGenError& e) {
            throw jac::Exception::create(jac::Exception::Type::SyntaxError, "SyntaxError: AOT compilation error: " + std::string(e.what()));
        }
        catch (const std::runtime_error& e) {
            throw jac::Exception::create(jac::Exception::Type::SyntaxError, "SyntaxError: " + std::string(e.what()));
        }
    }

    Value evalTless(const std::shared_ptr<cfg::tless::Function>& compiled) {
        JSValue result;
        if (compiled->isAsync) {
            result = cfg::tless::interp::runAsync(this->context(), *compiled, compiled, JS_UNDEFINED, 0, nullptr);
        }
        else {
            result = cfg::tless::interp::runSync(this->context(), *compiled, compiled, JS_UNDEFINED, 0, nullptr);
        }
        return Value(this->context(), result);
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
        return evalTless(compileTless(code, flags));
    }
};


} // namespace jac
