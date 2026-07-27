#pragma once

#include <jac/features/evalFeature.h>
#include <jac/machine/functionFactory.h>
#include <jac/machine/machine.h>

#include <jac/machine/compiler/ast.h>
#include "jac/machine/compiler/cfg2bc.h"
#include "jac/machine/compiler/bcWriter.h"
#include <jac/machine/compiler/scanner.h>
#include <jac/machine/compiler/tlessCfg.h>
#include <jac/machine/compiler/tlessCfgUtil.h>
#include <jac/machine/compiler/traverseFuncs.h>

#include <cstdint>
#include <cstdio>
#include <iostream>
#include <stdexcept>
#include <string>
#include <string_view>
#include <vector>


namespace jac {


template<class Next>
class AotEvalFeature : public EvalFeature<Next> {
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
            auto script = parseScript(code);

            BytecodeRoot root = jac::bc::emit(*script, filename);

            std::vector<uint8_t> bytecode;
            root.write(bytecode);

            auto res = JS_EvalFunction(this->context(), JS_ReadObject(this->context(), reinterpret_cast<uint8_t*>(bytecode.data()), bytecode.size(), JS_READ_OBJ_BYTECODE));  // NOLINT
            auto val = jac::Value(this->context(), res);
            return val;
        }
        catch (const jac::bc::_IRGenError& e) {
            throw jac::Exception::create(jac::Exception::Type::SyntaxError, "AOT compilation error: " + std::string(e.what()));
        }
    }
};


} // namespace jac
