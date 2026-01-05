#include "compiler/cfgInterpreter.h"
#include "compiler/types.h"
#include <catch2/catch_test_macros.hpp>
#include <catch2/generators/catch_generators.hpp>

#include <cstdint>
#include <iostream>

#include <jac/features/evalFeature.h>
#include <jac/machine/machine.h>
#include <jac/machine/values.h>

#include <jac/machine/compiler/ast.h>
#include <jac/machine/compiler/ast2cfg.h>
#include <jac/machine/compiler/cfgUtil.h>


jac::cfg::Function compile(const std::string& code) {
    bool hadError = false;
    std::vector<std::string> reports;
    jac::lex::Scanner scanner(code, [&hadError, &reports](int line, int col, const std::string& msg) {
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

    auto fun = jac::ast::parseFunctionDeclaration(state, false);
    if (!fun || !state.isEnd()) {
        jac::lex::Token errorToken = state.getErrorToken();
        std::cerr << "Parse error: " << state.getErrorMessage()
                    << " at " << errorToken.line << ":" << errorToken.column << '\n';
        throw std::runtime_error("Parse error");
    }

    auto sig = jac::cfg::getSignature(*fun);

    auto cfg = jac::cfg::ast2cfg(*fun, sig, {});

    auto res = cfg.output();
    jac::cfg::removeEmptyBlocks(res);

    return res;
}


TEST_CASE("CfgInterpreter", "[cfg]") {
    using Machine = jac::ComposeMachine<
        jac::MachineBase,
        jac::EvalFeature
    >;

    Machine machine;
    machine.initialize();

    SECTION("Linear I32") {
        auto code = R"(
            function test(a: int32, b: int32): int32 {
                let c: int32 = a + b;
                let d: int32 = c - 2;
                d = d * 2 + 1;
                let f: int32 = (-6) % 4;
                let g: int32 = 1 << 2;
                let h: int32 = 8 >> 2;
                let i: int32 = (g | h) & 5;
                return +(f + i == 2);
            }
        )";

        auto func = compile(code);

        std::map<std::string, jac::CompFn> em;
        jac::cfg::interp::CFGInterpreter interp(em);
        std::array<JSValue, 2> args = {
            JS_NewInt32(machine.context(), 1),
            JS_NewInt32(machine.context(), 2)
        };
        auto result = interp.run(func, machine.context(), JS_UNDEFINED, args.size(), args.data());

        int32_t value;
        JS_ToInt32(machine.context(), &value, result);

        REQUIRE(value == 1);
    }

    SECTION("Fib iterative") {
        auto code = R"(
            function fib(n: int32): int32 {
                let n0: int32 = 0;
                let n1: int32 = 1;
                let n2: int32 = 0;

                if (n == 0) {
                    return n0;
                }
                for (let i: int32 = 2; i <= n; i = i + 1) {
                    n2 = n0 + n1;
                    n0 = n1;
                    n1 = n2;
                }

                return n1;
            }
        )";

        auto func = compile(code);

        std::map<std::string, jac::CompFn> em;
        jac::cfg::interp::CFGInterpreter interp(em);
        std::array<JSValue, 2> args = {
            JS_NewInt32(machine.context(), 7)
        };
        auto result = interp.run(func, machine.context(), JS_UNDEFINED, args.size(), args.data());

        int32_t value;
        JS_ToInt32(machine.context(), &value, result);

        REQUIRE(value == 13);
    }

    SECTION("Object") {
        auto code = R"(
            function test(a: object): object {
                let b: object = a;
                let c: object = b;

                return c;
            }
        )";

        auto func = compile(code);

        std::map<std::string, jac::CompFn> em;
        jac::cfg::interp::CFGInterpreter interp(em);

        jac::Value val = machine.eval(R"(let a = { c: 1 }; a)", "<test>");
        auto obj = val.to<jac::Object>();

        std::array<JSValue, 1> args = {
            obj.getVal()
        };

        JSValue result;
        try {
            result = interp.run(func, machine.context(), JS_UNDEFINED, args.size(), args.data());
        }
        catch (const std::exception& e) {
            std::cerr << e.what() << '\n';
            REQUIRE(false);
        }
        catch (...) {
            std::cerr << "Unknown exception\n";
            REQUIRE(false);
        }

        auto resObj = jac::Object(machine.context(), result);
        REQUIRE(resObj.get("c").to<int>() == 1);
    }

    SECTION("Assign cast") {
        auto code = R"(
            function test(a: int32): float64 {
                let b: float64 = 1.1;
                b = a;
                return b;
            }
        )";
        auto func = compile(code);
        std::map<std::string, jac::CompFn> em;
        jac::cfg::interp::CFGInterpreter interp(em);
        std::array<JSValue, 1> args = {
            JS_NewInt32(machine.context(), 42)
        };
        auto result = interp.run(func, machine.context(), JS_UNDEFINED, args.size(), args.data());
        double value;
        JS_ToFloat64(machine.context(), &value, result);
        REQUIRE(value == 42.0);
    }

    SECTION("Member get") {
        auto code = R"(
            function test(a: object, num: int32): int32 {
                let c: int32 = a.b.c;

                return c + num;
            }
        )";

        auto func = compile(code);

        std::map<std::string, jac::CompFn> em;
        jac::cfg::interp::CFGInterpreter interp(em);

        jac::Value val = machine.eval(R"(let a = { b: { c: 7 } }; a)", "<test>");
        auto obj = val.to<jac::Object>();
        auto num = jac::Value::from<int32_t>(machine.context(), 3);

        std::array<JSValue, 2> args = {
            obj.getVal(),
            num.getVal()
        };

        JSValue result;
        try {
            result = interp.run(func, machine.context(), JS_UNDEFINED, args.size(), args.data());
        }
        catch (const std::exception& e) {
            std::cerr << e.what() << '\n';
            REQUIRE(false);
        }
        catch (...) {
            std::cerr << "Unknown exception\n";
            REQUIRE(false);
        }

        auto res = jac::Value(machine.context(), result);
        REQUIRE(res.to<int>() == 10);
    }

    SECTION("Member set") {
        auto code = R"(
            function test(a: object, num: int32): int32 {
                a.b.c = num;
                a.b = a.b;

                return a.b.c;
            }
        )";

        auto func = compile(code);

        std::map<std::string, jac::CompFn> em;
        jac::cfg::interp::CFGInterpreter interp(em);

        jac::Value val = machine.eval(R"(let a = { b: { c: {} } }; a)", "<test>");
        auto obj = val.to<jac::Object>();
        auto num = jac::Value::from<int32_t>(machine.context(), 42);

        std::array<JSValue, 2> args = {
            obj.getVal(),
            num.getVal()
        };

        JSValue result;
        try {
            result = interp.run(func, machine.context(), JS_UNDEFINED, args.size(), args.data());
        }
        catch (const std::exception& e) {
            std::cerr << e.what() << '\n';
            REQUIRE(false);
        }
        catch (...) {
            std::cerr << "Unknown exception\n";
            REQUIRE(false);
        }

        auto res = jac::Value(machine.context(), result);
        REQUIRE(res.to<int>() == 42);
    }

    SECTION("Member set complex") {
        auto code = R"(
            function test(a: object, d: object): int32 {
                a.b.c = d;
                a.b.c = a.b.c.e;

                return a.b.c;
            }
        )";

        auto func = compile(code);

        std::map<std::string, jac::CompFn> em;
        jac::cfg::interp::CFGInterpreter interp(em);

        jac::Value valA = machine.eval(R"(let a = { b: { c: {} } }; a)", "<test>");
        jac::Value valD = machine.eval(R"(let d = { e: 42 }; d)", "<test>");

        std::array<JSValue, 2> args = {
            valA.getVal(),
            valD.getVal()
        };

        JSValue result;
        try {
            result = interp.run(func, machine.context(), JS_UNDEFINED, args.size(), args.data());
        }
        catch (const std::exception& e) {
            std::cerr << e.what() << '\n';
            REQUIRE(false);
        }
        catch (...) {
            std::cerr << "Unknown exception\n";
            REQUIRE(false);
        }

        auto res = jac::Value(machine.context(), result);
        REQUIRE(res.to<int>() == 42);
    }
}
