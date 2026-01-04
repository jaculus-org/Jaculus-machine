#include <catch2/catch_test_macros.hpp>
#include <catch2/generators/catch_generators.hpp>

#include <string>

#include <jac/features/filesystemFeature.h>
#include <jac/features/simpleModuleLoaderFeature.h>
#include <jac/machine/machine.h>
#include <jac/machine/values.h>

#include "util.h"


TEST_CASE("TestReportFeature", "[reportFeature]") {
    using Machine =
        TestReportFeature<
        jac::MachineBase
    >;

    Machine machine;
    machine.initialize();

    using sgn = typename std::tuple<std::string, std::string, std::vector<std::string>>;
    auto [comment, code, expected] = GENERATE(
        sgn {
            "None",
            "",
            {}
        },
        sgn {
            "Single report",
            "report('hello world')",
            {"hello world"}
        },
        sgn {
            "Multiple reports",
            "report('hello world'); report('hello world 2')",
            {"hello world", "hello world 2"}
        },
        sgn {
            "Loop",
            "for (let i = 0; i < 10; i++) { report('hello world ' + i) }",
            []() {
                std::vector<std::string> v;
                for (int i = 0; i < 10; i++) {
                    v.push_back("hello world " + std::to_string(i));
                }
                return v;
            }()
        }
    );

    DYNAMIC_SECTION(comment) {
        machine.eval(code, "test", jac::EvalFlags::Global);
        REQUIRE(machine.getReports() == expected);
    }
}


TEST_CASE("Eval", "[eval]") {
    using Machine =
        TestReportFeature<
        jac::MachineBase
    >;

    Machine machine;

    SECTION("Syntax error") {
        machine.initialize();

        std::string code = "let x = 0; report(xx);";
        REQUIRE_THROWS_AS(machine.eval(code, "test", jac::EvalFlags::Global), jac::Exception);
    }

    SECTION("Return value") {
        machine.initialize();

        std::string code = "'hello world'";
        REQUIRE(machine.eval(code, "test", jac::EvalFlags::Global).to<std::string>() == "hello world");
    }

    SECTION("Throw") {
        machine.initialize();

        std::string code = "throw new Error('hello world')";
        REQUIRE_THROWS_AS(machine.eval(code, "test", jac::EvalFlags::Global), jac::Exception);
    }
}
