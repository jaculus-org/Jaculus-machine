#include <catch2/catch_test_macros.hpp>
#include <catch2/generators/catch_generators.hpp>
#include <catch2/matchers/catch_matchers_string.hpp>

#include <string>

#include <jac/features/eventLoopFeature.h>
#include <jac/features/eventQueueFeature.h>
#include <jac/features/filesystemFeature.h>
#include <jac/features/nodeModuleLoaderFeature.h>
#include <jac/machine/machine.h>
#include <jac/machine/values.h>

#include "util.h"

TEST_CASE("Import file", "[moduleLoader]") {
    using Machine = jac::ComposeMachine<
        jac::MachineBase,
        TestReportFeature,
        jac::EventQueueFeature,
        jac::EventLoopFeature,
        jac::FilesystemFeature,
        jac::NodeModuleLoaderFeature,
        jac::EventLoopTerminal
    >;
    Machine machine;

    SECTION("Everything") {
        machine.setCodeDir("./test_files/moduleLoader/node-test");
        machine.initialize();

        evalFile(machine, "./src/index.js");

        REQUIRE(machine.getReports() == std::vector<std::string>{
            "./relative.js",
            "implicit.mjs",
            "@namespace/package",
            "main-export",
            "exports",
            "exports/aliased",
            "exports/nested/a",
            "exports/extension/x.js",
            "exports/remapped/relative",
            "./node_modules/exports/remapped/index.js",
            "false",
            "exports/remapped",
            "exports/array",
            "specific-pattern",
            "no-package",
            "./src/index.js",
            "true"
        });
    }
}

TEST_CASE("Resolve Node module errors", "[moduleLoader]") {
    using Machine = jac::ComposeMachine<
        jac::MachineBase,
        TestReportFeature,
        jac::EventQueueFeature,
        jac::EventLoopFeature,
        jac::FilesystemFeature,
        jac::NodeModuleLoaderFeature,
        jac::EventLoopTerminal
    >;
    Machine machine;

    using sgn = typename std::tuple<std::string, std::string>;
    auto [comment, specifier] = GENERATE(
        sgn {
            "Null condition blocks fallback",
            "exports/blocked"
        },
        sgn {
            "Integer condition keys are invalid",
            "exports/integer"
        },
        sgn {
            "Invalid final target segment is rejected",
            "exports/invalid-target"
        },
        sgn {
            "Invalid final pattern segment is rejected",
            "exports/invalid-pattern/node_modules"
        },
        sgn {
            "Empty pattern matches are rejected",
            "exports/empty"
        },
        sgn {
            "Malformed package configuration is rejected",
            "malformed-package"
        }
    );

    DYNAMIC_SECTION(comment) {
        machine.setCodeDir("./test_files/moduleLoader/node-test");
        machine.initialize();

        evalCodeThrows(
            machine,
            "import '" + specifier + "';",
            "./src/error.js",
            jac::EvalFlags::Module
        );
    }
}
