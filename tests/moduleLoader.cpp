#include <catch2/catch_test_macros.hpp>
#include <catch2/generators/catch_generators.hpp>

#include <string>

#include <jac/features/evalFeature.h>
#include <jac/features/eventLoopFeature.h>
#include <jac/features/eventQueueFeature.h>
#include <jac/features/filesystemFeature.h>
#include <jac/features/moduleLoaderFeature.h>
#include <jac/machine/machine.h>
#include <jac/machine/values.h>

#include "util.h"


TEST_CASE("Eval file", "[moduleLoader]") {
    using Machine = jac::ComposeMachine<
        jac::MachineBase,
        jac::EvalFeature,
        TestReportFeature,
        jac::EventQueueFeature,
        jac::EventLoopFeature,
        jac::FilesystemFeature,
        jac::ModuleLoaderFeature,
        jac::EventLoopTerminal
    >;
    Machine machine;

    using sgn = typename std::tuple<std::string, std::string, std::vector<std::string>>;
    auto [comment, path, expected] = GENERATE(
        sgn {
            "Eval file",
            "fixtures/moduleLoader/test.js",
            { "first", "second" }
        }
    );

    DYNAMIC_SECTION(comment) {
        machine.setCodeDir(machine.path.dirname(path));
        machine.initialize();

        machine.evalFile(machine.path.basename(path));
        REQUIRE(machine.getReports() == expected);
    }

    SECTION("File not found") {
        machine.initialize();
        evalFileThrows(machine, "fixtures/moduleLoader/notFound.js");
    }

    SECTION("Exception") {
        machine.initialize();
        evalFileThrows(machine, "fixtures/moduleLoader/throw.js");
    }
}


TEST_CASE("Import file", "[moduleLoader]") {
    using Machine = jac::ComposeMachine<
        jac::MachineBase,
        jac::EvalFeature,
        TestReportFeature,
        jac::EventQueueFeature,
        jac::EventLoopFeature,
        jac::FilesystemFeature,
        jac::ModuleLoaderFeature,
        jac::EventLoopTerminal
    >;
    Machine machine;

    using sgn = typename std::tuple<std::string, std::string, std::vector<std::string>>;
    auto [comment, path, expected] = GENERATE(
        sgn {
            "Neighbor",
            "fixtures/moduleLoader/importNeighbor/main.js",
            { "callNeighbor" }
        },
        sgn {
            "Subdirectory",
            "fixtures/moduleLoader/importSubdir/main.js",
            { "callSubdir" }
        },
        sgn {
            "Subdirectory and up",
            "fixtures/moduleLoader/importSubdirUp/main.js",
            { "callUp", "callSubdir" }
        }
    );

    DYNAMIC_SECTION(comment) {
        machine.setCodeDir(machine.path.dirname(path));
        machine.initialize();

        evalFile(machine, machine.path.basename(path));

        REQUIRE(machine.getReports() == expected);
    }

    SECTION("Import not found") {
        machine.initialize();
        evalFileThrows(machine, "fixtures/moduleLoader/importNotFound/main.js");
    }
}
