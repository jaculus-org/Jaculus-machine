#include <catch2/catch_test_macros.hpp>
#include <catch2/generators/catch_generators.hpp>

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
            "@namespace/package",
            "main-export",
            "exports",
            "exports/aliased",
            "exports/nested/a",
            "exports/extension/x.js"
        });
    }
}
