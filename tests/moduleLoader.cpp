#include <catch2/catch_test_macros.hpp>
#include <catch2/generators/catch_generators.hpp>

#include <string>

#include <jac/features/eventLoopFeature.h>
#include <jac/features/eventQueueFeature.h>
#include <jac/features/filesystemFeature.h>
#include <jac/machine/machine.h>
#include <jac/machine/values.h>

#include "util.h"


#ifdef USE_NODE_MODULE_LOADER

    #include <jac/features/nodeModuleLoaderFeature.h>
    template<class Next>
    using ModuleLoaderFeature = jac::NodeModuleLoaderFeature<Next>;

#else

    #include <jac/features/simpleModuleLoaderFeature.h>
    template<class Next>
    using ModuleLoaderFeature = jac::SimpleModuleLoaderFeature<Next>;

#endif

TEST_CASE("Eval file", "[moduleLoader]") {
    using Machine = jac::ComposeMachine<
        jac::MachineBase,
        TestReportFeature,
        jac::EventQueueFeature,
        jac::EventLoopFeature,
        jac::FilesystemFeature,
        ModuleLoaderFeature,
        jac::EventLoopTerminal
    >;
    Machine machine;

    using sgn = typename std::tuple<std::string, std::string, std::vector<std::string>>;
    auto [comment, path, expected] = GENERATE(
        sgn {
            "Eval file",
            "./test_files/moduleLoader/test.js",
            { "first", "second" }
        }
    );

    DYNAMIC_SECTION(comment) {
        machine.setCodeDir(machine.path.dirname(path));
        machine.initialize();

        machine.evalFile("./" + machine.path.basename(path));
        REQUIRE(machine.getReports() == expected);
    }

    SECTION("File not found") {
        machine.initialize();
        evalFileThrows(machine, "./test_files/moduleLoader/notFound.js");
    }

    SECTION("Exception") {
        machine.initialize();
        evalFileThrows(machine, "./test_files/moduleLoader/throw.js");
    }
}


TEST_CASE("Import file", "[moduleLoader]") {
    using Machine = jac::ComposeMachine<
        jac::MachineBase,
        TestReportFeature,
        jac::EventQueueFeature,
        jac::EventLoopFeature,
        jac::FilesystemFeature,
        ModuleLoaderFeature,
        jac::EventLoopTerminal
    >;
    Machine machine;

    using sgn = typename std::tuple<std::string, std::string, std::vector<std::string>>;
    auto [comment, path, expected] = GENERATE(
        sgn {
            "Neighbor",
            "test_files/moduleLoader/importNeighbor/main.js",
            { "callNeighbor" }
        },
        sgn {
            "Subdirectory",
            "test_files/moduleLoader/importSubdir/main.js",
            { "callSubdir" }
        },
        sgn {
            "Subdirectory and up",
            "test_files/moduleLoader/importSubdirUp/main.js",
            { "callUp", "callSubdir" }
        }
    );

    DYNAMIC_SECTION(comment) {
        machine.setCodeDir(machine.path.dirname(path));
        machine.initialize();

        evalFile(machine, "./" + machine.path.basename(path));

        REQUIRE(machine.getReports() == expected);
    }

    SECTION("Import not found") {
        machine.initialize();
        evalFileThrows(machine, "./test_files/moduleLoader/importNotFound/main.js");
    }
}
