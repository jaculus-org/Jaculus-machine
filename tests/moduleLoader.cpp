#include <catch2/catch_test_macros.hpp>
#include <catch2/generators/catch_generators.hpp>
#include <catch2/matchers/catch_matchers_string.hpp>

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


TEST_CASE("Import JSON module", "[moduleLoader]") {
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
    machine.setCodeDir("test_files/moduleLoader/importJson");

    SECTION("With type attribute") {
        machine.initialize();
        evalFile(machine, "./main.js");
        REQUIRE(machine.getReports() == std::vector<std::string>{ "config", "42", "true" });
    }

#ifdef USE_NODE_MODULE_LOADER
    SECTION("json without type attribute") {
        machine.initialize();
        evalFileThrows(machine, "./mainSuffix.js");
        REQUIRE(machine.getReports().empty());
    }
#else
    SECTION("By .json suffix without attribute") {
        machine.initialize();
        evalFile(machine, "./mainSuffix.js");
        REQUIRE(machine.getReports() == std::vector<std::string>{ "config" });
    }
#endif

    SECTION("json5 type attribute allows extended syntax") {
        machine.initialize();
        evalFile(machine, "./mainJson5.js");
        REQUIRE(machine.getReports() == std::vector<std::string>{ "j5", "7" });
    }

    SECTION("json type rejects extended syntax in .json files") {
        machine.initialize();
        REQUIRE_THROWS_WITH(
            machine.evalModuleWithEventLoop("import data from './extended.json' with { type: 'json' }; exit(0);", "./strict.js"),
            Catch::Matchers::ContainsSubstring("SyntaxError")
        );
    }

    SECTION("json5 type also accepts strict JSON in .json files") {
        machine.initialize();
        evalModuleWithEventLoop(machine, "import data from './data.json' with { type: 'json5' }; report(data.name); exit(0);", "./extended.js");
        REQUIRE(machine.getReports() == std::vector<std::string>{"config"});
    }

#ifdef USE_NODE_MODULE_LOADER
    SECTION("JSON type on JavaScript is rejected") {
        machine.initialize();
        evalFileThrows(machine, "./wrongType.js");
    }
#endif

    SECTION("Unknown import type is rejected") {
        machine.initialize();
        evalFileThrows(machine, "./unknownType.js");
    }

    SECTION("Unsupported attribute is rejected") {
        machine.initialize();
        try {
            machine.evalFileWithEventLoop("./badAttr.js");
            FAIL("expected an exception for the unsupported import attribute");
        } catch (jac::Exception& e) {
            std::string message(e.what());
            CAPTURE(message);
            REQUIRE(message.find("not supported") != std::string::npos);
        }
    }
}

TEST_CASE("Import attributes on fresh and cached modules", "[moduleLoader]") {
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
    machine.setCodeDir("test_files/moduleLoader/importJson");
    machine.initialize();

    SECTION("Empty dynamic attributes are allowed") {
        evalModuleWithEventLoop(machine, R"(
            const first = await import('./neighbor.js', { with: {} });
            const second = await import('./neighbor.js', { with: {} });
            report(first === second);
            first.callNeighbor();
            exit(0);
        )", "./attributes.js");
        REQUIRE(machine.getReports() == std::vector<std::string>{"true", "callNeighbor"});
    }

    SECTION("Matching JSON attributes reuse the cached module") {
        evalModuleWithEventLoop(machine, R"(
            const first = await import('./data.json', { with: { type: 'json' } });
            const second = await import('./data.json', { with: { type: 'json' } });
            report(first === second);
            exit(0);
        )", "./attributes.js");
        REQUIRE(machine.getReports() == std::vector<std::string>{"true"});
    }
}
