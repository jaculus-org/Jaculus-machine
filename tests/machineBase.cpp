#include <catch2/catch_test_macros.hpp>
#include <catch2/generators/catch_generators.hpp>

#include <string>

#include <jac/features/eventLoopFeature.h>
#include <jac/features/eventQueueFeature.h>
#include <jac/features/filesystemFeature.h>
#include <jac/features/simpleModuleLoaderFeature.h>
#include <jac/machine/machine.h>
#include <jac/machine/values.h>

#include "jac/features/eventLoopTerminal.h"
#include "util.h"


TEST_CASE("Register global", "[base]") {
    using Machine =
        jac::EventLoopTerminal<
        jac::EventLoopFeature<
        jac::EventQueueFeature<
        TestReportFeature<
        jac::MachineBase
    >>>>;

    Machine machine;
    machine.initialize();
    jac::Object global = machine.context().getGlobalObject();

    global.defineProperty("test", jac::Value::from<std::string>(machine.context(), "test string"));


    evalCode(machine, "report(test);", "test", jac::EvalFlags::Global);

    REQUIRE(machine.getReports() == std::vector<std::string>{"test string"});
}


TEST_CASE("Cpp Module", "[base]") {
    using Machine =
        jac::EventLoopTerminal<
        jac::EventLoopFeature<
        jac::EventQueueFeature<
        TestReportFeature<
        jac::MachineBase
    >>>>;

    Machine machine;
    machine.initialize();

    SECTION("Simple") {
        machine.newModule("testModule", [&](jac::Module& mdl) {
            mdl.addExport("test", jac::Value::from<std::string>(machine.context(), "test string"));
        });

        evalModuleWithEventLoop(machine, "import * as testModule from 'testModule'; report(testModule.test); exit(1);", "test");

        REQUIRE(machine.getReports() == std::vector<std::string>{"test string"});
    }

    SECTION("Builder runs lazily on first import") {
        int builds = 0;
        machine.newModule("testModule", [&](jac::Module& mdl) {
            ++builds;
            mdl.addExport("test", jac::Value::from<std::string>(machine.context(), "test string"));
        });

        // Registering a module must not build it.
        REQUIRE(builds == 0);

        evalModuleWithEventLoop(machine, R"(
            import * as a from 'testModule';
            import * as b from 'testModule';
            report(a.test);
            exit(1);
        )", "test");

        REQUIRE(machine.getReports() == std::vector<std::string>{"test string"});
        // Built exactly once, despite two import statements.
        REQUIRE(builds == 1);
    }

    SECTION("Not imported - builder never runs") {
        bool built = false;
        machine.newModule("testModule", [&](jac::Module& mdl) {
            built = true;
            mdl.addExport("test", jac::Value::from<std::string>(machine.context(), "test string"));
        });

        evalModuleWithEventLoop(machine, "report('nothing'); exit(1);", "test");

        REQUIRE(machine.getReports() == std::vector<std::string>{"nothing"});
        REQUIRE_FALSE(built);
    }

    SECTION("Two modules") {
        machine.newModule("testModule1", [&](jac::Module& mdl) {
            mdl.addExport("test1", jac::Value::from<std::string>(machine.context(), "test string 1"));
        });

        machine.newModule("testModule2", [&](jac::Module& mdl) {
            mdl.addExport("test2", jac::Value::from<std::string>(machine.context(), "test string 2"));
        });

        evalModuleWithEventLoop(machine, R"(
            import * as testModule1 from 'testModule1';
            import * as testModule2 from 'testModule2';
            report(testModule1.test1);
            report(testModule2.test2);
            exit(1);
        )", "test");

        REQUIRE(machine.getReports() == std::vector<std::string>{"test string 1", "test string 2"});
    }

    SECTION("Builder that throws does not poison the module name") {
        int builds = 0;
        machine.newModule("boom", [&](jac::Module& mdl) {
            ++builds;
            mdl.addExport("ok", jac::Value::from<std::string>(machine.context(), "ok"));
            throw jac::Exception::create(jac::Exception::Type::Error, "builder boom");
        });

        // First import: the builder throws, so the import fails and the module
        // body must not run.
        evalModuleWithEventLoopThrows(machine, "import * as m from 'boom'; report('reached'); exit(1);", "t1");
        REQUIRE(builds == 1);
        REQUIRE(machine.getReports().empty());

        // The machine stays usable.
        machine.newModule("good", [&](jac::Module& mdl) {
            mdl.addExport("v", jac::Value::from<std::string>(machine.context(), "good"));
        });
        evalModuleWithEventLoop(machine, "import * as g from 'good'; report(g.v); exit(1);", "t2");
        REQUIRE(machine.getReports() == std::vector<std::string>{"good"});

        // Re-importing the failed module must re-run the builder and fail again,
        // not silently return a half-built module with undefined exports.
        evalModuleWithEventLoopThrows(machine, "import * as m from 'boom'; report('reached2'); exit(1);", "t3");
        REQUIRE(builds == 2);
        REQUIRE(machine.getReports() == std::vector<std::string>{"good"});
    }

    SECTION("Duplicate export name fails the import") {
        machine.newModule("dup", [&](jac::Module& mdl) {
            mdl.addExport("x", jac::Value::from<std::string>(machine.context(), "1"));
            mdl.addExport("x", jac::Value::from<std::string>(machine.context(), "2"));
        });

        evalModuleWithEventLoopThrows(machine, "import * as m from 'dup'; report('reached'); exit(1);", "t");
        REQUIRE(machine.getReports().empty());
    }
}

TEST_CASE("watchdog", "[base]") {
    using Machine =
        jac::EventLoopTerminal<
        jac::EventLoopFeature<
        jac::EventQueueFeature<
        TestReportFeature<
        jac::MachineBase
    >>>>;

    Machine machine;
    machine.initialize();

    SECTION("Stop") {
        int triggerCount = 0;
        machine.setWatchdogTimeout(std::chrono::milliseconds(50));
        machine.setWatchdogHandler(std::function<bool()>([&triggerCount]() {
            triggerCount++;
            return true;
        }));

        evalModuleWithEventLoopThrows(machine, R"(
            report('start');
            let until = Date.now() + 100;
            while (Date.now() < until) {}
            report('end');
            exit(1);
        )", "test");

        REQUIRE(machine.getReports() == std::vector<std::string>{"start"});
    }

    SECTION("Ok") {
        int triggerCount = 0;
        machine.setWatchdogTimeout(std::chrono::milliseconds(50));
        machine.setWatchdogHandler(std::function<bool()>([&triggerCount]() {
            triggerCount++;
            return true;
        }));

        evalModuleWithEventLoop(machine, R"(
            report('start');
            let until = Date.now() + 20;
            while (Date.now() < until) {}
            report('end');
            exit(1);
        )", "test");

        REQUIRE(machine.getReports() == std::vector<std::string>{"start", "end"});
    }
}
