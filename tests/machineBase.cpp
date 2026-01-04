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
        auto& mdl = machine.newModule("testModule");
        mdl.addExport("test", jac::Value::from<std::string>(machine.context(), "test string"));

        evalModuleWithEventLoop(machine, "import * as testModule from 'testModule'; report(testModule.test); exit(1);", "test");

        REQUIRE(machine.getReports() == std::vector<std::string>{"test string"});
    }

    SECTION("Not imported") {
        auto& mdl = machine.newModule("testModule");
        mdl.addExport("test", jac::Value::from<std::string>(machine.context(), "test string"));

        evalModuleWithEventLoop(machine, "report('nothing'); exit(1);", "test");

        REQUIRE(machine.getReports() == std::vector<std::string>{"nothing"});
    }

    SECTION("Two modules") {
        auto& mdl = machine.newModule("testModule1");
        mdl.addExport("test1", jac::Value::from<std::string>(machine.context(), "test string 1"));

        auto& module2 = machine.newModule("testModule2");
        module2.addExport("test2", jac::Value::from<std::string>(machine.context(), "test string 2"));

        evalModuleWithEventLoop(machine, R"(
            import * as testModule1 from 'testModule1';
            import * as testModule2 from 'testModule2';
            report(testModule1.test1);
            report(testModule2.test2);
            exit(1);
        )", "test");

        REQUIRE(machine.getReports() == std::vector<std::string>{"test string 1", "test string 2"});
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
