#include <catch2/catch_test_macros.hpp>
#include <catch2/generators/catch_generators.hpp>

#include <string>

#include <jac/features/evalFeature.h>
#include <jac/features/eventLoopFeature.h>
#include <jac/features/eventQueueFeature.h>
#include <jac/features/filesystemFeature.h>
#include <jac/features/moduleLoaderFeature.h>
#include <jac/features/timersFeature.h>
#include <jac/machine/machine.h>
#include <jac/machine/values.h>

#include "util.h"


TEST_CASE("Imported promise", "[moduleLoader]") {
    using Machine = jac::ComposeMachine<
        jac::MachineBase,
        jac::EvalFeature,
        TestReportFeature,
        jac::EventQueueFeature,
        jac::EventLoopFeature,
        jac::FilesystemFeature,
        jac::ModuleLoaderFeature,
        jac::TimersFeature,
        jac::EventLoopTerminal
    >;

    Machine machine;

    machine.setCodeDir("fixtures/regression/importPromise");
    machine.initialize();

    evalFile(machine, "main.js");
    machine.runEventLoop();

    REQUIRE(machine.getReports() == std::vector<std::string> { "before", "after", "then" });
}


TEST_CASE("Timer leak", "[timers]") {
    using Machine = jac::ComposeMachine<
        jac::MachineBase,
        jac::EvalFeature,
        TestReportFeature,
        jac::EventQueueFeature,
        jac::EventLoopFeature,
        jac::TimersFeature,
        jac::EventLoopTerminal
    >;

    Machine machine;

    machine.initialize();

    machine.eval("(async () => { for (let i = 0; i < 100000; i++) { await sleep(0); }; exit(0); })()", "<eval>");

    machine.runEventLoop();

    REQUIRE(machine.getReports().empty());
}
