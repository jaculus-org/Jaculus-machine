#include <catch2/catch_test_macros.hpp>
#include <catch2/generators/catch_generators.hpp>

#include <string>

#include <jac/features/filesystemFeature.h>
#include <jac/features/simpleModuleLoaderFeature.h>
#include <jac/machine/machine.h>
#include <jac/machine/values.h>

#include "util.h"


TEST_CASE("New function", "[functionFactory]") {
    using Machine =
        TestReportFeature<
        jac::MachineBase
    >;

    Machine machine;
    machine.initialize();
    jac::Object global = machine.context().getGlobalObject();

    jac::FunctionFactory ff(machine.context());

    SECTION("void(void)") {
        auto f = ff.newFunction([]() {});
        REQUIRE_NOTHROW(f.call<void>());
    }

    SECTION("int(void)") {
        auto f = ff.newFunction([]() { return 42; });
        REQUIRE(f.call<int>() == 42);
    }

    SECTION("int(int)") {
        auto f = ff.newFunction([](int a) { return a; });
        REQUIRE(f.call<int>(42) == 42);
    }

    SECTION("int(int, int)") {
        auto f = ff.newFunction([](int a, int b) { return a + b; });
        REQUIRE(f.call<int>(40, 2) == 42);
    }

    SECTION("int(std::string)") {
        auto f = ff.newFunction([](std::string a) { return static_cast<int>(a.size()); });
        REQUIRE(f.call<int>("test") == 4);
    }

    SECTION("Value(Function))") {
        auto f = ff.newFunction([](jac::Function func) { return func.call<void>("testReport"); });

        global.defineProperty("f", f);

        evalCode(machine, "f(report);", "test", jac::EvalFlags::Global);

        REQUIRE(machine.getReports() == std::vector<std::string>{"testReport"});
    }

    SECTION("void(void) - std::function") {
        int a = 0;
        auto f = ff.newFunction(std::function<void()>([&a]() { a++; }));
        f.call<void>();
        f.call<void>();
        f.call<void>();

        REQUIRE(a == 3);
    }
}


TEST_CASE("New function variadic", "[functionFactory]") {
    using Machine =
        TestReportFeature<
        jac::MachineBase
    >;

    Machine machine;
    machine.initialize();

    jac::FunctionFactory ff(machine.context());

    SECTION("void(args)") {
        auto f = ff.newFunctionVariadic([](std::vector<jac::ValueWeak> args) {});
        REQUIRE_NOTHROW(f.call<void>());
    }

    SECTION("int(args) - count") {
        auto f = ff.newFunctionVariadic([](std::vector<jac::ValueWeak> args) { return static_cast<int>(args.size()); });
        REQUIRE(f.call<int>(1, 2, 3) == 3);
    }

    SECTION("int(args) - values") {
        auto f = ff.newFunctionVariadic([](std::vector<jac::ValueWeak> args) {
            int sum = 0;
            for (auto& arg : args) {
                sum += arg.to<int>();
            }
            return sum;
        });
        REQUIRE(f.call<int>(1, 2, 3) == 6);
    }
}


TEST_CASE("New function this", "[functionFactory]") {
    using Machine =
        TestReportFeature<
        jac::MachineBase
    >;

    Machine machine;
    machine.initialize();

    jac::FunctionFactory ff(machine.context());

    SECTION("void(void)") {
        auto f = ff.newFunctionThis([](jac::ContextRef ctx, jac::ValueWeak thisValue) {
            thisValue.to<jac::Object>().set("test", jac::Value::from(ctx, 42));
        });
        auto obj = jac::Object::create(machine.context());
        f.callThis<void>(obj);

        REQUIRE(obj.get<int>("test") == 42);
    }

    SECTION("int(void)") {
        auto f = ff.newFunctionThis([](jac::ContextRef, jac::ValueWeak thisValue) {
            return thisValue.to<jac::Object>().get<int>("test");
        });
        auto obj = jac::Object::create(machine.context());
        obj.set("test", jac::Value::from(machine.context(), 42));

        REQUIRE(f.callThis<int>(obj) == 42);
    }

    SECTION("int(int)") {
        auto f = ff.newFunctionThis([](jac::ContextRef, jac::ValueWeak thisValue, int a) {
            auto obj = thisValue.to<jac::Object>();
            obj.set("test", obj.get<int>("test") + a);
            return obj.get<int>("test");
        });
        auto obj = jac::Object::create(machine.context());
        obj.set("test", jac::Value::from(machine.context(), 40));

        REQUIRE(f.callThis<int>(obj, 2) == 42);
        REQUIRE(obj.get<int>("test") == 42);
    }

    SECTION("int(int, int)") {
        auto f = ff.newFunctionThis([](jac::ContextRef, jac::ValueWeak thisValue, int a, int b) {
            auto obj = thisValue.to<jac::Object>();
            obj.set("test", (obj.get<int>("test") + a) * b);
            return obj.get<int>("test") * 10;
        });
        auto obj = jac::Object::create(machine.context());
        obj.set("test", jac::Value::from(machine.context(), 20));

        REQUIRE(f.callThis<int>(obj, 1, 2) == 420);
        REQUIRE(obj.get<int>("test") == 42);
    }
}


TEST_CASE("New function this variadic", "[functionFactory]") {
    using Machine =
        TestReportFeature<
        jac::MachineBase
    >;

    Machine machine;
    machine.initialize();

    jac::FunctionFactory ff(machine.context());

    SECTION("void(args)") {
        auto f = ff.newFunctionThisVariadic([](jac::ContextRef ctx, jac::ValueWeak thisValue, std::vector<jac::ValueWeak> argv) {
            auto obj = thisValue.to<jac::Object>();
            obj.set("test", jac::Value::from(ctx, static_cast<int>(argv.size())));
        });
        auto obj = jac::Object::create(machine.context());
        f.callThis<void>(obj, 1, 2, 3);

        REQUIRE(obj.get<int>("test") == 3);
    }

    SECTION("string(args)") {
        auto f = ff.newFunctionThisVariadic([](jac::ContextRef, jac::ValueWeak thisValue, std::vector<jac::ValueWeak> argv) {
            auto obj = thisValue.to<jac::Object>();
            auto result = obj.get<std::string>("prefix");
            for (auto& arg : argv) {
                result += arg.to<std::string>();
            }
            return result;
        });
        auto obj = jac::Object::create(machine.context());
        obj.set<std::string>("prefix", "__");
        auto result = f.callThis<std::string>(obj, "a", "b", "c");

        REQUIRE(result == "__abc");
    }
}
