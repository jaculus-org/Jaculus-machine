#include <catch2/catch_test_macros.hpp>
#include <catch2/generators/catch_generators.hpp>

#include <string>

#include <jac/features/filesystemFeature.h>
#include <jac/features/simpleModuleLoaderFeature.h>
#include <jac/machine/class.h>
#include <jac/machine/machine.h>
#include <jac/machine/values.h>

#include "util.h"


TEST_CASE("Class", "[class]") {
    using Machine =
        TestReportFeature<
        jac::MachineBase
    >;

    Machine machine;
    machine.initialize();
    jac::Object global = machine.context().getGlobalObject();

    SECTION("Empty") {
        struct ClassBuilder {};
        using TestClass = jac::Class<ClassBuilder>;
        TestClass::init("TestClass", false);

        auto ctor = TestClass::getConstructor(machine.context());

        global.defineProperty("TestClass", ctor);

        auto result = evalCode(machine, "new TestClass()", "test", jac::EvalFlags::Global);
        auto obj = result.to<jac::Object>();
        REQUIRE(obj.getPrototype().get<jac::Object>("constructor").get<std::string>("name") == "TestClass");
    }

    SECTION("Properties") {
        struct ClassBuilder : jac::ProtoBuilder::Properties {
            static void addProperties(jac::ContextRef ctx, jac::Object proto) {
                proto.defineProperty("testProp", jac::Value::from(ctx, 42));
                jac::FunctionFactory ff(ctx);
                proto.defineProperty("testMethod", ff.newFunction([](int a, int b) { return a + b; }));
            }
        };
        using TestClass = jac::Class<ClassBuilder>;
        TestClass::init("TestClass", false);

        auto ctor = TestClass::getConstructor(machine.context());

        global.defineProperty("TestClass", ctor);

        auto result = evalCode(machine, "let val = new TestClass(); report(val.testProp); report(val.testMethod(1, 2)); val;", "test", jac::EvalFlags::Global);
        auto obj = result.to<jac::Object>();
        REQUIRE(obj.getPrototype().get<jac::Object>("constructor").get<std::string>("name") == "TestClass");
        REQUIRE(obj.get("testProp").to<int>() == 42);
        REQUIRE(obj.get("testMethod").to<jac::Function>().call<int>(1, 2) == 3);

        REQUIRE(machine.getReports() == std::vector<std::string>{"42", "3"});
    }

    SECTION("Callable function") {
        struct ClassBuilder : jac::ProtoBuilder::Callable {
            static jac::Value callFunction(jac::ContextRef ctx, jac::ValueWeak, jac::ValueWeak, std::vector<jac::ValueWeak> args) {
                return jac::Value::from(ctx, static_cast<int>(args.size()));
            }
        };
        using TestClass = jac::Class<ClassBuilder>;
        TestClass::init("TestClass", false);

        auto ctor = TestClass::getConstructor(machine.context());

        global.defineProperty("TestClass", ctor);

        auto result = evalCode(machine, "let val = new TestClass(); report(val(1, 2, 3));", "test", jac::EvalFlags::Global);
        evalCodeThrows(machine, "let val = new TestClass(); report(new val(1, 2, 3));", "test", jac::EvalFlags::Global);

        REQUIRE(machine.getReports() == std::vector<std::string>{"3"});
    }

    SECTION("Callable constructor") {
        struct ClassBuilder : jac::ProtoBuilder::Callable {
            static jac::Value callConstructor(jac::ContextRef ctx, jac::ValueWeak, jac::ValueWeak, std::vector<jac::ValueWeak> args) {
                return jac::Value::from(ctx, static_cast<int>(args.size()));
            }
        };
        using TestClass = jac::Class<ClassBuilder>;
        TestClass::init("TestClass", true);

        auto ctor = TestClass::getConstructor(machine.context());

        global.defineProperty("TestClass", ctor);

        auto result = evalCode(machine, "let val = new TestClass(); report(new val(1, 2, 3));", "test", jac::EvalFlags::Global);
        evalCodeThrows(machine, "let val = new TestClass(); report(val(1, 2, 3));", "test", jac::EvalFlags::Global);

        REQUIRE(machine.getReports() == std::vector<std::string>{"3"});
    }

    SECTION("Callable function, constructor") {
        struct ClassBuilder : jac::ProtoBuilder::Callable {
            static jac::Value callFunction(jac::ContextRef ctx, jac::ValueWeak, jac::ValueWeak, std::vector<jac::ValueWeak> args) {
                return jac::Value::from(ctx, static_cast<int>(args.size()));
            }

            static jac::Value callConstructor(jac::ContextRef ctx, jac::ValueWeak, jac::ValueWeak, std::vector<jac::ValueWeak> args) {
                return jac::Value::from(ctx, static_cast<int>(args.size()));
            }
        };
        using TestClass = jac::Class<ClassBuilder>;
        TestClass::init("TestClass", true);

        auto ctor = TestClass::getConstructor(machine.context());

        global.defineProperty("TestClass", ctor);

        auto result = evalCode(machine, "let val = new TestClass(); report(val(1, 2, 3)); report(new val(1, 2, 3));", "test", jac::EvalFlags::Global);
        REQUIRE(machine.getReports() == std::vector<std::string>{"3", "3"});
    }

    SECTION("Opaque, not constructible") {
        static int aStatic = 0;
        struct Opq {
            int a = 42;

            void setStatic() {
                aStatic = a;
            }

            int add(int b) {
                return a + b;
            }

            int countArgs(std::vector<jac::ValueWeak> args) {
                return static_cast<int>(args.size());
            }
        };

        struct ClassBuilder : jac::ProtoBuilder::Opaque<Opq>, jac::ProtoBuilder::Properties {
            static void addProperties(jac::ContextRef ctx, jac::Value proto) {
                addPropMember<int, &Opq::a>(ctx, proto, "a");
                addMethodMember<decltype(&Opq::setStatic), &Opq::setStatic>(ctx, proto, "setStatic");
                addMethodMember<decltype(&Opq::add), &Opq::add>(ctx, proto, "add");
            }
        };

        using TestClass = jac::Class<ClassBuilder>;
        TestClass::init("TestClass", false);

        auto instance = TestClass::createInstance(machine.context(), new Opq());
        global.defineProperty("val", instance);

        auto result = evalCode(machine, R"(
            report(val.a);
            val.a = 43;
            report(val.a);
            val.setStatic();
            report(val.add(1));
        )","test", jac::EvalFlags::Global);
        REQUIRE(machine.getReports() == std::vector<std::string>{"42", "43", "44"});
        REQUIRE(aStatic == 43);
    }

    SECTION("Opaque, constructible") {
        static int aStatic = 0;
        struct Opq {
            int _a;
            Opq(int a) : _a(a) {}

            void setStatic() {
                aStatic = _a;
            }
        };

        struct ClassBuilder : jac::ProtoBuilder::Opaque<Opq>, jac::ProtoBuilder::Properties, jac::ProtoBuilder::Callable {
            static Opq* constructOpaque(jac::ContextRef, std::vector<jac::ValueWeak> args) {
                if (args.size() != 1) {
                    throw jac::Exception::create(jac::Exception::Type::TypeError, "invalid number of arguments");
                }
                return new Opq(args[0].to<int>());
            }

            static void addProperties(jac::ContextRef ctx, jac::Value proto) {
                addPropMember<int, &Opq::_a>(ctx, proto, "a");
                addMethodMember<decltype(&Opq::setStatic), &Opq::setStatic>(ctx, proto, "setStatic");
            }
        };

        using TestClass = jac::Class<ClassBuilder>;
        TestClass::init("TestClass", true);

        auto ctor = TestClass::getConstructor(machine.context());

        global.defineProperty("TestClass", ctor);

        auto result = evalCode(machine, "let val = new TestClass(7); report(val.a); val.a = 43; report(val.a); val.setStatic();", "test", jac::EvalFlags::Global);
        REQUIRE(machine.getReports() == std::vector<std::string>{"7", "43"});
        REQUIRE(aStatic == 43);
    }

    SECTION("Opaque, constructible, callable") {
        static int calls = 0;
        struct Opq {
            int _a;
            Opq(int a) : _a(a) {}

            int operator()(int b) {
                calls++;
                return _a + b;
            }
        };

        struct ClassBuilder : jac::ProtoBuilder::Opaque<Opq>, jac::ProtoBuilder::Properties, jac::ProtoBuilder::Callable {
            static Opq* constructOpaque(jac::ContextRef, std::vector<jac::ValueWeak> args) {
                if (args.size() != 1) {
                    throw jac::Exception::create(jac::Exception::Type::TypeError, "invalid number of arguments");
                }
                return new Opq(args[0].to<int>());
            }

            static jac::Value callFunction(jac::ContextRef ctx, jac::ValueWeak funcObj, jac::ValueWeak thisVal, std::vector<jac::ValueWeak> args) {
                if (args.size() != 1) {
                    throw jac::Exception::create(jac::Exception::Type::TypeError, "invalid number of arguments");
                }
                return callMember<decltype(&Opq::operator()), &Opq::operator()>(ctx, funcObj, thisVal, args);
            }

            static void addProperties(jac::ContextRef ctx, jac::Value proto) {
                addPropMember<int, &Opq::_a>(ctx, proto, "a");
            }
        };

        using TestClass = jac::Class<ClassBuilder>;
        TestClass::init("TestClass", true);

        auto ctor = TestClass::getConstructor(machine.context());

        global.defineProperty("TestClass", ctor);

        auto result = evalCode(machine, R"(
            let val = new TestClass(7);
            report(val(1));
            report(val(2));
            report(val.a);
            val.a = 43;
            report(val(3));
        )", "test", jac::EvalFlags::Global);
        REQUIRE(machine.getReports() == std::vector<std::string>{"8", "9", "7", "46"});
        REQUIRE(calls == 3);
    }

    SECTION("Access opaque") {
        struct Opq {
            std::string prefix = "__";
        };

        struct ClassBuilder : jac::ProtoBuilder::Opaque<Opq>, jac::ProtoBuilder::Properties {
            static void addProperties(jac::ContextRef ctx, jac::Object proto) {
                addPropMember<std::string, &Opq::prefix>(ctx, proto, "prefix");
                jac::FunctionFactory ff(ctx);
                proto.defineProperty("pref", ff.newFunctionThisVariadic([](jac::ContextRef _ctx, jac::ValueWeak thisVal, std::vector<jac::ValueWeak> args) {
                    auto opq = getOpaque(_ctx, thisVal);
                    std::string result = opq->prefix;
                    for (auto& arg : args) {
                        result += arg.to<std::string>();
                    }
                    return jac::Value::from(_ctx, result);
                }));
            }
        };

        using TestClass = jac::Class<ClassBuilder>;
        TestClass::init("TestClass", false);

        auto instance = TestClass::createInstance(machine.context(), new Opq());
        global.defineProperty("val", instance);

        auto result = evalCode(machine, R"(
            report(val.prefix);
            val.prefix = "pre";
            report(val.prefix);
            report(val.pref("fix", "ed"));
        )", "test", jac::EvalFlags::Global);
        REQUIRE(machine.getReports() == std::vector<std::string>{"__", "pre", "prefixed"});
    }

    SECTION("postConstruction + lifetime extension") {
        struct Owner {
            std::vector<jac::Object> objects;
            std::vector<std::string> reports;
        };

        static Owner theOwner;

        struct Opq {
            Owner* owner;
            int a;

            Opq(int a_) : owner(&theOwner), a(a_) {
                owner->reports.emplace_back("constructed");
            }

            ~Opq() {
                owner->reports.emplace_back("destroyed");
            }
        };

        struct ClassBuilder : jac::ProtoBuilder::Opaque<Opq>, jac::ProtoBuilder::LifetimeHandles {
            static Opq* constructOpaque(jac::ContextRef, std::vector<jac::ValueWeak> args) {
                if (args.size() != 1) {
                    throw jac::Exception::create(jac::Exception::Type::TypeError, "invalid number of arguments");
                }
                int a = args[0].to<int>();
                return new Opq(a);
            }

            static void postConstruction(jac::ContextRef ctx, jac::Object thisVal, std::vector<jac::ValueWeak>) {
                auto& opq = *getOpaque(ctx, thisVal);
                opq.owner->objects.push_back(thisVal);

                opq.owner->reports.emplace_back("postConstructed");
            }
        };

        using TestClass = jac::Class<ClassBuilder>;
        TestClass::init("TestClass", false);

        Opq* opq = nullptr;
        {
            // Create instance
            auto instance = TestClass::getConstructor(machine.context()).callConstructor(42);
            global.set("val", instance);

            REQUIRE(theOwner.reports == std::vector<std::string>{ "constructed", "postConstructed" });
            theOwner.reports.clear();

            // Save pointer - valid only as long as the instance is alive
            opq = ClassBuilder::getOpaque(machine.context(), instance);

            // Forget the instance
            global.set("val", jac::Value::undefined(machine.context()));

        }
        // The instance is still alive, but not reachable from JS
        REQUIRE(theOwner.reports.empty());
        REQUIRE(opq->a == 42);
        opq->a = 43;

        theOwner.reports.clear();
        theOwner.objects.clear();

        // Run GC, to make sure the instance is destroyed
        JS_RunGC(JS_GetRuntime(machine.context()));

        REQUIRE(theOwner.reports == std::vector<std::string>{ "destroyed" });
    }
}
