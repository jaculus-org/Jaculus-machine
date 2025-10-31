#include <string>

#include <jac/features/filesystemFeature.h>
#include <jac/features/moduleLoaderFeature.h>
#include <jac/machine/machine.h>
#include <jac/machine/values.h>

#ifdef COMPILE_CFG_ONLY
    #include "compiler/compileInterpEvalFeature.h"
#else
    #include <jac/features/aotEvalFeature.h>
#endif

#undef CHECK


#include "util.h"
#include <catch2/catch_test_macros.hpp>
#include <catch2/generators/catch_generators.hpp>


TEST_CASE("Basic", "[aot]") {
    using Machine = jac::ComposeMachine<
        jac::MachineBase,
        jac::AotEvalFeature,
        TestReportFeature
    >;

    Machine machine;

    SECTION("Id") {
        machine.initialize();
        std::string code(R"(
            function fun(a: int32): int32 {
                return a;
            }

            report(fun(1234));
        )");

        evalCode(machine, code, "test", jac::EvalFlags::Global);
        REQUIRE(machine.getReports() == std::vector<std::string>{"1234"});
    }

    SECTION("Simpler") {
        machine.initialize();
        std::string code(R"(
            function fun(a: int32, b: int32): int32 {
                return a + b + 1;
            }

            report(fun(1, 2));
        )");

        evalCode(machine, code, "test", jac::EvalFlags::Global);
        REQUIRE(machine.getReports() == std::vector<std::string>{"4"});
    }

    SECTION("Simple") {
        machine.initialize();
        std::string code(R"(
            function fun(a: int32, b: int32): int32 {
                let c: int32 = a + b + 3;
                c = c * 2;
                c = +(-c);
                return c;
            }

            report(fun(1, 2));
        )");

        evalCode(machine, code, "test", jac::EvalFlags::Global);
        REQUIRE(machine.getReports() == std::vector<std::string>{"-12"});
    }

    SECTION("Many args") {
        machine.initialize();
        std::string code(R"(

            function haha(a: int32, b: int32, c: int32, d: int32, e: int32, f: int32, g: int32, h: int32,
                          i: int32, j: int32, k: int32, l: int32, m: int32, n: int32, o: int32, p: int32,
                          q: int32, r: int32, s: int32, t: int32, u: int32, v: int32, w: int32, x: int32,
                          y: int32, z: int32, aa: int32, ab: int32, ac: int32, ad: int32, ae: int32, af: int32): int32 {
                return a + b + c + d + e + f + g + h
                     + i + j + k + l + m + n + o + p
                     + q + r + s + t + u + v + w + x
                     + y + z + aa + ab + ac + ad + ae + af;
            }

            report(haha(1, -2, 3, -4, 5, -6, 7, -8,
                       9, -10, 11, -12, 13, -14, 15, -16,
                       17, -18, 19, -20, 21, -22, 23, -24,
                       25, -26, 27, -28, 29, -30, 31, -32));
        )");

        evalCode(machine, code, "test", jac::EvalFlags::Global);
        REQUIRE(machine.getReports() == std::vector<std::string>{"-16"});
    }

    SECTION("Two") {
        machine.initialize();
        std::string code(R"(
            function id(a: int32): int32 {
                return a;
            }

            function fun(a: int32, b: int32): int32 {
                return a + b;
            }

            report(id(7));
            report(fun(1, 2));
        )");

        evalCode(machine, code, "test", jac::EvalFlags::Global);
        REQUIRE(machine.getReports() == std::vector<std::string>{"7", "3"});
    }

    SECTION("Call") {
        machine.initialize();
        std::string code(R"(
            function id(a: int32): int32 {
                return a;
            }

            function fun(a: int32, b: int32): int32 {
                return id(a) + id(b);
            }

            report(id(7));
            report(fun(1, 2));
        )");

        evalCode(machine, code, "test", jac::EvalFlags::Global);
        REQUIRE(machine.getReports() == std::vector<std::string>{"7", "3"});
    }

    SECTION("Block") {
        machine.initialize();
        std::string code(R"(
            function fun(a: int32, b: int32): int32 {
                let c: int32 = 0;
                {
                    c = a + b;
                }
                return c;
            }

            report(fun(1, 2));
        )");

        evalCode(machine, code, "test", jac::EvalFlags::Global);
        REQUIRE(machine.getReports() == std::vector<std::string>{"3"});
    }

    SECTION("Shadow") {
        machine.initialize();
        std::string code(R"(
            function fun(a: int32, b: int32): int32 {
                let c: int32 = 0;
                {
                    let c: int32 = a + b;
                    b = c;
                }
                return b;
            }

            report(fun(1, 2));
        )");

        evalCode(machine, code, "test", jac::EvalFlags::Global);
        REQUIRE(machine.getReports() == std::vector<std::string>{"3"});
    }

    SECTION("Dead code") {
        machine.initialize();
        std::string code(R"(
            function fun(): int32 {
                let c: int32 = 0;
                return 1;

                c = 2;
                return c;
            }
            report(fun());
        )");

        evalCode(machine, code, "test", jac::EvalFlags::Global);
        REQUIRE(machine.getReports() == std::vector<std::string>{"1"});
    }

    SECTION("Dead code block") {
        machine.initialize();
        std::string code(R"(
            function fun(): int32 {
                let c: int32 = 0;
                {
                    return 1;
                    c = 2;
                }
                return c;
            }
            report(fun());
        )");

        evalCode(machine, code, "test", jac::EvalFlags::Global);
        REQUIRE(machine.getReports() == std::vector<std::string>{"1"});
    }

    SECTION("Dead code branch") {
        machine.initialize();
        std::string code(R"(
            function fun(): int32 {
                let c: int32 = 0;
                if (c == 0) {
                    return 1;
                    c = 2;
                }
                else {
                    return 3;
                }
                return c;
            }
            report(fun());
        )");

        evalCode(machine, code, "test", jac::EvalFlags::Global);
        REQUIRE(machine.getReports() == std::vector<std::string>{"1"});
    }
}


TEST_CASE("Control flow", "[aot]") {
    using Machine = jac::ComposeMachine<
        jac::MachineBase,
        jac::AotEvalFeature,
        TestReportFeature
    >;

    Machine machine;

    SECTION("If") {
        machine.initialize();
        std::string code(R"(
            function fun(a: int32, b: int32): int32 {
                let c: int32 = 0;
                if (a > b) {
                    c = a;
                }
                else {
                    c = b;
                }
                return c;
            }

            report(fun(1, 2));
            report(fun(4, 3));
        )");

        evalCode(machine, code, "test", jac::EvalFlags::Global);
        REQUIRE(machine.getReports() == std::vector<std::string>{"2", "4"});
    }

    SECTION("Nested if") {
        machine.initialize();
        std::string code(R"(
            function fun(a: int32, b: int32, c: int32): int32 {
                if (a == 0) {
                    if (b > c) {
                        return b;
                    }
                    else {
                        return c;
                    }
                }
                else {
                    return a;
                }
            }

            report(fun(1, 2, 3));
            report(fun(0, 2, 3));
            report(fun(0, 4, 3));
        )");

        evalCode(machine, code, "test", jac::EvalFlags::Global);
        REQUIRE(machine.getReports() == std::vector<std::string>{"1", "3", "4"});
    }

    SECTION("Recursive fib") {
        machine.initialize();
        std::string code(R"(
            function fib(n: int32): int32 {
                if (n == 0 || n == 1) {
                    return n;
                }

                let first: int32 = fib(n - 1);
                let second: int32 = fib(n - 2);
                return first + second;
            }

            report(fib(0));
            report(fib(1));
            report(fib(2));
            report(fib(3));
            report(fib(4));
            report(fib(5));
            report(fib(6));
            report(fib(7));
            report(fib(8));
            report(fib(9));
        )");

        evalCode(machine, code, "test", jac::EvalFlags::Global);
        REQUIRE(machine.getReports() == std::vector<std::string>{"0", "1", "1", "2", "3", "5", "8", "13", "21", "34"});
    }

    SECTION("While") {
        machine.initialize();
        std::string code(R"(
            function pow(a: int32, b: int32): int32 {
                let c: int32 = 1;
                while (b > 0) {
                    c = c * a;
                    b -= 1;
                }

                return c;
            }

            report(pow(2, 8));
        )");

        evalCode(machine, code, "test", jac::EvalFlags::Global);
        REQUIRE(machine.getReports() == std::vector<std::string>{"256"});
    }

    SECTION("Empty while") {
        machine.initialize();
        std::string code(R"(
            function pow(a: int32, b: int32): int32 {
                while (b == 0) {}

                return a;
            }

            report(pow(2, 8));
        )");

        evalCode(machine, code, "test", jac::EvalFlags::Global);
        REQUIRE(machine.getReports() == std::vector<std::string>{"2"});
    }

    SECTION("Do while") {
        machine.initialize();
        std::string code(R"(
            function add(a: int32, b: int32): int32 {
                let c: int32 = 0;
                do {
                    c += a;
                    b = b - 1;
                } while (b > 0);

                return c;
            }

            report(add(2, 8));
            report(add(3, 0));
        )");

        evalCode(machine, code, "test", jac::EvalFlags::Global);
        REQUIRE(machine.getReports() == std::vector<std::string>{"16", "3"});
    }

    SECTION("For") {
        machine.initialize();
        std::string code(R"(
            function mul(a: int32, b: int32): int32 {
                let c: int32 = 0;
                for (let i: int32 = 0; i < b; i += 1) {
                    c = c + a;
                }

                return c;
            }

            report(mul(2, 8));
            report(mul(3, 0));
        )");

        evalCode(machine, code, "test", jac::EvalFlags::Global);
        REQUIRE(machine.getReports() == std::vector<std::string>{"16", "0"});
    }

    SECTION("Iterative fib") {
        machine.initialize();
        std::string code(R"(
            function fib(n: int32): int32 {
                let a: int32 = 0;
                let b: int32 = 1;
                let c: int32 = 0;
                for (let i: int32 = 0; i < n; i++) {
                    c = a + b;
                    a = b;
                    b = c;
                }

                return a;
            }

            report(fib(0));
            report(fib(1));
            report(fib(2));
            report(fib(3));
            report(fib(4));
            report(fib(5));
            report(fib(6));
            report(fib(7));
            report(fib(8));
            report(fib(9));
        )");

        evalCode(machine, code, "test", jac::EvalFlags::Global);
        REQUIRE(machine.getReports() == std::vector<std::string>{"0", "1", "1", "2", "3", "5", "8", "13", "21", "34"});
    }

    SECTION("Early return") {
        machine.initialize();
        std::string code(R"(
            function fun(a: int32, b: int32): int32 {
                if (a > b) {
                    return a;
                }
                else {
                    return b;
                }
            }

            report(fun(1, 2));
            report(fun(4, 3));
        )");

        evalCode(machine, code, "test", jac::EvalFlags::Global);
        REQUIRE(machine.getReports() == std::vector<std::string>{"2", "4"});
    }

    SECTION("Bool conversion") {
        machine.initialize();
        std::string code(R"(
            function pos(a: int32): int32 {
                while (a) {
                    if (a) {
                        for (; a;) {
                            return 1;
                        }
                        return 2;
                    }
                    return 3;
                }
                return 4;
            }
            function neg(a: int32): int32 {
                while (a) {
                    return 1;
                }
                if (a) {
                    return 2;
                }
                for (;a ;) {
                    return 3;
                }
                return 4;
            }

            report(pos(1));
            report(neg(0));
        )");

        evalCode(machine, code, "test", jac::EvalFlags::Global);
        REQUIRE(machine.getReports() == std::vector<std::string>{"1", "4"});
    }

    SECTION("Break") {
        machine.initialize();
        std::string code(R"(
            function fun(a: int32): int32 {
                let c: int32 = 0;
                for (let i: int32 = 0; i < a; ++i) {
                    let j: int32 = 0;
                    while (j < a) {
                        c++;
                        if (j == i) {
                            break;
                        }
                        j += 1;
                    }
                }
                return c;
            }

            report(fun(4));
        )");

        evalCode(machine, code, "test", jac::EvalFlags::Global);
        REQUIRE(machine.getReports() == std::vector<std::string>{"10"});
    }

    SECTION("Continue") {
        machine.initialize();
        std::string code(R"(
            function fun(a: int32): int32 {
                let c: int32 = 0;
                for (let i: int32 = 0; i < a; i++) {
                    let j: int32 = 0;
                    while (j < a) {
                        if (++j == i) {
                            continue;
                        }
                        c++;
                    }
                }
                return c;
            }

            report(fun(4));
        )");

        evalCode(machine, code, "test", jac::EvalFlags::Global);
        REQUIRE(machine.getReports() == std::vector<std::string>{"13"});
    }
}


TEST_CASE("Types", "[aot]") {
    using Machine = jac::ComposeMachine<
        jac::MachineBase,
        jac::AotEvalFeature,
        TestReportFeature
    >;

    Machine machine;

    SECTION("int32") {
        machine.initialize();
        std::string code(R"(
            function fun(a: int32): int32 {
                return a + 1;
            }

            report(fun(1234));
        )");

        evalCode(machine, code, "test", jac::EvalFlags::Global);
        REQUIRE(machine.getReports() == std::vector<std::string>{"1235"});
    }

    SECTION("Float64") {
        machine.initialize();
        std::string code(R"(
            function fun(a: float64): float64 {
                return a + 1.2;
            }

            report(fun(1234.5));
        )");

        evalCode(machine, code, "test", jac::EvalFlags::Global);
        REQUIRE(machine.getReports() == std::vector<std::string>{"1235.7"});
    }

    SECTION("Bool") {
        machine.initialize();
        std::string code(R"(
            function fun(a: boolean): boolean {
                return !a;
            }

            report(fun(true));
            report(fun(false));
        )");

        evalCode(machine, code, "test", jac::EvalFlags::Global);
        REQUIRE(machine.getReports() == std::vector<std::string>{"false", "true"});
    }

    SECTION("Void") {
        machine.initialize();
        std::string code(R"(
            function fun(): void {
                return;
            }

            report(fun());
        )");

        evalCode(machine, code, "test", jac::EvalFlags::Global);
        REQUIRE(machine.getReports() == std::vector<std::string>{"undefined"});
    }

    SECTION("Void called") {
        machine.initialize();
        std::string code(R"(
            function fun(): void {
                return;
            }

            function call(): int32 {
                fun();
                return 1;
            }

            report(call());
        )");

        evalCode(machine, code, "test", jac::EvalFlags::Global);
        REQUIRE(machine.getReports() == std::vector<std::string>{"1"});
    }

    SECTION("Mixed") {
        machine.initialize();
        std::string code(R"(
            function fun(a: int32, b: float64, c: boolean): float64 {
                if (a < b) {
                    return (a + b) + c;
                }
                else {
                    return (a + c) - b;
                }
            }

            report(fun(1, 2.5, false));
            report(fun(3, 2.5, true));
        )");

        evalCode(machine, code, "test", jac::EvalFlags::Global);
        REQUIRE(machine.getReports() == std::vector<std::string>{"3.5", "1.5"});
    }
}


TEST_CASE("Any", "[aot]") {
    using Machine = jac::ComposeMachine<
        jac::MachineBase,
        jac::AotEvalFeature,
        TestReportFeature
    >;

    Machine machine;

    SECTION("Copy") {
        machine.initialize();
        std::string code(R"(
            function fun(a: any): any {
                let b: any = a;
                return b;
            }

            let o = new Object();
            o.x = 1234;
            report(fun(o).x);
        )");

        evalCode(machine, code, "test", jac::EvalFlags::Global);
        REQUIRE(machine.getReports() == std::vector<std::string>{"1234"});
    }

    SECTION("Copy multiple") {
        machine.initialize();
        std::string code(R"(
            function fun(a: any, b: any): any {
                let c: any = a;
                let d: any = b;

                return d;
            }

            let o = new Object();
            o.x = 1234;
            let p = new Object();
            p.x = 5678;
            report(fun(o, p).x);
        )");

        evalCode(machine, code, "test", jac::EvalFlags::Global);
        REQUIRE(machine.getReports() == std::vector<std::string>{"5678"});
    }

    SECTION("Mixed") {
        machine.initialize();
        std::string code(R"(
            function fun(a: int32, b: any): any {
                let c: int32 = a;
                let d: any = b;

                return d;
            }

            let o = new Object();
            o.x = 1234;
            let p = new Object();
            p.x = 5678;
            report(fun(o, p).x);
        )");

        evalCode(machine, code, "test", jac::EvalFlags::Global);
        REQUIRE(machine.getReports() == std::vector<std::string>{"5678"});
    }

    SECTION("Wrap-unwrap") {
        machine.initialize();
        std::string code(R"(
            function toInt32(a: any): int32 { return a; }
            function toFloat(a: any): float64 { return a; }
            function toBool(a: any): boolean { return a; }

            function _fromInt32(a: int32): int32 { return a; }
            function _fromFloat(a: float64): float64 { return a; }
            function _fromBool(a: boolean): boolean { return a; }

            report(toInt32(1234));
            report(toFloat(5678.9));
            report(toBool(true));

            report(_fromInt32(1234));
            report(_fromFloat(5678.9));
            report(_fromBool(false));
        )");

        evalCode(machine, code, "test", jac::EvalFlags::Global);
        REQUIRE(machine.getReports() == std::vector<std::string>{
            "1234", "5678.9", "true",
            "1234", "5678.9", "false"
        });
    }

    SECTION("Call") {
        machine.initialize();
        std::string code(R"(
            function id(a: any): any {
                return a;
            }

            function fun(a: any): any {
                return id(a);
            }

            report(id(7));
            report(fun(2.5));
        )");

        evalCode(machine, code, "test", jac::EvalFlags::Global);
        REQUIRE(machine.getReports() == std::vector<std::string>{"7", "2.5"});
    }

    SECTION("Arithmetic") {
        machine.initialize();
        std::string code(R"(
            function add(a: any, b: any): any {
                return a + b;
            }
            function sub(a: any, b: any): any {
                return a - b;
            }
            function mul(a: any, b: any): any {
                return a * b;
            }
            function div(a: any, b: any): any {
                return a / b;
            }
            function rem(a: any, b: any): any {
                return a % b;
            }

            report(add(1, 2));
            report(add(1.5, 2.5));
            report(add(true, false));

            report(sub(1, 2));
            report(sub(1.5, 2.5));
            report(sub(true, false));

            report(mul(1, 2));

            report(div(1, 2));

            report(rem(5, 2));
        )");

        evalCode(machine, code, "test", jac::EvalFlags::Global);
        REQUIRE(machine.getReports() == std::vector<std::string>{
            "3", "4", "1",
            "-1", "-1", "1",
            "2",
            "0.5",
            "1"
        });
    }

    SECTION("Any-boolean") {
        machine.initialize();
        std::string code(R"(
            function fun(a: any): boolean {
                return a;
            }

            report(fun(1));
            report(fun(0));
            report(fun(true));
            report(fun(false));
        )");

        evalCode(machine, code, "test", jac::EvalFlags::Global);
        REQUIRE(machine.getReports() == std::vector<std::string>{"true", "false", "true", "false"});
    }

    SECTION("Compare") {
        machine.initialize();
        std::string code(R"(
            function lt(a: any, b: any): boolean {
                return a < b;
            }
            function le(a: any, b: any): boolean {
                return a <= b;
            }
            function gt(a: any, b: any): boolean {
                return a > b;
            }
            function ge(a: any, b: any): boolean {
                return a >= b;
            }
            function eq(a: any, b: any): boolean {
                return a == b;
            }
            function ne(a: any, b: any): boolean {
                return a != b;
            }

            report(lt("1", 2));
            report(le("1", 2));
            report(gt("1", 2));
            report(ge("1", 2));
            report(eq("1", 2));
            report(ne("1", 2));
        )");

        evalCode(machine, code, "test", jac::EvalFlags::Global);
        REQUIRE(machine.getReports() == std::vector<std::string>{"true", "true", "false", "false", "false", "true"});
    }

    SECTION("UpdateAny") {
        machine.initialize();
        std::string code(R"(
            function fun(a: any): any {
                return ++a;
            }

            report(fun(1));
            report(fun(2.5));
            report(fun(true));
        )");

        evalCode(machine, code, "test", jac::EvalFlags::Global);
        REQUIRE(machine.getReports() == std::vector<std::string>{"2", "3.5", "2"});
    }

    SECTION("Member get") {
        machine.initialize();
        std::string code(R"(
            function str(a: any): any {
                return a.x;
            }
            function num(a: any): any {
                return a[1];
            }
            function any(a: any, b: any): any {
                return a[b];
            }

            let o = new Object();
            o.x = 1234;
            let p = new Object();
            p[1] = 5678;
            report(str(o));
            report(num(p));
            report(any(o, "x"));
            report(any(p, 1));
        )");

        evalCode(machine, code, "test", jac::EvalFlags::Global);
        REQUIRE(machine.getReports() == std::vector<std::string>{"1234", "5678", "1234", "5678"});
    }

    SECTION("Member set") {
        machine.initialize();
        std::string code(R"(
            function str(a: any): void {
                a.x = 12;
            }
            function num(a: any): void {
                a[1] = 23;
            }
            function any(a: any, b: any): void {
                a[b] = 56;
            }

            let o = new Object();
            str(o);
            num(o);
            any(o, "y");
            any(o, 7);
            report(o.x);
            report(o[1]);
            report(o.y);
            report(o[7]);
        )");

        evalCode(machine, code, "test", jac::EvalFlags::Global);
        REQUIRE(machine.getReports() == std::vector<std::string>{"12", "23", "56", "56"});
    }

    SECTION("string") {
        machine.initialize();
        std::string code(R"(
            function str(): any {
                return "hello";
            }

            report(str());
        )");

        evalCode(machine, code, "test", jac::EvalFlags::Global);
        REQUIRE(machine.getReports() == std::vector<std::string>{"hello"});
    }
}


TEST_CASE("Object", "[aot]") {
    using Machine = jac::ComposeMachine<
        jac::MachineBase,
        jac::AotEvalFeature,
        TestReportFeature
    >;

    Machine machine;

    SECTION("Object") {
        machine.initialize();
        auto code = R"(
            function test(a: object): object {
                let b: object = a;
                let c: object = b;

                return c;
            }

            let o = new Object();
            o.x = 1234;
            report(test(o).x);
        )";

        evalCode(machine, code, "test", jac::EvalFlags::Global);
        REQUIRE(machine.getReports() == std::vector<std::string>{"1234"});
    }

    SECTION("Get member") {
        machine.initialize();
        auto code = R"(
            function test(a: object): any {
                let c: any = a.b;

                return c;
            }

            let o = new Object();
            o.b = 42;
            report(test(o));
        )";

        evalCode(machine, code, "test", jac::EvalFlags::Global);
        REQUIRE(machine.getReports() == std::vector<std::string>{"42"});
    }

    SECTION("Get member brackets") {
        machine.initialize();
        auto code = R"(
            function test(a: object, b: any): any {
                let c: any = a["b"];
                return c;
            }

            let o = new Object();
            o["b"] = 42;
            report(test(o, 42));
        )";

        evalCode(machine, code, "test", jac::EvalFlags::Global);
        REQUIRE(machine.getReports() == std::vector<std::string>{"42"});
    }

    SECTION("Set member") {
        machine.initialize();
        auto code = R"(
            function test(a: object, b: any): void {
                a.b = b;
            }

            let o = new Object();
            test(o, 42);
            report(o.b);
        )";

        evalCode(machine, code, "test", jac::EvalFlags::Global);
        REQUIRE(machine.getReports() == std::vector<std::string>{"42"});
    }

    SECTION("Set member brackets") {
        machine.initialize();
        auto code = R"(
            function test(a: object, b: any): void {
                a["b"] = b;
            }

            let o = new Object();
            test(o, 42);
            report(o.b);
        )";

        evalCode(machine, code, "test", jac::EvalFlags::Global);
        REQUIRE(machine.getReports() == std::vector<std::string>{"42"});
    }

    SECTION("Set member int int") {
        machine.initialize();
        auto code = R"(
            function test(a: object, b: int32): void {
                a[0] = b;
            }

            let o = new Object();
            test(o, 42);
            report(o[0]);
        )";

        evalCode(machine, code, "test", jac::EvalFlags::Global);
        REQUIRE(machine.getReports() == std::vector<std::string>{"42"});
    }

    SECTION("Get member int") {
        machine.initialize();
        auto code = R"(
            function test(a: object): any {
                let c: any = a[0];
                return c;
            }

            let o = new Object();
            o[0] = 42;
            report(test(o));
        )";

        evalCode(machine, code, "test", jac::EvalFlags::Global);
        REQUIRE(machine.getReports() == std::vector<std::string>{"42"});
    }

    SECTION("Set member int") {
        machine.initialize();
        auto code = R"(
            function test(a: object, b: any): void {
                a[0] = b;
            }

            let o = new Object();
            test(o, 42);
            report(o[0]);
        )";

        evalCode(machine, code, "test", jac::EvalFlags::Global);
        REQUIRE(machine.getReports() == std::vector<std::string>{"42"});
    }

    SECTION("Call(void)") {
        machine.initialize();
        auto code = R"(
            function test(a: object): any {
                return a();
            }

            function memFn() {  // no type annotations -> not compiled
                return 42;
            }

            report(test(memFn));
        )";

        evalCode(machine, code, "test", jac::EvalFlags::Global);
        REQUIRE(machine.getReports() == std::vector<std::string>{"42"});
    }

    SECTION("Call member(void)") {
        machine.initialize();
        auto code = R"(
            function test(a: object): any {
                let c: any = a.b();
                return c;
            }

            function memFn() {  // no type annotations -> not compiled
                return 42;
            }

            let o = new Object();
            o.b = memFn;
            report(test(o));
        )";

        evalCode(machine, code, "test", jac::EvalFlags::Global);
        REQUIRE(machine.getReports() == std::vector<std::string>{"42"});
    }

    SECTION("Call with args") {
        machine.initialize();
        auto code = R"(
            function test(a: object, b: int32): any {
                let c: any = a.b(b);
                return c;
            }

            function memFn(a) {  // no type annotations -> not compiled
                return a + 1;
            }

            let o = new Object();
            o.b = memFn;
            report(test(o, 41));
        )";

        evalCode(machine, code, "test", jac::EvalFlags::Global);
        REQUIRE(machine.getReports() == std::vector<std::string>{"42"});
    }

    SECTION("Call this") {
        machine.initialize();
        auto code = R"(
            function test(a: object): any {
                let c: any = a.b();
                return c;
            }

            function memFn() {  // no return type -> not compiled
                return this.a;
            }

            let o = new Object();
            o.a = 42;
            o.b = memFn;
            report(test(o));
        )";

        evalCode(machine, code, "test", jac::EvalFlags::Global);
        REQUIRE(machine.getReports() == std::vector<std::string>{"42"});
    }

    SECTION("Call expression member access") {
        machine.initialize();
        auto code = R"(
            function id(a: object): object {
                return a;
            }

            function test(a: object): any {
                return id(a).b["a"];
            }

            let o = new Object();
            let p = new Object();
            o.b = p;
            p.a = 42;
            report(test(o));
        )";

        evalCode(machine, code, "test", jac::EvalFlags::Global);
        REQUIRE(machine.getReports() == std::vector<std::string>{"42"});
    }

    SECTION("I32 toFixed") {
        machine.initialize();
        auto code = R"(
            function test(a: int32): any {
                return a.toFixed(3);
            }

            report(test(42));
        )";

        evalCode(machine, code, "test", jac::EvalFlags::Global);
        REQUIRE(machine.getReports() == std::vector<std::string>{"42.000"});
    }
}


TEST_CASE("Operators", "[aot]") {
    using Machine = jac::ComposeMachine<
        jac::MachineBase,
        jac::AotEvalFeature,
        TestReportFeature
    >;

    Machine machine;

    SECTION("Arithmetic int32") {
        machine.initialize();
        std::string code(R"(
            function fun(a: int32, b: int32): int32 {
                return a + b - 2 * 4 / 2 % 3;
            }

            report(fun(1, 2));
        )");

        evalCode(machine, code, "test", jac::EvalFlags::Global);
        REQUIRE(machine.getReports() == std::vector<std::string>{"2"});
    }

    SECTION("Arithmetic float64") {
        machine.initialize();
        std::string code(R"(
            function fun(a: float64, b: float64): float64 {
                return a + b - 2.0 * 4.0 / 2.0 % 3.0;
            }

            report(fun(1.5, 2.5));
        )");

        evalCode(machine, code, "test", jac::EvalFlags::Global);
        REQUIRE(machine.getReports() == std::vector<std::string>{"3"});
    }

    SECTION("Unary") {
        machine.initialize();
        std::string code(R"(
            function fun(a: int32): int32 {
                return -a + +a;
            }

            report(fun(1));
        )");

        evalCode(machine, code, "test", jac::EvalFlags::Global);
        REQUIRE(machine.getReports() == std::vector<std::string>{"0"});
    }

    SECTION("Bitwise") {
        machine.initialize();
        std::string code(R"(
            function fun(a: int32, b: int32): int32 {
                return a & b | a ^ b;
            }

            report(fun(1, 3));
        )");

        evalCode(machine, code, "test", jac::EvalFlags::Global);
        REQUIRE(machine.getReports() == std::vector<std::string>{"3"});
    }

    SECTION("Shift") {
        machine.initialize();
        std::string code(R"(
            function fun(a: int32, b: int32): int32 {
                return (a << b) >> 1;
            }

            report(fun(1, 2));
        )");

        evalCode(machine, code, "test", jac::EvalFlags::Global);
        REQUIRE(machine.getReports() == std::vector<std::string>{"2"});
    }

    SECTION("Assignment") {
        machine.initialize();
        std::string code(R"(
            function fun(a: int32, b: int32, c: int32): int32 {
                a += b;
                a -= c;
                a *= b;
                a /= c;
                a %= b;
                a &= b;
                a |= c;
                a ^= b;
                a <<= c;
                a >>= b;
                return a;
            }

            report(fun(1, 2, 3));
        )");

        evalCode(machine, code, "test", jac::EvalFlags::Global);
        REQUIRE(machine.getReports() == std::vector<std::string>{"2"});
    }

    SECTION("Update") {
        machine.initialize();
        std::string code(R"(
            function fun(a: int32, b: int32): int32 {
                a++;
                --b;
                let c: int32 = --a + b++;
                return c;
            }

            report(fun(1, 2));
            report(fun(3, 4));
        )");

        evalCode(machine, code, "test", jac::EvalFlags::Global);
        REQUIRE(machine.getReports() == std::vector<std::string>{"2", "6"});
    }

    SECTION("Short-circuit") {
        machine.initialize();
        std::string code(R"(
            function fun(a: boolean, b: boolean, c: boolean): boolean {
                return a && b || c;
            }

            report(fun(true, false, true));
            report(fun(false, false, false));
            report(fun(false, true, false));
        )");

        evalCode(machine, code, "test", jac::EvalFlags::Global);
        REQUIRE(machine.getReports() == std::vector<std::string>{"true", "false", "false"});
    }

    SECTION("Assignment short-circuit") {
        machine.initialize();
        std::string code(R"(
            function fun(a: boolean, b: boolean, c: boolean): boolean {
                a &&= b;
                c ||= a;
                return c;
            }

            report(fun(true, false, true));
            report(fun(false, false, false));
            report(fun(false, true, false));
        )");

        evalCode(machine, code, "test", jac::EvalFlags::Global);
        REQUIRE(machine.getReports() == std::vector<std::string>{"true", "false", "false"});
    }

    SECTION("Conditional") {
        machine.initialize();
        std::string code(R"(
            function fun(a: boolean, b: int32, c: int32): int32 {
                return a ? b : c;
            }

            report(fun(true, 1, 2));
            report(fun(false, 1, 2));
        )");

        evalCode(machine, code, "test", jac::EvalFlags::Global);
        REQUIRE(machine.getReports() == std::vector<std::string>{"1", "2"});
    }

    SECTION("Complex conditional") {
        machine.initialize();
        std::string code(R"(
            function fun(a: boolean, b: boolean, c: int32, d: int32, e: int32): int32 {
                return a && b ? c : a || b ? d : e;
            }

            report(fun(true, true, 1, 2, 3));
            report(fun(true, false, 1, 2, 3));
            report(fun(false, true, 1, 2, 3));
            report(fun(false, false, 1, 2, 3));
        )");

        evalCode(machine, code, "test", jac::EvalFlags::Global);
        REQUIRE(machine.getReports() == std::vector<std::string>{"1", "2", "2", "3"});
    }

    SECTION("Exponentiation") {
        machine.initialize();
        std::string code(R"(
            function funInt(a: int32, b: int32): int32 {
                return a ** b;
            }
            function funFloat(a: float64, b: float64): float64 {
                return a ** b;
            }
            function funAny(a: any, b: any): any {
                return a ** b;
            }

            report(funInt(2, 3));
            report(funFloat(2.25, 0.5));
            report(funAny(2, 3));
            report(funAny(9, 0.5));
        )");

        evalCode(machine, code, "test", jac::EvalFlags::Global);
        REQUIRE(machine.getReports() == std::vector<std::string>{
            "8", "1.5", "8", "3"
        });
    }

    SECTION("New") {
        machine.initialize();
        std::string code(R"(
            function fun(ctor: any): any {
                return new ctor("test");
            }

            report(fun(Error).message);
        )");

        evalCode(machine, code, "test", jac::EvalFlags::Global);
        REQUIRE(machine.getReports() == std::vector<std::string>{"test"});
    }

    SECTION("New argless") {
        machine.initialize();
        std::string code(R"(
            function fun(ctor: any): any {
                return new ctor;
            }

            report(fun(Error).message);
        )");

        evalCode(machine, code, "test", jac::EvalFlags::Global);
        REQUIRE(machine.getReports() == std::vector<std::string>{""});
    }
}


TEST_CASE("Exceptions", "[aot]") {
    using Machine = jac::ComposeMachine<
        jac::MachineBase,
        jac::AotEvalFeature,
        TestReportFeature
    >;

    Machine machine;

    SECTION("Pass") {
        machine.initialize();
        std::string code(R"(
            function fun(x: any): void {
                x();
            }

            function throws(): void {
                throw new Error("test");
            }

            fun(throws);
        )");

        evalCodeThrows(machine, code, "test", jac::EvalFlags::Global);
    }

    SECTION("Invalid conversion") {
        machine.initialize();
        std::string code(R"(
            function fun(x: any): void {
                let a: object = x;
            }

            fun(42);
        )");

        evalCodeThrows(machine, code, "test", jac::EvalFlags::Global);
    }

    SECTION("Throw") {
        machine.initialize();
        std::string code(R"(
            function fun(x: any): void {
                throw x;
            }

            fun(42);
        )");

        evalCodeThrows(machine, code, "test", jac::EvalFlags::Global);
    }

    SECTION("I32 div by zero") {
        machine.initialize();
        std::string code(R"(
            function fun(x: int32): int32 {
                return 42 / x;
            }

            fun(0);
        )");

        evalCodeThrows(machine, code, "test", jac::EvalFlags::Global);
    }

    SECTION("I32 rem by zero") {
        machine.initialize();
        std::string code(R"(
            function fun(x: int32): int32 {
                return 42 % x;
            }

            fun(0);
        )");

        evalCodeThrows(machine, code, "test", jac::EvalFlags::Global);
    }
}
