#include <catch2/catch_test_macros.hpp>
#include <catch2/generators/catch_generators.hpp>

#include <filesystem>
#include <set>
#include <string>

#include <jac/features/evalFeature.h>
#include <jac/features/filesystemFeature.h>
#include <jac/machine/machine.h>
#include <jac/machine/values.h>

#include "util.h"


std::string readFile(auto f) {
    std::string buffer;
    std::string read = f.read();
    while (!read.empty()) {
        buffer += read;
        read = f.read();
    }
    return buffer;
}


TEST_CASE("Filesystem", "[filesystem]") {
    using Machine = jac::ComposeMachine<
        jac::MachineBase,
        jac::EvalFeature,
        jac::FilesystemFeature
    >;
    Machine machine;


    SECTION("code - existing") {
        std::string file("fixtures/fs/test.js");
        std::string expected("report(\"first\")\nreport(\"second\")\n");
        machine.setCodeDir(machine.path.dirname(file));
        machine.initialize();

        REQUIRE(machine.fs.loadCode(machine.path.basename(file)) == expected);
    }

    SECTION("code - not existing") {
        std::string file("not_existing.js");
        machine.setCodeDir(".");
        machine.initialize();

        REQUIRE_THROWS_AS(machine.fs.loadCode(file), jac::Exception);
    }

    SECTION("data - existing") {
        std::string file("fixtures/fs/test.js");
        std::string expected("report(\"first\")\nreport(\"second\")\n");
        machine.setWorkingDir(machine.path.dirname(file));
        machine.initialize();

        REQUIRE(readFile(machine.fs.open(machine.path.basename(file), "r")) == expected);
    }

    SECTION("data - not existing") {
        std::string file("not_existing.js");
        machine.initialize();

        REQUIRE_THROWS_AS(readFile(machine.fs.open(file, "r")), jac::Exception);
    }

    SECTION("data - write") {
        std::string file("fixtures/fs/write.txt");
        if (std::filesystem::exists(file)) {
            std::filesystem::remove(file);
        }
        machine.initialize();

        auto f = machine.fs.open(file, "wt");
        f.write("test");
        f.close();

        REQUIRE(std::filesystem::exists(file));
        REQUIRE(readFile(machine.fs.open(file, "r")) == "test");

        std::filesystem::remove(file);
    }

    SECTION("exists") {
        machine.initialize();

        REQUIRE(machine.fs.exists("fixtures/fs/test.js"));
        REQUIRE(machine.fs.exists("fixtures/fs/test_dir"));
        REQUIRE_FALSE(machine.fs.exists("not_existing.js"));
    }

    SECTION("isFile") {
        machine.initialize();

        REQUIRE(machine.fs.isFile("fixtures/fs/test.js"));
        REQUIRE_FALSE(machine.fs.isFile("fixtures/fs/test_dir"));
        REQUIRE_FALSE(machine.fs.isFile("not_existing.js"));
    }

    SECTION("isDirectory") {
        machine.initialize();

        REQUIRE_FALSE(machine.fs.isDirectory("fixtures/fs/test.js"));
        REQUIRE(machine.fs.isDirectory("fixtures/fs/test_dir"));
        REQUIRE_FALSE(machine.fs.isDirectory("not_existing.js"));
    }

    SECTION("mkdir") {
        std::string dir("fixtures/fs/tomkdir");
        if (std::filesystem::exists(dir)) {
            std::filesystem::remove_all(dir);
        }
        machine.initialize();

        machine.fs.mkdir(dir);

        REQUIRE(std::filesystem::exists(dir));
        REQUIRE(std::filesystem::is_directory(dir));

        std::filesystem::remove_all(dir);
    }

    SECTION("mkdir - recursive") {
        std::string dir("fixtures/fs/tomkdirrec/dir2/dir3");
        if (std::filesystem::exists("fixtures/fs/tomkdirrec")) {
            std::filesystem::remove_all(dir);
        }
        machine.initialize();

        machine.fs.mkdir(dir);

        REQUIRE(std::filesystem::exists(dir));
        REQUIRE(std::filesystem::is_directory(dir));

        std::filesystem::remove_all(dir);
    }

    SECTION("rm") {
        std::string file("fixtures/fs/rmfile.txt");
        machine.initialize();

        machine.fs.rm(file);

        REQUIRE_FALSE(std::filesystem::exists(file));
    }

    SECTION("rmdir") {
        std::string dir("fixtures/fs/tormdir");
        if (!std::filesystem::exists(dir)) {
            std::filesystem::create_directory(dir);
        }
        machine.initialize();

        machine.fs.rmdir(dir);

        REQUIRE_FALSE(std::filesystem::exists(dir));
    }

    SECTION("readdir") {
        std::string dir("fixtures/fs/test_dir");
        machine.setWorkingDir(".");
        machine.initialize();

        auto files = machine.fs.readdir(dir);

        REQUIRE(std::set<std::string>(files.begin(), files.end()) == std::set<std::string>{ "file1", "file2", "subdir" });
    }
}


TEST_CASE("Path", "[filesystem]") {
    using Machine = jac::ComposeMachine<
        jac::MachineBase,
        jac::EvalFeature,
        jac::FilesystemFeature
    >;
    Machine machine;

    SECTION("dirname") {
        REQUIRE(machine.path.dirname("test.js") == ".");
        REQUIRE(machine.path.dirname("test/test.js") == "test");
        REQUIRE(machine.path.dirname("test/test/test.js") == "test/test");
    }

    SECTION("basename") {
        REQUIRE(machine.path.basename("test.js") == "test.js");
        REQUIRE(machine.path.basename("test/test.js") == "test.js");
        REQUIRE(machine.path.basename("test/test/test.js") == "test.js");
    }

    SECTION("join") {
        REQUIRE(machine.path.join({"test", "test.js"}) == "test/test.js");
        REQUIRE(machine.path.join({"test", "test", "test.js"}) == "test/test/test.js");
    }

    SECTION("normalize") {
        REQUIRE(machine.path.normalize("test/test.js") == "test/test.js");
        REQUIRE(machine.path.normalize("test/../test.js") == "test.js");
        REQUIRE(machine.path.normalize("test/./test.js") == "test/test.js");
        REQUIRE(machine.path.normalize("test/../test/../test.js") == "test.js");
        REQUIRE(machine.path.normalize("test/../../test.js") == "../test.js");
        REQUIRE(machine.path.normalize("test/../../test/../test.js") == "../test.js");
        REQUIRE(machine.path.normalize("../test/test.js") == "../test/test.js");
        REQUIRE(machine.path.normalize("../test/../test.js") == "../test.js");
        REQUIRE(machine.path.normalize("test///test.js") == "test/test.js");
        REQUIRE(machine.path.normalize("test/test") == "test/test");
        REQUIRE(machine.path.normalize("test/test/") == "test/test/");
    }
}


TEST_CASE("File class js", "[filesystem]") {
    using Machine = jac::ComposeMachine<
        jac::MachineBase,
        jac::EvalFeature,
        jac::FilesystemFeature,
        TestReportFeature
    >;

    Machine machine;
    machine.setCodeDir("fixtures/fs/");
    machine.setWorkingDir("fixtures/fs/");
    machine.initialize();

    SECTION("read") {
        std::string code("import { open } from 'fs'\n"
                         "var file = open('testRead.txt', 'r');\n"
                         "report(file.read(1024));\n");

        evalCode(machine, code, "test.js", jac::EvalFlags::Module);
        REQUIRE(machine.getReports() == std::vector<std::string> { "test1\ntest2\n" });
    }

    SECTION("write") {
        std::string code("import { open } from 'fs'\n"
                         "var file = open('testWrite.txt', 'w');\n"
                         "file.write('test');\n"
                         "file.close();\n");

        evalCode(machine, code, "test.js", jac::EvalFlags::Module);
        REQUIRE(readFile(machine.fs.open("testWrite.txt", "r")) == "test");
        std::filesystem::remove("fixtures/fs/testWrite.txt");
    }

    SECTION("close") {
        std::string code("import { open } from 'fs'\n"
                         "var file = open('testClose.txt', 'w');\n"
                         "file.write('test');\n"
                         "file.close();\n"
                         "report(file.isOpen());\n");

        evalCode(machine, code, "test.js", jac::EvalFlags::Module);
        REQUIRE(machine.getReports() == std::vector<std::string> { "false" });

        std::filesystem::remove("fixtures/fs/testClose.txt");
    }
}
