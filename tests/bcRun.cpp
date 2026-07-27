#include "jac/machine/compiler/ast.h"
#include "jac/machine/compiler/bcWriter.h"
#include "quickjs.h"
#include "util.h"
#include <cstddef>
#include <iostream>

#include <jac/features/basicStreamFeature.h>
#include <jac/features/evalFeature.h>
#include <jac/features/eventLoopFeature.h>
#include <jac/features/eventQueueFeature.h>
#include <jac/features/filesystemFeature.h>
#include <jac/features/moduleLoaderFeature.h>
#include <jac/features/stdioFeature.h>
#include <jac/features/timersFeature.h>
#include <jac/features/util/ostreamjs.h>
#include <jac/machine/class.h>
#include <jac/machine/machine.h>
#include <jac/machine/values.h>


using Machine = jac::ComposeMachine<
    jac::MachineBase,
    jac::EvalFeature,
    jac::EventQueueFeature,
    jac::BasicStreamFeature,
    jac::StdioFeature,
    jac::EventLoopFeature,
    jac::FilesystemFeature,
    jac::ModuleLoaderFeature,
    jac::TimersFeature,
    jac::EventLoopTerminal,
    TestReportFeature
>;


int main(const int argc, const char* argv[]) {
    // --path <file> --out <file>

    std::string path{"/home/xkubica1/jaculus/machine/cust.qbc"};
    std::vector<std::pair<std::string_view, std::string_view>> defines;

    for (int i = 1; i < argc; ++i) {
        std::string_view arg(argv[i]);

        if (arg == "--path") {
            if (i + 1 >= argc) {
                std::cerr << "Missing argument for --path" << std::endl;
                return 1;
            }
            path = argv[++i];
        }
        else {
            std::cerr << "Unknown argument: " << arg << std::endl;
            return 1;
        }
    }

    if (path.empty()) {
        std::cerr << "Path is required" << std::endl;
        return 1;
    }

    std::vector<uint8_t> code;
    {
        if (!std::filesystem::exists(path)) {
            std::cerr << "File does not exist: " << path << std::endl;
            return 1;
        }
        if (!std::filesystem::is_regular_file(path)) {
            std::cerr << "Not a file: " << path << std::endl;
            return 1;
        }
        std::ifstream file(path, std::ios::binary);
        if (!file || !file.is_open()) {
            std::cerr << "Failed to open file: " << path << std::endl;
            return 1;
        }

        file.seekg(0, std::ios::end);
        code.resize(file.tellg());
        file.seekg(0, std::ios::beg);
        file.read(reinterpret_cast<char*>(code.data()), code.size());  // NOLINT
    }

    Machine machine;
    initializeIo(machine);
    machine.initialize();
    try {
        auto res = JS_EvalFunction(machine.context(), JS_ReadObject(machine.context(), code.data(), code.size(), JS_READ_OBJ_BYTECODE));

        auto val = jac::Value(machine.context(), res);
        // std::string js = R"( console.log(res); )";
        // machine.context().getGlobalObject().defineProperty("res", val);
        // machine.eval(js, "main.js", jac::EvalFlags::Global);

        for (const auto& report : machine.getReports()) {
            std::cout << "Report: " << report << std::endl;
        }
    } catch (jac::Exception& e) {
        std::cout << "Exception: " << e.what() << std::endl;
        std::cout << "Stack: " << e.stackTrace() << std::endl;
    } catch (std::exception& e) {
        std::cout << "Exception: " << e.what() << std::endl;
    }
}
