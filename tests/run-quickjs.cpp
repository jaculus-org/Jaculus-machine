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

#include "test262/harness.h"


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
    Test262HarnessFeature,
    jac::EventLoopTerminal
>;


Test262Result runQuickJsFile(const std::string& path, bool isModule,
                             const std::string& harnessDir, const DumpConfig& dump) {
    Machine machine;
    jac::initializeIo(machine);
    machine.setHarnessDir(harnessDir);
    if (isModule) {
        machine.setCodeDir(std::filesystem::path(path).parent_path().string());
    }
    machine.initialize();

    try {
        machine.loadHarness();

        auto code = loadFile(path);
        auto filename = std::filesystem::path(path).filename().string();
        jac::EvalFlags flags = isModule ? jac::EvalFlags::Module : jac::EvalFlags::Global;
        auto bytecode = machine.eval(code, filename, flags | jac::EvalFlags::CompileOnly);

        if (dump.bc) {
            size_t size = 0;
            uint8_t* data = JS_WriteObject(machine.context(), &size, bytecode.getVal(),
                                           JS_WRITE_OBJ_BYTECODE);
            if (data == nullptr) {
                throw machine.context().getException();
            }
            std::vector<uint8_t> dataCopy(data, data + size);
            js_free(machine.context(), data);
            dumpBytecode(dataCopy, path, dump);
        }

        jac::Value evalResult(machine.context(),
                              JS_EvalFunction(machine.context(), bytecode.loot().second));

        drainJobs(machine);

        if (machine.isAsyncDone()) {
            return machine.getAsyncResult();
        }
        return makeResult("pass", "runtime");
    }
    catch (jac::Exception& e) {
        return classifyException(e);
    }
    catch (std::exception& e) {
        return makeResult("fail", "runtime", e.what(), "Error");
    }
}


int main(const int argc, const char* argv[]) {
    return runnerMain(argc, argv, runQuickJsFile);
}
