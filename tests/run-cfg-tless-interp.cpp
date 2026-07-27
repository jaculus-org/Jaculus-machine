#include <optional>

#include <jac/features/basicStreamFeature.h>
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

#include "compiler/compileTlessInterpEvalFeature.h"
#include "test262/harness.h"


using Machine = jac::ComposeMachine<
    jac::MachineBase,
    jac::EventQueueFeature,
    jac::AotEvalFeature,
    jac::BasicStreamFeature,
    jac::StdioFeature,
    jac::EventLoopFeature,
    jac::FilesystemFeature,
    jac::ModuleLoaderFeature,
    jac::TimersFeature,
    Test262HarnessFeature,
    jac::EventLoopTerminal
>;


namespace {

Test262Result runEvalFileTless(const std::string& path, bool isModule,
                               const std::string& harnessDir, const DumpConfig&) {
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

        jac::Value evalResult = machine.eval(std::move(code), std::move(filename), isModule ? jac::EvalFlags::Module : jac::EvalFlags::Global);

        if (!isModule && JS_IsException(evalResult.getVal())) {
            throw jac::ContextRef(machine.context()).getException();
        }

        std::optional<jac::Exception> rootError;
        if (isModule) {
            jac::FunctionFactory ff(machine.context());
            auto fail = ff.newFunction(noal::function([&rootError, &machine](jac::Value err) {
                rootError = err.to<jac::Exception>();
                machine.kill();
            }));
            auto catch_ = evalResult.to<jac::ObjectWeak>().get<jac::Function>("catch");
            catch_.callThis<void>(evalResult, fail);
        }

        machine.runEventLoop();
        drainJobs(machine);

        if (rootError) {
            throw *rootError;
        }

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

    return makeResult("fail", "runtime", "Unexpected end of function", "Error");
}

}  // namespace


int main(const int argc, const char* argv[]) {
    return runnerMain(argc, argv, runEvalFileTless);
}
