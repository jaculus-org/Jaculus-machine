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

#include "compiler/compileInterpEvalFeature.h"
#include "test262/harness.h"


using Machine = jac::ComposeMachine<
    jac::MachineBase,
    jac::AotEvalFeature,
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


int main(const int argc, const char* argv[]) {
    return runnerMain(argc, argv, runEvalFile<Machine>);
}
