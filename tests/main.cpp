#include <iostream>
#include <string>

#include <jac/features/basicStreamFeature.h>
#include <jac/features/eventLoopFeature.h>
#include <jac/features/eventQueueFeature.h>
#include <jac/features/filesystemFeature.h>
#include <jac/features/simpleModuleLoaderFeature.h>
#include <jac/features/stdioFeature.h>
#include <jac/features/timersFeature.h>
#include <jac/features/util/ostreamjs.h>
#include <jac/machine/class.h>
#include <jac/machine/machine.h>
#include <jac/machine/values.h>


using Machine = jac::ComposeMachine<
    jac::MachineBase,
    jac::EventQueueFeature,
    jac::BasicStreamFeature,
    jac::StdioFeature,
    jac::EventLoopFeature,
    jac::FilesystemFeature,
    jac::SimpleModuleLoaderFeature,
    jac::TimersFeature,
    jac::EventLoopTerminal
>;

const char* code = R"(
console.log("Hello world!");

exit(0);
)";


int main() {
    std::cout << "Start" << std::endl;

    Machine machine;
    initializeIo(machine);
    machine.initialize();
    try {
        auto val = machine.eval(code, "./main.js", jac::EvalFlags::Module);
        machine.runEventLoop();

        std::cout << "Exit code: " << machine.getExitCode() << std::endl;
    } catch (jac::Exception& e) {
        std::cout << "Exception: " << e.what() << std::endl;
        std::cout << "Stack: " << e.stackTrace() << std::endl;
    } catch (std::exception& e) {
        std::cout << "Exception: " << e.what() << std::endl;
    }
}
