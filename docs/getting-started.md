# Getting started

The core concept of Jaculus-machine is a Machine type. A Machine is parametrized by MFeatures
which specify the core functionality of the Machine and the JavaScript runtime. MFeatures are
added to the Machine using stack inheritance. At the bottom of the stack, there is always
a `jac::MachineBase` which provides the most basic functionality.
For example, a Machine with some built-in MFeatures can be created like this:

```cpp
#include <jaculus/machine.h>
#include <jaculus/features/stdioFeature.h>
#include <jaculus/features/filesystemFeature.h>
#include <jaculus/features/basicStreamFeature.h>
#include <jaculus/features/simpleModuleLoaderFeature.h>
#include <jaculus/features/util/ostreamjs.h>


using Machine = jac::ComposeMachine<
    jac::MachineBase
    jac::BasicStreamFeature,
    jac::StdioFeature,
    jac::FilesystemFeature,
    jac::ModuleLoaderFeature,
>;
```

To further extend the functionality of the Machine, we can use Plugins. Plugins are added to the
runtime dynamically after the runtime initialization.

After the Machine type is defined, we can instantiate it:

```cpp
Machine machine;
```

Then we can configure the Machine - we can, for example, set the output streams of the stdio MFeature:

```cpp
machine.stdio.out = machine.stdio.out = std::make_unique<OsWritable<Machine>>(std::cout);
machine.stdio.err = machine.stdio.err = std::make_unique<OsWritable<Machine>>(std::cerr);
```

At this point, we still can not run any JavaScript code, nor can we interact with the runtime in
any way. For that, we first need to initialize the Machine:

```cpp
machine.initialize();
```

At this point, we can add any Plugins to the Machine.

After the Machine is initialized, we can finally run some JavaScript code:

```cpp
jac::Value result = machine.eval("console.log('Hello, world!');", "<stdin>", jac::EvalFlags::Global);
```

Or we can evaluate a JavaScript file using `ModuleLoaderFeature::evalFile` method:

```cpp
jac::Value result = machine.evalFile("./main.js", jac::EvalFlags::Global);
```

Both `eval` and `evalFile`, however, only evaluate the code and do not control the event loop.
For that, we need to use the `EventLoopFeature`.


## MFeatures
MFeatures are the core building blocks of a Machine. The stack design of a Machine allows interfacing with
different MFeatures in C++ directly and to implement new MFeatures on top of existing ones without
a large performance penalty.

Only the most basic and necessary MFeatures are included with the library:

- `jac::MachineBase` - the base class of all Machines. It provides the basic functionality of the runtime.
- `jac::EventQueueFeature` - provides an event queue built on top of the standard library.
- `jac::EventLoopFeature` - provides a default event loop implementation.
- `jac::FileystemFeature` - provides filesystem access through `fs` and `path` modules
- `jac::ModuleLoaderFeature` - provides module loading through `import`, and `evalFile` method
- `jac::BasicStreamFeature` - provides `Readable` and `Writable` abstract classes
- `jac::StdioFeature` - provides standard io streams and a simplified `console` interface
- `jac::TimersFeature` - provides typical JavaScript timers (with a slight difference) and a `sleep` function

## Plugins

Plugins are the primary way to extend the functionality of the JavaScript runtime. They are added to
the machine dynamically after the runtime initialization.

Compared to MFeatures, Plugins are more high-level and should be used to implement functionality that
is not directly related to the JavaScript runtime.
