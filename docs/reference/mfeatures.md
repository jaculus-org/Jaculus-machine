# Custom MFeatures

As shown in the [Getting started](/getting-started) section, a Machine is parametrized by MFeatures that
define the core functionality of the Machine and the JavaScript runtime.

An MFeature is a class, that follows the stack inheritance and provides functionality to the Machine either
by providing a public C++ API that can be used by other MFeatures or the user, or by adding functionality
directly to the JavaScript runtime.

The MFeature must not interact with the JavaScript runtime in any way in its constructor, as it is not yet
initialized at that point. Initialization of the MFeature involving the JavaScript runtime should be done
in the `initialize` method.

## Modifying the global object

Let's create an MFeature that adds a `print` function to the global object of the JavaScript runtime.

```cpp
#include <jac/machine.h>
#include <jac/functionFactory.h>

template<class Next>
class PrintFeature : public Next {
public:
    std::ostream out;

    void print(std::string str) {
        out << str;
    }

    void initialize() {
        Next::initialize();

        jac::Value global = this->context().getGlobalObject();
        jac::FunctionFactory ff(this->context());

        global.defineProperty("print", ff.newFunction([this](std::string str) {
            this->print(str);
        }));
    }
};
```

The MFeature lets the user set the output stream of the `print` function by setting the `out` field.
The `initialize` method then adds the `print` function to the global JavaScript object.

The MFeature can be used like this:

```cpp
using Machine = PrintFeature<jac::MachineBase>;

Machine machine;
machine.print.out = std::cout;

machine.initialize();

machine.eval("print('Hello, world!');", "<eval>", jac::EvalFlags::Global);
```

## Defining a JavaScript module

Often, an MFeatures will want to define a JavaScript module. This can be done by using the
`MachineBase::newModule` method.

Modules are lazy: `newModule` only registers the module's name and a builder callback.
The builder is invoked at most once, the first time the module is actually imported, at
which point it adds the module's exports via `Module::addExport`. A module that is never
imported allocates nothing beyond the stored builder.

```cpp
#include <jac/machine.h>
#include <jac/functionFactory.h>

template<class Next>
class PrintFeature : public Next {
public:
    std::ostream out;

    void print(std::string str) {
        out << str;
    }

    void initialize() {
        Next::initialize();

        this->newModule("printer", [this](jac::Module& mdl) {
            jac::FunctionFactory ff(this->context());
            mdl.addExport("print", ff.newFunction([this](std::string str) {
                this->print(str);
            }));
        });
    }
};
```
