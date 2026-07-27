

# File stdioFeature.h

[**File List**](files.md) **>** [**features**](dir_6f95e06b732314161804ab1ef73c9681.md) **>** [**stdioFeature.h**](stdioFeature_8h.md)

[Go to the documentation of this file](stdioFeature_8h.md)


```C++
#pragma once

#include <jac/machine/functionFactory.h>
#include <jac/machine/machine.h>
#include <jac/machine/values.h>

#include <memory>
#include <string>

#include "types/streams.h"

namespace jac {


template<class Next>
class StdioFeature : public Next {
private:
    class Stdio {
    public:
        std::unique_ptr<Writable> out;
        std::unique_ptr<Writable> err;
        std::unique_ptr<Readable> in;
    };

    static std::string joinArgs(ValueVectorWeak args) {
        std::string line;
        for (std::size_t i = 0; i < args.size(); ++i) {
            if (i != 0) {
                line += ' ';
            }
            line += args[i].isString() ? args[i].to<std::string>() : args[i].inspect();
        }
        return line;
    }
public:
    Stdio stdio;

    void initialize() {
        Next::initialize();

        FunctionFactory ff(this->context());

        if (!this->stdio.out) {
            throw std::runtime_error("StdioFeature: stdio.out is not set");
        }
        if (!this->stdio.err) {
            throw std::runtime_error("StdioFeature: stdio.err is not set");
        }

        Object console = Object::create(this->context());
        console.set("debug", ff.newFunctionVariadic([this](ValueVectorWeak args) {
            this->stdio.out->write(joinArgs(args) + "\n");
        }));
        console.set("log", ff.newFunctionVariadic([this](ValueVectorWeak args) {
            this->stdio.out->write(joinArgs(args) + "\n");
        }));
        console.set("info", ff.newFunctionVariadic([this](ValueVectorWeak args) {
            this->stdio.out->write(joinArgs(args) + "\n");
        }));
        console.set("warn", ff.newFunctionVariadic([this](ValueVectorWeak args) {
            this->stdio.err->write(joinArgs(args) + "\n");
        }));
        console.set("error", ff.newFunctionVariadic([this](ValueVectorWeak args) {
            this->stdio.err->write(joinArgs(args) + "\n");
        }));
        Object global = this->context().getGlobalObject();
        global.defineProperty("console", console);

        this->newModule("stdio", [this](Module& mdl) {
            mdl.addExport("stdout", Next::WritableClass::createInstance(this->context(), new WritableRef(stdio.out.get())));
            mdl.addExport("stderr", Next::WritableClass::createInstance(this->context(), new WritableRef(stdio.err.get())));
            if (stdio.in) {
                mdl.addExport("stdin", Next::ReadableClass::createInstance(this->context(), new ReadableRef(stdio.in.get())));
            }
        });
    }
};


} // namespace jac
```


