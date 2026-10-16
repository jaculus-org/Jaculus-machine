

# File ostreamjs.h

[**File List**](files.md) **>** [**features**](dir_6f95e06b732314161804ab1ef73c9681.md) **>** [**util**](dir_8745a1fa89e3088deda48338e7669502.md) **>** [**ostreamjs.h**](ostreamjs_8h.md)

[Go to the documentation of this file](ostreamjs_8h.md)


```C++
#pragma once

#include <iostream>
#include <memory>

#include "../types/streams.h"

namespace jac {


class OsWritable : public Writable {
    std::ostream& _stream;
public:
    OsWritable(std::ostream& stream): _stream(stream) {}

    void write(std::string data) override {
        _stream.write(data.data(), data.size());
    }
};


template<class Machine>
static inline void initializeIo(Machine& machine) {
    if (!machine.stdio.out) {
        machine.stdio.out = std::make_unique<OsWritable>(std::cout);
    }
    if (!machine.stdio.err) {
        machine.stdio.err = std::make_unique<OsWritable>(std::cerr);
    }
}


} // namespace jac
```


