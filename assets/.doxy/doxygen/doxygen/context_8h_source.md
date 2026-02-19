

# File context.h

[**File List**](files.md) **>** [**jac**](dir_256037ad7d0c306238e2bc4f945d341d.md) **>** [**machine**](dir_10e7d6e7bc593e38e57ffe1bab5ed259.md) **>** [**context.h**](context_8h.md)

[Go to the documentation of this file](context_8h.md)


```C++
#pragma once

#include <quickjs.h>

#include "internal/declarations.h"


namespace jac {


class ContextRef {
    JSContext* _ctx;

public:
    ContextRef(JSContext* ctx) : _ctx(ctx) {}

    JSContext* get() { return _ctx; }

    Exception getException();

    Object getGlobalObject();

    operator JSContext*() { return _ctx; }
    operator JSContext*() const { return _ctx; }
    operator bool() { return _ctx != nullptr; }
    ContextRef& operator=(JSContext* ctx) { _ctx = ctx; return *this; }
};


}  // namespace jac
```


