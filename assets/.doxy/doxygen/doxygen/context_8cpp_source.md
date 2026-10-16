

# File context.cpp

[**File List**](files.md) **>** [**jac**](dir_256037ad7d0c306238e2bc4f945d341d.md) **>** [**machine**](dir_10e7d6e7bc593e38e57ffe1bab5ed259.md) **>** [**context.cpp**](context_8cpp.md)

[Go to the documentation of this file](context_8cpp.md)


```C++
#include "values.h"


namespace jac {


Exception ContextRef::getException() {
    try {
        return { _ctx, JS_GetException(_ctx) };
    }
    catch (std::exception& e) {
        throw Exception::create(Exception::Type::InternalError, "No exception");
    }
}

Object ContextRef::getGlobalObject() {
    return { _ctx, JS_GetGlobalObject(_ctx) };
}


} // namespace jac
```


