

# File moduleLoader.h

[**File List**](files.md) **>** [**features**](dir_6f95e06b732314161804ab1ef73c9681.md) **>** [**util**](dir_8745a1fa89e3088deda48338e7669502.md) **>** [**moduleLoader.h**](moduleLoader_8h.md)

[Go to the documentation of this file](moduleLoader_8h.md)


```C++
#pragma once

#include <jac/machine/values.h>

#include <quickjs.h>


namespace jac {


inline int moduleAttributesJsonType(JSContext* ctx, JSValueConst attributes) {
    if (JS_IsUndefined(attributes)) {
        return 0;
    }

    Value type(ctx, JS_GetPropertyStr(ctx, attributes, "type"));
    if (type.isUndefined()) {
        return 0;
    }
    if (!type.isString()) {
        throw jac::Exception::create(jac::Exception::Type::TypeError, "import type must be a string");
    }

    auto str = type.toString();
    if (str.view() == "json") {
        return 1;
    }
    if (str.view() == "json5") {
        return 2;
    }
    throw jac::Exception::create(jac::Exception::Type::TypeError, "unsupported import type");
}

inline JSModuleDef* createJsonModule(JSContext* ctx, const char* name, JSValue val) {
    JSModuleDef* m = JS_NewCModule(ctx, name, [](JSContext* c, JSModuleDef* mdl) -> int {
        JSValue value = JS_GetModulePrivateValue(c, mdl);
        return JS_SetModuleExport(c, mdl, "default", value);
    });
    if (!m) {
        JS_FreeValue(ctx, val);
        return nullptr;
    }
    JS_AddModuleExport(ctx, m, "default");
    JS_SetModulePrivateValue(ctx, m, val);
    return m;
}


} // namespace jac
```


