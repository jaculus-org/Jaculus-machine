

# File moduleLoader.h

[**File List**](files.md) **>** [**features**](dir_6f95e06b732314161804ab1ef73c9681.md) **>** [**util**](dir_8745a1fa89e3088deda48338e7669502.md) **>** [**moduleLoader.h**](moduleLoader_8h.md)

[Go to the documentation of this file](moduleLoader_8h.md)


```C++
#pragma once

#include <quickjs.h>

#include <cstdint>
#include <cstring>


namespace jac {


inline int moduleAttributesJsonType(JSContext* ctx, JSValueConst attributes) {
    if (JS_IsUndefined(attributes)) {
        return 0;
    }

    JSValue type = JS_GetPropertyStr(ctx, attributes, "type");
    if (!JS_IsString(type)) {
        JS_FreeValue(ctx, type);
        return 0;
    }

    size_t len = 0;
    const char* str = JS_ToCStringLen(ctx, &len, type);
    JS_FreeValue(ctx, type);
    if (!str) {
        return 0;
    }

    int res = 0;
    if (len == 4 && std::memcmp(str, "json", len) == 0) {
        res = 1;
    } else if (len == 5 && std::memcmp(str, "json5", len) == 0) {
        res = 2;
    }
    JS_FreeCString(ctx, str);
    return res;
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


