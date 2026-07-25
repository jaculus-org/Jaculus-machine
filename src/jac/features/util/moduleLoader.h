#pragma once

#include <quickjs.h>

#include <cstdint>
#include <cstring>


namespace jac {


/**
 * @brief Determine whether import attributes request a JSON module.
 *
 * @return 0 = not JSON, 1 = "json", 2 = "json5"
 */
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

/**
 * @brief Create a synthetic module whose only export is a default holding the
 *        given value.
 *
 * Takes ownership of @p val (it is freed on failure and otherwise attached to
 * the module as its private value until instantiation).
 *
 * @return The created module, or nullptr on error (with a pending exception)
 */
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
