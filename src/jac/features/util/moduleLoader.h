#pragma once

#include <jac/machine/values.h>

#include <quickjs.h>


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
