

# File funcUtil.h

[**File List**](files.md) **>** [**jac**](dir_256037ad7d0c306238e2bc4f945d341d.md) **>** [**machine**](dir_10e7d6e7bc593e38e57ffe1bab5ed259.md) **>** [**funcUtil.h**](funcUtil_8h.md)

[Go to the documentation of this file](funcUtil_8h.md)


```C++
#pragma once

#include <exception>
#include <quickjs.h>

#include "values.h"

namespace jac {


template<typename Func>
inline JSValue propagateExceptions(ContextRef ctx, Func& f) noexcept {
    try {
        return f();
    }
    catch (Exception& e) {
        return e.throwJS(ctx);
    }
    catch (std::exception& e) {
        return Exception::create(Exception::Type::InternalError, e.what()).throwJS(ctx);
    }
    catch (...) {
        return Exception::create(Exception::Type::InternalError, "unknown error").throwJS(ctx);
    }
}

template<typename Func>
inline JSValue propagateExceptions(ContextRef ctx, Func&& f) noexcept {
    return propagateExceptions(ctx, f);
}

template<typename Func, typename Res, typename... Args>
inline JSValue processCallRaw(ContextRef ctx, JSValueConst, int argc, JSValueConst* argv, Func& f) {
    std::tuple<Args...> args = convertArgs<Args...>(ctx, argv, argc, std::make_index_sequence<sizeof...(Args)>());

    if constexpr (std::is_same_v<Res, void>) {
        std::apply(f, args);
        return JS_UNDEFINED;
    }
    else {
        return Value::from(ctx, std::apply(f, args)).loot().second;
    }
}

template<typename Func, typename Res, typename... Args>
inline Value processCall(ContextRef ctx, ValueWeak, std::vector<ValueWeak> argv, Func& f) {
    std::tuple<Args...> args = convertArgs<Args...>(ctx, argv, std::make_index_sequence<sizeof...(Args)>());

    if constexpr (std::is_same_v<Res, void>) {
        std::apply(f, args);
        return Value::undefined(ctx);
    }
    else {
        return Value::from(ctx, std::apply(f, args));
    }
}

template<typename Func, typename Res>
inline JSValue processCallVariadicRaw(ContextRef ctx, JSValueConst, int argc, JSValueConst* argv, Func& f) {
    std::vector<ValueWeak> args;
    for (int i = 0; i < argc; i++) {
        args.emplace_back(ctx, argv[i]);
    }

    if constexpr (std::is_same_v<Res, void>) {
        f(args);
        return JS_UNDEFINED;
    }
    else {
        return Value::from(ctx, f(args)).loot().second;
    }
}

template<typename Func, typename Res>
inline Value processCallVariadic(ContextRef ctx, ValueWeak, std::vector<ValueWeak> argv, Func& f) {
    if constexpr (std::is_same_v<Res, void>) {
        f(argv);
        return Value::undefined(ctx);
    }
    else {
        return Value::from(ctx, f(argv));
    }
}

template<typename Func, typename Res, typename... Args>
inline Value processCallThis(ContextRef ctx, ValueWeak thisVal, std::vector<ValueWeak> argv, Func& f) {
    std::tuple<Args...> args = convertArgs<Args...>(ctx, argv, std::make_index_sequence<sizeof...(Args)>());

    if constexpr (std::is_same_v<Res, void>) {
        std::apply(f, std::tuple_cat(std::make_tuple(ctx, thisVal), args));
        return Value::undefined(ctx);
    }
    else {
        return Value::from(ctx, std::apply(f, std::tuple_cat(std::make_tuple(ctx, thisVal), args)));
    }
}

template<typename Func, typename Res>
inline Value processCallThisVariadic(ContextRef ctx, ValueWeak thisVal, std::vector<ValueWeak> argv, Func& f) {

    if constexpr (std::is_same_v<Res, void>) {
        f(ctx, thisVal, argv);
        return Value::undefined(ctx);
    }
    else {
        return Value::from(ctx, f(ctx, thisVal, argv));
    }
}

template<typename... Args, std::size_t... Is>
inline std::tuple<Args...> convertArgs([[maybe_unused]]ContextRef ctx, std::vector<ValueWeak> argv, std::index_sequence<Is...>) {
    if (argv.size() != sizeof...(Args)) {
        throw Exception::create(Exception::Type::TypeError, "invalid number of arguments");
    }

    return std::make_tuple(argv[Is].to<Args>()...);
}

template<typename... Args, std::size_t... Is>
inline std::tuple<Args...> convertArgs([[maybe_unused]]ContextRef ctx, JSValueConst* argv, int argc, std::index_sequence<Is...>) {
    if (argc != sizeof...(Args)) {
        throw Exception::create(Exception::Type::TypeError, "invalid number of arguments");
    }

    return std::make_tuple(ValueWeak(ctx, argv[Is]).to<Args>()...);
}


} // namespace jac
```


