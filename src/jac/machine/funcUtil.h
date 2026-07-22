#pragma once

#include <exception>
#include <quickjs.h>

#include "values.h"

namespace jac {


/**
 * Various functions to process function calls with unprocessed javascript arguments.
 * Arguments and return value of the functions are automatically converted to
 * and from javascript values. Exceptions thrown within the functions are
 * caught and propagated to the javascript side.
 */

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
inline Value processCall(ContextRef ctx, ValueWeak, ValueVectorWeak argv, Func& f) {
    std::tuple<Args...> args = convertArgs<Args...>(ctx, argv, std::make_index_sequence<sizeof...(Args)>());

    if constexpr (std::is_same_v<Res, void>) {
        std::apply(f, args);
        return Value::undefined(ctx);
    }
    else {
        return Value::from(ctx, std::apply(f, args));
    }
}

template<typename Func, typename Res, typename... Args>
inline JSValue processCallRaw(ContextRef ctx, JSValueConst thisVal, int argc, JSValueConst* argv, Func& f) {
    return processCall<Func, Res, Args...>(ctx, ValueWeak(ctx, thisVal), ValueVectorWeak(ctx, argv, argc), f).loot().second;
}

template<typename Func, typename Res>
inline Value processCallVariadic(ContextRef ctx, ValueWeak, ValueVectorWeak argv, Func& f) {
    if constexpr (std::is_same_v<Res, void>) {
        f(argv);
        return Value::undefined(ctx);
    }
    else {
        return Value::from(ctx, f(argv));
    }
}

template<typename Func, typename Res>
inline JSValue processCallVariadicRaw(ContextRef ctx, JSValueConst thisVal, int argc, JSValueConst* argv, Func& f) {
    return processCallVariadic<Func, Res>(ctx, ValueWeak(ctx, thisVal), ValueVectorWeak(ctx, argv, argc), f).loot().second;
}

template<typename Func, typename Res, typename... Args>
inline Value processCallThis(ContextRef ctx, ValueWeak thisVal, ValueVectorWeak argv, Func& f) {
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
inline Value processCallThisVariadic(ContextRef ctx, ValueWeak thisVal, ValueVectorWeak argv, Func& f) {

    if constexpr (std::is_same_v<Res, void>) {
        f(ctx, thisVal, argv);
        return Value::undefined(ctx);
    }
    else {
        return Value::from(ctx, f(ctx, thisVal, argv));
    }
}

template<typename... Args, std::size_t... Is>
inline std::tuple<Args...> convertArgs([[maybe_unused]]ContextRef ctx, ValueVectorWeak argv, std::index_sequence<Is...>) {
    if (argv.size() != sizeof...(Args)) {
        throw Exception::create(Exception::Type::TypeError, "invalid number of arguments");
    }

    return std::make_tuple(argv[Is].to<Args>()...);
}


} // namespace jac
