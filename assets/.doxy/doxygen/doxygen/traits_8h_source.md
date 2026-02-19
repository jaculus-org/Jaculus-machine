

# File traits.h

[**File List**](files.md) **>** [**jac**](dir_256037ad7d0c306238e2bc4f945d341d.md) **>** [**machine**](dir_10e7d6e7bc593e38e57ffe1bab5ed259.md) **>** [**traits.h**](traits_8h.md)

[Go to the documentation of this file](traits_8h.md)


```C++
#pragma once

#include <type_traits>
#include <vector>

#include "context.h"
#include "stringView.h"
#include "values.h"


namespace jac {


template<typename T, typename En = T>
struct ConvTraits {
    static void from(ContextRef, ValueWeak) {
        static_assert(sizeof(T) == 0, "No conversion defined");
    }

    static void to(ContextRef, T) {
        static_assert(sizeof(T) == 0, "No conversion defined");
    }
};

template<>
struct ConvTraits<bool> {
    static bool from(ContextRef ctx, ValueWeak val) {
        return JS_ToBool(ctx, val.getVal());
    }

    static Value to(ContextRef ctx, bool val) {
        return Value(ctx, JS_NewBool(ctx, val));
    }
};

namespace detail {

    template<typename T>
    constexpr bool is_leq_i32 = (std::is_signed_v<T> && sizeof(T) <= sizeof(int32_t))
                             || (std::is_unsigned_v<T> && sizeof(T) < sizeof(int32_t));

} // namespace detail


template<typename T>
struct ConvTraits<T, std::enable_if_t<
       std::is_integral_v<T>
    && detail::is_leq_i32<T>
, T>> {
    static T from(ContextRef ctx, ValueWeak val) {
        int32_t res;
        int ex = JS_ToInt32(ctx, &res, val.getVal());
        if (ex < 0) {
            throw Exception::create(Exception::Type::TypeError, "Failed to convert to int");
        }
        return res;
    }

    static Value to(ContextRef ctx, T val) {
        return Value(ctx, JS_NewInt32(ctx, val));
    }
};

template<typename T>
struct ConvTraits<T, std::enable_if_t<
       std::is_floating_point_v<T>
    && sizeof(T) <= sizeof(double)
, T>> {
    static T from(ContextRef ctx, ValueWeak val) {
        double res;
        int ex = JS_ToFloat64(ctx, &res, val.getVal());
        if (ex < 0) {
            throw Exception::create(Exception::Type::TypeError, "Failed to convert to double");
        }
        return res;
    }

    static Value to(ContextRef ctx, T val) {
        return Value(ctx, JS_NewFloat64(ctx, val));
    }
};

template<typename T>
struct ConvTraits<T, std::enable_if_t<
       std::is_integral_v<T>
    && sizeof(T) <= sizeof(int64_t)
    && !detail::is_leq_i32<T>
, T>> {
    static T from(ContextRef ctx, ValueWeak val) {
        int64_t res = 0;
        int ex = JS_ToInt64(ctx, &res, val.getVal());
        if (ex < 0) {
            throw Exception::create(Exception::Type::TypeError, "Failed to convert to int");
        }
        return res;
    }

    static Value to(ContextRef ctx, T val) {
        return Value(ctx, JS_NewInt64(ctx, val));
    }
};

template<>
struct ConvTraits<const char*> {
    static Value to(ContextRef ctx, const char* val) {
        return Value(ctx, JS_NewString(ctx, val));
    }
};

template<>
struct ConvTraits<char*> : public ConvTraits<const char*> {};

template<>
struct ConvTraits<StringView> {
    static StringView from(ContextRef ctx, ValueWeak val) {
        const char* str = JS_ToCString(ctx, val.getVal());
        if (!str) {
            throw Exception::create(Exception::Type::TypeError, "Failed to convert to string");
        }
        return StringView(ctx, str);
    }

    static Value to(ContextRef ctx, StringView val) {
        return ConvTraits<const char*>::to(ctx, val.c_str());
    }
};

template<>
struct ConvTraits<std::string> {
    static std::string from(ContextRef ctx, ValueWeak val) {
        return std::string(ConvTraits<StringView>::from(ctx, val));
    }

    static Value to(ContextRef ctx, const std::string& val) {
        return ConvTraits<const char*>::to(ctx, val.c_str());
    }
};

template<>
struct ConvTraits<ValueWeak> {
    static ValueWeak from(ContextRef, ValueWeak val) {
        return val;
    }

    static Value to(ContextRef ctx, ValueWeak val) {
        JS_DupValue(ctx, val.getVal());
        return Value(ctx, val.getVal());
    }
};

template<>
struct ConvTraits<Value> {
    static Value from(ContextRef ctx, ValueWeak val) {
        JS_DupValue(ctx, val.getVal());
        return Value(ctx, val.getVal());
    }

    static Value to(ContextRef, Value val) {
        return val;
    }
};

template<>
struct ConvTraits<ObjectWeak> {
    static ObjectWeak from(ContextRef ctx, ValueWeak val) {
        return ObjectWeak(ctx, val.getVal());
    }

    static Value to(ContextRef ctx, ObjectWeak val) {
        JS_DupValue(ctx, val.getVal());
        return Value(ctx, val.getVal());
    }
};

template<>
struct ConvTraits<Object> {
    static Object from(ContextRef ctx, ValueWeak val) {
        JS_DupValue(ctx, val.getVal());
        return Object(ctx, val.getVal());
    }

    static Value to(ContextRef, Object val) {
        return static_cast<Value>(val);
    }
};

template<>
struct ConvTraits<FunctionWeak> {
    static FunctionWeak from(ContextRef ctx, ValueWeak val) {
        return FunctionWeak(ctx, val.getVal());
    }

    static Value to(ContextRef ctx, FunctionWeak val) {
        JS_DupValue(ctx, val.getVal());
        return Value(ctx, val.getVal());
    }
};

template<>
struct ConvTraits<Function> {
    static Function from(ContextRef ctx, ValueWeak val) {
        JS_DupValue(ctx, val.getVal());
        return Function(ctx, val.getVal());
    }

    static Value to(ContextRef, Function val) {
        return static_cast<Value>(val);
    }
};

template<>
struct ConvTraits<ArrayWeak> {
    static ArrayWeak from(ContextRef ctx, ValueWeak val) {
        return ArrayWeak(ctx, val.getVal());
    }

    static Value to(ContextRef ctx, ArrayWeak val) {
        JS_DupValue(ctx, val.getVal());
        return Value(ctx, val.getVal());
    }
};

template<>
struct ConvTraits<Array> {
    static Array from(ContextRef ctx, ValueWeak val) {
        JS_DupValue(ctx, val.getVal());
        return Array(ctx, val.getVal());
    }

    static Value to(ContextRef, Array val) {
        return static_cast<Value>(val);
    }
};

template<>
struct ConvTraits<PromiseWeak> {
    static PromiseWeak from(ContextRef ctx, ValueWeak val) {
        return PromiseWeak(ctx, val.getVal());
    }

    static Value to(ContextRef ctx, PromiseWeak val) {
        JS_DupValue(ctx, val.getVal());
        return Value(ctx, val.getVal());
    }
};

template<>
struct ConvTraits<Promise> {
    static Promise from(ContextRef ctx, ValueWeak val) {
        JS_DupValue(ctx, val.getVal());
        return Promise(ctx, val.getVal());
    }

    static Value to(ContextRef, Promise val) {
        return static_cast<Value>(val);
    }
};

template<>
struct ConvTraits<ExceptionWeak> {
    static ExceptionWeak from(ContextRef ctx, ValueWeak val) {
        return ExceptionWeak(ctx, val.getVal());
    }

    static Value to(ContextRef ctx, ExceptionWeak val) {
        JS_DupValue(ctx, val.getVal());
        return Value(ctx, val.getVal());
    }
};

template<>
struct ConvTraits<Exception> {
    static Exception from(ContextRef ctx, ValueWeak val) {
        JS_DupValue(ctx, val.getVal());
        return Exception(ctx, val.getVal());
    }

    static Value to(ContextRef, Exception val) {
        return static_cast<Value>(val);
    }
};

template<>
struct ConvTraits<ArrayBuffer> {
    static ArrayBuffer from(ContextRef ctx, ValueWeak val) {
        JS_DupValue(ctx, val.getVal());
        return ArrayBuffer(ctx, val.getVal());
    }

    static Value to(ContextRef, ArrayBuffer val) {
        return static_cast<Value>(val);
    }
};

template<>
struct ConvTraits<ArrayBufferWeak> {
    static ArrayBufferWeak from(ContextRef ctx, ValueWeak val) {
        return ArrayBufferWeak(ctx, val.getVal());
    }

    static Value to(ContextRef ctx, ArrayBufferWeak val) {
        JS_DupValue(ctx, val.getVal());
        return Value(ctx, val.getVal());
    }
};

template<typename T>
struct ConvTraits<std::vector<T>> {
    static std::vector<T> from(ContextRef, ValueWeak val) {
        auto arr = val.to<Array>();
        std::vector<T> res;
        for (int i = 0; i < arr.length(); i++) {
            try {
                res.push_back(arr.get(i).to<T>());
            }
            catch (Exception& e) {
                throw Exception::create(Exception::Type::TypeError, "Failed to convert array element");
            }
        }
        return res;
    }

    static Value to(ContextRef ctx, const std::vector<T>& val) {
        Array arr = Array::create(ctx);
        for (size_t i = 0; i < val.size(); i++) {
            arr.set(i, val[i]);
        }
        return arr;
    }
};

template<typename... Args>
struct ConvTraits<std::tuple<Args...>> {
    template<std::size_t... Is>
    static std::tuple<Args...> unwrapHelper(ContextRef, ValueWeak val, std::index_sequence<Is...>) {
        auto arr = val.to<Array>();
        if (arr.length() < static_cast<int>(sizeof...(Args))) {
            throw Exception::create(Exception::Type::TypeError, "Tuple size mismatch");
        }
        return std::make_tuple(arr.get<Args>(Is)...);
    }

    static std::tuple<Args...> from(ContextRef ctx, ValueWeak val) {
        return unwrapHelper(ctx, val, std::index_sequence_for<Args...>{});
    }

    template<std::size_t... Is>
    static Value wrapHelper(ContextRef ctx, const std::tuple<Args...>& val, std::index_sequence<Is...>) {
        Array arr = Array::create(ctx);
        (arr.set(Is, std::get<Is>(val)), ...);
        return arr;
    }

    static Value to(ContextRef ctx, std::tuple<Args...> val) {
        return wrapHelper(ctx, val, std::index_sequence_for<Args...>{});
    }
};


} // namespace jac
```


