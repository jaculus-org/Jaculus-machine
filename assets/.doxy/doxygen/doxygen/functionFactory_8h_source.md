

# File functionFactory.h

[**File List**](files.md) **>** [**jac**](dir_256037ad7d0c306238e2bc4f945d341d.md) **>** [**machine**](dir_10e7d6e7bc593e38e57ffe1bab5ed259.md) **>** [**functionFactory.h**](functionFactory_8h.md)

[Go to the documentation of this file](functionFactory_8h.md)


```C++
#pragma once

#include <functional>

#include "class.h"
#include "funcUtil.h"
#include "values.h"


namespace jac {

namespace detail {

class CallableBase {
public:
    virtual Value invoke(ContextRef ctx, ValueWeak thisVal, ValueVectorWeak args) = 0;
    virtual ~CallableBase() = default;
};

template<typename Func, typename Signature, bool withThis, bool variadic>
class CallableHolder;

template<typename Func, typename Res, typename... Args>
class CallableHolder<Func, std::function<Res(Args...)>, false, false> final : public CallableBase {
    Func _func;
public:
    explicit CallableHolder(Func func) : _func(std::move(func)) {}
    Value invoke(ContextRef ctx, ValueWeak thisVal, ValueVectorWeak args) override {
        return processCall<Func, Res, Args...>(ctx, thisVal, args, _func);
    }
};

template<typename Func, typename Res>
class CallableHolder<Func, std::function<Res(ValueVectorWeak)>, false, true> final : public CallableBase {
    Func _func;
public:
    explicit CallableHolder(Func func) : _func(std::move(func)) {}
    Value invoke(ContextRef ctx, ValueWeak thisVal, ValueVectorWeak args) override {
        return processCallVariadic<Func, Res>(ctx, thisVal, args, _func);
    }
};

template<typename Func, typename Res, typename... Args>
class CallableHolder<Func, std::function<Res(ContextRef, ValueWeak, Args...)>, true, false> final : public CallableBase {
    Func _func;
public:
    explicit CallableHolder(Func func) : _func(std::move(func)) {}
    Value invoke(ContextRef ctx, ValueWeak thisVal, ValueVectorWeak args) override {
        return processCallThis<Func, Res, Args...>(ctx, thisVal, args, _func);
    }
};

template<typename Func, typename Res>
class CallableHolder<Func, std::function<Res(ContextRef, ValueWeak, ValueVectorWeak)>, true, true> final : public CallableBase {
    Func _func;
public:
    explicit CallableHolder(Func func) : _func(std::move(func)) {}
    Value invoke(ContextRef ctx, ValueWeak thisVal, ValueVectorWeak args) override {
        return processCallThisVariadic<Func, Res>(ctx, thisVal, args, _func);
    }
};

struct CallableProtoBuilder : public ProtoBuilder::Opaque<CallableBase>, public ProtoBuilder::Callable {
    static Value callFunction(ContextRef ctx, ValueWeak funcObj, ValueWeak thisVal, ValueVectorWeak args) {
        return getOpaque(ctx, funcObj)->invoke(ctx, thisVal, args);
    }
};

using CallableClass = Class<CallableProtoBuilder>;

} // namespace detail

class FunctionFactory {

    ContextRef _context;

    template<bool withThis, bool variadic, typename Func>
    Function makeFunction(Func func) {
        using Signature = decltype(std::function(func));
        using Holder = detail::CallableHolder<Func, Signature, withThis, variadic>;

        detail::CallableClass::init("CppFunction");
        return static_cast<Value>(detail::CallableClass::createInstance(_context, new Holder(std::move(func)))).to<Function>();
    }
public:
    FunctionFactory(ContextRef context) : _context(context) {}

    template<class Func>
    Function newFunction(Func func) {
        return makeFunction<false, false>(std::move(func));
    }

    template<class Func>
    Function newFunctionVariadic(Func func) {
        return makeFunction<false, true>(std::move(func));
    }

    template<class Func>
    Function newFunctionThis(Func func) {
        return makeFunction<true, false>(std::move(func));
    }

    template<class Func>
    Function newFunctionThisVariadic(Func func) {
        return makeFunction<true, true>(std::move(func));
    }
};


} // namespace jac
```


