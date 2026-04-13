

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

class FunctionFactory {

    ContextRef _context;

    template<typename Func, typename Res, typename... Args>
    inline Function newFunctionHelper(Func& func, std::function<Res(Args...)>);

    template<typename Func, typename Res>
    inline Function newFunctionVariadicHelper(Func& func, std::function<Res(std::vector<ValueWeak>)>);

    template<typename Func, typename Res, typename... Args>
    inline Function newFunctionThisHelper(Func& func, std::function<Res(ContextRef, ValueWeak, Args...)>);

    template<typename Func, typename Res>
    inline Function newFunctionThisVariadicHelper(Func& func, std::function<Res(ContextRef, ValueWeak, std::vector<ValueWeak>)>);
public:
    FunctionFactory(ContextRef context) : _context(context) {}

    template<class Func>
    Function newFunction(Func func) {
        return newFunctionHelper(func, std::function(func));
    }

    template<class Func>
    Function newFunctionVariadic(Func func) {
        return newFunctionVariadicHelper(func, std::function(func));
    }

    template<class Func>
    Function newFunctionThis(Func func) {
        return newFunctionThisHelper(func, std::function(func));
    }

    template<class Func>
    Function newFunctionThisVariadic(Func func) {
        return newFunctionThisVariadicHelper(func, std::function(func));
    }
};


template<typename Func, typename Res, typename... Args>
inline Function FunctionFactory::newFunctionHelper(Func& func, std::function<Res(Args...)>) {
    Func* funcPtr = new Func(std::move(func));

    struct FuncProtoBuilder : public ProtoBuilder::Opaque<Func>, public ProtoBuilder::Callable {
        static Value callFunction(ContextRef ctx, ValueWeak funcObj, ValueWeak thisVal, std::vector<ValueWeak> args) {
            Func* ptr = ProtoBuilder::Opaque<Func>::getOpaque(ctx, funcObj);
            return processCall<Func, Res, Args...>(ctx, thisVal, args, *ptr);
        }
    };

    using FuncClass = Class<FuncProtoBuilder>;
    FuncClass::init("CppFunction");

    return static_cast<Value>(FuncClass::createInstance(_context, funcPtr)).to<Function>();
}

template<class Func, typename Res>
Function FunctionFactory::newFunctionVariadicHelper(Func& func, std::function<Res(std::vector<ValueWeak>)>) {
    Func* funcPtr = new Func(std::move(func));

    struct FuncProtoBuilder : public ProtoBuilder::Opaque<Func>, public ProtoBuilder::Callable {
        static Value callFunction(ContextRef ctx, ValueWeak funcObj, ValueWeak thisVal, std::vector<ValueWeak> args) {
            Func* ptr = ProtoBuilder::Opaque<Func>::getOpaque(ctx, funcObj);
            return processCallVariadic<Func, Res>(ctx, thisVal, args, *ptr);
        }
    };

    using FuncClass = Class<FuncProtoBuilder>;
    FuncClass::init("CppFunction");

    return static_cast<Value>(FuncClass::createInstance(_context, funcPtr)).to<Function>();
}

template<typename Func, typename Res, typename... Args>
Function FunctionFactory::newFunctionThisHelper(Func& func, std::function<Res(ContextRef, ValueWeak, Args...)>) {
    Func* funcPtr = new Func(std::move(func));

    struct FuncProtoBuilder : public ProtoBuilder::Opaque<Func>, public ProtoBuilder::Callable {
        static Value callFunction(ContextRef ctx, ValueWeak funcObj, ValueWeak thisVal, std::vector<ValueWeak> args) {
            Func* ptr = ProtoBuilder::Opaque<Func>::getOpaque(ctx, funcObj);
            return processCallThis<Func, Res, Args...>(ctx, thisVal, args, *ptr);
        }
    };

    using FuncClass = Class<FuncProtoBuilder>;
    FuncClass::init("CppFunction");

    return static_cast<Value>(FuncClass::createInstance(_context, funcPtr)).to<Function>();
}

template<typename Func, typename Res>
Function FunctionFactory::newFunctionThisVariadicHelper(Func& func, std::function<Res(ContextRef, ValueWeak, std::vector<ValueWeak>)>) {
    Func* funcPtr = new Func(std::move(func));

    struct FuncProtoBuilder : public ProtoBuilder::Opaque<Func>, public ProtoBuilder::Callable {
        static Value callFunction(ContextRef ctx, ValueWeak funcObj, ValueWeak thisVal, std::vector<ValueWeak> args) {
            Func* ptr = ProtoBuilder::Opaque<Func>::getOpaque(ctx, funcObj);
            return processCallThisVariadic<Func, Res>(ctx, thisVal, args, *ptr);
        }
    };

    using FuncClass = Class<FuncProtoBuilder>;
    FuncClass::init("CppFunction");

    return static_cast<Value>(FuncClass::createInstance(_context, funcPtr)).to<Function>();
}


} // namespace jac
```


