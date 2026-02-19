

# File class.h

[**File List**](files.md) **>** [**jac**](dir_256037ad7d0c306238e2bc4f945d341d.md) **>** [**machine**](dir_10e7d6e7bc593e38e57ffe1bab5ed259.md) **>** [**class.h**](class_8h.md)

[Go to the documentation of this file](class_8h.md)


```C++
#pragma once

#include <quickjs.h>

#include <string>
#include <tuple>
#include <vector>

#include "funcUtil.h"
#include "values.h"


namespace jac {


template<typename Sgn>
struct SgnUnwrap;

template<typename Res, typename... Args>
struct SgnUnwrap<Res(Args...)> {
    using ResType = Res;
    using ArgTypes = std::tuple<Args...>;

    template<class Class>
    SgnUnwrap(Res (Class::*)(Args...)) {}
    template<class Class>
    SgnUnwrap(Res (Class::*)(Args...) const) {}
};
template<class Class, typename Res, typename... Args>
SgnUnwrap(Res (Class::*)(Args...)) -> SgnUnwrap<Res(Args...)>;
template<class Class, typename Res, typename... Args>
SgnUnwrap(Res (Class::*)(Args...) const) -> SgnUnwrap<Res(Args...)>;

namespace detail {
    template<template <typename...> class Base, typename Derived>
    struct is_base_of_template_impl {
        template<typename... Ts>
        static constexpr void is_callable(Base<Ts...>*);

        template<typename T>
        using is_callable_t = decltype(is_callable(std::declval<T*>()));

        template<template<class...> class A, class Void = void>
        struct check : std::false_type {};

        template<template<class...> class A>
        struct check<A, std::void_t<A<Derived>>> : std::true_type {};

        using value_t = check<is_callable_t>;
    };
} // namespace detail

template<template <typename...> class Base, typename Derived>
struct is_base_of_template : detail::is_base_of_template_impl<Base, Derived>::value_t {};

template<template <typename...> class Base, typename Derived>
using is_base_of_template_t = typename is_base_of_template<Base, Derived>::type;

template<template <typename...> class Base, typename Derived>
inline constexpr bool is_base_of_template_v = is_base_of_template<Base, Derived>::value;


namespace ProtoBuilder {

    template<typename T>
    struct Opaque {
        using OpaqueType = T;
        static inline JSClassID classId;

        static T* constructOpaque(ContextRef /*ctx*/, std::vector<ValueWeak> /*args*/) {
            throw Exception::create(Exception::Type::TypeError, "Class cannot be instantiated");
        }

        static void destroyOpaque(JSRuntime* /*rt*/, T* ptr) noexcept {
            delete ptr;
        }

        static T* getOpaque(ContextRef /*ctx*/, ValueWeak thisVal) {
            T* ptr = static_cast<T*>(JS_GetOpaque(thisVal.getVal(), classId));
            if (!ptr) {
                throw Exception::create(Exception::Type::TypeError, "Invalid opaque data");
            }
            return ptr;
        }

        template<typename Sgn, Sgn member>
        static Value callMember(ContextRef ctx, ValueWeak funcObj, ValueWeak thisVal, std::vector<ValueWeak> argv) {
            const SgnUnwrap Unwrap_(member);

            return [&]<typename Res, typename... Args>(SgnUnwrap<Res(Args...)>) {
                auto f = [&](Args... args) -> Res {
                    T* ptr = static_cast<T*>(JS_GetOpaque(funcObj.getVal(), classId));
                    return (ptr->*member)(args...);
                };

                return processCall<decltype(f), Res, Args...>(ctx, thisVal, argv, f);
            }(Unwrap_);
        }


        template<typename U, U(T::*member)>
        static void addPropMember(ContextRef ctx, Object proto, std::string name, PropFlags flags = PropFlags::Default) {
            using GetRaw = JSValue(*)(JSContext* ctx_, JSValueConst thisVal);
            using SetRaw = JSValue(*)(JSContext* ctx_, JSValueConst thisVal, JSValueConst val);

            GetRaw get = [](JSContext* ctx_, JSValueConst thisVal) -> JSValue {
                T* ptr = static_cast<T*>(JS_GetOpaque(thisVal, classId));
                return Value::from(ctx_, ptr->*member).loot().second;
            };
            SetRaw set = [](JSContext* ctx_, JSValueConst thisVal, JSValueConst val) -> JSValue {
                T* ptr = static_cast<T*>(JS_GetOpaque(thisVal, classId));
                ptr->*member = ValueWeak(ctx_, val).to<U>();
                return JS_UNDEFINED;
            };

            JSValue getter = JS_NewCFunction2(ctx, reinterpret_cast<JSCFunction*>(reinterpret_cast<void*>(get)), ("get " + name).c_str(), 0, JS_CFUNC_getter, 0); // NOLINT
            JSValue setter = JS_NewCFunction2(ctx, reinterpret_cast<JSCFunction*>(reinterpret_cast<void*>(set)), ("set " + name).c_str(), 1, JS_CFUNC_setter, 0); // NOLINT

            Atom atom = Atom(ctx, JS_NewAtom(ctx, name.c_str()));
            JS_DefinePropertyGetSet(ctx, proto.getVal(), atom.get(), getter, setter, static_cast<int>(flags));
        }


        template<typename Sgn, Sgn member>
        static void addMethodMember(ContextRef ctx, Object proto, std::string name, PropFlags flags = PropFlags::Default) {
            using MethodRaw = JSValue(*)(JSContext* ctx, JSValueConst thisVal, int argc, JSValueConst *argv);

            const SgnUnwrap Unwrap_(member);

            [&]<typename Res, typename... Args>(SgnUnwrap<Res(Args...)>) {
                MethodRaw func = [](JSContext* ctx_, JSValueConst thisVal, int argc, JSValueConst* argv) -> JSValue {
                    T* ptr = static_cast<T*>(JS_GetOpaque(thisVal, classId));

                    auto f = [ptr](Args... args) -> Res {
                        return (ptr->*member)(args...);
                    };

                    return propagateExceptions(ctx_, [&]() -> JSValue {
                        return processCallRaw<decltype(f), Res, Args...>(ctx_, thisVal, argc, argv, f);
                    });
                };

                JSValue funcVal = JS_NewCFunction(ctx, static_cast<JSCFunction*>(func), name.c_str(), 0);

                Atom atom = Atom(ctx, JS_NewAtom(ctx, name.c_str()));
                JS_DefinePropertyValue(ctx, proto.getVal(), atom.get(), funcVal, static_cast<int>(flags));
            }(Unwrap_);
        }
    };

    struct LifetimeHandles {
        static void postConstruction(ContextRef ctx, Object thisVal, std::vector<ValueWeak> args) {
            // do nothing
        }
    };

    struct Callable {
        static Value callFunction(ContextRef /*ctx*/, ValueWeak /*funcObj*/, ValueWeak /*thisVal*/, std::vector<ValueWeak> /*args*/) {
            throw Exception::create(Exception::Type::TypeError, "Class cannot be called as a function");
        }

        static Value callConstructor(ContextRef /*ctx*/, ValueWeak /*funcObj*/, ValueWeak /*target*/, std::vector<ValueWeak> /*args*/) {
            throw Exception::create(Exception::Type::TypeError, "Class cannot be called as a constructor");
        }
    };

    struct Properties {
        static void addProperties(ContextRef ctx, Object proto) {}
    };
} // namespace ProtoBuilder


template<class Builder>
class Class {
    static inline JSClassID classId;
    static inline JSClassDef classDef;
    static inline std::string className;
    static inline bool isConstructor;

    static JSValue constructor_impl(JSContext* ctx, JSValueConst thisVal, int argc, JSValueConst *argv) noexcept {
        return propagateExceptions(ctx, [&]() -> JSValue {
            Value proto = Value::undefined(ctx);
            if (JS_IsUndefined(thisVal)) {
                proto = Value(ctx, JS_GetClassProto(ctx, classId));
            }
            else {
                proto = Value(ctx, JS_GetPropertyStr(ctx, thisVal, "prototype"));
            }
            Object obj(ctx, JS_NewObjectProtoClass(ctx, proto.getVal(), classId));

            if constexpr (std::is_base_of_v<ProtoBuilder::Callable, Builder>) {
                JS_SetConstructorBit(ctx, obj.getVal(), isConstructor);
            }

            constexpr bool isPbOpaque = is_base_of_template_v<ProtoBuilder::Opaque, Builder>;
            constexpr bool isPbConstructor = std::is_base_of_v<ProtoBuilder::LifetimeHandles, Builder>;

            if constexpr (isPbOpaque || isPbConstructor) {
                std::vector<ValueWeak> args;
                for (int i = 0; i < argc; i++) {
                    args.emplace_back(ctx, argv[i]);
                }

                if constexpr (isPbOpaque) {
                    auto instance = Builder::constructOpaque(ctx, args);
                    JS_SetOpaque(obj.getVal(), instance);
                }

                if constexpr (isPbConstructor) {
                    Builder::postConstruction(ctx, obj, args);
                }
            }

            return obj.loot().second;
        });
    }

public:
    static void init(std::string name, bool isCtor = false) {
        if (classId != 0) {
            if (className != name || isConstructor != isCtor) {
                throw std::runtime_error("Class already initialized with different name or constructor flag");
            }
            return;
        }
        JS_NewClassID(&classId);

        className = name;
        isConstructor = isCtor;

        JSClassFinalizer* finalizer = nullptr;
        JSClassCall* call = nullptr;

        if constexpr (is_base_of_template_v<ProtoBuilder::Opaque, Builder>) {
            Builder::classId = classId;
            finalizer = [](JSRuntime* rt, JSValue val) noexcept {
                static_assert(noexcept(Builder::destroyOpaque(rt, static_cast<typename Builder::OpaqueType*>(nullptr))));
                Builder::destroyOpaque(rt, static_cast<typename Builder::OpaqueType*>(JS_GetOpaque(val, classId)));
            };
        }

        if constexpr (std::is_base_of_v<ProtoBuilder::Callable, Builder>) {
            call = [](JSContext* ctx, JSValueConst funcObj, JSValueConst thisVal, int argc, JSValueConst* argv, int flags) noexcept -> JSValue {
                std::vector<ValueWeak> args;
                args.reserve(argc);
                for (int i = 0; i < argc; i++) {
                    args.emplace_back(ctx, argv[i]);
                }

                return propagateExceptions(ctx, [&]() -> JSValue {
                    if (flags & JS_CALL_FLAG_CONSTRUCTOR) {
                        return Builder::callConstructor(ctx, ValueWeak(ctx, funcObj), ValueWeak(ctx, thisVal), args).loot().second;
                    } else {
                        return Builder::callFunction(ctx, ValueWeak(ctx, funcObj), ValueWeak(ctx, thisVal), args).loot().second;
                    }

                    return JS_UNDEFINED;
                });
            };
        }

        classDef = {
            .class_name = className.c_str(),
            .finalizer = finalizer,
            .gc_mark = nullptr,
            .call = call,
            .exotic = nullptr
        };
    }

    static void initContext(ContextRef ctx) {
        JSRuntime* rt = JS_GetRuntime(ctx);
        if (!JS_IsRegisteredClass(rt, classId)) {
            JS_NewClass(rt, classId, &classDef);
        }
        auto proto = Object::create(ctx);


        if constexpr (std::is_base_of_v<ProtoBuilder::Properties, Builder>) {
            Builder::addProperties(ctx, proto);
        }

        Function ctor(ctx, JS_NewCFunction2(ctx, constructor_impl, className.c_str(), 0, JS_CFUNC_constructor, 0));
        JS_SetConstructor(ctx, ctor.getVal(), proto.getVal());

        JS_SetClassProto(ctx, classId, proto.loot().second);
    }

    static JSClassID getClassId() {
        return classId;
    }

    static Object getProto(ContextRef ctx) {
        JSRuntime* rt = JS_GetRuntime(ctx);
        if (!JS_IsRegisteredClass(rt, classId)) {
            JS_NewClass(rt, classId, &classDef);
        }
        Value proto = Value(ctx, JS_GetClassProto(ctx, classId));
        if (!JS_IsObject(proto.getVal())) {
            initContext(ctx);
            proto = Value(ctx, JS_GetClassProto(ctx, classId));
        }
        return proto.to<Object>();
    }

    static Function getConstructor(ContextRef ctx) {
        Object proto = getProto(ctx);
        return proto.get("constructor").to<Function>();
    }

    template<typename T, typename Bdr = Builder>
    static std::enable_if_t<is_base_of_template_v<ProtoBuilder::Opaque, Bdr>
            && std::is_base_of_v<typename Bdr::OpaqueType, T>
            && std::is_same_v<Bdr, Builder>, Value>
        createInstance(ContextRef ctx, T* instance) {
        Value proto = getProto(ctx);
        Value obj(ctx, JS_NewObjectProtoClass(ctx, proto.getVal(), classId));
        JS_SetOpaque(obj.getVal(), instance);
        JS_SetConstructorBit(ctx, obj.getVal(), isConstructor);
        return obj;
    }
};


} // namespace jac
```


