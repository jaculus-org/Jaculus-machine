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

/**
 * @brief Checks if a type is derived from a template class
 *
 * @tparam Base The base template class
 * @tparam Derived The tested type
 */
template<template <typename...> class Base, typename Derived>
struct is_base_of_template : detail::is_base_of_template_impl<Base, Derived>::value_t {};

template<template <typename...> class Base, typename Derived>
using is_base_of_template_t = typename is_base_of_template<Base, Derived>::type;

template<template <typename...> class Base, typename Derived>
inline constexpr bool is_base_of_template_v = is_base_of_template<Base, Derived>::value;


namespace ProtoBuilder {

    /**
     * @brief A base class for javascript classes with opaque data
     *
     * The functions `constructOpaque` and `destroyOpaque` can be overriden
     * to provide custom construction and destruction of the opaque data.
     *
     * @tparam T The type of the opaque data
     */
    template<typename T>
    struct Opaque {
        using OpaqueType = T;
        static inline JSClassID classId;

        /**
         * @brief Construct a new Opaque object from javascript arguments
         * @note This function is only called upon javascript class instantiation
         *
         * @param ctx context to work in
         * @param args arguments passed to the constructor
         * @return A pointer to the opaque data
         */
        static T* constructOpaque(ContextRef /*ctx*/, ValueVectorWeak /*args*/) {
            throw Exception::create(Exception::Type::TypeError, "Class cannot be instantiated");
        }

        /**
         * @brief Destroy the Opaque object
         * @note This function is only called when the javascript object is garbage collected
         *
         * @param rt runtime to work in
         * @param ptr pointer to the opaque data
         */
        static void destroyOpaque(JSRuntime* /*rt*/, T* ptr) noexcept {
            delete ptr;
        }

        /**
         * @brief Get the Opaque object from an instance of the class
         *
         * @param ctx context to work in
         * @param thisVal the instance of the class
         * @return A pointer to the opaque data
         */
        static T* getOpaque(ContextRef /*ctx*/, ValueWeak thisVal) {
            T* ptr = static_cast<T*>(JS_GetOpaque(thisVal.getVal(), classId));
            if (!ptr) {
                throw Exception::create(Exception::Type::TypeError, "Invalid opaque data");
            }
            return ptr;
        }

        /**
         * @brief Process a call to a member function of the wrapped class
         * @note The arguments and return value are automatically converted to and from javascript values
         *
         * @tparam Sgn the signature of the member function
         * @tparam member pointer to the member function
         * @param ctx context to work in
         * @param funcObj instance of the class
         * @param thisVal value of `this` in the function
         * @param argv arguments passed to the function
         * @return Result of the call
         */
        template<typename Sgn, Sgn member>
        static Value callMember(ContextRef ctx, ValueWeak funcObj, ValueWeak thisVal, ValueVectorWeak argv) {
            const SgnUnwrap Unwrap_(member);

            return [&]<typename Res, typename... Args>(SgnUnwrap<Res(Args...)>) {
                auto f = [&](Args... args) -> Res {
                    T* ptr = static_cast<T*>(JS_GetOpaque(funcObj.getVal(), classId));
                    return (ptr->*member)(args...);
                };

                return processCall<decltype(f), Res, Args...>(ctx, thisVal, argv, f);
            }(Unwrap_);
        }


        /**
         * @brief Add a property to the object prototype from a member variable of the wrapped class
         *
         * @tparam U the type of the member variable
         * @tparam U(T::*member) pointer to the member variable
         * @param ctx context to work in
         * @param proto the prototype of the class
         * @param name name of the property
         * @param flags flags of the property
         */
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


        /**
         * @brief Add a property to the object prototype from a member function of the wrapped class
         *
         * @tparam Sgn signature of the member function
         * @tparam member pointer to the member function
         * @param ctx context to work in
         * @param proto the prototype of the class
         * @param name name of the property
         * @param flags flags of the property
         */
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

    /**
     * @brief A base class used to add handles for lifetime events of an instance
     *
     * The functions `postConstruction` can be overriden to provide custom
     * handling of the class instance after it's constructed.
     */
    struct LifetimeHandles {
        static void postConstruction(ContextRef ctx, Object thisVal, ValueVectorWeak args) {
            // do nothing
        }
    };

    /**
     * @brief A base class for javascript classes with callable instances
     *
     * The functions `callFunction` and `callConstructor` can be overriden
     * to provide custom handling of the class instance when it's called
     * as a function or as a constructor (with `new`).
     */
    struct Callable {
        /**
         * @brief Process a call to the wrapped class
         *
         * @param ctx context to work in
         * @param funcObj instance of the class
         * @param thisVal value of `this` in the function
         * @param args arguments passed to the function
         * @return result of the call
         */
        static Value callFunction(ContextRef /*ctx*/, ValueWeak /*funcObj*/, ValueWeak /*thisVal*/, ValueVectorWeak /*args*/) {
            throw Exception::create(Exception::Type::TypeError, "Class cannot be called as a function");
        }

        /**
         * @brief Process a call to the wrapped class as a constructor
         *
         * @param ctx context to work in
         * @param funcObj instance of the class
         * @param target value of `new.target` in the function
         * @param args arguments passed to the function
         * @return Result of the call
         */
        static Value callConstructor(ContextRef /*ctx*/, ValueWeak /*funcObj*/, ValueWeak /*target*/, ValueVectorWeak /*args*/) {
            throw Exception::create(Exception::Type::TypeError, "Class cannot be called as a constructor");
        }
    };

    /**
     * @brief A base class for javascript classes with added properties
     *
     * The function `addProperties` can be overriden to specify custom properties
     * for the class prototype.
     */
    struct Properties {
        /**
         * @brief Add properties to the object prototype
         * @note This function is only called when the class prototype is created
         *
         * @param ctx context to work in
         * @param proto the prototype of the class
         */
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
                ValueVectorWeak args(ctx, argv, argc);

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
    /**
     * @brief Initialize the class
     * @note This function should be called only once. Any subsequent calls
     * with different parameters will throw an exception
     *
     * @param name name of the class
     * @param isCtor whether or not the class is callable a constructor (if it's callable at all)
     */
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
                return propagateExceptions(ctx, [&]() -> JSValue {
                    ValueVectorWeak args(ctx, argv, argc);
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

    /**
     * @brief Initialize the class prototype
     * @note If the class is already initialized, this function does nothing
     *
     * @param ctx context to work in
     */
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

    /**
     * @brief Get the class id of this class
     *
     * @return JSClassID
     */
    static JSClassID getClassId() {
        return classId;
    }

    /**
     * @brief Get the prototype object of this class in given context
     * @note if the class wasn't initialized in the context, it will be initialized
     *
     * @param ctx context to work in
     * @return The prototype object
     */
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

    /**
     * @brief Get the constructor object of this class in given context
     * @note if the class wasn't initialized in the context, it will be initialized
     *
     * @param ctx context to work in
     * @return The constructor object
     */
    static Function getConstructor(ContextRef ctx) {
        Object proto = getProto(ctx);
        return proto.get("constructor").to<Function>();
    }

    /**
     * @brief Create a new instance of this class in given context
     * @note if the class wasn't initialized in the context, it will be initialized
     *
     * @param ctx the context
     * @param instance a new-allocated instance to be saved as opaque data
     * @return The new instance
     */
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
