

# File values.h

[**File List**](files.md) **>** [**jac**](dir_256037ad7d0c306238e2bc4f945d341d.md) **>** [**machine**](dir_10e7d6e7bc593e38e57ffe1bab5ed259.md) **>** [**values.h**](values_8h.md)

[Go to the documentation of this file](values_8h.md)


```C++
#pragma once

#include <quickjs.h>
#include <span>
#include <string>
#include <tuple>
#include <vector>

#include "atom.h"
#include "context.h"
#include "internal/declarations.h"
#include "stringView.h"


namespace jac {


enum class PropFlags : int {
    Default = 0,
    Configurable = JS_PROP_CONFIGURABLE,
    Writable = JS_PROP_WRITABLE,
    Enumerable = JS_PROP_ENUMERABLE,
    C_W_E = JS_PROP_C_W_E
};

inline constexpr PropFlags operator|(PropFlags a, PropFlags b) {
    return static_cast<PropFlags>(static_cast<int>(a) | static_cast<int>(b));
}

inline constexpr PropFlags operator&(PropFlags a, PropFlags b) {
    return static_cast<PropFlags>(static_cast<int>(a) & static_cast<int>(b));
}


template<typename T>
T fromValue(ContextRef ctx, ValueWeak val);

template<typename T>
Value toValue(ContextRef ctx, T val);


template<bool managed>
class ValueWrapper {
protected:
    ContextRef _ctx;
    JSValue _val;
public:
    ValueWrapper(ContextRef ctx, JSValue val);
    ValueWrapper(const ValueWrapper &other):
        _ctx(other._ctx),
        _val(managed ? JS_DupValue(_ctx, other._val) : other._val)
    {}
    ValueWrapper(ValueWrapper &&other) : _ctx(other._ctx), _val(other._val) {
        other._ctx = nullptr;
        other._val = JS_UNDEFINED;
    }

    ValueWrapper& operator=(const ValueWrapper &other) {
        if (managed) {
            if (_ctx) {
                JS_FreeValue(_ctx, _val);
            }
            _val = JS_DupValue(_ctx, other._val);
        }
        else {
            _val = other._val;
        }
        _ctx = other._ctx;
        return *this;
    }

    ValueWrapper& operator=(ValueWrapper &&other) {
        if (managed && _ctx) {
            JS_FreeValue(_ctx, _val);
        }
        _val = other._val;
        _ctx = other._ctx;
        other._val = JS_UNDEFINED;
        other._ctx = nullptr;
        return *this;
    }

    operator ValueWeak() {
        return ValueWeak(_ctx, _val);
    }

    ~ValueWrapper() {
        if (managed && _ctx) {
            JS_FreeValue(_ctx, _val);
        }
    }

    std::pair<ContextRef, JSValue> loot() {
        JSValue ret_ = _val;
        ContextRef ctx_ = this->_ctx;
        _ctx = nullptr;
        _val = JS_UNDEFINED;
        return {ctx_, ret_};
    }

    JSValue& getVal() {
        return _val;
    }

    bool isUndefined() {
        return JS_IsUndefined(_val);
    }

    bool isNull() {
        return JS_IsNull(_val);
    }

    bool isNumber() {
        return JS_IsNumber(_val);
    }

    bool isBigInt() {
        return JS_IsBigInt(_ctx, _val);
    }

    bool isBoolean() {
        return JS_IsBool(_val);
    }

    bool isString() {
        return JS_IsString(_val);
    }

    bool isSymbol() {
        return JS_IsSymbol(_val);
    }

    bool isObject() {
        return JS_IsObject(_val);
    }

    bool isArray() {
        return JS_IsArray(_ctx, _val);
    }

    bool isFunction() {
        return JS_IsFunction(_ctx, _val);
    }

    bool isInstanceOf(ObjectWeak obj);

    StringView toString() {
        return to<StringView>();
    }

    template<typename T>
    T to() {
        return fromValue<T>(_ctx, *this);
    }

    Value stringify(int indent = 0) {
        auto idt = Value::from(_ctx, indent);
        return Value(_ctx, JS_JSONStringify(_ctx, _val, JS_UNDEFINED, idt.getVal()));
    }

    template<typename T>
    static Value from(ContextRef ctx, T val) {
        return toValue(ctx, val);
    }

    static Value fromJSON(ContextRef ctx, std::string json, std::string filename = "<json>", bool extended = false) {
        return Value(ctx, JS_ParseJSON2(ctx, json.c_str(), json.size(), filename.c_str(), extended ? JS_PARSE_JSON_EXT : 0));
    }

    static Value undefined(ContextRef ctx) {
        return ValueWrapper(ctx, JS_UNDEFINED);
    }

    static Value null(ContextRef ctx) {
        return ValueWrapper(ctx, JS_NULL);
    }

    friend std::ostream& operator<<(std::ostream& os, ValueWrapper& val) {
        os << val.toString();
        return os;
    }
};


template<bool managed>
class ExceptionWrapper : public ValueWrapper<managed>, public std::exception {
public:
    enum class Type {
        Any,
        Error,
        SyntaxError,
        TypeError,
        ReferenceError,
        RangeError,
        InternalError
    };
private:
    std::string _message;
    Type _type;

    ExceptionWrapper(Type type, std::string message) : ValueWrapper<managed>(nullptr, JS_UNDEFINED), _message(std::move(message)), _type(type) {}
protected:
    using ValueWrapper<managed>::_val;
    using ValueWrapper<managed>::_ctx;
public:
    ExceptionWrapper(ValueWrapper<managed> value) : ValueWrapper<managed>(std::move(value)), _type(Type::Any) {
        _message = this->toString();
    }
    ExceptionWrapper(ContextRef ctx, JSValue val) : ExceptionWrapper(ValueWrapper<managed>(ctx, val)) {}

    std::string stackTrace() noexcept;

    const char* what() const noexcept override {
        return _message.c_str();
    }

    static Exception create(Type type, std::string message) {
        return ExceptionWrapper(type, message);
    }

    JSValue throwJS(ContextRef ctx);
};


template<bool managed>
class ObjectWrapper : public ValueWrapper<managed> {
protected:
    using ValueWrapper<managed>::_val;
    using ValueWrapper<managed>::_ctx;
public:
    ObjectWrapper(ValueWrapper<managed> value) : ValueWrapper<managed>(std::move(value)) {
        if (!this->isObject()) {
            throw Exception::create(Exception::Type::TypeError, "not an object");
        }
    }
    ObjectWrapper(ContextRef ctx, JSValue val) : ObjectWrapper(ValueWrapper<managed>(ctx, val)) {}

    template<typename T = Value>
    T get(Atom prop) {
        Value val(_ctx, JS_GetProperty(_ctx, _val, prop.get()));
        return val.to<T>();
    }

    template<typename T = Value>
    T get(const std::string& name) {
        return get<T>(Atom::create(_ctx, name.c_str()));
    }

    template<typename T = Value>
    T get(uint32_t idx) {
        return get<T>(Atom::create(_ctx, idx));
    }

    template<typename T>
    void set(Atom prop, T val) {
        if (JS_SetProperty(_ctx, _val, prop.get(), toValue(_ctx, val).loot().second) < 0) {
            throw _ctx.getException();
        }
    }

    template<typename T>
    void set(const std::string& name, T val) {
        set(Atom::create(_ctx, name.c_str()), val);
    }

    template<typename T>
    void set(uint32_t idx, T val) {
        set(Atom::create(_ctx, idx), val);
    }

    template<typename Res, typename... Args>
    Res invoke(Atom key, Args... args);

    template<typename Res, typename... Args>
    Res invoke(const std::string& key, Args... args) {
        return invoke<Res>(Atom::create(_ctx, key.c_str()), args...);
    }

    template<typename Res, typename... Args>
    Res invoke(uint32_t idx, Args... args) {
        return invoke<Res>(Atom::create(_ctx, idx), args...);
    }

    template<typename Id>
    void defineProperty(Id id, Value value, PropFlags flags = PropFlags::Default) {
        Atom atom = Atom::create(_ctx, id);
        if (JS_DefinePropertyValue(_ctx, _val, atom.get(), value.loot().second, static_cast<int>(flags)) < 0) {
            throw _ctx.getException();
        }
    }

    template<typename Id>
    bool hasProperty(Id id) {
        Atom atom = Atom::create(_ctx, id);
        int res = JS_HasProperty(_ctx, _val, atom.get());
        if (res < 0) {
            throw _ctx.getException();
        }
        return res;
    }

    template<typename Id>
    void deleteProperty(Id id) {
        Atom atom = Atom::create(_ctx, id);
        if (JS_DeleteProperty(_ctx, _val, atom.get(), 0) < 0) {
            throw _ctx.getException();
        }
    }

    Object getPrototype() {
        return Object(_ctx, JS_GetPrototype(this->_ctx, this->_val));
    }

    void setPrototype(Object proto) {
        if (JS_SetPrototype(this->_ctx, this->_val, proto.getVal()) < 0) {
            throw _ctx.getException();
        }
    }

    std::vector<Atom> getOwnPropertyNames(int flags = JS_GPN_STRING_MASK | JS_GPN_ENUM_ONLY) {
        JSPropertyEnum* props;
        uint32_t len;
        if (JS_GetOwnPropertyNames(_ctx, &props, &len, _val, flags) < 0) {
            throw _ctx.getException();
        }

        std::vector<Atom> result;
        result.reserve(len);
        for (uint32_t i = 0; i < len; i++) {
            result.emplace_back(Atom(_ctx, props[i].atom));
        }
        js_free(_ctx, props);
        return result;
    }

    static Object create(ContextRef ctx) {
        return Object(ctx, JS_NewObject(ctx));
    }
};


template<bool managed>
class FunctionWrapper : public ObjectWrapper<managed> {
protected:
    using ObjectWrapper<managed>::_val;
    using ObjectWrapper<managed>::_ctx;
public:
    FunctionWrapper(ObjectWrapper<managed> value) : ObjectWrapper<managed>(std::move(value)) {
        if (!this->isFunction()) {
            throw Exception::create(Exception::Type::TypeError, "not a function");
        }
    }
    FunctionWrapper(ContextRef ctx, JSValue val) : FunctionWrapper(ObjectWrapper<managed>(ctx, val)) {}

    template<typename Res, typename... Args>
    Res callThis(Value thisVal, Args... args) {
        std::vector<JSValue> vals;
        vals.reserve(sizeof...(Args));
        try {
            (vals.push_back(toValue(_ctx, args).loot().second), ...);
            Value ret(_ctx, JS_Call(_ctx, _val, thisVal.getVal(), vals.size(), vals.data()));

            for (auto &v : vals) {
                JS_FreeValue(_ctx, v);
            }
            vals.clear();

            return ret.to<Res>();
        } catch (Exception &e) {
            for (auto &v : vals) {
                JS_FreeValue(_ctx, v);
            }
            throw e;
        }
    }

    template<typename Res, typename... Args>
    Res call(Args... args) {
        return callThis<Res>(Value::undefined(_ctx), args...);
    }

    template<typename... Args>
    Value callConstructor(Args... args) {
        std::vector<JSValue> vals;
        vals.reserve(sizeof...(Args));
        try {
            (vals.push_back(toValue(_ctx, args).loot().second), ...);
            Value ret(_ctx, JS_CallConstructor(_ctx, _val, vals.size(), vals.data()));

            for (auto &v : vals) {
                JS_FreeValue(_ctx, v);
            }
            vals.clear();

            return ret;
        } catch (Exception &e) {
            for (auto &v : vals) {
                JS_FreeValue(_ctx, v);
            }
            throw e;
        }
    }
};


template<bool managed>
class ArrayWrapper : public ObjectWrapper<managed> {
protected:
    using ObjectWrapper<managed>::_val;
    using ObjectWrapper<managed>::_ctx;
public:
    ArrayWrapper(ObjectWrapper<managed> value) : ObjectWrapper<managed>(std::move(value)) {
        if (!this->isArray()) {
            throw Exception::create(Exception::Type::TypeError, "not an array");
        }
    }
    ArrayWrapper(ContextRef ctx, JSValue val) : ArrayWrapper(ObjectWrapper<managed>(ctx, val)) {}

    int length() {
        return this->template get<int>("length");
    }

    static Array create(ContextRef ctx) {
        return Array(ctx, JS_NewArray(ctx));
    }
};


template<bool managed>
class PromiseWrapper : public ObjectWrapper<managed> {
protected:
    using ObjectWrapper<managed>::_val;
    using ObjectWrapper<managed>::_ctx;
public:
    PromiseWrapper(ObjectWrapper<managed> value) : ObjectWrapper<managed>(std::move(value)) {
        // TODO: check if value is Promise
        // not implemented, because a convenient check is not a part of QuickJS API
        // different type being converted to promise may cause hard to find errors
    }
    PromiseWrapper(ContextRef ctx, JSValue val) : PromiseWrapper(ObjectWrapper<managed>(ctx, val)) {}

    static std::tuple<Promise, Function, Function> create(ContextRef ctx) {
        JSValue functions[2];
        JSValue promise = JS_NewPromiseCapability(ctx, functions);
        return std::make_tuple(Promise(ctx, promise), Function(ctx, functions[0]), Function(ctx, functions[1]));
    }
};


template<bool managed>
class ArrayBufferWrapper : public ObjectWrapper<managed> {
protected:
    using ObjectWrapper<managed>::_val;
    using ObjectWrapper<managed>::_ctx;

    static void freeArrayBuffer(JSRuntime*, void*, void *ptr) {
        delete[] static_cast<uint8_t*>(ptr);
    }
public:
    ArrayBufferWrapper(ObjectWrapper<managed> value) : ObjectWrapper<managed>(std::move(value)) {
        // TODO: check if value is ArrayBuffer
        // not implemented, because a convenient check is not a part of QuickJS API
        // different type being converted to promise may cause hard to find errors
        // methods of ArrayBufferWrapper will throw exceptions if the type is not ArrayBuffer
    }
    ArrayBufferWrapper(ContextRef ctx, JSValue val) : ArrayBufferWrapper(ObjectWrapper<managed>(ctx, val)) {}

    uint8_t* data() {
        return static_cast<uint8_t*>(JS_GetArrayBuffer(_ctx, nullptr, _val));
    }

    size_t size() {
        size_t size;
        JS_GetArrayBuffer(_ctx, &size, _val);
        return size;
    }

    template<typename T>
    std::span<T> typedView() {
        if (size() % sizeof(T) != 0) {
            throw Exception::create(Exception::Type::TypeError, "size is not a multiple of the element size");
        }
        size_t size;
        T* ptr = static_cast<T*>(JS_GetArrayBuffer(_ctx, &size, _val));
        return std::span<T>(ptr, size / sizeof(T));
    }

    static ArrayBuffer create(ContextRef ctx, size_t size) {
        return ArrayBuffer(ctx, JS_NewArrayBuffer(ctx, new uint8_t[size]{}, size, freeArrayBuffer, nullptr, false));
    }

    static ArrayBuffer create(ContextRef ctx, std::span<const uint8_t> data) {
        return ArrayBuffer(ctx, JS_NewArrayBufferCopy(ctx, data.data(), data.size()));
    }
};

template<bool managed>
ValueWrapper<managed>::ValueWrapper(ContextRef ctx, JSValue val) : _ctx(ctx), _val(val) {
    if (JS_IsException(_val)) {
        throw ctx.getException();
    }
}

template<bool managed>
bool ValueWrapper<managed>::isInstanceOf(ObjectWeak obj) {
    return JS_IsInstanceOf(_ctx, _val, obj.getVal());
}

template<bool managed>
template<typename Res, typename... Args>
Res ObjectWrapper<managed>::invoke(Atom key, Args... args) {
    return get<Function>(key).template callThis<Res>(*this, args...);
};


template<bool managed>
std::string ExceptionWrapper<managed>::stackTrace() noexcept {
    try {
        ObjectWeak obj(*this);
        return obj.get("stack").toString();
    } catch (std::exception &e) {
        return "failed to get stack trace: " + std::string(e.what());
    }
}


template<bool managed>
JSValue ExceptionWrapper<managed>::throwJS(ContextRef ctx) {
    if (_type == Type::Any) {
        auto [_, val] = ValueWrapper<managed>::loot();
        return JS_Throw(ctx, val);
    }


    if (_type == Type::Error) {
        ObjectWeak errObj(ctx, JS_NewError(ctx));
        errObj.set("message", _message);

        return JS_Throw(ctx, errObj.getVal());
    }

    switch (_type) {
        case Type::SyntaxError:
            return JS_ThrowSyntaxError(ctx, "%s", _message.c_str());
        case Type::TypeError:
            return JS_ThrowTypeError(ctx, "%s", _message.c_str());
        case Type::ReferenceError:
            return JS_ThrowReferenceError(ctx, "%s", _message.c_str());
        case Type::RangeError:
            return JS_ThrowRangeError(ctx, "%s", _message.c_str());
        case Type::InternalError:
            return JS_ThrowInternalError(ctx, "%s", _message.c_str());
        default:
            return JS_Throw(ctx, JS_NewError(ctx));
    }
}


} // namespace jac


#include "traits.h"


namespace jac {


template<typename T>
T fromValue([[maybe_unused]] ContextRef ctx, [[maybe_unused]] ValueWeak val) {
    if constexpr (std::is_same_v<T, void>) {
        return;
    }
    else {
        return ConvTraits<T>::from(ctx, val);
    }
}

template<typename T>
Value toValue([[maybe_unused]] ContextRef ctx, [[maybe_unused]] T value) {
    if constexpr (std::is_same_v<T, void>) {
        return Value::undefined(ctx);
    }

    auto val = ConvTraits<T>::to(ctx, value);

    return val;
}


} // namespace jac
```


