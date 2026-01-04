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


/**
 * @brief Flags to specify property access attributes
 */
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


/**
 * @brief A wrapper around JSValue with RAII.
 *
 * @tparam managed whether the JSValue should be freed when the wrapper is destroyed.
 */
template<bool managed>
class ValueWrapper {
protected:
    ContextRef _ctx;
    JSValue _val;
public:
    /**
     * @brief Wrap an existing JSValue. If managed is true, JSValue will be freed when the Value is destroyed.
     * @note Used internally when directly working with QuickJS API. New Value should be created using Value::from<T>(), Value::undefined(), etc.
     *
     * @param ctx context to work in
     * @param val JSValue to wrap
     */
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

    /**
     * @brief Release ownership of the JSValue. The JSValue will have to be freed manually.
     * @note After this call, the Value is in an undefined state.
     *
     * @return Pair of ContextRef and JSValue
     */
    std::pair<ContextRef, JSValue> loot() {
        JSValue ret_ = _val;
        ContextRef ctx_ = this->_ctx;
        _ctx = nullptr;
        _val = JS_UNDEFINED;
        return {ctx_, ret_};
    }

    /**
     * @brief Get reference to the underlying JSValue.
     *
     * @return JSValue reference
     */
    JSValue& getVal() {
        return _val;
    }

    /**
     * @brief Check if the Value is undefined.
     *
     * @return true if the Value is undefined, false otherwise
     */
    bool isUndefined() {
        return JS_IsUndefined(_val);
    }

    /**
     * @brief Check if the Value is null.
     *
     * @return true if the Value is null, false otherwise
     */
    bool isNull() {
        return JS_IsNull(_val);
    }

    /**
     * @brief Check if the Value is number.
     *
     * @return true if the Value is number, false otherwise
     */
    bool isNumber() {
        return JS_IsNumber(_val);
    }

    /**
     * @brief Check if the Value is BigInt.
     *
     * @return true if the Value is BigInt, false otherwise
     */
    bool isBigInt() {
        return JS_IsBigInt(_ctx, _val);
    }

    /**
     * @brief Check if the Value is boolean.
     *
     * @return true if the Value is boolean, false otherwise
     */
    bool isBoolean() {
        return JS_IsBool(_val);
    }

    /**
     * @brief Check if the Value is string.
     *
     * @return true if the Value is string, false otherwise
     */
    bool isString() {
        return JS_IsString(_val);
    }

    /**
     * @brief Check if the Value is symbol.
     *
     * @return true if the Value is symbol, false otherwise
     */
    bool isSymbol() {
        return JS_IsSymbol(_val);
    }

    /**
     * @brief Check if the Value is an object.
     *
     * @return true if the Value is an object, false otherwise
     */
    bool isObject() {
        return JS_IsObject(_val);
    }

    /**
     * @brief Check if the Value is an array.
     *
     * @return true if the Value is an array, false otherwise
     */
    bool isArray() {
        return JS_IsArray(_ctx, _val);
    }

    /**
     * @brief Check if the Value is a function.
     *
     * @return true if the Value is a function, false otherwise
     */
    bool isFunction() {
        return JS_IsFunction(_ctx, _val);
    }

    /**
     * @brief Check if the Value is an instance of the given object.
     * @param obj object to check against
     *
     * @return true if the Value is an instance of the given object, false otherwise
     */
    bool isInstanceOf(ObjectWeak obj);

    /**
     * @brief Convert the Value to a StringView.
     *
     * @return The StringView
     */
    StringView toString() {
        return to<StringView>();
    }

    /**
     * @brief Convert the Value to a specified type.
     *
     * @tparam T Type to convert to
     * @return The converted value
     */
    template<typename T>
    T to() {
        return fromValue<T>(_ctx, *this);
    }

    /**
     * @brief Convert the Value to a JSON representation.
     *
     * @param indent indentation level
     * @return Value containing the JSON representation
     */
    Value stringify(int indent = 0) {
        auto idt = Value::from(_ctx, indent);
        return Value(_ctx, JS_JSONStringify(_ctx, _val, JS_UNDEFINED, idt.getVal()));
    }

    /**
     * @brief Create a new Value by converting a given value.
     *
     * @tparam T Type of the value
     * @param ctx context to work in
     * @param val value to convert
     * @return The resulting Value
     */
    template<typename T>
    static Value from(ContextRef ctx, T val) {
        return toValue(ctx, val);
    }

    /**
     * @brief Create a new Value from a given JSON string.
     *
     * @param ctx context to work in
     * @param json JSON string
     * @param filename filename to use as the source of the JSON
     * @param extended whether to use extended JSON
     * @return The resulting Value
     */
    static Value fromJSON(ContextRef ctx, std::string json, std::string filename = "<json>", bool extended = false) {
        return Value(ctx, JS_ParseJSON2(ctx, json.c_str(), json.size(), filename.c_str(), extended ? JS_PARSE_JSON_EXT : 0));
    }

    /**
     * @brief Create a new Value containing undefined.
     *
     * @param ctx context to work in
     * @return The resulting Value
     */
    static Value undefined(ContextRef ctx) {
        return ValueWrapper(ctx, JS_UNDEFINED);
    }

    /**
     * @brief Create a new Value containing null.
     *
     * @param ctx context to work in
     * @return The resulting Value
     */
    static Value null(ContextRef ctx) {
        return ValueWrapper(ctx, JS_NULL);
    }

    friend std::ostream& operator<<(std::ostream& os, ValueWrapper& val) {
        os << val.toString();
        return os;
    }
};


/**
 * @brief An exception wrapper which can either wrap a JSValue or contain an exception description
 * and can be thrown into JS as a specific Error type.
 *
 * @tparam managed whether the JSValue should be freed when the wrapper is destroyed.
 */
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
    /**
     * @brief Wrap an existing JSValue. If managed is true, JSValue will be freed when the Exception is destroyed.
     * @note Used internally when directly working with QuickJS API. New Exception should be created using Exception::create()
     * or by converting an existing Value to an Exception using Value::to<Exception>().
     *
     * @param ctx context to work in
     * @param val JSValue to wrap
     */
    ExceptionWrapper(ValueWrapper<managed> value) : ValueWrapper<managed>(std::move(value)), _type(Type::Any) {
        _message = this->toString();
    }
    ExceptionWrapper(ContextRef ctx, JSValue val) : ExceptionWrapper(ValueWrapper<managed>(ctx, val)) {}

    /**
     * @brief Get the exception stack trace.
     *
     * @return std::string containing the stack trace
     */
    std::string stackTrace() noexcept;

    /**
     * @brief Get the exception message.
     *
     * @return std::string containing the exception message
     */
    const char* what() const noexcept override {
        return _message.c_str();
    }

    /**
     * @brief Create a new Exception.
     *
     * @param type type of the exception
     * @param message exception message
     * @return The resulting Exception
     */
    static Exception create(Type type, std::string message) {
        return ExceptionWrapper(type, message);
    }

    /**
     * @brief Throw the exception into JS.
     * @note Used internally when directly working with QuickJS API. In most cases, exceptions should be
     * thrown using a throw statement and wrapper functions will propagate the exception to JS.
     * @param ctx context to throw the exception in
     *
     * @return JSValue containing the exception
     */
    JSValue throwJS(ContextRef ctx);
};


/**
 * @brief A wrapper for JSValue with Object type with RAII.
 *
 * @tparam managed whether the JSValue should be freed when the wrapper is destroyed.
 */
template<bool managed>
class ObjectWrapper : public ValueWrapper<managed> {
protected:
    using ValueWrapper<managed>::_val;
    using ValueWrapper<managed>::_ctx;
public:
    /**
     * @brief Wrap an existing JSValue. If managed is true, JSValue will be freed when the Object is destroyed.
     * @note Used internally when directly working with QuickJS API. New Object should be created using Object::create() or
     * by converting an existing Value to an Object using Value::to<Object>().
     *
     * @param ctx context to work in
     * @param val JSValue to wrap
     */
    ObjectWrapper(ValueWrapper<managed> value) : ValueWrapper<managed>(std::move(value)) {
        if (!this->isObject()) {
            throw Exception::create(Exception::Type::TypeError, "not an object");
        }
    }
    ObjectWrapper(ContextRef ctx, JSValue val) : ObjectWrapper(ValueWrapper<managed>(ctx, val)) {}

    /**
     * @brief Get a property of the object.
     *
     * @tparam T type to convert the property to
     * @param prop the property identifier
     * @return The resulting value
     */
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

    /**
     * @brief Set a property of the object.
     *
     * @tparam T type of the value to set
     * @param prop the property identifier
     * @param val the value to set
     */
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

    /**
     * @brief Invoke a method of the object.
     * @note The call will automatically convert the arguments to their JavaScript counterparts, the result
     * will be converted to the specified type and Exceptions thrown in JS will be propagated to C++ as
     * jac::Exception.
     *
     * @tparam Res type to convert the result to
     * @tparam Args types of the arguments
     * @param prop the property identifier
     * @param args the arguments
     * @return The resulting value
     */
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

    /**
     * @brief Define a property of the object.
     *
     * @tparam Id the type of the property identifier (Atom, std::string, uint32_t)
     * @param id the property identifier
     * @param value the value to set
     * @param flags the property flags
     */
    template<typename Id>
    void defineProperty(Id id, Value value, PropFlags flags = PropFlags::Default) {
        Atom atom = Atom::create(_ctx, id);
        if (JS_DefinePropertyValue(_ctx, _val, atom.get(), value.loot().second, static_cast<int>(flags)) < 0) {
            throw _ctx.getException();
        }
    }

    /**
     * @brief Check if the object has a property.
     *
     * @tparam Id the type of the property identifier (Atom, std::string, uint32_t)
     * @param id the property identifier
     * @return true if the object has the property, false otherwise
     */
    template<typename Id>
    bool hasProperty(Id id) {
        Atom atom = Atom::create(_ctx, id);
        int res = JS_HasProperty(_ctx, _val, atom.get());
        if (res < 0) {
            throw _ctx.getException();
        }
        return res;
    }

    /**
     * @brief Delete a property of the object.
     *
     * @tparam Id the type of the property identifier (Atom, std::string, uint32_t)
     * @param id the property identifier
     */
    template<typename Id>
    void deleteProperty(Id id) {
        Atom atom = Atom::create(_ctx, id);
        if (JS_DeleteProperty(_ctx, _val, atom.get(), 0) < 0) {
            throw _ctx.getException();
        }
    }

    /**
     * @brief Get the prototype of the object.
     *
     * @return The prototype
     */
    Object getPrototype() {
        return Object(_ctx, JS_GetPrototype(this->_ctx, this->_val));
    }

    /**
     * @brief Set the prototype of the object.
     *
     * @param proto the prototype
     */
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

    /**
     * @brief Create a new empty object.
     *
     * @param ctx the context to create the object in
     * @return The new object
     */
    static Object create(ContextRef ctx) {
        return Object(ctx, JS_NewObject(ctx));
    }
};


/**
 * @brief A wrapper for JSValue with Function type with RAII.
 *
 * @tparam managed whether the JSValue should be freed when the wrapper is destroyed.
 */
template<bool managed>
class FunctionWrapper : public ObjectWrapper<managed> {
protected:
    using ObjectWrapper<managed>::_val;
    using ObjectWrapper<managed>::_ctx;
public:
    /**
     * @brief Wrap an existing JSValue. If managed is true, JSValue will be freed when the Function is destroyed.
     * @note Used internally when directly working with QuickJS API. New Function should be created using FunctionFactory.
     *
     * @param ctx context to work in
     * @param val JSValue to wrap
     */
    FunctionWrapper(ObjectWrapper<managed> value) : ObjectWrapper<managed>(std::move(value)) {
        if (!this->isFunction()) {
            throw Exception::create(Exception::Type::TypeError, "not a function");
        }
    }
    FunctionWrapper(ContextRef ctx, JSValue val) : FunctionWrapper(ObjectWrapper<managed>(ctx, val)) {}

    /**
     * @brief Call the function with `this` set to a given object.
     * @note The call will automatically convert the arguments to their JavaScript counterparts, the result
     * will be converted to the specified type and Exceptions thrown in JS will be propagated to C++ as
     * jac::Exception.
     *
     * @tparam Res type to convert the result to
     * @tparam Args types of the arguments
     * @param prop the property identifier
     * @param args the arguments
     * @return The resulting value
     */
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

    /**
     * @brief Call the function.
     * @note The call will automatically convert the arguments to their JavaScript counterparts, the result
     * will be converted to the specified type and Exceptions thrown in JS will be propagated to C++ as
     * jac::Exception.
     *
     * @tparam Res type to convert the result to
     * @tparam Args types of the arguments
     * @param prop the property identifier
     * @param args the arguments
     * @return The resulting value
     */
    template<typename Res, typename... Args>
    Res call(Args... args) {
        return callThis<Res>(Value::undefined(_ctx), args...);
    }

    /**
     * @brief Call the function as a constructor.
     * @note The call will automatically convert the arguments to their JavaScript counterparts, the result
     * will be converted to the specified type and Exceptions thrown in JS will be propagated to C++ as
     * jac::Exception.
     *
     * @tparam Res type to convert the result to
     * @tparam Args types of the arguments
     * @param prop the property identifier
     * @param args the arguments
     * @return The resulting value
     */
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


/**
 * @brief A wrapper for JSValue with Array type with RAII.
 *
 * @tparam managed whether the JSValue should be freed when the wrapper is destroyed.
 */
template<bool managed>
class ArrayWrapper : public ObjectWrapper<managed> {
protected:
    using ObjectWrapper<managed>::_val;
    using ObjectWrapper<managed>::_ctx;
public:
    /**
     * @brief Wrap an existing JSValue. If managed is true, JSValue will be freed when the Array is destroyed.
     * @note Used internally when directly working with QuickJS API. New Array should be created using Array::create().
     *
     * @param ctx context to work in
     * @param val JSValue to wrap
     */
    ArrayWrapper(ObjectWrapper<managed> value) : ObjectWrapper<managed>(std::move(value)) {
        if (!this->isArray()) {
            throw Exception::create(Exception::Type::TypeError, "not an array");
        }
    }
    ArrayWrapper(ContextRef ctx, JSValue val) : ArrayWrapper(ObjectWrapper<managed>(ctx, val)) {}

    /**
     * @brief Get the length of the array
     *
     * @return The length
     */
    int length() {
        return this->template get<int>("length");
    }

    /**
     * @brief Create a new Array object
     *
     * @param ctx context to work in
     * @return The new Array object
     */
    static Array create(ContextRef ctx) {
        return Array(ctx, JS_NewArray(ctx));
    }
};


/**
 * @brief A wrapper for JSValue with Promise type with RAII.
 *
 * @tparam managed whether the JSValue should be freed when the wrapper is destroyed.
 */
template<bool managed>
class PromiseWrapper : public ObjectWrapper<managed> {
protected:
    using ObjectWrapper<managed>::_val;
    using ObjectWrapper<managed>::_ctx;
public:
    /**
     * @brief Wrap an existing JSValue. If managed is true, JSValue will be freed when the Promise is destroyed.
     * @note Used internally when directly working with QuickJS API. New Promise should be created using Promise::create().
     *
     * @param ctx context to work in
     * @param val JSValue to wrap
     */
    PromiseWrapper(ObjectWrapper<managed> value) : ObjectWrapper<managed>(std::move(value)) {
        // TODO: check if value is Promise
        // not implemented, because a convenient check is not a part of QuickJS API
        // different type being converted to promise may cause hard to find errors
    }
    PromiseWrapper(ContextRef ctx, JSValue val) : PromiseWrapper(ObjectWrapper<managed>(ctx, val)) {}

    /**
     * @brief Create a new Promise object
     * @return Tuple of the Promise object, resolve function, reject function
     */
    static std::tuple<Promise, Function, Function> create(ContextRef ctx) {
        JSValue functions[2];
        JSValue promise = JS_NewPromiseCapability(ctx, functions);
        return std::make_tuple(Promise(ctx, promise), Function(ctx, functions[0]), Function(ctx, functions[1]));
    }
};


/**
 * @brief A wrapper for JSValue with ArrayBuffer type with RAII.
 *
 * @tparam managed whether the JSValue should be freed when the wrapper is destroyed.
 */
template<bool managed>
class ArrayBufferWrapper : public ObjectWrapper<managed> {
protected:
    using ObjectWrapper<managed>::_val;
    using ObjectWrapper<managed>::_ctx;

    static void freeArrayBuffer(JSRuntime*, void*, void *ptr) {
        delete[] static_cast<uint8_t*>(ptr);
    }
public:
    /**
     * @brief Wrap an existing JSValue. If managed is true, JSValue will be freed when the ArrayBuffer is destroyed.
     * @note Used internally when directly working with QuickJS API. New ArrayBuffer should be created using ArrayBuffer::create().
     *
     * @param ctx context to work in
     * @param val JSValue to wrap
     */
    ArrayBufferWrapper(ObjectWrapper<managed> value) : ObjectWrapper<managed>(std::move(value)) {
        // TODO: check if value is ArrayBuffer
        // not implemented, because a convenient check is not a part of QuickJS API
        // different type being converted to promise may cause hard to find errors
        // methods of ArrayBufferWrapper will throw exceptions if the type is not ArrayBuffer
    }
    ArrayBufferWrapper(ContextRef ctx, JSValue val) : ArrayBufferWrapper(ObjectWrapper<managed>(ctx, val)) {}

    /**
     * @brief Get a pointer to the underlying buffer
     *
     * @return Pointer to the data
     */
    uint8_t* data() {
        return static_cast<uint8_t*>(JS_GetArrayBuffer(_ctx, nullptr, _val));
    }

    /**
     * @brief Get the size of the underlying buffer
     *
     * @return Size of the data
     */
    size_t size() {
        size_t size;
        JS_GetArrayBuffer(_ctx, &size, _val);
        return size;
    }

    /**
     * @brief Get a typed view of the underlying buffer
     *
     * @tparam T Type of the view
     * @return Typed view of the data
     */
    template<typename T>
    std::span<T> typedView() {
        if (size() % sizeof(T) != 0) {
            throw Exception::create(Exception::Type::TypeError, "size is not a multiple of the element size");
        }
        size_t size;
        T* ptr = static_cast<T*>(JS_GetArrayBuffer(_ctx, &size, _val));
        return std::span<T>(ptr, size / sizeof(T));
    }

    /**
     * @brief Create a new ArrayBuffer object
     *
     * @param ctx context to work in
     * @param size size of the buffer
     * @return The new ArrayBuffer object
     */
    static ArrayBuffer create(ContextRef ctx, size_t size) {
        return ArrayBuffer(ctx, JS_NewArrayBuffer(ctx, new uint8_t[size]{}, size, freeArrayBuffer, nullptr, false));
    }

    /**
     * @brief Create a new ArrayBuffer object
     *
     * @param ctx context to work in
     * @param data data to copy into the buffer
     * @return The new ArrayBuffer object
     */
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
