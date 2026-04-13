

# File noal\_func.h

[**File List**](files.md) **>** [**src**](dir_68267d1309a1af8e8297ef4c3efbcdba.md) **>** [**noal\_func.h**](noal__func_8h.md)

[Go to the documentation of this file](noal__func_8h.md)


```C++
#pragma once

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <utility>


namespace noal {

template<typename Res, typename... Args>
class funcptr {
    Res (*_func)(Args...);
public:
    funcptr(Res (*func)(Args...)) : _func(func) {}
    inline Res operator()(Args... args) {
        return _func(std::forward<Args>(args)...);
    }
    static Res call(void* self, Args... args) {
        return (static_cast<funcptr*>(self))->operator()(std::forward<Args>(args)...);
    }
};

template<typename Res, typename... Args>
Res invoker(void* func, Args... args) {
    return (*static_cast<Res(**)(Args...)>(func))(std::forward<Args>(args)...);
}

template<class Class, typename Res, typename... Args>
class memberfuncptr {
    Res (Class::*_func)(Args...);
    Class* _self;
public:
    memberfuncptr(Res (Class::*func)(Args...), Class* self) : _func(func), _self(self) {}
    inline Res operator()(Args... args) {
        return (_self->*_func)(std::forward<Args>(args)...);
    }
    static Res call(void* self, Args... args) {
        return static_cast<memberfuncptr*>(self)->operator()(std::forward<Args>(args)...);
    }
};

template<class Class, typename Res, typename... Args>
class memberconstfuncptr {
    Res (Class::*_func)(Args...) const;
    Class* _self;
public:
    memberconstfuncptr(Res (Class::*func)(Args...) const, Class* self) : _func(func), _self(self) {}
    inline Res operator()(Args... args) {
        return _func(std::forward<Args>(args)...);
    }
    static Res call(void* self, Args... args) {
        return static_cast<memberconstfuncptr*>(self)->operator()(std::forward<Args>(args)...);
    }
};

template<typename Func>
struct signatureHelper;

template<typename Func, typename Res, typename... Args>
struct signatureHelper<Res (Func::*)(Args...)> {
    using type = Res(Args...);
};

template<typename Func, typename Res, typename... Args>
struct signatureHelper<Res (Func::*)(Args...) &> {
    using type = Res(Args...);
};

template<typename Func, typename Res, typename... Args>
struct signatureHelper<Res (Func::*)(Args...) const> {
    using type = Res(Args...);
};

template<typename Func, typename Res, typename... Args>
struct signatureHelper<Res (Func::*)(Args...) const &> {
    using type = Res(Args...);
};

template<typename Func, typename Sign>
class callableany;

template<typename Func, typename Res, typename... Args>
class callableany<Func, Res(Args...)> {
    Func func;
public:
    callableany(Func _func) : func(_func) {
        static_assert(std::is_trivially_copyable_v<Func>);
        static_assert(std::is_trivially_destructible_v<Func>);
    }
    inline Res operator()(Args... args) {
        return func(std::forward<Args>(args)...);
    }
    static Res call(void* self, Args... args) {
        return static_cast<callableany<Func, Res(Args...)>*>(self)->operator()(std::forward<Args>(args)...);
    }
};
template<typename Func, typename Sign = typename signatureHelper<decltype(&Func::operator())>::type>
callableany(Func) -> callableany<Func, Sign>;


template<typename Sign, size_t dataSize>
class function;

template<typename Res, typename... Args, size_t dataSize>
class function<Res(Args...), dataSize> {
    using Sign = Res(Args...);

    Res (*call)(void*, Args...);
    uint8_t data[dataSize];
public:
// default/copy/move constructors, copy/move assignment operators
    function() = default;
    template<size_t otherSize>
    function& operator=(const function<Sign, otherSize>& other) {
        static_assert(otherSize <= dataSize, "Other function object is too large");
        // static_assert(sizeof(other) <= sizeof(*this), "Other function object is too large");

        std::copy(static_cast<uint8_t*>(&other), static_cast<uint8_t*>(&other) + sizeof(other), static_cast<uint8_t*>(this));
        std::fill(static_cast<uint8_t*>(this) + sizeof(other), static_cast<uint8_t*>(this) + sizeof(*this), 0);  // possibly unnecessary - prevent data leaks?
        return *this;
    }
    template<size_t otherSize>
    function& operator=(function<Sign, otherSize>&& other) { *this = other; }
    template<size_t otherSize>
    function(const function<Sign, otherSize>& other) { *this = other; }
    template<size_t otherSize>
    function(function<Sign, otherSize>&& other) { *this = other; }

// member functions
    Res operator()(Args... args) {
        return call(data, std::forward<Args>(args)...);
    }

    explicit operator bool() const {
        return call != nullptr;
    }

// type erasure constructors, assignment operators
    explicit function(Res (*func)(Args...)) {
        // new (data) funcptr<Res, Args...>(func);
        // call = funcptr<Res, Args...>::call;
        std::copy(static_cast<uint8_t*>(&func), static_cast<uint8_t*>(&func) + sizeof(func), data);
        call = invoker<Res, Args...>;
    }
    function& operator=(Res (*func)(Args...)) { new (this) function(func); return *this; }

    template<class Class>
    function(Res (Class::*func)(Args...), Class* self) {
        new (data) memberfuncptr<Class, Res, Args...>(func, self);
        call = memberfuncptr<Class, Res, Args...>::call;
    }

    template<class Class>
    function(Res (Class::*func)(Args...) const, Class* self) {
        new (data) memberconstfuncptr<Class, Res, Args...>(func, self);
        call = memberconstfuncptr<Class, Res, Args...>::call;
    }
    template<typename Func, typename Sign = typename signatureHelper<decltype(&Func::operator())>::type>
    explicit function(Func func) {
        new (data) callableany<Func, Sign>(func);
        call = callableany<Func, Sign>::call;
    }
    template<typename Func, typename Sign = typename signatureHelper<decltype(&Func::operator())>::type>
    function& operator=(Func func) { new (this) function(func); return *this; }
};

// template<typename Res, typename... Args>
// function(Res (*)(Args...)) -> function<Res(Args...), sizeof(funcptr<Res, Args...>)>;
template<typename Res, typename... Args>
function(Res (*)(Args...)) -> function<Res(Args...), sizeof(Res (*)(Args...))>;

template<class Class, typename Res, typename... Args>
function(Res (Class::*)(Args...), Class*) -> function<Res(Args...), sizeof(memberfuncptr<Class, Res, Args...>)>;

template<class Class, typename Res, typename... Args>
function(Res (Class::*)(Args...) const, Class*) -> function<Res(Args...), sizeof(memberconstfuncptr<Class, Res, Args...>)>;

template<typename Func, typename Sign = typename signatureHelper<decltype(&Func::operator())>::type>
function(Func) -> function<Sign, sizeof(callableany<Func, Sign>)>;

} // namespace noal
```


