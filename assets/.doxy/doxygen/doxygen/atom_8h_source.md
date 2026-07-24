

# File atom.h

[**File List**](files.md) **>** [**jac**](dir_256037ad7d0c306238e2bc4f945d341d.md) **>** [**machine**](dir_10e7d6e7bc593e38e57ffe1bab5ed259.md) **>** [**atom.h**](atom_8h.md)

[Go to the documentation of this file](atom_8h.md)


```C++
#pragma once

#include <quickjs.h>

#include <ostream>
#include <string>

#include "internal/declarations.h"

#include "context.h"
#include "stringView.h"


namespace jac {


template<typename T=int>
constexpr bool static_false() {
    return false;
};


class Atom {
protected:
    ContextRef _ctx;
    JSAtom _atom;
public:
    Atom(ContextRef ctx, JSAtom atom) : _ctx(ctx), _atom(atom) {}
    Atom(const Atom &other):
        _ctx(other._ctx),
        _atom(JS_DupAtom(_ctx, other._atom))
    {}
    Atom(Atom &&other) : _ctx(other._ctx), _atom(other._atom) {
        other._atom = JS_ATOM_NULL;
        other._ctx = nullptr;
    }

    Atom& operator=(const Atom &other) {
        if (_ctx) {
            JS_FreeAtom(_ctx, _atom);
        }
        _atom = JS_DupAtom(_ctx, other._atom);
        _ctx = other._ctx;

        return *this;
    }

    Atom& operator=(Atom &&other) {
        if (_ctx) {
            JS_FreeAtom(_ctx, _atom);
        }
        _atom = other._atom;
        _ctx = other._ctx;
        other._atom = JS_ATOM_NULL;
        other._ctx = nullptr;
        return *this;
    }

    ~Atom() {
        if (_ctx) {
            JS_FreeAtom(_ctx, _atom);
        }
    }

    StringView toString() const {
        return { _ctx, JS_AtomToCString(_ctx, _atom) };
    }

    std::pair<ContextRef, JSAtom> loot() {
        JSAtom atom_ = _atom;
        ContextRef ctx_ = this->_ctx;
        _ctx = nullptr;
        _atom = JS_ATOM_NULL;
        return {ctx_, atom_};
    }

    JSAtom& get() {
        return _atom;
    }

    static Atom create(ContextRef ctx, uint32_t value) {
        return { ctx, JS_NewAtomUInt32(ctx, value) };
    }

    static Atom create(ContextRef ctx, const char* value) {
        return { ctx, JS_NewAtom(ctx, value) };
    }

    static Atom create(ContextRef ctx, std::string value) {
        return create(ctx, value.c_str());
    }

    friend std::ostream& operator<<(std::ostream& os, Atom& val) {
        os << val.toString().c_str();
        return os;
    }
};


}  // namespace jac
```


