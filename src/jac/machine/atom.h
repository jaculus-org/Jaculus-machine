#pragma once

#include <quickjs.h>

#include <ostream>
#include <string>

#include "internal/declarations.h"

#include "context.h"
#include "ownedString.h"


namespace jac {


template<typename T=int>
constexpr bool static_false() {
    return false;
};


/**
 * @brief A wrapper around JSAtom with RAII.
 * In the context of QuickJS, Atom is used to represent
 * identifiers of properties, variables, functions, etc.
 */
class Atom {
protected:
    ContextRef _ctx;
    JSAtom _atom;
public:
    /**
     * @brief Wrap an existing JSAtom. The JSAtom will be freed when the Atom is destroyed.
     * @note Used internally when directly working with QuickJS API. New Atom should be created using Atom::create().
     *
     * @param ctx context to work in
     * @param atom JSAtom to wrap
     */
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

    /**
     * @brief Get string representation of the atom
     *
     * @return OwnedString
     */
    OwnedString toString() const {
        size_t len = 0;
        const char* str = JS_AtomToCStringLen(_ctx, &len, _atom);
        return { _ctx, str, len };
    }

    /**
     * @brief Release ownership of the JSAtom. The JSAtom will have to be freed manually.
     * @note After this call, the Atom will be in an invalid state.
     *
     * @return Pair of ContextRef and JSAtom
     */
    std::pair<ContextRef, JSAtom> loot() {
        JSAtom atom_ = _atom;
        ContextRef ctx_ = this->_ctx;
        _ctx = nullptr;
        _atom = JS_ATOM_NULL;
        return {ctx_, atom_};
    }

    /**
     * @brief Get reference to the underlying JSAtom.
     *
     * @return JSAtom reference
     */
    JSAtom& get() {
        return _atom;
    }

    /**
     * @brief Create a new atom from an uint32_t value
     *
     * @param ctx context to create the atom in
     * @param value the value
     * @return The newly constructed atom
     */
    static Atom create(ContextRef ctx, uint32_t value) {
        return { ctx, JS_NewAtomUInt32(ctx, value) };
    }

    /**
     * @brief Create a new atom from a string
     *
     * @param ctx context to create the atom in
     * @param value the value
     * @return The newly constructed atom
     */
    static Atom create(ContextRef ctx, const char* value) {
        return { ctx, JS_NewAtom(ctx, value) };
    }

    /**
     * @brief Create a new atom from a string
     *
     * @param ctx context to create the atom in
     * @param value the value
     * @return The newly constructed atom
     */
    static Atom create(ContextRef ctx, std::string value) {
        return create(ctx, value.c_str());
    }

    friend std::ostream& operator<<(std::ostream& os, Atom& val) {
        os << val.toString().c_str();
        return os;
    }
};


}  // namespace jac
