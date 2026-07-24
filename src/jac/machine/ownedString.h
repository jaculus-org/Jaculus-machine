#pragma once

#include <quickjs.h>
#include <memory>
#include <ostream>
#include <string>
#include <string_view>

#include "context.h"


namespace jac {


/**
 * @brief An owning RAII handle for a QuickJS-allocated C string.
 *
 * The string must be allocated using QuickJS functions (JS_ToCString,
 * JS_AtomToCString, etc.); it is released with JS_FreeCString when the handle
 * is destroyed. The handle is move-only. Use @ref view to obtain a
 * (non-owning) std::string_view for inspection.
 */
class OwnedString {
    struct Deleter {
        ContextRef ctx = nullptr;
        void operator()(const char* ptr) const noexcept {
            if (ctx) {
                JS_FreeCString(ctx, ptr);
            }
        }
    };

    std::unique_ptr<const char[], Deleter> _data;
    std::size_t _size = 0;
public:
    OwnedString() = default;

    /**
     * @brief Wrap a QuickJS allocated, null-terminated string.
     *
     * @param ctx context to work in
     * @param str string to take ownership of
     */
    OwnedString(ContextRef ctx, const char* str)
        : _data(str, Deleter{ctx}), _size(str ? std::char_traits<char>::length(str) : 0) {}

    /**
     * @brief Wrap a QuickJS allocated string of a known length. The string may
     *        contain embedded null bytes.
     *
     * @param ctx context to work in
     * @param str string to take ownership of
     * @param len length of the string in bytes
     */
    OwnedString(ContextRef ctx, const char* str, std::size_t len)
        : _data(str, Deleter{ctx}), _size(len) {}

    /**
     * @brief Get a non-owning view of the string.
     *
     * @return std::string_view valid for the lifetime of this handle
     */
    std::string_view view() const noexcept {
        return { _data.get(), _size };
    }

    operator std::string_view() const noexcept {
        return view();
    }

    operator std::string() const {
        return std::string(_data.get(), _size);
    }

    /**
     * @brief Get the underlying null-terminated C string.
     *
     * @return const char*
     */
    const char* c_str() const noexcept {
        return _data.get();
    }

    const char* data() const noexcept {
        return _data.get();
    }

    std::size_t size() const noexcept {
        return _size;
    }

    bool empty() const noexcept {
        return _size == 0;
    }

    friend std::ostream& operator<<(std::ostream& os, const OwnedString& str) {
        return os << str.view();
    }
};


}  // namespace jac
