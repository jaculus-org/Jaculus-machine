

# File ownedString.h

[**File List**](files.md) **>** [**jac**](dir_256037ad7d0c306238e2bc4f945d341d.md) **>** [**machine**](dir_10e7d6e7bc593e38e57ffe1bab5ed259.md) **>** [**ownedString.h**](ownedString_8h.md)

[Go to the documentation of this file](ownedString_8h.md)


```C++
#pragma once

#include <quickjs.h>
#include <memory>
#include <ostream>
#include <string>
#include <string_view>

#include "context.h"


namespace jac {


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

    OwnedString(ContextRef ctx, const char* str)
        : _data(str, Deleter{ctx}), _size(str ? std::char_traits<char>::length(str) : 0) {}

    OwnedString(ContextRef ctx, const char* str, std::size_t len)
        : _data(str, Deleter{ctx}), _size(len) {}

    std::string_view view() const noexcept {
        return { _data.get(), _size };
    }

    operator std::string_view() const noexcept {
        return view();
    }

    operator std::string() const {
        return std::string(_data.get(), _size);
    }

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
```


