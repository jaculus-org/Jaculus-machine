

# File stringView.h

[**File List**](files.md) **>** [**jac**](dir_256037ad7d0c306238e2bc4f945d341d.md) **>** [**machine**](dir_10e7d6e7bc593e38e57ffe1bab5ed259.md) **>** [**stringView.h**](stringView_8h.md)

[Go to the documentation of this file](stringView_8h.md)


```C++
#pragma once

#include <quickjs.h>
#include <string>
#include <string_view>

#include "context.h"


namespace jac {


class StringView : public std::basic_string_view<char> {
    using basic_string_view = std::basic_string_view<char>;

    ContextRef _ctx = nullptr;
public:
    StringView(StringView &&other) : basic_string_view(other), _ctx(other._ctx) {
        other._ctx = nullptr;
    }

    StringView &operator=(StringView &&other) {
        if (_ctx) {
            JS_FreeCString(_ctx, data());
        }
        basic_string_view::operator=(other);
        _ctx = other._ctx;
        other._ctx = nullptr;
        return *this;
    }

    StringView(const basic_string_view &other) = delete;
    StringView(const StringView &other) = delete;
    StringView &operator=(const StringView &other) = delete;

    const char* c_str() const {
        return data();
    }

    StringView() = default;

    StringView(ContextRef ctx, const char* str) : basic_string_view(str), _ctx(ctx) {}

    ~StringView() {
        if (_ctx) {
            JS_FreeCString(_ctx, data());
        }
    }

    operator std::string() const {
        return std::string(data(), size());
    }
};


}  // namespace jac
```


