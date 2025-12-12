#pragma once

#include <utility>


#define RETURN_IF_NOT_VOID(expr) \
    if constexpr (std::is_same_v<decltype(expr), void>) { \
        expr; \
        return; \
    } else { \
        return expr; \
    }


namespace jac {


template<typename T>
struct Defer {
    T _func;

    Defer(T&& func): _func(std::move(func)) {}
    ~Defer() { _func(); }
};


enum class TriState {
    False,
    True,
    Unknown
};


template<typename... Ts>
struct TypeList {};


template<typename... Ts, typename... Us>
static auto ConcatTypeLists_impl(TypeList<Ts...>, TypeList<Us...>, auto... rest) {
    if constexpr (sizeof...(rest) == 0) {
        return TypeList<Ts..., Us...>{};
    } else {
        return ConcatTypeLists_impl(TypeList<Ts..., Us...>{}, rest...);
    }
}

template<typename... Lists>
using ConcatTypeLists = decltype(ConcatTypeLists_impl(Lists{}...));


template<class... Ts>
struct overloaded : Ts... { using Ts::operator()...; };
template<class... Ts>
overloaded(Ts...) -> overloaded<Ts...>;


} // namespace jac
