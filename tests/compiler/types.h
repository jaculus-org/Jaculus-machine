#pragma once

#include <jac/machine/compiler/cfg.h>
#include <jac/machine/values.h>
#include <optional>
#include <string>
#include <string_view>


namespace jac {


struct CompFn {
    cfg::Function fn;
    std::string name;
    std::string alias;
    std::string_view code;
    std::optional<Function> jsFn;
};

} // namespace jac
