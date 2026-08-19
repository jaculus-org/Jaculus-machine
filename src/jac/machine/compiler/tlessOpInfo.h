#pragma once

#include <array>
#include <cstddef>
#include <cstdint>
#include <stdexcept>
#include <string_view>

#include "tlessOpcode.h"


namespace jac::cfg::tless {


constexpr uint8_t VARIADIC = 0xFF;


struct OpInfo {
    std::string_view name;
    uint8_t argCount;
    uint8_t resCount;
};


namespace detail {


constexpr auto OP_INFO = std::to_array<OpInfo>({
#define JAC_TLESS_DECLARE_OP_INFO(name, argCount, resCount) { #name, argCount, resCount },
    JAC_TLESS_OPCODE_TABLE(JAC_TLESS_DECLARE_OP_INFO)
#undef JAC_TLESS_DECLARE_OP_INFO
#undef JAC_TLESS_OPCODE_TABLE
});

}  // namespace detail


inline constexpr const OpInfo& opInfo(Opcode op) {
    const auto index = static_cast<size_t>(op);
    if (index >= detail::OP_INFO.size()) {
        throw std::out_of_range("Invalid tless opcode");
    }
    return detail::OP_INFO[index];
}

}  // namespace jac::cfg::tless
