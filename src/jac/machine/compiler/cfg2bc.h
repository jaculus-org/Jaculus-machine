#pragma once

#include "tlessCfg.h"
#include "bcWriter.h"


namespace jac::bc {


class _IRGenError : public std::runtime_error {
public:
    explicit _IRGenError(const std::string& message): std::runtime_error(message) {}
};


enum class CompileMode {
    Module,
    Script
};


void cfg2bc(BytecodeRoot& root, const cfg::tless::Function& fun, const std::string& filename);
void cfg2bc(BytecodeRoot& root, const cfg::tless::Function& fun, const std::string& filename, CompileMode mode);


}  // namespace jac::bc
