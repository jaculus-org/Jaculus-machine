#pragma once

#include "ast.h"
#include "tlessCfg.h"

#include <map>
#include <stdexcept>
#include <string>


namespace jac::cfg::tless {


class IRGenError : public std::runtime_error {
public:
    explicit IRGenError(const std::string& message): std::runtime_error(message) {}
};


SignaturePtr getSignature(const ast::Function& decl);
FunctionEmitter ast2cfg(const ast::Function& decl, SignaturePtr sig, FunctionEmitter* parent = nullptr);
FunctionEmitter ast2cfg(const ast::Script& s);
FunctionEmitter ast2cfg(const ast::Module& m);


}  // namespace jac::cfg
