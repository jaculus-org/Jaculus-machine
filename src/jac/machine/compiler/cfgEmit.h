#pragma once

#include "ast.h"
#include "cfg.h"

#include <map>
#include <stdexcept>
#include <string>


namespace jac::cfg {


class IRGenError : public std::runtime_error {
public:
    explicit IRGenError(const std::string& message): std::runtime_error(message) {}
};


SignaturePtr getSignature(const ast::Function& decl);
FunctionEmitter emit(const ast::Function& decl, SignaturePtr sig, const std::map<cfg::Identifier, cfg::SignaturePtr>& otherSignatures);


}  // namespace jac::cfg
