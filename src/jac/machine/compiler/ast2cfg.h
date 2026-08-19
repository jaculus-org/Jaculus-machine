#pragma once

#include "ast.h"
#include "cfg.h"


namespace jac::cfg {


SignaturePtr getSignature(const ast::Function& decl);
FunctionEmitter ast2cfg(const ast::Function& decl, SignaturePtr sig, FunctionEmitter* parent = nullptr);
FunctionEmitter ast2cfg(const ast::Script& s);
FunctionEmitter ast2cfg(const ast::Module& m);


}  // namespace jac::cfg
