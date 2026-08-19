#pragma once

#include "cfg.h"
#include "opInfo.h"


namespace jac::cfg {


// Returns implementation using simpler opcodes or nullptr if no implementation is available.
const Function* opcodeBody(Opcode opcode);


}  // namespace jac::cfg
