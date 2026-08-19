#pragma once

#include "tlessCfg.h"
#include "tlessOpInfo.h"


namespace jac::cfg::tless {


// Returns implementation using simpler opcodes or nullptr if no implementation is available.
const Function* opcodeBody(Opcode opcode);


}  // namespace jac::cfg
