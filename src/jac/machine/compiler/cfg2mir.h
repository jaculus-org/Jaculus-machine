#pragma once

#include "cfg.h"
#include "mirUtil.h"

#include <mir.h>


namespace jac::cfg::mir_emit {


inline std::string name(RegId id) {
    return "_" + std::to_string(id);
}

inline void getArgsCfg(Function& cfg, std::vector<std::string>& argNamesOut, std::vector<MIR_var_t>& argsOut) {
    getArgs(std::views::transform(std::views::iota(static_cast<size_t>(0), cfg.args.size()),
        [&](auto i) { return std::pair{ cfg.args[i].type, name(cfg.args[i].id) }; }
    ), cfg.ret, argNamesOut, argsOut);
}

inline MIR_item_t getPrototype(MIR_context_t ctx, Function& cfg) {
    std::vector<std::string> argNames;
    std::vector<MIR_var_t> args;
    getArgsCfg(cfg, argNames, args);

    std::string name = "_proto_" + cfg.name();

    if (cfg.ret == ValueType::Void || hasRetArg(cfg.ret)) {
        return MIR_new_proto_arr(ctx, name.c_str(), 0, nullptr, args.size(), args.data());
    }
    auto retType = getMIRRetType(cfg.ret);
    return MIR_new_proto_arr(ctx, name.c_str(), 1, &retType, args.size(), args.data());
}


MIR_item_t compile(MIR_context_t ctx, const std::map<std::string, std::pair<MIR_item_t, MIR_item_t>>& prototypes, Function& cfg, Builtins& builtins);


MIR_item_t compileCaller(MIR_context_t ctx, Builtins& builtins, Function& cfg, MIR_item_t callee, MIR_item_t proto);


}  // namespace jac::cfg::mir_emit
