#include "cfg2mir.h"

#include "cfg.h"
#include "mirUtil.h"
#include "opcode.h"
#include "stackAllocator.h"

#include <cstdint>
#include <cstdlib>
#include <iostream>
#include <memory>

#include <jac/util.h>
#include <mir.h>
#include <quickjs.h>


namespace jac::cfg::mir_emit {


auto getJSTag(ValueType type) {
    switch (type) {
        case ValueType::I32:    return JS_TAG_INT;
        case ValueType::F64:    return JS_TAG_FLOAT64;
        case ValueType::Bool:   return JS_TAG_BOOL;
        case ValueType::Object: return JS_TAG_OBJECT;
        default:
            assert(false && "Invalid JS tag");
    }
}

MIR_insn_code_t chooseSimpleMove(ValueType type) {
    switch (type) {
        case ValueType::I32:  case ValueType::Bool:  case ValueType::Object:
            return MIR_MOV;
        case ValueType::F64:
            return MIR_DMOV;
        default:
            assert(false && "Invalid simple move type");
    }
}

const char* chooseArithmeticBuiltin(Opcode op) {
    switch (op) {
        case Opcode::Add:  return "__add";
        case Opcode::Sub:  return "__sub";
        case Opcode::Mul:  return "__mul";
        case Opcode::Div:  return "__div";
        case Opcode::Rem:  return "__rem";
        default:
            assert(false && "Invalid arithmetic operation");
    }
}

// if res is not void, the last arg is the return target
void generateCall(MIR_context_t ctx, MIR_item_t fun, Builtins& builtins, std::string ident, std::vector<std::variant<MIR_reg_t, MIR_op_t>> ops, bool mayThrow) {
    auto func = builtins.fn(ident);
    auto args = func.args;
    auto res = func.ret;
    auto proto = func.prototype;
    auto callable = func.func;
    auto rtCtx = builtins.rtCtx;

    assert(func.mayThrow == mayThrow);

    bool retArg = hasRetArg(res);
    if ((args.size() + (res != ValueType::Void)) != ops.size()) {
        assert(false && "Invalid number of arguments");
    }

    auto getOp = [&ctx](auto op) {
        if (auto reg = std::get_if<MIR_reg_t>(&op)) {
            return MIR_new_reg_op(ctx, *reg);
        }
        if (auto v = std::get_if<MIR_op_t>(&op)) {
            return *v;
        }
        assert(false && "Invalid argument type");
    };
    auto getReg = [](auto op) {
        if (auto reg = std::get_if<MIR_reg_t>(&op)) {
            return *reg;
        }
        assert(false && "Invalid argument type");
    };

    std::vector<MIR_op_t> callArgs;
    callArgs.push_back(MIR_new_ref_op(ctx, proto));
    callArgs.push_back(MIR_new_ref_op(ctx, callable));
    if (res != ValueType::Void && !retArg) {
        callArgs.push_back(getOp(ops.back()));
    }

    callArgs.push_back(MIR_new_int_op(ctx, reinterpret_cast<int64_t>(rtCtx)));  // NOLINT
    for (size_t i = 0; i < args.size(); ++i) {
        auto [type, size] = getMIRArgType(args[i]);
        if (MIR_blk_type_p(type)) {
            callArgs.push_back(MIR_new_mem_op(ctx, type, size, getReg(ops[i]), 0, 0));
        }
        else {
            callArgs.push_back(getOp(ops[i]));
        }
    }
    if (retArg) {
        callArgs.push_back(getOp(ops.back()));
    }
    MIR_append_insn(ctx, fun, MIR_new_insn_arr(ctx, MIR_CALL, callArgs.size(), callArgs.data()));
}


void generateCheckException(MIR_context_t ctx, MIR_item_t fun, MIR_reg_t flagAddr, MIR_label_t exceptionLabel) {
    auto flag = MIR_new_mem_op(ctx, MIR_T_I32, 0, flagAddr, 0, 0);
    MIR_append_insn(ctx, fun, MIR_new_insn(ctx, MIR_BNES, MIR_new_label_op(ctx, exceptionLabel), flag, MIR_new_int_op(ctx, 0)));
}

void generateSetExceptionFlag(MIR_context_t ctx, MIR_item_t fun, MIR_reg_t flagAddr, int value) {
    auto flag = MIR_new_mem_op(ctx, MIR_T_I32, 0, flagAddr, 0, 0);
    MIR_append_insn(ctx, fun, MIR_new_insn(ctx, MIR_MOV, flag, MIR_new_int_op(ctx, value)));
}

void generateCheckExceptionJSValue(MIR_context_t ctx, MIR_item_t fun, MIR_reg_t valAddr, MIR_label_t exceptionLabel) {
    auto tag = MIR_new_mem_op(ctx, MIR_T_I64, sizeof(int64_t), valAddr, 0, 0);
    MIR_append_insn(ctx, fun, MIR_new_insn(ctx, MIR_BEQ, MIR_new_label_op(ctx, exceptionLabel), tag, MIR_new_int_op(ctx, JS_TAG_EXCEPTION)));
}

void generateThrowError(MIR_context_t ctx, MIR_item_t fun, Builtins& builtins, const char* msg, ErrorType type) {
    generateCall(ctx, fun, builtins, "__throwError", { MIR_new_int_op(ctx, reinterpret_cast<uint64_t>(msg)), MIR_new_int_op(ctx, static_cast<int32_t>(type)) }, true);  // NOLINT
}

void generateConvertFromJSVal(MIR_context_t ctx, MIR_item_t fun, Builtins& builtins, MIR_reg_t srcAddr, MIR_reg_t dstReg, ValueType type, MIR_label_t invalidConversionLabel) {
    auto insert = [&](MIR_insn_t insn) {
        MIR_append_insn(ctx, fun, insn);
    };

    auto srcL = MIR_new_mem_op(ctx, getMIRRegType(type), 0, srcAddr, 0, 0);
    auto srcH = MIR_new_mem_op(ctx, MIR_T_I64, sizeof(int64_t), srcAddr, 0, 0);
    auto dstOp = MIR_new_reg_op(ctx, dstReg);

    auto same = MIR_new_label(ctx);
    auto different = MIR_new_label(ctx);
    auto end = MIR_new_label(ctx);

    insert(MIR_new_insn(ctx, MIR_BNE, MIR_new_label_op(ctx, different), srcH, MIR_new_int_op(ctx, getJSTag(type))));
    insert(MIR_new_insn(ctx, MIR_JMP, MIR_new_label_op(ctx, same)));

    insert(same);
    insert(MIR_new_insn(ctx, chooseSimpleMove(type), dstOp, srcL));
    insert(MIR_new_insn(ctx, MIR_JMP, MIR_new_label_op(ctx, end)));

    insert(different);
    switch (type) {
        case ValueType::Bool:    [[fallthrough]];
        case ValueType::I32:     generateCall(ctx, fun, builtins, "__convertI32", { srcAddr, dstReg }, true);   break;
        case ValueType::F64:     generateCall(ctx, fun, builtins, "__convertF64", { srcAddr, dstReg }, true);   break;
        case ValueType::Object:
            insert(MIR_new_insn(ctx, MIR_JMP, MIR_new_label_op(ctx, invalidConversionLabel)));
            break;
        default:
            assert(false && "Invalid type");
    }
    if (type == ValueType::Bool) {
        insert(MIR_new_insn(ctx, MIR_NES, dstOp, dstOp, MIR_new_int_op(ctx, 0)));
    }
    insert(MIR_new_insn(ctx, MIR_JMP, MIR_new_label_op(ctx, end)));

    insert(end);
}

void generateConvertToJSVal(MIR_context_t ctx, MIR_item_t fun, Builtins& builtins, MIR_reg_t src, MIR_reg_t dstAddr, ValueType type) {
    if (type == ValueType::StringConst) {
        generateCall(ctx, fun, builtins, "__newString", { src, dstAddr }, true);
        return;
    }

    auto dstL = MIR_new_mem_op(ctx, getMIRRegType(type),   0, dstAddr, 0, 0);
    auto dstH = MIR_new_mem_op(ctx, MIR_T_I64, sizeof(int64_t), dstAddr, 0, 0);

    MIR_append_insn(ctx, fun, MIR_new_insn(ctx, chooseSimpleMove(type), dstL, MIR_new_reg_op(ctx, src)));
    MIR_append_insn(ctx, fun, MIR_new_insn(ctx, MIR_MOV, dstH, MIR_new_int_op(ctx, getJSTag(type))));
}


struct CompileContext {
    MIR_context_t ctx;
    MIR_module_t mod;
    MIR_item_t fun;
    MIR_func_t func;

    std::map<RegId, MIR_reg_t> regs;
    std::map<BasicBlockPtr, MIR_label_t> labels;
    std::map<RegId, int> stackSlots;

    MIR_reg_t allocaPtr;
    MIR_reg_t exceptionFlagAddr;
    MIR_label_t exceptionLabel;
    MIR_label_t invalidConversionLabel;
    MIR_label_t divisionByZeroLabel;

    Builtins& builtins;
    const std::map<std::string, std::pair<MIR_item_t, MIR_item_t>>& prototypes;

    CompileContext(MIR_context_t ctx_, Builtins& builtins_, const std::map<std::string, std::pair<MIR_item_t, MIR_item_t>>& prototypes_):
        ctx(ctx_),
        builtins(builtins_),
        prototypes(prototypes_)
    {}

    auto label(BasicBlockPtr block) {
        auto it = labels.find(block);
        if (it == labels.end()) {
            it = labels.emplace(block, MIR_new_label(ctx)).first;
        }
        return it->second;
    }

    auto labelOp(BasicBlockPtr block) {
        return MIR_new_label_op(ctx, label(block));
    }

    auto reg(RegId id, ValueType type) {
        auto it = regs.find(id);
        if (it == regs.end()) {
            std::string n = name(id);
            auto r = MIR_new_func_reg(ctx, func, getMIRRegType(type), n.c_str());
            it = regs.emplace_hint(it, id, r);
        }

        return it->second;
    }

    auto regOp(RegId id) {
        return MIR_new_reg_op(ctx, regs.at(id));
    }

    auto scratch(auto prefix, MIR_type_t type) {
        auto r = MIR_new_func_reg(ctx, func, type, (prefix + std::to_string(getId())).c_str());
        return r;
    }

    auto calcOff(MIR_reg_t base, int off, size_t size) {
        auto addr = scratch("_addr", MIR_T_I64);
        MIR_append_insn(ctx, fun, MIR_new_insn(ctx, MIR_ADD, MIR_new_reg_op(ctx, addr), MIR_new_reg_op(ctx, base), MIR_new_int_op(ctx, off * size)));
        return addr;
    }

    MIR_reg_t jsValAddr(RegId id) {
        auto it = stackSlots.find(id);
        if (it == stackSlots.end()) {
            return regs.at(id);
        }
        return calcOff(allocaPtr, it->second, sizeof(JSValue));
    };

    auto insert(MIR_insn_t insn) {
        MIR_append_insn(ctx, fun, insn);
    }

    auto copyJSVal(MIR_reg_t srcAddr, MIR_reg_t dstAddr) {
        static_assert(sizeof(JSValue) == 2 * sizeof(int64_t));

        auto srcL = MIR_new_mem_op(ctx, MIR_T_I64, 0,               srcAddr, 0, 0);
        auto srcH = MIR_new_mem_op(ctx, MIR_T_I64, sizeof(int64_t), srcAddr, 0, 0);
        auto dstL = MIR_new_mem_op(ctx, MIR_T_I64, 0,               dstAddr, 0, 0);
        auto dstH = MIR_new_mem_op(ctx, MIR_T_I64, sizeof(int64_t), dstAddr, 0, 0);
        insert(MIR_new_insn(ctx, MIR_MOV, dstL, srcL));
        insert(MIR_new_insn(ctx, MIR_MOV, dstH, srcH));
    }

    auto toJSVal(Reg a, MIR_reg_t dstAddr) {
        generateConvertToJSVal(ctx, fun, builtins, regs.at(a.id), dstAddr, a.type);
    }

    auto fromJSVal(MIR_reg_t srcAddr, Reg res) {
        generateConvertFromJSVal(ctx, fun, builtins, srcAddr, regs.at(res.id), res.type, invalidConversionLabel);
    }

    void checkException() {
        generateCheckException(ctx, fun, exceptionFlagAddr, exceptionLabel);
    }
    void checkExceptionJSValue(MIR_reg_t valAddr) {
        generateCheckExceptionJSValue(ctx, fun, valAddr, exceptionLabel);
    }
};

void generateDup(CompileContext& cc, Reg a) {
    if (a.type == ValueType::Any) {
        generateCall(cc.ctx, cc.fun, cc.builtins, "__dupVal", { cc.jsValAddr(a.id) }, false);
    }
    else if (a.type == ValueType::Object) {
        generateCall(cc.ctx, cc.fun, cc.builtins, "__dupObj", { cc.regs.at(a.id) }, false);
    }
    else {
        assert((isNumeric(a.type) || a.type == ValueType::StringConst) && "Invalid Dup operand type");
    }
}

void generatePushFree(CompileContext& cc, Reg a) {
    if (a.type == ValueType::Any) {
        generateCall(cc.ctx, cc.fun, cc.builtins, "__pushFreeVal", { cc.jsValAddr(a.id) }, false);
    }
    else if (a.type == ValueType::Object) {
        generateCall(cc.ctx, cc.fun, cc.builtins, "__pushFreeObj", { cc.regs.at(a.id) }, false);
    }
    else {
        assert((isNumeric(a.type) || a.type == ValueType::StringConst) && "Invalid PushFree operand type");
    }
}

void generateArithmetic(CompileContext& cc, Opcode op, Reg a, Reg b, Reg res) {
    assert(a.type == b.type && a.type == res.type);
    if (a.type == ValueType::Any) {
        generateCall(cc.ctx, cc.fun, cc.builtins, chooseArithmeticBuiltin(op), { cc.jsValAddr(a.id), cc.jsValAddr(b.id), cc.jsValAddr(res.id) }, true);
        cc.checkException();
        return;
    }

    if (a.type == ValueType::I32 && (op == Opcode::Div || op == Opcode::Rem)) {
        cc.insert(MIR_new_insn(cc.ctx, MIR_BFS, MIR_new_label_op(cc.ctx, cc.divisionByZeroLabel), cc.regOp(b.id)));
    }

    MIR_insn_code_t code;
    switch (op) {
        case Opcode::Add:  switch (a.type) {
            case ValueType::I32:  code = MIR_ADDS; break;
            case ValueType::F64:  code = MIR_DADD; break;
            default: assert(false && "Invalid Add operand type");
        } break;
        case Opcode::Sub:  switch (a.type) {
            case ValueType::I32:  code = MIR_SUBS; break;
            case ValueType::F64:  code = MIR_DSUB; break;
            default: assert(false && "Invalid Sub operand type");
        } break;
        case Opcode::Mul:  switch (a.type) {
            case ValueType::I32:  code = MIR_MULS; break;
            case ValueType::F64:  code = MIR_DMUL; break;
            default: assert(false && "Invalid Mul operand type");
        } break;
        case Opcode::Div:  switch (a.type) {
            case ValueType::I32:  code = MIR_DIVS; break;
            case ValueType::F64:  code = MIR_DDIV; break;
            default: assert(false && "Invalid Div operand type");
        } break;
        case Opcode::Rem:  switch (a.type) {
            case ValueType::I32:  code = MIR_MODS; break;
            case ValueType::F64:
                generateCall(cc.ctx, cc.fun, cc.builtins, "__remF64", { cc.regOp(a.id), cc.regOp(b.id), cc.regOp(res.id) }, false);
                return;
            default: assert(false && "Invalid Rem operand type");
        } break;
        default:
            assert(false && "Invalid arithmetic operation");
    }

    cc.insert(MIR_new_insn(cc.ctx, code, cc.regOp(res.id), cc.regOp(a.id), cc.regOp(b.id)));
}

void generatePow(CompileContext& cc, Reg a, Reg b, Reg res) {
    assert(a.type == ValueType::F64 && b.type == ValueType::F64 && res.type == ValueType::F64);
    generateCall(cc.ctx, cc.fun, cc.builtins, "__powF64", { cc.regs.at(a.id), cc.regs.at(b.id), cc.regs.at(res.id) }, false);
}

void generateBitwise(CompileContext& cc, Opcode op, Reg a, Reg b, Reg res) {
    assert(a.type == ValueType::I32 && b.type == ValueType::I32 && res.type == ValueType::I32);

    MIR_insn_code_t code;
    switch (op) {
        case Opcode::LShift:   code = MIR_LSHS;   break;
        case Opcode::RShift:   code = MIR_RSHS;   break;
        case Opcode::URShift:  code = MIR_URSHS;  break;
        case Opcode::BitAnd:   code = MIR_ANDS;   break;
        case Opcode::BitOr:    code = MIR_ORS;    break;
        case Opcode::BitXor:   code = MIR_XORS;   break;
        default:
            assert(false && "Invalid bitwise operation");
    }
    cc.insert(MIR_new_insn(cc.ctx, code, cc.regOp(res.id), cc.regOp(a.id), cc.regOp(b.id)));
}

void generateRelational(CompileContext& cc, Opcode op, Reg a, Reg b, Reg res) {
    assert(a.type == b.type && res.type == ValueType::Bool);

    if (isNumeric(a.type)) {
        MIR_insn_code_t code;
        switch (op) {
            case Opcode::Eq:  switch (a.type) {
                case ValueType::I32:  [[fallthrough]];
                case ValueType::Bool: code = MIR_EQS; break;
                case ValueType::F64:  code = MIR_DEQ; break;
                default: assert(false && "Invalid Eq operand type");
            } break;
            case Opcode::Neq:  switch (a.type) {
                case ValueType::I32:  [[fallthrough]];
                case ValueType::Bool: code = MIR_NES; break;
                case ValueType::F64:  code = MIR_DNE; break;
                default: assert(false && "Invalid Neq operand type");
            } break;
            case Opcode::Gt:  switch (a.type) {
                case ValueType::I32:  [[fallthrough]];
                case ValueType::Bool: code = MIR_GTS; break;
                case ValueType::F64:  code = MIR_DGT; break;
                default: assert(false && "Invalid Gt operand type");
            } break;
            case Opcode::Gte:  switch (a.type) {
                case ValueType::I32:  [[fallthrough]];
                case ValueType::Bool: code = MIR_GES; break;
                case ValueType::F64:  code = MIR_DGE; break;
                default: assert(false && "Invalid Gte operand type");
            } break;
            case Opcode::Lt:  switch (a.type) {
                case ValueType::I32:  [[fallthrough]];
                case ValueType::Bool: code = MIR_LTS; break;
                case ValueType::F64:  code = MIR_DLT; break;
                default: assert(false && "Invalid Lt operand type");
            } break;
            case Opcode::Lte:  switch (a.type) {
                case ValueType::I32:  [[fallthrough]];
                case ValueType::Bool: code = MIR_LES; break;
                case ValueType::F64:  code = MIR_DLE; break;
                default: assert(false && "Invalid Lte operand type");
            } break;
            default:
                assert(false && "Invalid comparison operation");
        }

        cc.insert(MIR_new_insn(cc.ctx, code, cc.regOp(res.id), cc.regOp(a.id), cc.regOp(b.id)));
    }
    else if (a.type == ValueType::Any) {
        switch (op) {
            case Opcode::Eq:   generateCall(cc.ctx, cc.fun, cc.builtins, "__eqAny",        { cc.jsValAddr(a.id), cc.jsValAddr(b.id), cc.regOp(res.id) }, true); break;
            case Opcode::Neq:  generateCall(cc.ctx, cc.fun, cc.builtins, "__neqAny",       { cc.jsValAddr(a.id), cc.jsValAddr(b.id), cc.regOp(res.id) }, true); break;
            case Opcode::Gt:   generateCall(cc.ctx, cc.fun, cc.builtins, "__greaterAny",   { cc.jsValAddr(a.id), cc.jsValAddr(b.id), cc.regOp(res.id) }, true); break;
            case Opcode::Gte:  generateCall(cc.ctx, cc.fun, cc.builtins, "__greaterEqAny", { cc.jsValAddr(a.id), cc.jsValAddr(b.id), cc.regOp(res.id) }, true); break;
            case Opcode::Lt:   generateCall(cc.ctx, cc.fun, cc.builtins, "__lessAny",      { cc.jsValAddr(a.id), cc.jsValAddr(b.id), cc.regOp(res.id) }, true); break;
            case Opcode::Lte:  generateCall(cc.ctx, cc.fun, cc.builtins, "__lessEqAny",    { cc.jsValAddr(a.id), cc.jsValAddr(b.id), cc.regOp(res.id) }, true); break;
            default:
                assert(false && "Invalid comparison operation");
        }
    }
    else {
        assert(false && "Invalid comparison operand type");
    }
}

void generateBoolConv(CompileContext cc, Reg a, Reg res, bool inverted) {
    if (a.type == ValueType::Bool && !inverted) {
        cc.insert(MIR_new_insn(cc.ctx, MIR_MOV, cc.regOp(res.id), cc.regOp(a.id)));
        return;
    }
    if (a.type == ValueType::Any) {
        generateCall(cc.ctx, cc.fun, cc.builtins, "__boolConv", { cc.jsValAddr(a.id), cc.regOp(res.id) }, true);
        cc.checkException();
        return;
    }
    if (!inverted) {
        switch (a.type) {
            case ValueType::I32:  case ValueType::Bool:
                cc.insert(MIR_new_insn(cc.ctx, MIR_NES, cc.regOp(res.id), cc.regOp(a.id), MIR_new_int_op(cc.ctx, 0)));
                break;
            case ValueType::F64:
                cc.insert(MIR_new_insn(cc.ctx, MIR_DNE, cc.regOp(res.id), cc.regOp(a.id), MIR_new_double_op(cc.ctx, 0)));
                break;
            case ValueType::Object:  // Object is never null
                cc.insert(MIR_new_insn(cc.ctx, MIR_NE, cc.regOp(res.id), cc.regOp(a.id), MIR_new_int_op(cc.ctx, 0)));
                break;
            default:
                throw std::runtime_error("BoolNot type not implemented");
        }
    }
    else {
        switch (a.type) {
            case ValueType::I32:  case ValueType::Bool:
                cc.insert(MIR_new_insn(cc.ctx, MIR_EQS, cc.regOp(res.id), cc.regOp(a.id), MIR_new_int_op(cc.ctx, 0)));
                break;
            case ValueType::F64:
                cc.insert(MIR_new_insn(cc.ctx, MIR_DEQ, cc.regOp(res.id), cc.regOp(a.id), MIR_new_double_op(cc.ctx, 0)));
                break;
            case ValueType::Object:  // Object is never null
                cc.insert(MIR_new_insn(cc.ctx, MIR_EQ, cc.regOp(res.id), cc.regOp(a.id), MIR_new_int_op(cc.ctx, 0)));
                break;
            default:
                throw std::runtime_error("BoolNot type not implemented");
        }
    }
}

void generateSet(CompileContext& cc, Reg a, Reg res) {
    if (a.type == ValueType::Any && res.type == ValueType::Any) {
        auto baseSrc = cc.jsValAddr(a.id);
        auto baseDst = cc.jsValAddr(res.id);
        cc.copyJSVal(baseSrc, baseDst);
        return;
    }
    if (a.type == ValueType::Any) {
        cc.fromJSVal(cc.jsValAddr(a.id), res);
        return;
    }
    if (res.type == ValueType::Any) {
        if (a.type == ValueType::Object) {
            generateDup(cc, a);
        }
        cc.toJSVal(a, cc.jsValAddr(res.id));
        return;
    }
    if (res.type == ValueType::Bool) {
        generateBoolConv(cc, a, res, false);
        return;
    }

    // reg -> reg copy
    MIR_insn_code_t code;
    if (a.type == res.type) {
        code = chooseSimpleMove(a.type);
    }
    else {
        switch (a.type) {
            case ValueType::I32:
                switch (res.type) {
                    case ValueType::F64:  code = MIR_I2D;  break;
                    default: throw std::runtime_error("Conversion not implemented");
                } break;
            case ValueType::F64:
                switch (res.type) {
                    case ValueType::I32:  code = MIR_D2I;  break;
                    default: throw std::runtime_error("Conversion not implemented");
                } break;
            case ValueType::Bool:
                switch (res.type) {
                    case ValueType::I32:  code = MIR_MOV;  break;
                    case ValueType::F64:  code = MIR_I2D;  break;
                    default: throw std::runtime_error("Conversion not implemented");
                } break;
            default:
                throw std::runtime_error("Conversion not implemented");
        }
    }
    cc.insert(MIR_new_insn(cc.ctx, code, cc.regOp(res.id), cc.regOp(a.id)));
}

void generateGetMember(CompileContext& cc, Reg a, Reg b, Reg res) {
    assert(res.type == ValueType::Any);

    switch (a.type) {
        case ValueType::Any: switch (b.type) {
            case ValueType::I32:
                generateCall(cc.ctx, cc.fun, cc.builtins, "__getMemberAnyI32", { cc.jsValAddr(a.id), cc.regs.at(b.id), cc.jsValAddr(res.id) }, true);
                break;
            case ValueType::StringConst:
                generateCall(cc.ctx, cc.fun, cc.builtins, "__getMemberAnyCStr", { cc.jsValAddr(a.id), cc.regs.at(b.id), cc.jsValAddr(res.id) }, true);
                break;
            case ValueType::Any:
                generateCall(cc.ctx, cc.fun, cc.builtins, "__getMemberAnyAny", { cc.jsValAddr(a.id), cc.jsValAddr(b.id), cc.jsValAddr(res.id) }, true);
                break;
            default: assert(false && "Invalid GetMember operand type");
        } break;
        case ValueType::Object: switch (b.type) {
            case ValueType::I32:
                generateCall(cc.ctx, cc.fun, cc.builtins, "__getMemberObjI32", { cc.regs.at(a.id), cc.regs.at(b.id), cc.jsValAddr(res.id) }, true);
                break;
            case ValueType::StringConst:
                generateCall(cc.ctx, cc.fun, cc.builtins, "__getMemberObjCStr", { cc.regs.at(a.id), cc.regs.at(b.id), cc.jsValAddr(res.id) }, true);
                break;
            case ValueType::Any:
                generateCall(cc.ctx, cc.fun, cc.builtins, "__getMemberObjAny", { cc.regs.at(a.id), cc.jsValAddr(b.id), cc.jsValAddr(res.id) }, true);
                break;
            default: assert(false && "Invalid GetMember operand type");
        } break;
        default:
            assert(false && "Invalid GetMember operand type");
    }
    cc.checkException();
}

void generateSetMember(CompileContext& cc, Reg a, Reg b, Reg res) {
    MIR_reg_t srcAddr;
    std::optional<MIR_reg_t> startBlock;
    if (b.type == ValueType::Any) {
        srcAddr = cc.jsValAddr(b.id);
    }
    else {
        srcAddr = cc.scratch("_addr", MIR_T_I64);
        startBlock = cc.scratch("_bstart", MIR_T_I64);
        cc.insert(MIR_new_insn(cc.ctx, MIR_BSTART, MIR_new_reg_op(cc.ctx, *startBlock)));
        cc.insert(MIR_new_insn(cc.ctx, MIR_ALLOCA,
            MIR_new_reg_op(cc.ctx, srcAddr),
            MIR_new_int_op(cc.ctx, sizeof(JSValue)))
        );
        cc.toJSVal(b, srcAddr);
    }

    switch (res.type) {
        case ValueType::Any: switch (a.type) {
            case ValueType::I32:
                generateCall(cc.ctx, cc.fun, cc.builtins, "__setMemberAnyI32", { cc.jsValAddr(res.id), cc.regs.at(a.id), srcAddr }, true);
                break;
            case ValueType::StringConst:
                generateCall(cc.ctx, cc.fun, cc.builtins, "__setMemberAnyCStr", { cc.jsValAddr(res.id), cc.regs.at(a.id), srcAddr }, true);
                break;
            case ValueType::Any:
                generateCall(cc.ctx, cc.fun, cc.builtins, "__setMemberAnyAny", { cc.jsValAddr(res.id), cc.jsValAddr(a.id), srcAddr }, true);
                break;
            default: assert(false && "Invalid SetMember operand type");
        } break;
        case ValueType::Object: switch (a.type) {
            case ValueType::I32:
                generateCall(cc.ctx, cc.fun, cc.builtins, "__setMemberObjI32", { cc.regs.at(res.id), cc.regs.at(a.id), srcAddr }, true);
                break;
            case ValueType::StringConst:
                generateCall(cc.ctx, cc.fun, cc.builtins, "__setMemberObjCStr", { cc.regs.at(res.id), cc.regs.at(a.id), srcAddr }, true);
                break;
            case ValueType::Any:
                generateCall(cc.ctx, cc.fun, cc.builtins, "__setMemberObjAny", { cc.regs.at(res.id), cc.jsValAddr(a.id), srcAddr }, true);
                break;
            default: assert(false && "Invalid SetMember operand type");
        } break;
        default:
            assert(false && "Invalid SetMember operand type");
    }
    cc.checkException();

    if (startBlock) {
        cc.insert(MIR_new_insn(cc.ctx, MIR_BEND, MIR_new_reg_op(cc.ctx, *startBlock)));
    }
}

void generateBitNot(CompileContext& cc, Reg a, Reg res) {
    assert(a.type == ValueType::I32 && res.type == ValueType::I32);
    cc.insert(MIR_new_insn(cc.ctx, MIR_XORS, cc.regOp(res.id), cc.regOp(a.id), MIR_new_int_op(cc.ctx, -1)));
}

void generateUnMinus(CompileContext& cc, Reg a, Reg res) {
    assert(a.type == res.type);

    if (!isNumeric(a.type)) {
        throw std::runtime_error("Unsupported UnMinus operand");
    }

    MIR_insn_code_t code;
    switch (a.type) {
        case ValueType::I32:   [[fallthrough]];
        case ValueType::Bool:  code = MIR_NEGS; break;
        case ValueType::F64:   code = MIR_DNEG; break;
        default: assert(false && "Invalid UnMinus operand type");
    }
    cc.insert(MIR_new_insn(cc.ctx, code, cc.regOp(res.id), cc.regOp(a.id)));
}

bool isRegType(ValueType type) {
    return type != ValueType::Any && type != ValueType::Void;
}


void generateInstruction(CompileContext& cc, Operation& insn) {
    if (isRegType(insn.res.type)) {
        cc.reg(insn.res.id, insn.res.type);
    }
    switch (insn.op) {
        // Binary
        case Opcode::Add:  case Opcode::Sub:  case Opcode::Mul:
        case Opcode::Div:  case Opcode::Rem:
            generateArithmetic(cc, insn.op, insn.a, insn.b, insn.res);
            break;
        case Opcode::Pow:
            generatePow(cc, insn.a, insn.b, insn.res);
            break;
        case Opcode::LShift:  case Opcode::RShift:  case Opcode::URShift:
        case Opcode::BitAnd:  case Opcode::BitOr:   case Opcode::BitXor:
            generateBitwise(cc, insn.op, insn.a, insn.b, insn.res);
            break;
        case Opcode::Eq:  case Opcode::Neq:
        case Opcode::Gt:  case Opcode::Gte:
        case Opcode::Lt:  case Opcode::Lte:
            generateRelational(cc, insn.op, insn.a, insn.b, insn.res);
            break;
        case Opcode::GetMember:
            generateGetMember(cc, insn.a, insn.b, insn.res);
            break;
        case Opcode::SetMember:
            generateSetMember(cc, insn.a, insn.b, insn.res);
            break;
        // Unary
        case Opcode::Set:
            generateSet(cc, insn.a, insn.res);
            break;
        case Opcode::BoolNot:
            generateBoolConv(cc, insn.a, insn.res, true);
            break;
        case Opcode::BitNot:
            generateBitNot(cc, insn.a, insn.res);
            break;
        case Opcode::UnPlus:
            assert(isNumeric(insn.res.type));
            generateSet(cc, insn.a, insn.res);
            break;
        case Opcode::UnMinus:
            generateUnMinus(cc, insn.a, insn.res);
            break;
        case Opcode::Dup:
            generateDup(cc, insn.a);
            break;
        case Opcode::PushUnref:
            generatePushFree(cc, insn.a);
            break;
        default:
            assert(false && "Invalid operation");
    }
}

void generateInstruction(CompileContext& cc, ConstInit& init) {
    auto reg = MIR_new_reg_op(cc.ctx, cc.reg(init.id, init.type()));
    std::visit([&](auto val) {
        if constexpr (std::is_same_v<std::decay_t<decltype(val)>, int32_t>) {
            auto vOp = MIR_new_int_op(cc.ctx, val);
            cc.insert(MIR_new_insn(cc.ctx, MIR_MOV, reg, vOp));
        }
        else if constexpr (std::is_same_v<std::decay_t<decltype(val)>, double>) {
            auto vOp = MIR_new_double_op(cc.ctx, val);
            cc.insert(MIR_new_insn(cc.ctx, MIR_DMOV, reg, vOp));
        }
        else if constexpr (std::is_same_v<std::decay_t<decltype(val)>, bool>) {
            auto vOp = MIR_new_int_op(cc.ctx, val);
            cc.insert(MIR_new_insn(cc.ctx, MIR_MOV, reg, vOp));
        }
        else if constexpr (std::is_same_v<std::decay_t<decltype(val)>, std::string>) {
            auto strDup = std::make_unique<char[]>(val.size() + 1);
            std::copy(val.begin(), val.end(), strDup.get());
            strDup[val.size()] = '\0';
            auto vOp = MIR_new_int_op(cc.ctx, reinterpret_cast<uint64_t>(strDup.get()));  // NOLINT
            cc.insert(MIR_new_insn(cc.ctx, MIR_MOV, reg, vOp));
            cc.builtins.rtCtx->stringConsts.emplace_back(std::move(strDup));
        }
    }, init.value);
}

void generateInstruction(CompileContext& cc, Call& call) {
    std::vector<MIR_op_t> args;
    if (call.isNative()) {
        if (call.res.type != ValueType::Void && call.res.type != ValueType::Any) {
            cc.reg(call.res.id, call.res.type);
        }
        auto [proto, forward] = cc.prototypes.at(std::get<Identifier>(call.obj));
        args.push_back(MIR_new_ref_op(cc.ctx, proto));
        args.push_back(MIR_new_ref_op(cc.ctx, forward));
        if (call.res.type != ValueType::Void && call.res.type != ValueType::Any) {
            args.push_back(cc.regOp(call.res.id));
        }
        for (auto& arg : call.args) {
            auto [type, size] = getMIRArgType(arg.type);
            if (MIR_blk_type_p(type)) {
                args.push_back(MIR_new_mem_op(cc.ctx, type, size, cc.jsValAddr(arg.id), 0, 0));
            }
            else {
                args.push_back(cc.regOp(arg.id));
        }
        }
        if (call.res.type == ValueType::Any) {
            args.push_back(MIR_new_reg_op(cc.ctx, cc.jsValAddr(call.res.id)));
        }
        cc.insert(MIR_new_insn_arr(cc.ctx, MIR_CALL, args.size(), args.data()));
        cc.checkException();
    }
    else {
        assert(!call.args.empty());

        auto startBlock = cc.scratch("_bstart", MIR_T_I64);
        cc.insert(MIR_new_insn(cc.ctx, MIR_BSTART, MIR_new_reg_op(cc.ctx, startBlock)));

        auto argsStart = cc.scratch("_args", MIR_T_I64);
        auto argCount = cc.scratch("_argCount", MIR_T_I64);
        cc.insert(MIR_new_insn(cc.ctx, MIR_MOV, MIR_new_reg_op(cc.ctx, argCount), MIR_new_int_op(cc.ctx, call.args.size() - 1)));
        cc.insert(MIR_new_insn(cc.ctx, MIR_ALLOCA,
            MIR_new_reg_op(cc.ctx, argsStart),
            MIR_new_int_op(cc.ctx, sizeof(JSValue) * std::max(call.args.size() - 1, static_cast<size_t>(1))))
        );
        for (size_t i = 1; i < call.args.size(); ++i) {
            auto arg = call.args[i];
            auto addr = cc.calcOff(argsStart, i - 1, sizeof(JSValue));
            if (arg.type == ValueType::Any) {
                cc.copyJSVal(cc.jsValAddr(arg.id), addr);
            }
            else {
                cc.toJSVal(arg, addr);
            }
        }

        auto funObj = std::get<Reg>(call.obj);
        auto thisArg = call.args[0];
        if (!call.isConstructor) {
            if (funObj.type == ValueType::Any) {
                auto funReg = cc.jsValAddr(funObj.id);
                if (thisArg.type == ValueType::Any) {
                    generateCall(cc.ctx, cc.fun, cc.builtins, "__callAnyAny", { funReg, cc.jsValAddr(thisArg.id), argCount, argsStart }, true);
                }
                else if (thisArg.type == ValueType::Object) {
                    generateCall(cc.ctx, cc.fun, cc.builtins, "__callAnyObj", { funReg, cc.regs.at(thisArg.id), argCount, argsStart }, true);
                }
                else if (thisArg.type == ValueType::Void) {
                    generateCall(cc.ctx, cc.fun, cc.builtins, "__callAnyUndefined", { funReg, argCount, argsStart }, true);
                }
                else {
                    assert(false && "Invalid call operand type");
                }
            }
            else if (funObj.type == ValueType::Object) {
                auto funReg = cc.regs.at(funObj.id);
                if (thisArg.type == ValueType::Any) {
                    generateCall(cc.ctx, cc.fun, cc.builtins, "__callObjAny", { funReg, cc.jsValAddr(thisArg.id), argCount, argsStart }, true);
                }
                else if (thisArg.type == ValueType::Object) {
                    generateCall(cc.ctx, cc.fun, cc.builtins, "__callObjObj", { funReg, cc.regs.at(thisArg.id), argCount, argsStart }, true);
                }
                else if (thisArg.type == ValueType::Void) {
                    generateCall(cc.ctx, cc.fun, cc.builtins, "__callObjUndefined", { funReg, argCount, argsStart }, true);
                }
                else {
                    assert(false && "Invalid call operand type");
                }
            }
            else {
                assert(false && "Invalid call operand type");
            }
        }
        else {
            if (funObj.type == ValueType::Any) {
                generateCall(cc.ctx, cc.fun, cc.builtins, "__callCtorAny", { cc.jsValAddr(funObj.id), argCount, argsStart }, true);
            }
            else if (funObj.type == ValueType::Object) {
                generateCall(cc.ctx, cc.fun, cc.builtins, "__callCtorObj", { cc.regs.at(funObj.id), argCount, argsStart }, true);
            }
            else {
                assert(false && "Invalid call operand type");
            }
        }

        cc.copyJSVal(argsStart, cc.jsValAddr(call.res.id));

        cc.insert(MIR_new_insn(cc.ctx, MIR_BEND, MIR_new_reg_op(cc.ctx, startBlock)));
        cc.checkExceptionJSValue(cc.jsValAddr(call.res.id));
    }
}

void generateTerminator(CompileContext& cc, Terminator& jump, Function& cfg) {
    switch (jump.type) {
        case Terminator::None:
            assert(false && "Invalid terminator");
        case Terminator::Branch: {
            auto cond = jump.value;
            assert(cond.type == ValueType::Bool);
            cc.insert(MIR_new_insn(cc.ctx, MIR_BTS, cc.labelOp(jump.target), cc.regOp(cond.id)));
            cc.insert(MIR_new_insn(cc.ctx, MIR_JMP, cc.labelOp(jump.other)));
        } break;
        case Terminator::Jump: {
            cc.insert(MIR_new_insn(cc.ctx, MIR_JMP, cc.labelOp(jump.target)));
        } break;
        case Terminator::Return:
            if (cfg.ret == ValueType::Void) {

                generateCall(cc.ctx, cc.fun, cc.builtins, "__exitStackFrame", {}, false);
                cc.insert(MIR_new_ret_insn(cc.ctx, 0));
                break;
            }
            else {
                generateCall(cc.ctx, cc.fun, cc.builtins, "__exitStackFrame", {}, false);
                if (hasRetArg(cfg.ret)) {
                    auto res = jump.value;
                    assert(res.type == cfg.ret);
                    auto addr = cc.jsValAddr(res.id);
                    auto resReg = MIR_reg(cc.ctx, "res", cc.func);
                    cc.copyJSVal(addr, resReg);
                    cc.insert(MIR_new_ret_insn(cc.ctx, 0));
                }
                else {
                    cc.insert(MIR_new_ret_insn(cc.ctx, 1, cc.regOp(jump.value.id)));
                }
            }
            break;
        case Terminator::Throw:
            assert(jump.value.type == ValueType::Any);
            generateCall(cc.ctx, cc.fun, cc.builtins, "__throwVal", { cc.jsValAddr(jump.value.id) }, true);
            cc.checkException();
            break;
        default:
            std::cout << "Jump type not implemented: " << static_cast<int>(jump.type) << std::endl;
    }
}


MIR_item_t compile(MIR_context_t ctx, const std::map<std::string, std::pair<MIR_item_t, MIR_item_t>>& prototypes, Function& cfg, Builtins& builtins) {
    CompileContext cc(ctx, builtins, prototypes);
    cc.stackSlots = allocateStackSlots(cfg);

    {
        std::vector<std::string> argNames;
        std::vector<MIR_var_t> args;
        getArgsCfg(cfg, argNames, args);

        if (cfg.ret == ValueType::Void || hasRetArg(cfg.ret)) {
            cc.fun = MIR_new_func_arr(ctx, cfg.name().c_str(), 0, nullptr, args.size(), args.data());
        }
        else {
            auto retType = getMIRRetType(cfg.ret);
            cc.fun = MIR_new_func_arr(ctx, cfg.name().c_str(), 1, &retType, args.size(), args.data());
        }

        cc.func = MIR_get_item_func(ctx, cc.fun);

        for (size_t i = 0; i < cfg.args.size(); ++i) {
            cc.regs.emplace(cfg.args[i].id, MIR_reg(ctx, args[i].name, cc.func));
        }
    }

    Defer d ([&] {
        MIR_finish_func(ctx);
    });

    cc.exceptionFlagAddr = cc.scratch("_exFlagAddr", MIR_T_I64);
    cc.insert(MIR_new_insn(ctx, MIR_MOV, MIR_new_reg_op(ctx, cc.exceptionFlagAddr), MIR_new_int_op(ctx, reinterpret_cast<int64_t>(&builtins.rtCtx->exceptionFlag))));  // NOLINT
    cc.exceptionLabel = MIR_new_label(ctx);
    cc.invalidConversionLabel = MIR_new_label(ctx);
    cc.divisionByZeroLabel = MIR_new_label(ctx);

    cc.allocaPtr = MIR_new_func_reg(ctx, cc.func, MIR_T_I64, "allocaPtr");
    MIR_append_insn(ctx, cc.fun, MIR_new_insn(ctx, MIR_ALLOCA, MIR_new_reg_op(ctx, cc.allocaPtr), MIR_new_int_op(ctx, sizeof(JSValue) * cc.stackSlots.size())));

    generateCall(ctx, cc.fun, builtins, "__enterStackFrame", {}, false);

    for (auto& block : cfg.blocks) {
        MIR_append_insn(ctx, cc.fun, cc.label(block.get()));

        for (auto instruction : block->instructions) {
            std::visit([&](auto&& arg) { generateInstruction(cc, arg); }, instruction.op);
        }

        generateTerminator(cc, block->jump, cfg);
    }

    cc.insert(cc.divisionByZeroLabel);
    generateThrowError(ctx, cc.fun, builtins, "Division by zero", ErrorType::RangeError);
    cc.insert(MIR_new_insn(ctx, MIR_JMP, MIR_new_label_op(ctx, cc.exceptionLabel)));

    cc.insert(cc.invalidConversionLabel);
    generateThrowError(ctx, cc.fun, builtins, "Invalid conversion", ErrorType::TypeError);
    cc.insert(MIR_new_insn(ctx, MIR_JMP, MIR_new_label_op(ctx, cc.exceptionLabel)));

    cc.insert(cc.exceptionLabel);
    generateSetExceptionFlag(ctx, cc.fun, cc.exceptionFlagAddr, 1);
    generateCall(ctx, cc.fun, builtins, "__exitStackFrame", {}, false);
    switch (cfg.ret) {
        case ValueType::Void:
        case ValueType::Any:
            cc.insert(MIR_new_ret_insn(ctx, 0));
            break;
        case ValueType::I32:
        case ValueType::Bool:
            cc.insert(MIR_new_ret_insn(ctx, 1, MIR_new_int_op(ctx, 0)));
            break;
        case ValueType::F64:
            cc.insert(MIR_new_ret_insn(ctx, 1, MIR_new_double_op(ctx, 0.0)));
            break;
        case ValueType::Object:
            cc.insert(MIR_new_ret_insn(ctx, 1, MIR_new_int_op(ctx, 0)));
            break;
        default:
            assert(false && "Invalid return type");
    }

    return cc.fun;
}


// returning composite types from MIR is not supported
MIR_item_t compileCaller(MIR_context_t ctx, Builtins& builtins, Function& cfg, MIR_item_t callee, MIR_item_t proto) {
    std::string callerName = "_caller_" + cfg.name();

    std::vector<MIR_var_t> args {
        { .type = MIR_T_I64, .name = "argc" },
        { .type = MIR_T_P, .name = "argv" }
    };

    args.push_back({ .type = MIR_T_P, .name = "res" });

    MIR_item_t caller;
    caller = MIR_new_func_arr(ctx, callerName.c_str(), 0, nullptr, args.size(), args.data());
    auto callerFunc = MIR_get_item_func(ctx, caller);

    Defer d ([&] {
        MIR_finish_func(ctx);
    });

    auto insert = [&](MIR_insn_t insn) {
        MIR_append_insn(ctx, caller, insn);
    };

    MIR_label_t entry = MIR_new_label(ctx);
    insert(entry);
    MIR_label_t argErrorLabel = MIR_new_label(ctx);
    MIR_label_t exceptionLabel = MIR_new_label(ctx);
    MIR_reg_t exceptionFlagAddr = MIR_new_func_reg(ctx, callerFunc, MIR_T_I64, "exFlagAddr");
    insert(MIR_new_insn(ctx, MIR_MOV, MIR_new_reg_op(ctx, exceptionFlagAddr), MIR_new_int_op(ctx, reinterpret_cast<int64_t>(&builtins.rtCtx->exceptionFlag))));  // NOLINT
    generateSetExceptionFlag(ctx, caller, exceptionFlagAddr, 0);

    std::vector<MIR_op_t> argOps;
    MIR_reg_t argcReg = MIR_reg(ctx, "argc", callerFunc);
    MIR_reg_t argvReg = MIR_reg(ctx, "argv", callerFunc);
    MIR_reg_t posReg;
    {
        std::string name = "pos";
        posReg = MIR_new_func_reg(ctx, callerFunc, MIR_T_I64, name.c_str());
    }

    insert(MIR_new_insn(ctx, MIR_BLT, MIR_new_label_op(ctx, argErrorLabel), MIR_new_reg_op(ctx, argcReg), MIR_new_int_op(ctx, cfg.args.size())));
    auto addr = MIR_new_func_reg(ctx, callerFunc, MIR_T_I64, "_addr");

    argOps.reserve(cfg.args.size());
    for (size_t i = 0; i < cfg.args.size(); ++i) {
        auto& arg = cfg.args[i];

        std::string name = "_arg" + std::to_string(i);
        insert(MIR_new_insn(ctx, MIR_MOV, MIR_new_reg_op(ctx, posReg), MIR_new_int_op(ctx, i)));

        static constexpr size_t argOffset = sizeof(JSValue);

        insert(MIR_new_insn(ctx, MIR_ADD, MIR_new_reg_op(ctx, addr), MIR_new_reg_op(ctx, argvReg), MIR_new_int_op(ctx, argOffset * i)));

        if (arg.type == ValueType::Any) {
            auto reg = MIR_new_func_reg(ctx, callerFunc, MIR_T_I64, name.c_str());
            insert(MIR_new_insn(ctx, MIR_MOV, MIR_new_reg_op(ctx, reg), MIR_new_reg_op(ctx, addr)));

            auto [type, size] = getMIRArgType(ValueType::Any);
            auto op = MIR_new_mem_op(ctx, type, size, reg, 0, 0);
            argOps.push_back(op);
        }
        else {
            MIR_reg_t reg;
            reg = MIR_new_func_reg(ctx, callerFunc, getMIRRegType(arg.type), name.c_str());
            generateConvertFromJSVal(ctx, caller, builtins, addr, reg, arg.type, argErrorLabel);
            argOps.push_back(MIR_new_reg_op(ctx, reg));
        }
    }

    MIR_reg_t resReg = MIR_reg(ctx, "res", callerFunc);
    MIR_reg_t calleeRes;

    std::vector<MIR_op_t> callArgs;
    callArgs.push_back(MIR_new_ref_op(ctx, proto));
    callArgs.push_back(MIR_new_ref_op(ctx, callee));
    if (cfg.ret != ValueType::Void && !hasRetArg(cfg.ret)) {
        std::string name = "calleeRes";
        calleeRes = MIR_new_func_reg(ctx, callerFunc, getMIRRegType(cfg.ret), name.c_str());
        callArgs.push_back(MIR_new_reg_op(ctx, calleeRes));
    }
    for (auto& op : argOps) {
        callArgs.push_back(op);
    }
    if (hasRetArg(cfg.ret)) {
        callArgs.push_back(MIR_new_reg_op(ctx, resReg));
    }
    insert(MIR_new_insn_arr(ctx, MIR_CALL, callArgs.size(), callArgs.data()));
    generateCheckException(ctx, caller, exceptionFlagAddr, exceptionLabel);

    // perform conversion from resReg to JSValue in *res

    static constexpr size_t TAG_DISP = offsetof(JSValue, tag);
    static constexpr size_t VAL_DISP = offsetof(JSValue, u);
    auto tagOp = MIR_new_mem_op(ctx, MIR_T_I64, TAG_DISP, resReg, 0, 0);
    switch (cfg.ret) {
        case ValueType::I32:  case ValueType::Bool:  case ValueType::Object:  case ValueType::F64:
            generateConvertToJSVal(ctx, caller, builtins, calleeRes, resReg, cfg.ret);
            break;
        case ValueType::Void:
            insert(MIR_new_insn(ctx, MIR_MOV, tagOp, MIR_new_int_op(ctx, JS_TAG_UNDEFINED)));
            insert(MIR_new_insn(ctx, MIR_MOV, MIR_new_mem_op(ctx, MIR_T_I64, VAL_DISP, resReg, 0, 0), MIR_new_int_op(ctx, 0)));
            break;
        case ValueType::Any:
            break;  // return is performed by callee
        default:
            throw std::runtime_error("Invalid return type");
    }
    insert(MIR_new_ret_insn(ctx, 0));

    insert(argErrorLabel);
    generateThrowError(ctx, caller, builtins, "Invalid arguments", ErrorType::TypeError);

    insert(exceptionLabel);

    insert(MIR_new_insn(ctx, MIR_MOV, tagOp, MIR_new_int_op(ctx, JS_TAG_EXCEPTION)));
    insert(MIR_new_insn(ctx, MIR_MOV, MIR_new_mem_op(ctx, MIR_T_I64, VAL_DISP, resReg, 0, 0), MIR_new_int_op(ctx, 0)));
    insert(MIR_new_ret_insn(ctx, 0));

    return caller;
}


}  // namespace jac::cfg::mir_emit
