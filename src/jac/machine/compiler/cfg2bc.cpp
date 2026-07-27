#include "cfg2bc.h"

#include "jac/machine/compiler/bcWriter.h"
#include "jac/machine/compiler/qjsBcOpcodes.h"
#include "jac/machine/compiler/tlessCfg.h"
#include "jac/machine/compiler/tlessOpcode.h"

#include <algorithm>
#include <cstring>
#include <deque>

namespace jac::bc {

namespace cfg = jac::cfg::tless;

namespace {
    enum class GlobalKind : int { Var = 0, Let = 1, Const = 2 };

    constexpr uint8_t DEFINE_GLOBAL_LEX_VAR = 0x80;
    constexpr uint8_t GLOBAL_WRITABLE       = 0x02;
}

struct SlotInfo {
    enum Type {
        Arg,
        Global,
        Local,
        Closure
    } type;
    std::variant<int, std::string> id;
    int kind = 0;  // GlobalKind: only meaningful when type == Global
};


struct ClosureCapture {
    SlotInfo::Type type;
    int index;
    bool isArg;
    bool isClosure;
};

struct GlobalDef {
    std::string name;
    int kind;
};

struct State {
    std::map<cfg::RegInfo*, SlotInfo> slotMap;
    std::set<cfg::RegInfo*> unmaterializedRegs;
    int localCount = 0;
    std::map<int, std::vector<ClosureCapture>> closureMappings;
    int argCount = -1;
    std::vector<GlobalDef> globalDefs;
    std::set<std::string> initializedGlobals;
};

struct OpcodeInfo {
    std::vector<int> matArgStackOrder;
    std::vector<int> unmaterializedRes;
    bool isVariadic = false;
};

static OpcodeInfo getOpcodeInfo(cfg::Opcode op) {
    using Op = cfg::Opcode;
    switch (op) {
        case Op::CreateLocal:       return {{}, {0}};
        case Op::CreateUndefined:   return {{}, {}};
        case Op::Copy:              return {{0}, {}};
        case Op::BoolNot:           return {{0}, {}};
        case Op::BitNot:            return {{0}, {}};
        case Op::UnPlus:            return {{0}, {}};
        case Op::UnMinus:           return {{0}, {}};
        case Op::Load:              return {{}, {1}};
        case Op::Dup:               return {{0}, {}};
        case Op::Kill:              return {{0}, {}};
        case Op::CreateGlobalSlot:  return {{}, {0}};
        case Op::GetArgRef:         return {{}, {0}};
        case Op::GetClosureRef:     return {{}, {0}};
        case Op::GetGlobalRef:      return {{}, {0}};
        case Op::Add:               return {{0, 1}, {}};
        case Op::Sub:               return {{0, 1}, {}};
        case Op::Mul:               return {{0, 1}, {}};
        case Op::Div:               return {{0, 1}, {}};
        case Op::Rem:               return {{0, 1}, {}};
        case Op::Pow:               return {{0, 1}, {}};
        case Op::LShift:            return {{0, 1}, {}};
        case Op::RShift:            return {{0, 1}, {}};
        case Op::URShift:           return {{0, 1}, {}};
        case Op::BitAnd:            return {{0, 1}, {}};
        case Op::BitOr:             return {{0, 1}, {}};
        case Op::BitXor:            return {{0, 1}, {}};
        case Op::Eq:                return {{0, 1}, {}};
        case Op::Neq:               return {{0, 1}, {}};
        case Op::StrictEq:          return {{0, 1}, {}};
        case Op::StrictNeq:         return {{0, 1}, {}};
        case Op::Gt:                return {{0, 1}, {}};
        case Op::Gte:               return {{0, 1}, {}};
        case Op::Lt:                return {{0, 1}, {}};
        case Op::Lte:               return {{0, 1}, {}};
        case Op::GetMember:         return {{0, 1}, {}};
        case Op::SetMember:         return {{0, 1, 2}, {}};
        case Op::Store:             return {{0}, {0}};
        case Op::Call:              return {{}, {}, true};
        case Op::CallMethod:        return {{}, {}, true};
        case Op::Construct:         return {{}, {}, true};
        case Op::Await:             return {{0}, {}};
        case Op::MakeClosure:       return {{}, {}, true};
    }
    return {{}, {}};
}

void prepass(const cfg::Function& fun, State& state) {
    std::deque<cfg::BasicBlockPtr> worklist;
    worklist.push_back(fun.entry);
    std::set<cfg::BasicBlockPtr> visited{ fun.entry };

    auto enqueue = [&](cfg::BasicBlockPtr blockPtr) {
        if (!visited.contains(blockPtr)) {
            worklist.push_back(blockPtr);
            visited.insert(blockPtr);
        }
    };

    auto pullConst = [&](cfg::RegInfo* regInfo) -> cfg::ConstInit {
        if (regInfo->assign && !regInfo->assign->isOperation()) {
            return regInfo->assign->asConstInit();
        }
        throw _IRGenError("Expected constant value in prepass");
    };

    while (!worklist.empty()) {
        auto blockPtr = worklist.front();
        worklist.pop_front();

        auto remapRegs = [&](const auto& succ) {
            for (size_t i = 0; i < blockPtr->terminator.args.size(); ++i) {
                auto ptrPred = blockPtr->terminator.args[i].get();
                auto ptrSucc = succ[i].get();
                if (state.unmaterializedRegs.contains(ptrPred)) {
                    state.unmaterializedRegs.insert(ptrSucc);
                }
                if (auto it = state.slotMap.find(ptrPred); it != state.slotMap.end()) {
                    state.slotMap[ptrSucc] = it->second;
                }
            }
        };

        for (auto& instr : blockPtr->instructions) {
            if (auto* op = std::get_if<cfg::Operation>(&instr->op)) {
                const auto& info = getOpcodeInfo(op->op);

                for (size_t i = 0; i < op->args.size(); i++) {
                    if (!info.isVariadic && std::find(info.matArgStackOrder.begin(), info.matArgStackOrder.end(), static_cast<int>(i)) == info.matArgStackOrder.end()) {
                        state.unmaterializedRegs.insert(op->args[i].get());
                    }
                }
                for (int idx : info.unmaterializedRes) {
                    state.unmaterializedRegs.insert(op->res[idx].get());
                }

                switch (op->op) {
                case cfg::Opcode::CreateLocal:
                    state.slotMap[op->res[0].get()] = SlotInfo{
                        .type = SlotInfo::Local,
                        .id = state.localCount++
                    };
                    break;
                case cfg::Opcode::Load:
                    state.slotMap[op->res[1].get()] = state.slotMap.at(op->args[0].get());
                    break;
                case cfg::Opcode::Dup:
                    if (auto it = state.slotMap.find(op->args[0].get()); it != state.slotMap.end()) {
                        state.slotMap[op->res[0].get()] = it->second;
                        state.slotMap[op->res[1].get()] = it->second;
                    }
                    if (state.unmaterializedRegs.contains(op->args[0].get())) {
                        state.unmaterializedRegs.insert(op->res[0].get());
                        state.unmaterializedRegs.insert(op->res[1].get());
                    }
                    break;
                case cfg::Opcode::Store:
                    state.slotMap[op->res[0].get()] = state.slotMap.at(op->args[1].get());
                    break;
                // Global slots are pre-declared in the entry block, so BFS order preserves source order.
                case cfg::Opcode::CreateGlobalSlot: {
                    auto nameConst = pullConst(op->args[0].get());
                    auto kindConst = pullConst(op->args[1].get());
                    auto* namePtr = std::get_if<std::string>(&nameConst.value);
                    auto* kindPtr = std::get_if<int>(&kindConst.value);
                    if (!namePtr || !kindPtr) {
                        throw _IRGenError("Invalid constants in CreateGlobalSlot");
                    }
                    const auto& name = *namePtr;
                    int kind = *kindPtr;
                    state.slotMap[op->res[0].get()] = SlotInfo{
                        .type = SlotInfo::Global,
                        .id = name,
                        .kind = kind
                    };
                    state.globalDefs.push_back({ name, kind });
                    break;
                }
                case cfg::Opcode::GetArgRef:
                    state.slotMap[op->res[0].get()] = SlotInfo{
                        .type = SlotInfo::Arg,
                        .id = std::get<int>(pullConst(op->args[0].get()).value)
                    };
                    break;
                case cfg::Opcode::GetClosureRef:
                    state.slotMap[op->res[0].get()] = SlotInfo{
                        .type = SlotInfo::Closure,
                        .id = std::get<int>(pullConst(op->args[0].get()).value)
                    };
                    break;
                case cfg::Opcode::GetGlobalRef:
                    state.slotMap[op->res[0].get()] = SlotInfo{
                        .type = SlotInfo::Global,
                        .id = std::get<std::string>(pullConst(op->args[0].get()).value)
                    };
                    break;
                case cfg::Opcode::MakeClosure: {
                    auto constInit = pullConst(op->args[0].get());
                    auto poolConst = std::get<cfg::PoolConst>(constInit.value);
                    assert(!state.closureMappings.contains(poolConst.id));
                    std::vector<ClosureCapture> captures;
                    for (size_t i = 1; i < op->args.size(); i++) {
                        auto it = state.slotMap.find(op->args[i].get());
                        if (it == state.slotMap.end()) {
                            throw _IRGenError("Closure capture register has no slot mapping");
                        }
                        int id = 0;
                        bool isArg = (it->second.type == SlotInfo::Arg);
                        bool isClosure = (it->second.type == SlotInfo::Closure);
                        if (auto* pid = std::get_if<int>(&it->second.id)) {
                            id = *pid;
                        }
                        captures.push_back({ it->second.type, id, isArg, isClosure });
                    }
                    state.closureMappings[poolConst.id] = captures;
                    break;
                }
                default: break;
                }
            }
            else if (auto* const_ = std::get_if<cfg::ConstInit>(&instr->op)) {
                if (std::get_if<cfg::PoolConst>(&const_->value)) {
                    state.unmaterializedRegs.insert(const_->reg.get());
                }
            }
        }

        if (blockPtr->terminator.type == cfg::Terminator::Type::Branch) {
            enqueue(blockPtr->terminator.target);
            remapRegs(blockPtr->terminator.target->args);
            enqueue(blockPtr->terminator.other);
            remapRegs(blockPtr->terminator.other->args);
        }
        else if (blockPtr->terminator.type == cfg::Terminator::Type::Jump) {
            enqueue(blockPtr->terminator.target);
            remapRegs(blockPtr->terminator.target->args);
        }
    }

    auto maxArg = -1;
    for (const auto& slot : state.slotMap) {
        if (slot.second.type == SlotInfo::Arg) {
            maxArg = std::max(maxArg, std::get<int>(slot.second.id));
        }
    }
    state.argCount = maxArg + 1;
}

std::unique_ptr<FunctionBytecode> emitFunction(BytecodeRoot& root, const cfg::Function& fun, const std::string& filename,
                                                 CompileMode mode, const std::vector<ClosureCapture>* closureMapping = nullptr) {
    State state;
    prepass(fun, state);

    auto bcPtr = std::make_unique<FunctionBytecode>(root.addAtom(fun.name()), root.addAtom(filename));
    auto& bc = *bcPtr;

    if (mode == CompileMode::Script) {
        for (const auto& def : state.globalDefs) {
            uint32_t atom = root.addAtom(def.name);
            uint8_t flags = 0;
            if (def.kind == static_cast<int>(GlobalKind::Let) || def.kind == static_cast<int>(GlobalKind::Const)) {
                flags |= DEFINE_GLOBAL_LEX_VAR;
            }
            if (def.kind == static_cast<int>(GlobalKind::Let)) {
                flags |= GLOBAL_WRITABLE;
            }
            writeByte(bc.bytecode, OP_check_define_var);
            writeInt<4>(bc.bytecode, atom);
            writeByte(bc.bytecode, flags);
            writeByte(bc.bytecode, OP_define_var);
            writeInt<4>(bc.bytecode, atom);
            writeByte(bc.bytecode, flags);
        }
    }

    std::map<int, int> functionPoolIndexMap;
    for (size_t i = 0; i < fun.constPool.size(); i++) {
        if (auto* childFn = std::get_if<std::unique_ptr<cfg::Function>>(&fun.constPool[i].value)) {
            functionPoolIndexMap[static_cast<int>(i)] = static_cast<int>(bcPtr->cpool.size());
            auto it = state.closureMappings.find(static_cast<int>(i));
            // Inner functions share the script global environment; do not re-emit the prologue.
            if (it != state.closureMappings.end()) {
                bcPtr->cpool.push_back(emitFunction(root, **childFn, filename, CompileMode::Module, &it->second));
            }
            else {
                bcPtr->cpool.push_back(emitFunction(root, **childFn, filename, CompileMode::Module, nullptr));
            }
        }
    }

    std::deque<cfg::BasicBlockPtr> worklist;
    worklist.push_back(fun.entry);
    std::set<cfg::BasicBlockPtr> visited{ fun.entry };
    std::map<cfg::BasicBlockPtr, std::vector<cfg::RegInfo*>> entryStacks;
    entryStacks[fun.entry] = {};

    std::map<cfg::BasicBlockPtr, uint32_t> blockOffsets;
    std::vector<std::pair<uint32_t, cfg::BasicBlockPtr>> termOffsets;
    std::vector<std::tuple<uint32_t, uint32_t, size_t>> blockRanges;

    uint32_t maxStackDepth = 0;

    auto enqueue = [&](cfg::BasicBlockPtr blockPtr) {
        if (!visited.contains(blockPtr)) {
            worklist.push_back(blockPtr);
            visited.insert(blockPtr);
        }
    };

    while (!worklist.empty()) {
        auto blockPtr = worklist.front();
        worklist.pop_front();

        auto stackState = [](auto& vec) { return std::list<cfg::RegInfo*>(vec.begin(), vec.end()); }(entryStacks[blockPtr]);
        size_t blockEntryDepth = stackState.size();
        blockOffsets[blockPtr] = bc.pos();

        maxStackDepth = std::max(maxStackDepth, static_cast<uint32_t>(stackState.size()));

        auto useReg = [&](cfg::RegInfo* regInfo) {
            if (state.unmaterializedRegs.contains(regInfo)) {
                return;
            }
            if (stackState.empty() || stackState.back() != regInfo) {
                throw _IRGenError("useReg called with out-of-order register (should be at stack top for terminators)");
            }
            stackState.pop_back();
        };
        auto assignReg = [&](cfg::RegInfo* regInfo) {
            if (state.unmaterializedRegs.contains(regInfo)) {
                return;
            }
            stackState.push_back(regInfo);
        };
        auto rotateTop = [&](int n) {
            auto rit = stackState.rbegin();
            for (int i = 1; i < n; i++) ++rit;
            auto* x = *rit;
            stackState.erase(std::prev(rit.base()));
            stackState.push_back(x);
        };

        auto consumeArgs = [&](const cfg::Operation& op) {
            const auto& info = getOpcodeInfo(op.op);

            std::vector<int> order = info.matArgStackOrder;
            if (info.isVariadic) {
                order.clear();
                for (size_t i = 0; i < op.args.size(); ++i) {
                    order.push_back(static_cast<int>(i));
                }
            }

            for (int argIdx : order) {
                auto* target = op.args[argIdx].get();
                if (state.unmaterializedRegs.contains(target)) continue;

                int depth = 0;
                auto it = stackState.rbegin();
                for (; it != stackState.rend(); ++it) {
                    if (*it == target) break;
                    depth++;
                }

                if (it == stackState.rend()) {
                    throw _IRGenError("Arg register not found on stack during consumeArgs");
                }

                auto n = std::min(depth + 1, static_cast<int>(stackState.size()));
                switch (n) {
                    case 1: break;
                    case 2: writeByte(bc.bytecode, OP_swap); rotateTop(2); break;
                    case 3: writeByte(bc.bytecode, OP_rot3l); rotateTop(3); break;
                    case 4: writeByte(bc.bytecode, OP_rot4l); rotateTop(4); break;
                    case 5: writeByte(bc.bytecode, OP_rot5l); rotateTop(5); break;
                    default: {
                        writeByte(bc.bytecode, OP_array_from);
                        writeInt<2>(bc.bytecode, static_cast<uint16_t>(n));
                        for (int idx = 1; idx < n; idx++) {
                            writeByte(bc.bytecode, OP_push_i32);
                            writeInt<4>(bc.bytecode, idx);
                            writeByte(bc.bytecode, OP_get_array_el2);
                            writeByte(bc.bytecode, OP_swap);
                        }
                        writeByte(bc.bytecode, OP_push_i32);
                        writeInt<4>(bc.bytecode, 0);
                        writeByte(bc.bytecode, OP_get_array_el);
                        rotateTop(n);
                        break;
                    }
                }
            }

            for (int argIdx : order) {
                auto* target = op.args[argIdx].get();
                if (state.unmaterializedRegs.contains(target)) continue;
                auto it = std::find(stackState.rbegin(), stackState.rend(), target);
                stackState.erase(std::prev(it.base()));
            }
        };
        auto remapStack = [&](const auto& succ) {
            std::map<cfg::RegInfo*, cfg::RegInfo*> remap;
            for (size_t i = 0; i < blockPtr->terminator.args.size(); ++i) {
                remap[blockPtr->terminator.args[i].get()] = succ->args[i].get();
            }
            std::vector<cfg::RegInfo*> newStack;
            for (auto& regInfo : stackState) {
                auto it = remap.find(regInfo);
                if (it == remap.end()) {
                    throw _IRGenError("Invalid stack state when remapping for successor");
                }
                newStack.push_back(it->second);
            }

            if (auto it = entryStacks.find(succ); it != entryStacks.end()) {
                if (it->second != newStack) {
                    throw _IRGenError("Inconsistent stack state for successor");
                }
                return false;
            }
            entryStacks[succ] = std::move(newStack);
            return true;
        };

        auto withEx = [&](const auto& pre, const auto& body) {
            pre();
            writeByte(bc.bytecode, OP_catch);
            auto catchOffsetPos = bc.pos();
            writeInt<4>(bc.bytecode, 0);
            body();
            writeByte(bc.bytecode, OP_nip_catch);
            writeByte(bc.bytecode, OP_undefined);
            writeByte(bc.bytecode, OP_push_false);
            writeByte(bc.bytecode, OP_goto);
            auto endOffsetPos = bc.pos();
            writeInt<4>(bc.bytecode, 0);

            auto catchOffset = static_cast<int32_t>(bc.pos() - catchOffsetPos);
            std::memcpy(&bc.bytecode[catchOffsetPos], &catchOffset, sizeof(catchOffset));
            writeByte(bc.bytecode, OP_undefined);
            writeByte(bc.bytecode, OP_swap);
            writeByte(bc.bytecode, OP_push_true);

            auto endOffset = static_cast<int32_t>(bc.pos() - endOffsetPos);
            std::memcpy(&bc.bytecode[endOffsetPos], &endOffset, sizeof(endOffset));
        };

        auto withExVoid = [&](const auto& pre, const auto& body) {
            pre();
            writeByte(bc.bytecode, OP_catch);
            auto catchOffsetPos = bc.pos();
            writeInt<4>(bc.bytecode, 0);
            body();
            writeByte(bc.bytecode, OP_drop);
            writeByte(bc.bytecode, OP_undefined);
            writeByte(bc.bytecode, OP_push_false);
            writeByte(bc.bytecode, OP_goto);
            auto endOffsetPos = bc.pos();
            writeInt<4>(bc.bytecode, 0);

            auto catchOffset = static_cast<int32_t>(bc.pos() - catchOffsetPos);
            std::memcpy(&bc.bytecode[catchOffsetPos], &catchOffset, sizeof(catchOffset));
            writeByte(bc.bytecode, OP_push_true);

            auto endOffset = static_cast<int32_t>(bc.pos() - endOffsetPos);
            std::memcpy(&bc.bytecode[endOffsetPos], &endOffset, sizeof(endOffset));
        };


        for (const auto& instr : blockPtr->instructions) {
            if (auto* op = std::get_if<cfg::Operation>(&instr->op)) {
                consumeArgs(*op);
                for (auto& dest : op->res) {
                    assignReg(dest.get());
                }
                switch (op->op) {
                    case cfg::Opcode::CreateLocal:
                        break;
                    case cfg::Opcode::CreateUndefined:
                        writeByte(bc.bytecode, OP_undefined);
                        break;
                    case cfg::Opcode::Copy:
                        break;
                    case cfg::Opcode::BoolNot:
                        writeByte(bc.bytecode, OP_lnot);
                        break;
                    case cfg::Opcode::BitNot:
                        withEx([&] {}, [&] { writeByte(bc.bytecode, OP_swap); writeByte(bc.bytecode, OP_not); });
                        break;
                    case cfg::Opcode::UnPlus:
                        withEx([&] {}, [&] { writeByte(bc.bytecode, OP_swap); writeByte(bc.bytecode, OP_plus); });
                        break;
                    case cfg::Opcode::UnMinus:
                        withEx([&] {}, [&] { writeByte(bc.bytecode, OP_swap); writeByte(bc.bytecode, OP_neg); });
                        break;
                    case cfg::Opcode::Load: {
                        withEx([&] {}, [&] {
                            switch (state.slotMap.at(op->args[0].get()).type) {
                                case SlotInfo::Arg:
                                    writeByte(bc.bytecode, OP_get_arg);
                                    writeInt<2>(bc.bytecode, std::get<int>(state.slotMap.at(op->args[0].get()).id));
                                    break;
                                case SlotInfo::Local:
                                    writeByte(bc.bytecode, OP_get_loc);
                                    writeInt<2>(bc.bytecode, std::get<int>(state.slotMap.at(op->args[0].get()).id));
                                    break;
                                case SlotInfo::Global:
                                    writeByte(bc.bytecode, OP_get_var);
                                    writeInt<4>(bc.bytecode, root.addAtom(std::get<std::string>(state.slotMap.at(op->args[0].get()).id)));
                                    break;
                                case SlotInfo::Closure: {
                                    auto closureIdx = std::get<int>(state.slotMap.at(op->args[0].get()).id);
                                    writeByte(bc.bytecode, OP_get_var_ref);
                                    writeInt<2>(bc.bytecode, closureIdx);
                                    break;
                                }
                            }
                        });
                    } break;
                    case cfg::Opcode::Dup:
                        if (!state.unmaterializedRegs.contains(op->args[0].get())) {
                            writeByte(bc.bytecode, OP_dup);
                        }
                        break;
                    case cfg::Opcode::Kill:
                        if (!state.unmaterializedRegs.contains(op->args[0].get())) {
                            writeByte(bc.bytecode, OP_drop);
                        }
                        break;
                    case cfg::Opcode::CreateGlobalSlot:
                        break;
                    case cfg::Opcode::GetArgRef:
                        break;
                    case cfg::Opcode::GetClosureRef:
                        break;
                    case cfg::Opcode::GetGlobalRef:
                        break;
                    case cfg::Opcode::Add:
                        withEx([&] {}, [&] { writeByte(bc.bytecode, OP_rot3r); writeByte(bc.bytecode, OP_add); });
                        break;
                    case cfg::Opcode::Sub:
                        withEx([&] {}, [&] { writeByte(bc.bytecode, OP_rot3r); writeByte(bc.bytecode, OP_sub); });
                        break;
                    case cfg::Opcode::Mul:
                        withEx([&] {}, [&] { writeByte(bc.bytecode, OP_rot3r); writeByte(bc.bytecode, OP_mul); });
                        break;
                    case cfg::Opcode::Div:
                        withEx([&] {}, [&] { writeByte(bc.bytecode, OP_rot3r); writeByte(bc.bytecode, OP_div); });
                        break;
                    case cfg::Opcode::Rem:
                        withEx([&] {}, [&] { writeByte(bc.bytecode, OP_rot3r); writeByte(bc.bytecode, OP_mod); });
                        break;
                    case cfg::Opcode::Pow:
                        withEx([&] {}, [&] { writeByte(bc.bytecode, OP_rot3r); writeByte(bc.bytecode, OP_pow); });
                        break;
                    case cfg::Opcode::LShift:
                        withEx([&] {}, [&] { writeByte(bc.bytecode, OP_rot3r); writeByte(bc.bytecode, OP_shl); });
                        break;
                    case cfg::Opcode::RShift:
                        withEx([&] {}, [&] { writeByte(bc.bytecode, OP_rot3r); writeByte(bc.bytecode, OP_sar); });
                        break;
                    case cfg::Opcode::URShift:
                        withEx([&] {}, [&] { writeByte(bc.bytecode, OP_rot3r); writeByte(bc.bytecode, OP_shr); });
                        break;
                    case cfg::Opcode::BitAnd:
                        withEx([&] {}, [&] { writeByte(bc.bytecode, OP_rot3r); writeByte(bc.bytecode, OP_and); });
                        break;
                    case cfg::Opcode::BitOr:
                        withEx([&] {}, [&] { writeByte(bc.bytecode, OP_rot3r); writeByte(bc.bytecode, OP_or); });
                        break;
                    case cfg::Opcode::BitXor:
                        withEx([&] {}, [&] { writeByte(bc.bytecode, OP_rot3r); writeByte(bc.bytecode, OP_xor); });
                        break;
                    case cfg::Opcode::Eq:
                        withEx([&] {}, [&] { writeByte(bc.bytecode, OP_rot3r); writeByte(bc.bytecode, OP_eq); });
                        break;
                    case cfg::Opcode::Neq:
                        withEx([&] {}, [&] { writeByte(bc.bytecode, OP_rot3r); writeByte(bc.bytecode, OP_neq); });
                        break;
                    case cfg::Opcode::StrictEq:
                        writeByte(bc.bytecode, OP_strict_eq);
                        break;
                    case cfg::Opcode::StrictNeq:
                        writeByte(bc.bytecode, OP_strict_neq);
                        break;
                    case cfg::Opcode::Gt:
                        withEx([&] {}, [&] { writeByte(bc.bytecode, OP_rot3r); writeByte(bc.bytecode, OP_gt); });
                        break;
                    case cfg::Opcode::Gte:
                        withEx([&] {}, [&] { writeByte(bc.bytecode, OP_rot3r); writeByte(bc.bytecode, OP_gte); });
                        break;
                    case cfg::Opcode::Lt:
                        withEx([&] {}, [&] { writeByte(bc.bytecode, OP_rot3r); writeByte(bc.bytecode, OP_lt); });
                        break;
                    case cfg::Opcode::Lte:
                        withEx([&] {}, [&] { writeByte(bc.bytecode, OP_rot3r); writeByte(bc.bytecode, OP_lte); });
                        break;
                    case cfg::Opcode::GetMember:
                        withEx([&] {}, [&] { writeByte(bc.bytecode, OP_rot3r); writeByte(bc.bytecode, OP_get_array_el); });
                        break;
                    case cfg::Opcode::SetMember:
                        withExVoid([&] {}, [&] {
                            writeByte(bc.bytecode, OP_rot4l);
                            writeByte(bc.bytecode, OP_rot4l);
                            writeByte(bc.bytecode, OP_rot4l);
                            writeByte(bc.bytecode, OP_put_array_el);
                        });
                        break;
                    case cfg::Opcode::Store: {
                        withExVoid([&] {}, [&] {
                            writeByte(bc.bytecode, OP_swap);
                            switch (state.slotMap.at(op->args[1].get()).type) {
                                case SlotInfo::Arg:
                                    writeByte(bc.bytecode, OP_put_arg);
                                    writeInt<2>(bc.bytecode, std::get<int>(state.slotMap.at(op->args[1].get()).id));
                                    break;
                                case SlotInfo::Local:
                                    writeByte(bc.bytecode, OP_put_loc);
                                    writeInt<2>(bc.bytecode, std::get<int>(state.slotMap.at(op->args[1].get()).id));
                                    break;
                                case SlotInfo::Global: {
                                    const auto& targetSlot = state.slotMap.at(op->args[1].get());
                                    auto name = std::get<std::string>(targetSlot.id);
                                    uint32_t atom = root.addAtom(name);
                                    bool useInit = false;
                                    if (mode == CompileMode::Script && targetSlot.kind != static_cast<int>(GlobalKind::Var)) {
                                        if (!state.initializedGlobals.contains(name)) {
                                            useInit = true;
                                            state.initializedGlobals.insert(name);
                                        }
                                    }
                                    writeByte(bc.bytecode, useInit ? OP_put_var_init : OP_put_var);
                                    writeInt<4>(bc.bytecode, atom);
                                    break;
                                }
                                case SlotInfo::Closure: {
                                    auto closureIdx = std::get<int>(state.slotMap.at(op->args[1].get()).id);
                                    writeByte(bc.bytecode, OP_put_var_ref);
                                    writeInt<2>(bc.bytecode, closureIdx);
                                    break;
                                }
                            }
                        });
                    } break;
                    case cfg::Opcode::Call:
                        withEx([&] {
                            int nargs = static_cast<int>(op->args.size() - 1);
                            writeByte(bc.bytecode, OP_array_from);
                            writeInt<2>(bc.bytecode, nargs);
                            writeByte(bc.bytecode, OP_undefined);
                            writeByte(bc.bytecode, OP_swap);
                        }, [&] {
                            writeByte(bc.bytecode, OP_rot4l);
                            writeByte(bc.bytecode, OP_rot4l);
                            writeByte(bc.bytecode, OP_rot4l);
                            writeByte(bc.bytecode, OP_apply);
                            writeInt<2>(bc.bytecode, 0);
                        });
                        break;
                    case cfg::Opcode::CallMethod:
                        withEx([&] {
                            int nargs = static_cast<int>(op->args.size() - 2);
                            writeByte(bc.bytecode, OP_array_from);
                            writeInt<2>(bc.bytecode, nargs);
                            writeByte(bc.bytecode, OP_rot3l);
                            writeByte(bc.bytecode, OP_swap);
                        }, [&] {
                            writeByte(bc.bytecode, OP_rot4l);
                            writeByte(bc.bytecode, OP_rot4l);
                            writeByte(bc.bytecode, OP_rot4l);
                            writeByte(bc.bytecode, OP_apply);
                            writeInt<2>(bc.bytecode, 0);
                        });
                        break;
                    case cfg::Opcode::Construct:
                        withEx([&] {
                            int nargs = static_cast<int>(op->args.size() - 1);
                            writeByte(bc.bytecode, OP_array_from);
                            writeInt<2>(bc.bytecode, nargs);
                            writeByte(bc.bytecode, OP_swap);
                            writeByte(bc.bytecode, OP_dup);
                            writeByte(bc.bytecode, OP_rot3l);
                        }, [&] {
                            writeByte(bc.bytecode, OP_rot4l);
                            writeByte(bc.bytecode, OP_rot4l);
                            writeByte(bc.bytecode, OP_rot4l);
                            writeByte(bc.bytecode, OP_apply);
                            writeInt<2>(bc.bytecode, 1);
                        });
                        break;
                    case cfg::Opcode::Await:
                        throw _IRGenError("Opcode::Await is not supported by cfg2bc; async bytecode lowering is unavailable");
                    case cfg::Opcode::MakeClosure: {
                        auto* codeReg = op->args[0].get();
                        auto& constInit = codeReg->assign->asConstInit();
                        auto poolConst = std::get<cfg::PoolConst>(constInit.value);
                        auto it = functionPoolIndexMap.find(poolConst.id);
                        if (it == functionPoolIndexMap.end()) {
                            throw _IRGenError("Closure target missing from function constant pool");
                        }

                        writeByte(bc.bytecode, OP_fclosure);
                        writeInt<4>(bc.bytecode, it->second);
                        break;
                    }
                }
            }
            else if (auto* const_ = std::get_if<cfg::ConstInit>(&instr->op)) {
                if (!state.unmaterializedRegs.contains(const_->reg.get())) {
                    if (std::get_if<int>(&const_->value)) {
                        writeByte(bc.bytecode, OP_push_i32);
                        writeInt<4>(bc.bytecode, std::get<int>(const_->value));
                    }
                    else if (std::get_if<double>(&const_->value)) {
                        throw std::runtime_error("double constants not supported");
                    }
                    else if (std::get_if<std::string>(&const_->value)) {
                        writeByte(bc.bytecode, OP_push_atom_value);
                        writeInt<4>(bc.bytecode, root.addAtom(std::get<std::string>(const_->value)));
                    }
                    else if (std::get_if<bool>(&const_->value)) {
                        writeByte(bc.bytecode, std::get<bool>(const_->value) ? OP_push_true : OP_push_false);
                    }
                    else {
                        throw std::runtime_error("Unsupported constant type");
                    }
                    assignReg(const_->reg.get());
                }
            }
        }

        if (blockPtr->terminator.type == cfg::Terminator::Type::Branch) {
            useReg(blockPtr->terminator.value.get());

            enqueue(blockPtr->terminator.target);
            remapStack(blockPtr->terminator.target);
            enqueue(blockPtr->terminator.other);
            remapStack(blockPtr->terminator.other);

            writeByte(bc.bytecode, OP_if_false);
            auto offsetPos = bc.pos();
            writeInt<4>(bc.bytecode, 0);
            termOffsets.emplace_back(offsetPos, blockPtr->terminator.other);

            writeByte(bc.bytecode, OP_goto);
            offsetPos = bc.pos();
            writeInt<4>(bc.bytecode, 0);
            termOffsets.emplace_back(offsetPos, blockPtr->terminator.target);
        }
        else if (blockPtr->terminator.type == cfg::Terminator::Type::Jump) {
            enqueue(blockPtr->terminator.target);
            remapStack(blockPtr->terminator.target);

            writeByte(bc.bytecode, OP_goto);
            auto offsetPos = bc.pos();
            writeInt<4>(bc.bytecode, 0);
            termOffsets.emplace_back(offsetPos, blockPtr->terminator.target);
        }
        else if (blockPtr->terminator.type == cfg::Terminator::Type::Return) {
            if (blockPtr->terminator.value.id() != 0) {
                useReg(blockPtr->terminator.value.get());
                writeByte(bc.bytecode, OP_return);
            }
            else {
                writeByte(bc.bytecode, OP_return_undef);
            }
        }
        else if (blockPtr->terminator.type == cfg::Terminator::Type::Throw) {
            if (blockPtr->terminator.value.id() != 0) {
                useReg(blockPtr->terminator.value.get());
                writeByte(bc.bytecode, OP_throw);
            }
            else {
                writeByte(bc.bytecode, OP_undefined);
                writeByte(bc.bytecode, OP_throw);
            }
        }

        for (auto& arg : std::ranges::reverse_view(blockPtr->terminator.args)) {
            useReg(arg.get());
        }

        blockRanges.emplace_back(blockOffsets[blockPtr], static_cast<uint32_t>(bc.pos()), blockEntryDepth);
    }

    for (auto& [offsetPos, target] : termOffsets) {
        auto offset = blockOffsets[target] - offsetPos;
        std::memcpy(&bc.bytecode[offsetPos], &offset, sizeof(offset));
    }

    for (const auto& [start, end, entryDepth] : blockRanges) {
        int64_t depth = static_cast<int64_t>(entryDepth);
        maxStackDepth = std::max(maxStackDepth, static_cast<uint32_t>(depth));

        uint32_t pos = start;
        while (pos < end) {
            uint8_t opByte = bc.bytecode[pos];
            const auto& info = short_opcode_info(opByte);

            int nPop = info.n_pop;
            int nPush = info.n_push;
            if (opByte == OP_array_from) {
                uint16_t nargs = static_cast<uint16_t>(bc.bytecode[pos + 1]) |
                                  (static_cast<uint16_t>(bc.bytecode[pos + 2]) << 8);
                nPop = nargs;
                nPush = 1;
            }

            depth += nPush - nPop;
            if (depth > 0) {
                maxStackDepth = std::max(maxStackDepth, static_cast<uint32_t>(depth));
            }

            pos += info.size;
        }
    }

    bc.jsMode = (1 << 0);  // JS_MODE_STRICT
    bc.varCount = 0;
    bc.stackSize = maxStackDepth;

    auto maxLocal = -1;
    auto maxArg = -1;
    for (const auto& slot : state.slotMap) {
        switch (slot.second.type) {
            case SlotInfo::Arg:
                maxArg = std::max(maxArg, std::get<int>(slot.second.id));
                break;
            case SlotInfo::Local:
                maxLocal = std::max(maxLocal, std::get<int>(slot.second.id));
                bc.addLocal(root.addAtom("loc" + std::to_string(std::get<int>(slot.second.id))), 0);
                break;
            case SlotInfo::Global:
                break;
            case SlotInfo::Closure:
                break;
        }
    }
    if (closureMapping) {
        for (size_t ci = 0; ci < closureMapping->size(); ci++) {
            auto& cap = (*closureMapping)[ci];
            auto nameAtom = root.addAtom("__closure_" + std::to_string(ci));
            uint8_t flags = 0;
            if (!cap.isClosure) {
                flags |= 1;          // is_local
                if (cap.isArg) {
                    flags |= 2;      // is_arg
                }
            }
            bc.closures.emplace_back(nameAtom, cap.index, flags);
        }
    }
    for (int argIndex = 0; argIndex <= maxArg; ++argIndex) {
        bc.args.push_back(root.addAtom("arg" + std::to_string(argIndex)));
    }
    bc.argCount = static_cast<uint32_t>(bc.args.size());
    bc.varCount = 0;

    return bcPtr;
}

void cfg2bc(BytecodeRoot& root, const cfg::Function& fun, const std::string& filename, CompileMode mode) {
    root.child = emitFunction(root, fun, filename, mode);
}

void cfg2bc(BytecodeRoot& root, const cfg::Function& fun, const std::string& filename) {
    cfg2bc(root, fun, filename, CompileMode::Module);
}


}  // namespace jac::bc
