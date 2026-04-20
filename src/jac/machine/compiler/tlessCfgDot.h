#pragma once


#include <cstddef>
#include <iomanip>
#include <ostream>
#include <set>
#include <sstream>
#include <variant>

#include "tlessCfg.h"
#include "tlssOpcode.h"


namespace jac::cfg::tless::dotprint {


inline void print(std::ostream& os, Opcode op) {
    switch (op) {
        case Opcode::CreateSlot: os << "CreateSlot"; break;
        case Opcode::CreateUndefined: os << "CreateUndefined"; break;
        case Opcode::Copy: os << "Copy"; break;
        case Opcode::BoolNot: os << "BoolNot"; break;
        case Opcode::BitNot: os << "BitNot"; break;
        case Opcode::UnPlus: os << "UnPlus"; break;
        case Opcode::UnMinus: os << "UnMinus"; break;
        case Opcode::Load: os << "Load"; break;
        case Opcode::Dup: os << "Dup"; break;
        case Opcode::Kill: os << "Kill"; break;
        case Opcode::CreateGlobalSlot: os << "CreateGlobalSlot"; break;
        case Opcode::GetArgRef: os << "GetArgRef"; break;
        case Opcode::GetClosureRef: os << "GetClosureRef"; break;
        case Opcode::GetGlobalRef: os << "GetGlobalRef"; break;
        case Opcode::Add: os << "Add"; break;
        case Opcode::Sub: os << "Sub"; break;
        case Opcode::Mul: os << "Mul"; break;
        case Opcode::Div: os << "Div"; break;
        case Opcode::Rem: os << "Rem"; break;
        case Opcode::Pow: os << "Pow"; break;
        case Opcode::LShift: os << "LShift"; break;
        case Opcode::RShift: os << "RShift"; break;
        case Opcode::URShift: os << "URShift"; break;
        case Opcode::BitAnd: os << "BitAnd"; break;
        case Opcode::BitOr: os << "BitOr"; break;
        case Opcode::BitXor: os << "BitXor"; break;
        case Opcode::Eq: os << "Eq"; break;
        case Opcode::Neq: os << "Neq"; break;
        case Opcode::Gt: os << "Gt"; break;
        case Opcode::Gte: os << "Gte"; break;
        case Opcode::Lt: os << "Lt"; break;
        case Opcode::Lte: os << "Lte"; break;
        case Opcode::GetMember: os << "GetMember"; break;
        case Opcode::Store: os << "Store"; break;
        case Opcode::SetMember: os << "SetMember"; break;
        case Opcode::Call: os << "Call"; break;
        case Opcode::CallMethod: os << "CallMethod"; break;
        case Opcode::Construct: os << "Construct"; break;
        case Opcode::MakeClosure: os << "MakeClosure"; break;
    }
}


inline void printRegId(std::ostream& os, RegId id) {
    os << "_" << std::abs(id);
}

inline void print(std::ostream& os, const Reg& v) {
    printRegId(os, v.id());
}

inline void print(std::ostream& os, const std::vector<Reg>& regs, bool bracket = false) {
    if (bracket) { os << "["; }
    for (const auto& r : regs) {
        os << " ";
        printRegId(os, r.id());
    }
    if (bracket) { os << " ]"; }
}

inline void print(std::ostream& os, const Operation& op) {
    if (op.res.size() > 0) {
        print(os, op.res);
        os << " ← ";
    }
    print(os, op.op);
    os << " ";
    print(os, op.args);
    os << "";
}

inline void print(std::ostream& os, const ConstInit& init) {
    printRegId(os, init.reg.id());
    os << " ← const ";
    std::visit([&os](const auto& value) {
        if constexpr (std::is_same_v<std::decay_t<decltype(value)>, std::string>) {
            os << std::quoted(value, '\'');
        }
        else if constexpr (std::is_same_v<std::decay_t<decltype(value)>, bool>) {
            os << (value ? "True" : "False");
        }
        else if constexpr (std::is_same_v<std::decay_t<decltype(value)>, PoolConst>) {
            os << "Pool(" << value.id << ")";
        }
        else {
            os << value;
        }
    }, init.value);
    os << "";
}

inline void print(std::ostream& os, const Instruction& instruction) {
    std::visit([&os](const auto& op) {
        print(os, op);
    }, instruction.op);
}


inline void print(std::ostream& os, const BasicBlock& block, std::set<const BasicBlock*>& seen, bool isEntry = false, std::optional<std::string> title = std::nullopt) {
    if (seen.contains(&block)) {
        return;
    }
    seen.insert(&block);
    os << "  block" << &block << " [label=\"{";
    if (title) {
        os << *title << "|";
    }
    if (isEntry) {
        os << "*";
    }
    os << &block;

    if (!block.args.empty()) {
        os << "|";
        os << "<args" << &block << "> ";
        os << "args: ";
        print(os, block.args);
        os << "\\l";
    }

    if (!block.predecessors.empty()) {
        os << "|";
    }
    for (const auto& pred : block.predecessors) {
        os << "pred " << pred << "\\l";
    }

    for (const auto& instruction : block.instructions) {
        os << "|";
        os << "<inst" << instruction.get() << "> ";
        print(os, *instruction);
        os << "\\l";
    }

    auto printTermLabel = [&]() {
        os << "|<term" << &block << "> ";
    };
    switch (block.terminator.type) {
        case Terminator::None:
            printTermLabel();
            os << "<<none>>";
            break;
        case Terminator::Branch:
            printTermLabel();
            os << "if (";
            printRegId(os, block.terminator.value.id());
            os << ") ";
            print(os, block.terminator.args, true);
            break;
        case Terminator::Jump:
            printTermLabel();
            os << "jump ";
            print(os, block.terminator.args, true);
            break;
        case Terminator::Return:
            printTermLabel();
            os << "return";
            if (!block.terminator.value.void_()) {
                os << " ";
                printRegId(os, block.terminator.value.id());
            }
            break;
        case Terminator::Throw:
            printTermLabel();
            os << "throw ";
            printRegId(os, block.terminator.value.id());
            break;
    }
    os << "}\"];\n";

    switch (block.terminator.type) {
        case Terminator::None:
            break;
        case Terminator::Branch:
            os << "  block" << &block << ":s -> block" << block.terminator.target << ":n [label=\"true\"];\n";
            os << "  block" << &block << ":s -> block" << block.terminator.other << ":n [label=\"false\"];\n";
            break;
        case Terminator::Jump:
            os << "  block" << &block << ":s -> block" << block.terminator.target << ":n;\n";
            break;
        case Terminator::Return:
            break;
        case Terminator::Throw:
            break;
    }

    // auto connectUses = [&](const Reg& reg, std::string label) {
    //     std::visit([&](const auto& use) {
    //         using T = std::decay_t<decltype(use)>;
    //         if constexpr (std::is_same_v<T, std::monostate>) {
    //             // no uses
    //         }
    //         else if constexpr (std::is_same_v<T, InstructionPtr>) {
    //             auto useBlock = use->parentBlock;
    //             os << "  block" << useBlock << ":inst" << use << ":e -> " << label << " [style=dashed color=blue];\n";
    //         }
    //         else if constexpr (std::is_same_v<T, BasicBlockPtr>) {
    //             auto useBlock = use;
    //             os << "  block" << useBlock << ":term" << useBlock << ":e -> " << label << " [style=dashed color=blue];\n";
    //         }
    //     }, reg->use);
    // };

    // for (const auto& instruction : block.instructions) {
    //     for (const auto& reg : instruction->res()) {
    //         std::stringstream ss;
    //         ss << "block" << &block << ":inst" << instruction.get() << ":e";
    //         connectUses(reg, ss.str());
    //     }
    // }
    // for (const auto& arg : block.args) {
    //     std::stringstream ss;
    //     ss << "block" << &block << ":args" << &block << ":e";
    //     connectUses(arg, ss.str());
    // }
}


inline void printContent(std::ostream& os, const Function& fn, std::optional<std::string> title = std::nullopt) {
    std::set<const BasicBlock*> seen;
    print(os, *fn.entry, seen, true, title);
    for (auto& block : fn.blocks) {
        print(os, *block, seen);
    }

    for (size_t i = 0; i < fn.constPool.size(); ++i) {
        const auto& constant = fn.constPool[i];
        std::visit([&os, i](const auto& value) {
            if constexpr (std::is_same_v<std::decay_t<decltype(value)>, std::unique_ptr<Function>>) {
                printContent(os, *value, std::string("Pool(") + std::to_string(i) + ")");
            }
        }, constant.value);
    }
}

inline void print(std::ostream& os, const Function& fn) {

    os << "digraph {\n";
    os << "  node [shape=record fontname=\"consolas\"];\n";
    os << "  edge [fontname=\"consolas\"];\n";
    printContent(os, fn);
    os << "}\n";
}


}  // namespace jac::cfg::dotprint
