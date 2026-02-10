#pragma once


#include <iomanip>
#include <ostream>
#include <set>
#include <variant>

#include "cfg.h"
#include "opcode.h"


namespace jac::cfg::dotprint {


inline void print(std::ostream& os, Opcode op) {
    switch (op) {
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
        case Opcode::Set: os << "Set"; break;
        case Opcode::BoolNot: os << "BoolNot"; break;
        case Opcode::BitNot: os << "BitNot"; break;
        case Opcode::UnPlus: os << "UnPlus"; break;
        case Opcode::UnMinus: os << "UnMinus"; break;
        case Opcode::Dup: os << "Dup"; break;
        case Opcode::PushUnref: os << "PushFree"; break;
        case Opcode::SetMember: os << "SetMember"; break;
        }
}

inline void printShort(std::ostream& os, ValueType type) {
    switch (type) {
        case ValueType::Void: os << "v"; break;
        case ValueType::I32: os << "i32"; break;
        case ValueType::F64: os << "f64"; break;
        case ValueType::Bool: os << "b"; break;
        case ValueType::Object: os << "o"; break;
        case ValueType::StringConst: os << "sc"; break;
        case ValueType::Any: os << "a"; break;
    }
}


inline void print(std::ostream& os, const Reg& v) {
    os << "_" << v.id << ":";
    printShort(os, v.type);
}

inline void print(std::ostream& os, const LVRef& v) {
    os << "_" << v.self.id << ":";
    printShort(os, v.self.type);
}


inline void print(std::ostream& os, const Operation& op) {
    if (hasResult(op.op)) {
        print(os, op.res);
        os << " ← ";
    }
    print(os, op.op);
    os << " ";
    print(os, op.a);
    if (isBinary(op.op)) {
        os << " ";
        print(os, op.b);
    }
    os << "";
}

inline void print(std::ostream& os, const ConstInit& init) {
    os << "_" << init.id << " ← const ";
    std::visit([&os](const auto& value) {
        if constexpr (std::is_same_v<std::decay_t<decltype(value)>, std::string>) {
            os << std::quoted(value, '\'');
        }
        else if constexpr (std::is_same_v<std::decay_t<decltype(value)>, bool>) {
            os << (value ? "True" : "False");
        }
        else {
            os << value;
        }
    }, init.value);
    os << "";
}

inline void print(std::ostream& os, const Call& call) {
    print(os, call.res);
    if (call.isConstructor) {
        os << " ← ctor ";
    }
    else {
        os << " ← call ";
    }
    struct visitor {
        std::ostream& os;
        void operator()(const Identifier& id) const {
            os << id;
        }
        void operator()(const Reg& rvalue) const {
            os << "_" << rvalue.id;
        }
    };
    std::visit(visitor{os}, call.obj);
    os << "(";

    bool first = true;
    for (const auto& arg : call.args) {
        if (!first) {
            os << " ";
        }
        os << "_" << arg.id;
        first = false;
    }
    os << ")";
}


inline void print(std::ostream& os, const Instruction& instruction) {
    std::visit([&os](const auto& op) {
        print(os, op);
    }, instruction.op);
}


inline void print(std::ostream& os, const BasicBlock& block, std::set<const BasicBlock*>& seen, bool isEntry = false) {
    if (seen.contains(&block)) {
        return;
    }
    seen.insert(&block);
    os << "  block" << &block << " [label=\"{";
    if (isEntry) {
        os << "*";
    }
    os << &block;
    if (!block.instructions.empty()) {
        os << "|";
    }

    for (const auto& instruction : block.instructions) {
        print(os, instruction);
        os << "\\l";
    }
    switch (block.jump.type) {
        case Terminator::None:
            os << "|<<none>>";
            break;
        case Terminator::Branch:
            os << "|if (_" << block.jump.value.id << ")";
            break;
        case Terminator::Jump:
            break;
        case Terminator::Return:
            os << "|return";
            if (block.jump.value.type != ValueType::Void) {
                os << " _" << block.jump.value.id;
            }
            break;
        case Terminator::Throw:
            os << "|throw _" << block.jump.value.id;
            break;
    }
    os << "}\"];\n";

    switch (block.jump.type) {
        case Terminator::None:
            break;
        case Terminator::Branch:
            os << "  block" << &block << " -> block" << block.jump.target << " [label=\"true\"];\n";
            os << "  block" << &block << " -> block" << block.jump.other << " [label=\"false\"];\n";
            break;
        case Terminator::Jump:
            os << "  block" << &block << " -> block" << block.jump.target << ";\n";
            break;
        case Terminator::Return:
            break;
        case Terminator::Throw:
            break;
    }
}

inline void print(std::ostream& os, const Function& fn) {
    std::set<const BasicBlock*> seen;

    os << "digraph {\n";
    os << "  node [shape=record fontname=\"consolas\"];\n";
    os << "  edge [fontname=\"consolas\"];\n";
    print(os, *fn.entry, seen, true);
    for (auto& block : fn.blocks) {
        print(os, *block, seen);
    }
    os << "}\n";
}


}  // namespace jac::cfg::dotprint
