#pragma once


#include <cstddef>
#include <iomanip>
#include <limits>
#include <locale>
#include <ostream>
#include <set>
#include <sstream>
#include <string_view>
#include <variant>

#include "tlessCfg.h"
#include "tlessOpInfo.h"


namespace jac::cfg::tless::dotprint {


inline void print(std::ostream& os, Opcode op) {
    os << opInfo(op).name;
}


constexpr std::string_view name(Tag tag) {
    switch (tag) {
        case Tag::Int: return "Int";
        case Tag::Float64: return "Float64";
        case Tag::Bool: return "Bool";
        case Tag::Undefined: return "Undefined";
        case Tag::Null: return "Null";
        case Tag::String: return "String";
        case Tag::Object: return "Object";
        case Tag::Symbol: return "Symbol";
        case Tag::BigInt: return "BigInt";
        case Tag::Exception: return "Exception";
        case Tag::Uninitialized: return "Uninitialized";
        case Tag::Closure: return "Closure";
        case Tag::Other: return "Other";
    }
    return "InvalidTag";
}

inline void print(std::ostream& os, Tag tag) {
    os << name(tag);
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
        print(os, r);
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

inline void printRecordText(std::ostream& os, std::string_view text) {
    for (const char ch : text) {
        const auto byte = static_cast<unsigned char>(ch);
        switch (ch) {
            case '\\': os << "\\\\"; break;
            case '"': os << "\\\""; break;
            case '{':
            case '}':
            case '|':
            case '<':
            case '>':
            case '\'': os << '\\' << ch; break;
            case '\n': os << "\\n"; break;
            case '\r': os << "\\r"; break;
            case '\t': os << "\\t"; break;
            default:
                if (byte < 0x20 || byte == 0x7F) {
                    constexpr std::string_view HEX = "0123456789ABCDEF";
                    os << "\\\\x" << HEX[(byte >> 4) & 0x0F] << HEX[byte & 0x0F];
                }
                else {
                    os << ch;
                }
                break;
        }
    }
}

inline void printF64(std::ostream& os, double value) {
    std::ostringstream valueStream;
    valueStream.imbue(std::locale::classic());
    valueStream << std::setprecision(std::numeric_limits<double>::max_digits10) << value;
    os << valueStream.str();
}

inline void print(std::ostream& os, const ConstInit& init) {
    print(os, init.reg);
    os << " ← const ";
    std::visit([&os](const auto& value) {
        if constexpr (std::is_same_v<std::decay_t<decltype(value)>, std::string>) {
            os << "'";
            printRecordText(os, value);
            os << "'";
        }
        else if constexpr (std::is_same_v<std::decay_t<decltype(value)>, bool>) {
            os << (value ? "True" : "False");
        }
        else if constexpr (std::is_same_v<std::decay_t<decltype(value)>, PoolConst>) {
            os << "Pool(" << value.id << ")";
        }
        else if constexpr (std::is_same_v<std::decay_t<decltype(value)>, RawI32Const>) {
            os << "RawI32(" << value.v << ")";
        }
        else if constexpr (std::is_same_v<std::decay_t<decltype(value)>, RawF64Const>) {
            os << "RawF64(";
            printF64(os, value.v);
            os << ")";
        }
        else if constexpr (std::is_same_v<std::decay_t<decltype(value)>, RawBoolConst>) {
            os << "RawBool(" << (value.v ? "True" : "False") << ")";
        }
        else if constexpr (std::is_same_v<std::decay_t<decltype(value)>, RawTagConst>) {
            os << "RawTag(";
            print(os, value.v);
            os << ")";
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
            os << "\\<\\<none\\>\\>";
            break;
        case Terminator::Branch:
            printTermLabel();
            os << "if (";
            print(os, block.terminator.value);
            os << ") ";
            print(os, block.terminator.args, true);
            break;
        case Terminator::Jump:
            printTermLabel();
            os << "jump ";
            print(os, block.terminator.args, true);
            break;
        case Terminator::Exit:
            printTermLabel();
            os << "exit ";
            print(os, block.terminator.args, true);
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
        case Terminator::Exit:
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
    std::string label;
    if (title) {
        label = *title + " ";
    }
    label += "isAsync=" + std::string(fn.isAsync ? "true" : "false");

    std::set<const BasicBlock*> seen;
    print(os, *fn.entry, seen, true, label);
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
