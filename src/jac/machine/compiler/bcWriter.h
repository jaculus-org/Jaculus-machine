#pragma once

#include <cstdint>
#include <memory>
#include <string>
#include <tuple>
#include <vector>

#include "qjsBcOpcodes.h"


inline void writeByte(std::vector<uint8_t>& buffer, uint8_t byte) {
    buffer.push_back(byte);
}

template<int Width>
inline void writeInt(std::vector<uint8_t>& buffer, auto value) {
    auto u = static_cast<std::make_unsigned_t<decltype(value)>>(value);
    for (int i = 0; i < Width; i++) {
        writeByte(buffer, static_cast<uint8_t>((u >> (i * 8)) & 0xFF));
    }
}

inline void writeLeb128(std::vector<uint8_t>& buffer, uint64_t value) {
    while (value >= 0x80) {
        writeByte(buffer, static_cast<uint8_t>(value | 0x80));
        value >>= 7;
    }
    writeByte(buffer, static_cast<uint8_t>(value));
}

inline void writeAtom(std::vector<uint8_t>& buffer, uint32_t atomIndex) {
    writeLeb128(buffer, atomIndex << 1);
}


struct BytecodeNode {
    virtual ~BytecodeNode() = default;
    virtual void write(std::vector<uint8_t>& out) = 0;
};


struct BytecodeRoot {
    std::vector<std::string> atoms;
    int8_t version = 4;
    std::unique_ptr<BytecodeNode> child;

    uint32_t addAtom(const std::string& atom) {
        for (uint32_t i = 0; i < atoms.size(); ++i) {
            if (atoms[i] == atom) {
                return i + 223;
            }
        }
        atoms.push_back(atom);
        return static_cast<uint32_t>(atoms.size() - 1) + 223;
    }

    void write(std::vector<uint8_t>& out) {
        writeByte(out, version);
        writeLeb128(out, static_cast<uint32_t>(atoms.size()));
        for (const auto& atom : atoms) {
            bool isWideChar = false;
            uint32_t len = atom.size();
            if (isWideChar) {
                len |= 1;
            }
            else {
                len <<= 1;
            }
            writeLeb128(out, len);
            for (char c : atom) {
                writeByte(out, static_cast<uint8_t>(c));
            }
        }

        if (child) {
            child->write(out);
        }
    }
};

struct FunctionBytecode : public BytecodeNode {
    FunctionBytecode(uint32_t nameAtom, uint32_t filenameAtom):
        _nameAtom(nameAtom),
        _filenameAtom(filenameAtom)
    {}
    FunctionBytecode(FunctionBytecode&&) = delete;
    FunctionBytecode& operator=(FunctionBytecode&&) = delete;
    FunctionBytecode(const FunctionBytecode&) = delete;
    FunctionBytecode& operator=(const FunctionBytecode&) = delete;

    std::vector<uint8_t> intro;
    std::vector<uint8_t> bytecode;
    std::vector<uint32_t> args;  /* atoms */
    std::vector<std::tuple<uint32_t /*atom*/, uint32_t /*scopeLevel*/, uint32_t /*scopeNext*/, uint8_t /*flags*/>> locals;
    std::vector<std::tuple<uint32_t /*nameAtom*/, uint32_t /*varIdx*/, uint8_t /*flags*/>> closures;
    std::vector<std::unique_ptr<BytecodeNode>> cpool;

    uint16_t flags = 0x643;
    uint8_t jsMode = 0;
    uint32_t argCount = 0;
    uint32_t varCount = 0;
    uint32_t definedArgCount = 0;
    uint32_t stackSize = 2;

    uint32_t _nameAtom;
    uint32_t _filenameAtom;

    std::vector<std::tuple<int /*id*/, int /*nextVar*/>> scopeStack{ {0, 0} };

    uint32_t addLocal(uint32_t atom, uint8_t flags_) {
        auto& [scopeLevel, scopeNext] = scopeStack.back();
        locals.emplace_back(atom, scopeLevel, scopeNext, flags_);
        scopeNext = static_cast<int>(locals.size());

        return static_cast<uint32_t>(locals.size() - 1);
    }

    void write(std::vector<uint8_t>& out) override {
        writeByte(out, BC_TAG_FUNCTION_BYTECODE);

        writeByte(out, static_cast<uint8_t>(flags & 0xFF));
        writeByte(out, static_cast<uint8_t>((flags >> 8) & 0xFF));
        writeByte(out, jsMode);
        writeAtom(out, _nameAtom);
        writeLeb128(out, argCount);
        writeLeb128(out, static_cast<uint32_t>(locals.size()));
        writeLeb128(out, definedArgCount);
        writeLeb128(out, stackSize);
        writeLeb128(out, static_cast<uint32_t>(closures.size()));
        writeLeb128(out, static_cast<uint32_t>(cpool.size()));
        writeLeb128(out, static_cast<uint32_t>(bytecode.size() + intro.size()));
        writeLeb128(out, static_cast<uint32_t>(locals.size()) + argCount);

        for (const auto& arg : args) {
            writeAtom(out, arg);  // atom
            writeLeb128(out, 0);  // scopeLevel
            writeLeb128(out, 1);  // scopeNext
            writeByte(out, 0);    // flags
        }

        for (const auto& local : locals) {
            writeAtom(out, std::get<0>(local));  // atom
            writeLeb128(out, std::get<1>(local));  // scopeLevel
            writeLeb128(out, std::get<2>(local));  // scopeNext
            writeByte(out, std::get<3>(local));    // flags
        }

        for (const auto& closure : closures) {
            writeAtom(out, std::get<0>(closure));  // nameAtom
            writeLeb128(out, std::get<1>(closure));  // varIdx
            writeByte(out, std::get<2>(closure));    // flags
        }

        out.insert(out.end(), intro.begin(), intro.end());
        out.insert(out.end(), bytecode.begin(), bytecode.end());
        writeAtom(out, _filenameAtom);
        writeLeb128(out, 0); // pc2line len
        writeLeb128(out, 0); // src len

        for (const auto& entry : cpool) {
            entry->write(out);
        }
    }

    auto pos() const {
        return bytecode.size();
    }
};
