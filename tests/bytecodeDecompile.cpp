#include <jac/machine/compiler/qjsBcOpcodes.h>

#include <cstddef>
#include <filesystem>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <map>
#include <span>
#include <sstream>
#include <string_view>
#include <vector>


struct BCParserState {
    std::span<const uint8_t> data;
    std::span<const uint8_t>::iterator pos;
    std::ostream& output;
    std::vector<std::string> atoms;
};


void parseByTag(BCParserState& state, int indent);
void parseTypedArray(BCParserState& state, int indent);
void parseModule(BCParserState& state, int indent);
void parseFunctionBytecode(BCParserState& state, int indent);


const std::map<int, std::tuple<std::string, void(*)(BCParserState&, int)>> BCTagNames = {
    { BC_TAG_NULL, { "null", nullptr } },
    { BC_TAG_UNDEFINED, { "undefined", nullptr } },
    { BC_TAG_BOOL_FALSE, { "false", nullptr } },
    { BC_TAG_BOOL_TRUE, { "true", nullptr } },
    { BC_TAG_INT32, { "int32", nullptr } },
    { BC_TAG_FLOAT64, { "float64", nullptr } },
    { BC_TAG_STRING, { "string", nullptr } },
    { BC_TAG_OBJECT, { "object", nullptr } },
    { BC_TAG_ARRAY, { "array", nullptr } },
    { BC_TAG_BIG_INT, { "bigint", nullptr } },
    { BC_TAG_TEMPLATE_OBJECT, { "template_object", nullptr } },
    { BC_TAG_FUNCTION_BYTECODE, { "function_bytecode", parseFunctionBytecode } },
    { BC_TAG_MODULE, { "module", parseModule } },
    { BC_TAG_TYPED_ARRAY, { "typed_array", parseTypedArray } },
    { BC_TAG_ARRAY_BUFFER, { "array_buffer", nullptr } },
    { BC_TAG_SHARED_ARRAY_BUFFER, { "shared_array_buffer", nullptr } },
    { BC_TAG_DATE, { "date", nullptr } },
    { BC_TAG_OBJECT_VALUE, { "object_value", nullptr } },
    { BC_TAG_OBJECT_REFERENCE, { "object_reference", nullptr } },
};


uint8_t readByte(BCParserState& state) {
    if (state.pos >= state.data.end()) {
        throw std::runtime_error("Unexpected end of data");
    }
    return *state.pos++;
}

uint32_t readLeb128(BCParserState& state) {
    uint32_t v, a, i;
    v = 0;
    for(i = 0; i < 5; i++) {
        a = readByte(state);
        v |= (a & 0x7f) << (i * 7);
        if (!(a & 0x80)) {
            return v;
        }
    }
    throw std::runtime_error("Invalid LEB128 encoding");
}

// returns { index, isInt }
std::pair<uint32_t, bool> readAtomIndex(BCParserState& state) {
    uint32_t v = readLeb128(state);
    if (v & 1) {
        return { v >> 1, true };
    }
    v >>= 1;
    return { v, false };
}

void parseAtoms(BCParserState& state, int indent) {
    state.output << std::string(indent, ' ') << "<atom_list>" << std::endl;
    indent += 2;
    int version = readByte(state);
    state.output << std::string(indent, ' ') << "Version: " << version << std::endl;
    int count = readLeb128(state);
    state.output << std::string(indent, ' ') << "Count: " << count << std::endl;

    for (int i = 0; i < count; i++) {
        size_t len = readLeb128(state);
        bool isWideChar = len & 1;
        len >>= 1;
        int size = len << isWideChar;
        if (state.pos + size > state.data.end()) {
            throw std::runtime_error("Unexpected end of data while reading atom");
        }
        std::string atom(reinterpret_cast<const char*>(state.pos.base()), size);  // NOLINT
        state.pos += size;
        state.output << std::string(indent, ' ') << "Atom[" << i << "]: " << atom << std::endl;
        state.atoms.push_back(std::move(atom));
    }

    state.output << std::string(indent - 2, ' ') << "</atom_list>" << std::endl;
}

void parseByTag(BCParserState& state, int indent) {
    uint8_t tag = readByte(state);
    auto& [id, func] = BCTagNames.at(tag);
    state.output << std::string(indent, ' ') << "<" << id << ">";
    if (func) {
        state.output << std::endl;
        func(state, indent + 2);
        state.output << std::string(indent, ' ') << "</" << id << ">" << std::endl;
    }
    else {
        state.output << " " << std::string(indent, ' ') << "</" << id << ">" << std::endl;
    }
}

std::string atomToString(BCParserState& state, std::pair<uint32_t, bool> atom) {
    auto [ val, isInt ] = atom;
    if (isInt) {
        return std::to_string(val);
    }
    if (val < 223) {
        return "<builtin atom " + std::to_string(val) + ">";
    }
    return state.atoms.at(val - 223);
}

void parseModule(BCParserState& state, int indent) {
    auto printField = [&](const std::string& name, const std::string& value) {
        state.output << std::string(indent, ' ') << name << ": " << value << std::endl;
    };

    auto moduleName = readAtomIndex(state);
    printField("Name", atomToString(state, moduleName));

    uint32_t reqModuleCount = readLeb128(state);
    printField("Required modules", std::to_string(reqModuleCount));
    if (reqModuleCount > 0) {
        throw std::runtime_error("Required modules not supported in this parser");
    }

    uint32_t exportCount = readLeb128(state);
    printField("Exports", std::to_string(exportCount));
    if (exportCount > 0) {
        throw std::runtime_error("Exports not supported in this parser");
    }

    uint32_t starExportCount = readLeb128(state);
    printField("Star exports", std::to_string(starExportCount));
    if (starExportCount > 0) {
        throw std::runtime_error("Star exports not supported in this parser");
    }

    uint32_t importCount = readLeb128(state);
    printField("Imports", std::to_string(importCount));
    if (importCount > 0) {
        throw std::runtime_error("Imports not supported in this parser");
    }

    uint8_t hasTla = readByte(state);
    printField("Has TLA", hasTla ? "true" : "false");

    parseByTag(state, indent);
}

void parseBytecode(BCParserState& state, int indent, size_t len) {
    auto initial = state.pos;
    while (state.pos < initial + len) {
        auto offset = static_cast<size_t>(state.pos - initial);
        uint8_t op = *state.pos;
        int size = short_opcode_info(op).size;
        std::ostringstream address;
        address << std::setw(4) << std::setfill('0') << offset;

        state.output << std::string(indent, ' ') << address.str() << ": "
                     << opcodeNames[static_cast<int>(op)] << " ";
        for (int i = 1; i < size; i++) {
            state.output << " " << static_cast<int>(*(state.pos + i));
        }
        state.output << std::endl;
        state.pos += size;
    }
}

void parseFunctionBytecode(BCParserState& state, int indent) {
    uint16_t flags = readByte(state);
    flags |= readByte(state) << 8;
    uint8_t jsMode = readByte(state);
    auto nameAtom = readAtomIndex(state);

    uint32_t arg_count = readLeb128(state);
    uint32_t var_count = readLeb128(state);
    uint32_t defined_arg_count = readLeb128(state);
    uint32_t stack_size = readLeb128(state);
    uint32_t closure_var_count = readLeb128(state);
    uint32_t cpool_count = readLeb128(state);
    uint32_t byte_code_len = readLeb128(state);
    uint32_t local_count = readLeb128(state);

    state.output << std::string(indent, ' ') << "Flags: " << flags << std::endl;
    state.output << std::string(indent, ' ') << "JS Mode: " << static_cast<int>(jsMode) << std::endl;
    state.output << std::string(indent, ' ') << "Name: " << atomToString(state, nameAtom) << std::endl;
    state.output << std::string(indent, ' ') << "Args: " << arg_count << std::endl;
    state.output << std::string(indent, ' ') << "Vars: " << var_count << std::endl;
    state.output << std::string(indent, ' ') << "Defargs: " << defined_arg_count << std::endl;
    state.output << std::string(indent, ' ') << "Stack: " << stack_size << std::endl;
    state.output << std::string(indent, ' ') << "Closures: " << closure_var_count << std::endl;
    state.output << std::string(indent, ' ') << "Cpool: " << cpool_count << std::endl;
    state.output << std::string(indent, ' ') << "Byte code len: " << byte_code_len << std::endl;

    state.output << std::string(indent, ' ') << "Locals: " << local_count << std::endl;
    for (uint32_t i = 0; i < local_count; i++) {
        auto name = readAtomIndex(state);
        uint32_t scopeLevel = readLeb128(state);
        uint32_t scopeNext = readLeb128(state);
        uint8_t flags_ = readByte(state);
        state.output << std::string(indent + 2, ' ');
        if (i < arg_count) {
            state.output << "Arg[" << i;
        }
        else {
            state.output << "Local[" << (i - arg_count);
        }
        state.output << "] Name: " << atomToString(state, name)
                  << " ScopeLevel: " << scopeLevel << " ScopeNext: " << (static_cast<int>(scopeNext) - 1)
                  << " Flags: " << static_cast<int>(flags_) << std::endl;
    }

    state.output << std::string(indent, ' ') << "Closures: " << closure_var_count << std::endl;
    for (uint32_t i = 0; i < closure_var_count; i++) {
        auto name = readAtomIndex(state);
        uint32_t var_idx = readLeb128(state);
        uint8_t flags_ = readByte(state);
        state.output << std::string(indent + 2, ' ') << "Local[" << i << "] Name: " << atomToString(state, name)
                  << " VarIdx: " << var_idx << " Flags: " << static_cast<int>(flags_) << std::endl;
    }

    state.output << std::string(indent, ' ') << "<bytecode>" << std::endl;
    parseBytecode(state, indent + 2, byte_code_len);
    state.output << std::string(indent, ' ') << "</bytecode>" << std::endl;

    auto filenameAtom = readAtomIndex(state);
    state.output << std::string(indent, ' ') << "Filename: " << atomToString(state, filenameAtom) << std::endl;
    uint32_t pc2line_len = readLeb128(state);
    state.output << std::string(indent, ' ') << "PC2Line len: " << pc2line_len << std::endl;
    state.pos += pc2line_len;

    uint32_t src_len = readLeb128(state);
    state.output << std::string(indent, ' ') << "Source len: " << src_len << std::endl;
    state.pos += src_len;

    state.output << std::string(indent, ' ') << "<cpool>" << std::endl;
    for (uint32_t i = 0; i < cpool_count; i++) {
        parseByTag(state, indent + 2);
    }
    state.output << std::string(indent, ' ') << "</cpool>" << std::endl;
}


void parseTypedArray(BCParserState& state, int indent) {
    int kind = readByte(state);
    int len = readLeb128(state);
    int offset = readLeb128(state);
    state.output << std::string(indent, ' ') << "Kind: " << kind << std::endl;
    state.output << std::string(indent, ' ') << "Length: " << len << std::endl;
    state.output << std::string(indent, ' ') << "Offset: " << offset << std::endl;
    state.output << std::string(indent, ' ') << "Data: ";
    abort();
}


int main(const int argc, const char* argv[]) {
    // --path <file> --out <file>
    std::string path;
    std::string out;
    std::vector<std::pair<std::string_view, std::string_view>> defines;

    for (int i = 1; i < argc; ++i) {
        std::string_view arg(argv[i]);

        if (arg == "--path") {
            if (i + 1 >= argc) {
                std::cerr << "Missing argument for --path" << std::endl;
                return 1;
            }
            path = argv[++i];
        }
        else if (arg == "--out") {
            if (i + 1 >= argc) {
                std::cerr << "Missing argument for --out" << std::endl;
                return 1;
            }
            out = argv[++i];
        }
        else {
            std::cerr << "Unknown argument: " << arg << std::endl;
            return 1;
        }
    }

    if (path.empty()) {
        std::cerr << "Path is required" << std::endl;
        return 1;
    }

    std::vector<uint8_t> bc;
    {
        if (!std::filesystem::exists(path)) {
            std::cerr << "File does not exist: " << path << std::endl;
            return 1;
        }
        if (!std::filesystem::is_regular_file(path)) {
            std::cerr << "Not a file: " << path << std::endl;
            return 1;
        }
        std::ifstream file(path);
        if (!file || !file.is_open()) {
            std::cerr << "Failed to open file: " << path << std::endl;
            return 1;
        }

        while (file) {
            std::array<char, 4096> buffer;
            file.read(buffer.data(), buffer.size());
            auto read = file.gcount();
            if (read > 0) {
                bc.insert(bc.end(), buffer.data(), buffer.data() + read);
            }
        }
    }
    std::cout << "Read " << bc.size() << " bytes from " << path << std::endl;

    std::stringstream ss;

    std::span<const uint8_t> inSpan(bc);
    BCParserState state {
        .data = inSpan,
        .pos = inSpan.begin(),
        .output = out.empty() ? std::cout : ss
    };

    parseAtoms(state, 0);
    while (state.pos < state.data.end()) {
        parseByTag(state, 0);
        state.output << std::endl;
    }

    if (!out.empty()) {
        std::ofstream outFile(out);
        if (!outFile || !outFile.is_open()) {
            std::cerr << "Failed to open output file: " << out << std::endl;
            return 1;
        }
        outFile << ss.str();
    }
}
