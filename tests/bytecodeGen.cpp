#include "jac/machine/compiler/ast.h"
#include "jac/machine/compiler/astPrint.h"
#include "jac/machine/compiler/tlessAst2cfg.h"
#include "jac/machine/compiler/tlessCfg.h"
#include "jac/machine/compiler/tlessCfgDot.h"
#include "quickjs.h"
#include <cstddef>
#include <iostream>

#include <jac/features/basicStreamFeature.h>
#include <jac/features/evalFeature.h>
#include <jac/features/eventLoopFeature.h>
#include <jac/features/eventQueueFeature.h>
#include <jac/features/filesystemFeature.h>
#include <jac/features/moduleLoaderFeature.h>
#include <jac/features/stdioFeature.h>
#include <jac/features/timersFeature.h>
#include <jac/features/util/ostreamjs.h>
#include <jac/machine/class.h>
#include <jac/machine/machine.h>
#include <jac/machine/values.h>


using Machine = jac::ComposeMachine<
    jac::MachineBase,
    jac::EvalFeature,
    jac::EventQueueFeature,
    jac::BasicStreamFeature,
    jac::StdioFeature,
    jac::EventLoopFeature,
    jac::FilesystemFeature,
    jac::ModuleLoaderFeature,
    jac::TimersFeature,
    jac::EventLoopTerminal
>;


int main(const int argc, const char* argv[]) {
    // --path <file> --out <file>

    std::string path;
    std::string out;
    std::string astOut = "-";
    int mode = 0;
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
        else if (arg == "--ast-out") {
            if (i + 1 >= argc) {
                std::cerr << "Missing argument for --ast-out" << std::endl;
                return 1;
            }
            astOut = argv[++i];
        }
        else if (arg == "--alt") {
            assert(mode == 0);
            mode = 1;
        }
        else if (arg == "--alt-cfg") {
            assert(mode == 0);
            mode = 2;
        }
        else {
            std::cerr << "Unknown argument: " << arg << std::endl;
            return 1;
        }
    }

    auto getTokens = [&](const std::string& code) {
        bool hadError = false;
        std::vector<std::string> reports;
        jac::lex::Scanner scanner(code, [&hadError, &reports](int line, int col, const std::string& msg) {
            hadError = true;
            reports.push_back("Lex error: " + msg + " at " + std::to_string(line) + ":" + std::to_string(col));
        });

        if (hadError) {
            for (const auto& report : reports) {
                std::cerr << report << '\n';
            }
            throw std::runtime_error("Lex error");
        }

        return scanner.scan();
    };

    auto printAst = [&](const jac::ast::ASTNode& node) {
        if (astOut == "-") {
            jac::ast::printAST(node, std::cout, 0);
        }
        else {
            std::ofstream astFile(astOut);
            if (!astFile || !astFile.is_open()) {
                std::cerr << "Failed to open AST output file" << std::endl;
                return;
            }
            jac::ast::printAST(node, astFile, 0);
        }
    };

    if (path.empty()) {
        std::cerr << "Path is required" << std::endl;
        return 1;
    }
    if (out.empty()) {
        std::cerr << "Out is required" << std::endl;
        return 1;
    }

    std::string code;
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
            std::string line;
            std::getline(file, line);
            code += line + '\n';
        }
    }

    Machine machine;
    try {
        if (mode == 0) {
            initializeIo(machine);
            machine.initialize();

            auto val = machine.eval(code, "main.js", jac::EvalFlags::Module | jac::EvalFlags::CompileOnly);
            size_t size;
            uint8_t* data = JS_WriteObject(machine.context(), &size, val.getVal(), JS_WRITE_OBJ_BYTECODE);

            JS_FreeValue(machine.context(), JS_ReadObject(machine.context(), data, size, JS_READ_OBJ_BYTECODE));

            std::ofstream outFile(out, std::ios::binary);
            if (!outFile || !outFile.is_open()) {
                std::cerr << "Failed to open output file" << std::endl;
                return 1;
            }

            outFile.write(reinterpret_cast<const char*>(data), size);  // NOLINT
            js_free(machine.context(), data);
        }
        else if (mode == 1) {
            abort();
        }
        else if (mode == 2) {
            auto tokens = getTokens(code);

            jac::ast::ParserState state(tokens);

            auto mod = jac::ast::parseModule(state);
            if (!mod || !state.isEnd()) {
                jac::lex::Token errorToken = state.getErrorToken();
                std::cerr << "Parse error: " << state.getErrorMessage()
                        << " at " << errorToken.line << ":" << errorToken.column << '\n';
                throw std::runtime_error("Parse error");
            }

            jac::ast::hoistModule(*mod);
            printAst(*mod);

            auto cfgFuncEm = jac::cfg::tless::ast2cfg(*mod);

            auto cfgFunc = cfgFuncEm.output();
            std::fstream outFile("cfg.dot", std::ios::out | std::ios::trunc);
            jac::cfg::tless::dotprint::print(outFile, cfgFunc);
        }
    } catch (jac::Exception& e) {
        std::cout << "Exception: " << e.what() << std::endl;
        std::cout << "Stack: " << e.stackTrace() << std::endl;
        exit(1);
    }
}
