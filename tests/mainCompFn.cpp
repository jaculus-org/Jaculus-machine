#include <filesystem>
#include <fstream>
#include <iostream>
#include <string>

#include <jac/machine/compiler/ast.h>
#include <jac/machine/compiler/ast2cfg.h>
#include <jac/machine/compiler/astPrint.h>
#include <jac/machine/compiler/cfg.h>
#include <jac/machine/compiler/cfgDot.h>
#include <jac/machine/compiler/cfgUtil.h>


int main(const int argc, const char* argv[]) {
    // --path <file>

    std::string path;
    std::string mode;

    for (int i = 1; i < argc; ++i) {
        if (std::string_view(argv[i]) == "--path") {
            if (i + 1 >= argc) {
                std::cerr << "Missing argument for --path" << std::endl;
                return 1;
            }
            path = argv[++i];
        }
        else if(std::string_view(argv[i]) == "--mode") {
            if (i + 1 >= argc) {
                std::cerr << "Missing argument for --mode" << std::endl;
                return 1;
            }
            mode = argv[++i];
        }
        else if (std::string_view(argv[i]) == "--help") {
            std::cerr << "Usage: jac --path <file> --mode <cfg|ast>" << std::endl;
            return 0;
        }
        else {
            std::cerr << "Unknown argument: " << argv[i] << std::endl;
            return 1;
        }
    }

    if (mode != "cfg" && mode != "ast") {
        std::cerr << "Invalid mode: " << mode << std::endl;
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

    auto tokens = scanner.scan();

    jac::ast::ParserState state(tokens);

    auto fun = jac::ast::parseFunctionDeclaration(state, false);
    if (!fun || !state.isEnd()) {
        jac::lex::Token errorToken = state.getErrorToken();
        std::cerr << "Parse error: " << state.getErrorMessage()
                    << " at " << errorToken.line << ":" << errorToken.column << '\n';
        throw std::runtime_error("Parse error");
    }

    if (mode == "ast") {
        jac::ast::printAST(*fun, std::cout, 0);  // TODO: improve printer
        return 0;
    }

    auto sig = jac::cfg::getSignature(*fun);
    auto cfg = jac::cfg::ast2cfg(*fun, sig, {});

    auto fn = cfg.output();
    jac::cfg::removeEmptyBlocks(fn);
    jac::cfg::dotprint::print(std::cout, fn);

    return 0;
}
