#include <iostream>
#include <string>
#include <fstream>
#include <filesystem>

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

#include <jac/machine/compiler/ast.h>
#include <jac/machine/compiler/astPrint.h>
#include <jac/machine/compiler/cfg2bc.h>
#include <jac/machine/compiler/bcWriter.h>
#include <jac/machine/compiler/scanner.h>
#include <jac/machine/compiler/tlessAst2cfg.h>
#include <jac/machine/compiler/tlessCfg.h>
#include <jac/machine/compiler/tlessCfgDot.h>
#include <jac/machine/compiler/tlessCfgUtil.h>

#include "quickjs.h"
#include "test262/harness.h"

namespace fs = std::filesystem;


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
    Test262HarnessFeature,
    jac::EventLoopTerminal
>;


namespace {

fs::path dumpPath(const std::string& inputPath,
                  const std::string& dir,
                  const std::string& suffix) {
    fs::path outDir(dir);
    fs::path in(inputPath);
    return outDir / (in.stem().string() + suffix);
}

void dumpAST(const jac::ast::ASTNode& node, const std::string& inputPath,
             const DumpConfig& dump) {
    if (!dump.ast) {
        return;
    }
    auto path = dumpPath(inputPath, dump.dir, ".ast.json");
    std::ofstream file(path);
    if (!file) {
        std::cerr << "Failed to open " << path << " for AST dump\n";
        return;
    }
    jac::ast::printASTJson(node, file);
    if (!file) {
        std::cerr << "Failed to write " << path << " for AST dump\n";
    }
}

void dumpCFG(const jac::cfg::tless::Function& cfgFunc, const std::string& inputPath,
             const DumpConfig& dump) {
    if (!dump.cfg) {
        return;
    }
    auto path = dumpPath(inputPath, dump.dir, ".cfg.dot");
    std::ofstream file(path);
    if (!file) {
        std::cerr << "Failed to open " << path << " for CFG dump\n";
        return;
    }
    jac::cfg::tless::dotprint::print(file, cfgFunc);
    if (!file) {
        std::cerr << "Failed to write " << path << " for CFG dump\n";
    }
}

template<typename AST>
std::vector<uint8_t> compileAndDump(const AST& node, const std::string& path,
                                    bool isScript, const DumpConfig& dump) {
    dumpAST(node, path, dump);

    auto cfgEm = jac::cfg::tless::ast2cfg(node);
    auto cfgFunc = cfgEm.output();
    jac::cfg::tless::removeUnreachableBlocks(cfgFunc);

    dumpCFG(cfgFunc, path, dump);

    BytecodeRoot root;
    if (isScript) {
        jac::bc::cfg2bc(root, cfgFunc, path, jac::bc::CompileMode::Script);
    } else {
        jac::bc::cfg2bc(root, cfgFunc, path);
    }

    std::vector<uint8_t> bcData;
    root.write(bcData);

    dumpBytecode(bcData, path, dump);

    return bcData;
}

template<typename ParseFn, typename HoistFn>
std::vector<uint8_t> compileSource(const std::string& code, const std::string& path,
                                   ParseFn parse, HoistFn hoist,
                                   bool isScript, const DumpConfig& dump,
                                   std::string& outError, std::string& outErrorConstructor) {
    bool hadError = false;
    std::vector<std::string> reports;
    jac::lex::Scanner scanner(code, [&hadError, &reports](int line, int col, const std::string& msg) {
        hadError = true;
        reports.push_back("Lex error: " + msg + " at " + std::to_string(line) + ":" + std::to_string(col));
    });

    auto tokens = scanner.scan();

    if (hadError) {
        for (const auto& report : reports) {
            std::cerr << report << '\n';
        }
        outError = "Lex error";
        outErrorConstructor = "SyntaxError";
        return {};
    }

    jac::ast::ParserState state(tokens);
    auto node = parse(state);
    if (!node || !state.isEnd()) {
        jac::lex::Token errorToken = state.getErrorToken();
        outError = std::string(state.getErrorMessage()) + " at " + std::to_string(errorToken.line) + ":" + std::to_string(errorToken.column);
        outErrorConstructor = "SyntaxError";
        return {};
    }

    hoist(*node);

    return compileAndDump(*node, path, isScript, dump);
}

} // namespace


Test262Result runFile(const std::string& path, bool isModule,
                      const std::string& harnessDir, const DumpConfig& dump) {
    Machine machine;
    jac::initializeIo(machine);
    machine.setHarnessDir(harnessDir);
    if (isModule) {
        machine.setCodeDir(fs::path(path).parent_path().string());
    }
    machine.initialize();

    try {
        machine.loadHarness();

        if (dump.ast || dump.cfg || dump.bc) {
            std::error_code ec;
            std::filesystem::create_directories(dump.dir, ec);
            if (ec) {
                std::cerr << "Failed to create dump directory " << dump.dir
                          << ": " << ec.message() << '\n';
            }
        }

        auto code = loadFile(path);

        std::string parseError;
        std::string parseErrorConstructor;
        auto bcData = isModule
            ? compileSource(code, path, jac::ast::parseModule, jac::ast::hoistModule, false, dump, parseError, parseErrorConstructor)
            : compileSource(code, path, jac::ast::parseScript, jac::ast::hoistScript, true, dump, parseError, parseErrorConstructor);

        if (bcData.empty()) {
            return makeResult("fail", "parse", parseError, parseErrorConstructor);
        }

        auto res = JS_ReadObject(machine.context(), bcData.data(), bcData.size(), JS_READ_OBJ_BYTECODE);
        auto evalRes = JS_EvalFunction(machine.context(), res);
        if (JS_IsException(evalRes)) {
            jac::ContextRef ctx(machine.context());
            jac::Exception ex = ctx.getException();
            JS_FreeValue(machine.context(), evalRes);
            throw ex;
        }
        JS_FreeValue(machine.context(), evalRes);

        drainJobs(machine);

        if (machine.isAsyncDone()) {
            return machine.getAsyncResult();
        }
        return makeResult("pass", "runtime");
    }
    catch (jac::Exception& e) {
        return classifyException(e);
    }
    catch (std::exception& e) {
        return makeResult("fail", "runtime", e.what(), "Error");
    }
}


int main(const int argc, const char* argv[]) {
    return runnerMain(argc, argv, runFile);
}
