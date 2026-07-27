#pragma once

#include <jac/features/evalFeature.h>
#include <jac/features/util/ostreamjs.h>
#include <jac/machine/functionFactory.h>
#include <jac/machine/machine.h>
#include <jac/machine/values.h>

#include <filesystem>
#include <fstream>
#include <iostream>
#include <string>
#include <string_view>

struct DumpConfig {
    bool ast = false;
    bool cfg = false;
    bool bc = false;
    std::string dir = ".";
};

void dumpBytecode(const std::vector<uint8_t>& bytecode,
                  const std::string& inputPath,
                  const DumpConfig& dump) {
    if (!dump.bc) {
        return;
    }

    std::error_code ec;
    std::filesystem::create_directories(dump.dir, ec);
    if (ec) {
        throw std::runtime_error("Failed to create dump directory " + dump.dir +
                                 ": " + ec.message());
    }

    auto path = std::filesystem::path(dump.dir) /
                (std::filesystem::path(inputPath).stem().string() + ".qbc");
    std::ofstream file(path, std::ios::binary);
    if (!file) {
        throw std::runtime_error("Failed to open " + path.string());
    }
    file.write(reinterpret_cast<const char*>(bytecode.data()), bytecode.size());
    if (!file) {
        throw std::runtime_error("Failed to write " + path.string());
    }
}

namespace {

struct Test262Result {
    std::string result;
    std::string phase;
    std::string error;
    std::string type;
    std::string stack;
};

std::string loadFile(const std::string& path) {
    std::ifstream file(path);
    if (!file.is_open()) {
        throw std::runtime_error("Failed to open file: " + path);
    }
    std::string content;
    std::string line;
    while (std::getline(file, line)) {
        content += line + '\n';
    }
    return content;
}

std::string loadHarnessFiles(const std::string& harnessDir, const std::vector<std::string>& names) {
    std::string result;
    for (const auto& name : names) {
        auto path = harnessDir + "/" + name;
        if (std::filesystem::exists(path)) {
            result += loadFile(path) + '\n';
        }
    }
    return result;
}

Test262Result makeResult(const std::string& result, const std::string& phase,
                        const std::string& error = "", const std::string& type = "") {
    return {
        .result = result,
        .phase = phase,
        .error = error,
        .type = type
    };
}

} // namespace

Test262Result classifyException(jac::Exception& e) {
    std::string type = "Error";
    std::string msg = e.what();
    std::string phase = "runtime";

    if (msg.find("SyntaxError") != std::string::npos) {
        phase = "parse";
        type = "SyntaxError";
    }
    else if (msg.find("TypeError") != std::string::npos) {
        type = "TypeError";
    }
    else if (msg.find("ReferenceError") != std::string::npos) {
        type = "ReferenceError";
    }
    else if (msg.find("RangeError") != std::string::npos) {
        type = "RangeError";
    }

    Test262Result r = makeResult("fail", phase, msg, type);
    try {
        r.stack = e.stackTrace();
    } catch (...) {
    }
    return r;
}

template<typename Machine>
void drainJobs(Machine& machine) {
    JSRuntime* rt = machine.runtime();
    JSContext* ctx;
    int err;
    while ((err = JS_ExecutePendingJob(rt, &ctx)) > 0) {
        machine.resetWatchdog();
    }
    if (err < 0) {
        throw jac::ContextRef(ctx).getException();
    }
}

template<typename Machine>
Test262Result runEvalFile(const std::string& path, bool isModule,
                          const std::string& harnessDir, const DumpConfig&) {
    Machine machine;
    jac::initializeIo(machine);
    machine.setHarnessDir(harnessDir);
    if (isModule) {
        machine.setCodeDir(std::filesystem::path(path).parent_path().string());
    }
    machine.initialize();

    try {
        machine.loadHarness();

        auto code = loadFile(path);
        auto filename = std::filesystem::path(path).filename().string();

        jac::EvalFlags flags = isModule ? jac::EvalFlags::Module : jac::EvalFlags::Global;
        machine.eval(code, filename, flags);

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

    return makeResult("fail", "runtime", "Unexpected end of function", "Error");
}

template<typename RunFn>
int runnerMain(int argc, const char* argv[], RunFn&& runFn) {
    std::string path;
    bool isModule = false;
    std::string harnessDir;
    bool jsonOutput = false;
    DumpConfig dumpCfg;

    for (int i = 1; i < argc; ++i) {
        std::string_view arg(argv[i]);

        if (arg == "--path") {
            if (i + 1 >= argc) {
                std::cerr << "Missing argument for --path" << std::endl;
                return 1;
            }
            path = argv[++i];
        }
        else if (arg == "--module") {
            isModule = true;
        }
        else if (arg == "--harness-dir") {
            if (i + 1 >= argc) {
                std::cerr << "Missing argument for --harness-dir" << std::endl;
                return 1;
            }
            harnessDir = argv[++i];
        }
        else if (arg == "--format") {
            if (i + 1 >= argc) {
                std::cerr << "Missing argument for --format" << std::endl;
                return 1;
            }
            auto format = std::string_view(argv[++i]);
            if (format == "json") {
                jsonOutput = true;
            }
        }
        else if (arg == "--dump-ast") {
            dumpCfg.ast = true;
        }
        else if (arg == "--dump-cfg") {
            dumpCfg.cfg = true;
        }
        else if (arg == "--dump-bc") {
            dumpCfg.bc = true;
        }
        else if (arg == "--dump-dir") {
            if (i + 1 >= argc) {
                std::cerr << "Missing argument for --dump-dir" << std::endl;
                return 1;
            }
            dumpCfg.dir = argv[++i];
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

    auto result = runFn(path, isModule, harnessDir, dumpCfg);

    if (jsonOutput) {
        std::cout << "{\"result\":\"" << result.result
                  << "\",\"phase\":\"" << result.phase
                  << "\",\"error\":\"" << result.error
                  << "\",\"type\":\"" << result.type << "\"}" << std::endl;
    }
    else if (result.result == "fail") {
        std::cerr << "Error: " << result.error << std::endl;
        if (!result.stack.empty()) {
            std::cerr << result.stack << std::endl;
        }
    }

    return result.result == "pass" ? 0 : 1;
}

template<class Next>
class Test262HarnessFeature : public Next {
    std::string _harnessDir;
    bool _asyncCompleted = false;
    bool _asyncFailed = false;
    std::string _asyncMessage;

public:
    void setHarnessDir(std::string dir) {
        _harnessDir = std::move(dir);
    }

    const std::string& harnessDir() const {
        return _harnessDir;
    }

    void initialize() {
        _asyncCompleted = false;
        _asyncFailed = false;
        _asyncMessage.clear();
        Next::initialize();

        jac::FunctionFactory ff(this->context());
        jac::Object global = this->context().getGlobalObject();

        global.defineProperty("print", ff.newFunction([this](jac::ValueWeak val) {
            auto msg = val.to<std::string>();
            if (msg == "Test262:AsyncTestComplete") {
                _asyncCompleted = true;
                this->exit(0);
            } else if (msg.starts_with("Test262:AsyncTestFailure:")) {
                _asyncFailed = true;
                _asyncMessage = std::string(msg.substr(strlen("Test262:AsyncTestFailure:")));
                this->exit(1);
            }
            return jac::Value::undefined(this->context());
        }));

        jac::Object dollar262 = jac::Object::create(this->context());
        dollar262.defineProperty("global", global);
        dollar262.defineProperty("gc", ff.newFunction([this]() {
            JS_RunGC(JS_GetRuntime(this->context()));
            return jac::Value::undefined(this->context());
        }));
        dollar262.defineProperty("evalScript", ff.newFunction([this](jac::ValueWeak val) {
            return this->eval(val.to<std::string>(), "<evalScript>", jac::EvalFlags::Global);
        }));
        global.defineProperty("$262", dollar262);

        global.defineProperty("$DONOTEVALUATE", ff.newFunction([this]() {
            return jac::Value::undefined(this->context());
        }));
    }

    void loadHarness() {
        if (_harnessDir.empty()) {
            return;
        }
        std::string harnessCode = loadHarnessFiles(_harnessDir, {"assert.js", "sta.js", "doneprintHandle.js"});
        if (!harnessCode.empty()) {
            this->eval(harnessCode, "<harness>", jac::EvalFlags::Global);
        }
    }

    Test262Result getAsyncResult() {
        if (_asyncCompleted && !_asyncFailed) {
            return makeResult("pass", "runtime");
        } else if (_asyncFailed) {
            return makeResult("fail", "runtime", _asyncMessage, "Error");
        }
        return makeResult("fail", "runtime", "Async test did not complete", "Error");
    }

    bool isAsyncDone() const {
        return _asyncCompleted || _asyncFailed;
    }
};
