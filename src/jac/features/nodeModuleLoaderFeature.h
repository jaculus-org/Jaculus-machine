#pragma once

#include <jac/machine/machine.h>
#include <jac/machine/values.h>

#include <jac/features/util/moduleLoader.h>

#include <sstream>
#include <optional>
#include <vector>
#include <algorithm>
#include <cctype>
#include <cstdint>
#include <string>
#include <set>


// XXX: experimental and not fully implemented and tested
//
// To avoid using URLs (i.e., "file://" in our case), module names are canonical paths relative
// to the configured code directory. Package specifiers are resolved to such paths by the module
// normalizer before QuickJS looks up or loads the module.


namespace jac {


template<class Next>
class NodeModuleLoaderFeature : public Next {
public:
    static inline const std::set<std::string_view> defaultConditions = { "jaculus", "node", "import" };  // XXX: different from spec

    enum class PackageTargetStatus {
        Undefined,
        Null,
        Resolved,
        InvalidTarget,
    };

    struct PackageTargetResult {
        PackageTargetStatus status;
        std::string path;
    };

    static bool isArrayIndex(std::string_view key) {
        if (key.empty() || (key.size() > 1 && key.front() == '0')) {
            return false;
        }

        uint64_t value = 0;
        for (char c : key) {
            if (c < '0' || c > '9') {
                return false;
            }
            uint64_t digit = static_cast<uint64_t>(c - '0');
            value = value * 10 + digit;
            if (value > static_cast<uint64_t>(UINT32_MAX) - 1) {
                return false;
            }
        }
        return true;
    }

    static std::string decodePercent(std::string_view value) {
        auto hexValue = [](char c) -> int {
            if (c >= '0' && c <= '9') return c - '0';
            if (c >= 'a' && c <= 'f') return c - 'a' + 10;
            if (c >= 'A' && c <= 'F') return c - 'A' + 10;
            return -1;
        };

        std::string decoded;
        for (size_t i = 0; i < value.size(); ++i) {
            if (value[i] == '%' && i + 2 < value.size()) {
                int high = hexValue(value[i + 1]);
                int low = hexValue(value[i + 2]);
                if (high >= 0 && low >= 0) {
                    decoded.push_back(static_cast<char>((high << 4) | low));
                    i += 2;
                    continue;
                }
            }
            decoded.push_back(value[i]);
        }
        return decoded;
    }

    static bool hasInvalidSegment(std::string_view path) {
        size_t segmentStart = 0;
        while (segmentStart <= path.size()) {
            size_t segmentEnd = path.find_first_of("/\\", segmentStart);
            if (segmentEnd == std::string_view::npos) {
                segmentEnd = path.size();
            }

            std::string segment = decodePercent(path.substr(segmentStart, segmentEnd - segmentStart));
            std::transform(segment.begin(), segment.end(), segment.begin(), [](unsigned char c) {
                return static_cast<char>(std::tolower(c));
            });
            if (segment.empty() || segment == "." || segment == ".." || segment == "node_modules") {
                return true;
            }

            segmentStart = segmentEnd + 1;
        }
        return false;
    }

    static PackageTargetResult PACKAGE_TARGET_RESOLVE(const std::string& packageURL, jac::Value target, std::optional<std::string> patternMatch, NodeModuleLoaderFeature<Next>& self) {
        // XXX: isImports is always false for now
        // XXX: conditions is always default for now

        if (target.isString()) {
            auto targetStr = target.toString();
            if (!targetStr.view().starts_with("./")) {
                return { PackageTargetStatus::InvalidTarget, {} };
            }
            if (hasInvalidSegment(targetStr.view().substr(2))) {
                return { PackageTargetStatus::InvalidTarget, {} };
            }
            std::string resolvedTarget = self.path.join({ packageURL, targetStr });  // XXX: "URL resolution"

            if (!patternMatch) {
                return { PackageTargetStatus::Resolved, std::move(resolvedTarget) };
            }

            if (hasInvalidSegment(*patternMatch)) {
                throw jac::Exception::create(jac::Exception::Type::TypeError, "invalid module specifier");
            }

            std::stringstream ss;
            for (char c : resolvedTarget) {
                if (c == '*') {
                    ss << *patternMatch;
                }
                else {
                    ss << c;
                }
            }
            return { PackageTargetStatus::Resolved, ss.str() };  // TODO: "URL resolution"
        }
        else if (target.isArray()) {
            auto targets = target.to<jac::Array>();
            auto fallbackStatus = PackageTargetStatus::Null;
            for (int i = 0; i < targets.length(); ++i) {
                auto resolved = PACKAGE_TARGET_RESOLVE(packageURL, targets.get<jac::Value>(i), patternMatch, self);
                if (resolved.status == PackageTargetStatus::InvalidTarget) {
                    fallbackStatus = PackageTargetStatus::InvalidTarget;
                }
                else if (resolved.status != PackageTargetStatus::Undefined) {
                    return resolved;
                }
            }
            return { fallbackStatus, {} };
        }
        else if (target.isObject()) {
            auto objectTarget = target.to<jac::Object>();
            auto keys = objectTarget.getOwnPropertyNames();
            for (auto& key : keys) {
                auto keyStr = key.toString();
                if (isArrayIndex(keyStr.view())) {
                    throw jac::Exception::create(jac::Exception::Type::TypeError, "conditional exports cannot contain integer keys");
                }

                if (keyStr.view() == "default" || defaultConditions.contains(keyStr)) {
                    auto targetValue = objectTarget.get<jac::Value>(key);
                    auto resolved = PACKAGE_TARGET_RESOLVE(packageURL, targetValue, patternMatch, self);
                    if (resolved.status != PackageTargetStatus::Undefined) {
                        return resolved;
                    }
                }
            }
            return { PackageTargetStatus::Undefined, {} };
        }
        else if (target.isNull()) {
            return { PackageTargetStatus::Null, {} };
        }
        return { PackageTargetStatus::InvalidTarget, {} };
    }

    static PackageTargetResult PACKAGE_IMPORTS_EXPORTS_RESOLVE(const std::string& matchKey, jac::Object matchObj, const std::string& packageURL, NodeModuleLoaderFeature<Next>& self) {
        // XXX: isImports is always false for now
        // XXX: conditions is always default for now

        if (matchKey.ends_with("/")) {
            throw jac::Exception::create(jac::Exception::Type::TypeError, "invalid module specifier");
        }
        if (matchKey.find('*') == std::string::npos && matchObj.hasProperty(matchKey)) {
            auto target = matchObj.get<jac::Value>(matchKey);
            return PACKAGE_TARGET_RESOLVE(packageURL, target, std::nullopt, self);
        }
        std::vector<std::string> expansionKeys;
        {
            auto keys = matchObj.getOwnPropertyNames();
            for (auto& key : keys) {
                auto keyStr = key.toString();
                auto starPos = keyStr.view().find('*');
                if (starPos == std::string::npos) {
                    continue;
                }
                if (keyStr.view().find('*', starPos + 1) != std::string::npos) {
                    continue;
                }
                expansionKeys.emplace_back(keyStr.view());
            }
        }

        std::stable_sort(expansionKeys.begin(), expansionKeys.end(), [](const std::string& a, const std::string& b) {
            size_t baseA = a.find('*');
            size_t baseB = b.find('*');
            return baseA != baseB ? baseA > baseB : a.size() > b.size();
        });

        for (auto& expansionKey : expansionKeys) {
            size_t starPos = expansionKey.find('*');
            auto patternBase = expansionKey.substr(0, starPos);
            if (!matchKey.starts_with(patternBase) || matchKey == patternBase) {
                continue;
            }
            auto patternTrailer = expansionKey.substr(starPos + 1);
            if (patternTrailer.empty() || (matchKey.size() >= expansionKey.size() && matchKey.ends_with(patternTrailer))) {
                auto target = matchObj.get<jac::Value>(expansionKey);
                auto patternMatch = matchKey.substr(patternBase.size(), matchKey.size() - patternBase.size() - patternTrailer.size());
                return PACKAGE_TARGET_RESOLVE(packageURL, target, patternMatch, self);
            }
        }

        return { PackageTargetStatus::Null, {} };
    }

    static std::string PACKAGE_EXPORTS_RESOLVE(const std::string& packageURL, const std::string& subpath, jac::Value exports, NodeModuleLoaderFeature<Next>& self) {
        // XXX: non-default conditions not supported
        bool objOnlyDots = false;
        if (exports.isObject()) {
            auto obj = exports.to<jac::Object>();
            auto props = obj.getOwnPropertyNames();
            bool anyDot = false;
            bool anyNonDot = false;
            for (auto& prop : props) {
                if (prop.toString().view().starts_with(".")) {
                    anyDot = true;
                }
                else {
                    anyNonDot = true;
                }
            }
            if (anyDot && anyNonDot) {
                throw jac::Exception::create(jac::Exception::Type::TypeError, "invalid package configuration");
            }
            objOnlyDots = anyDot;
        }

        PackageTargetResult resolved { PackageTargetStatus::Undefined, {} };
        if (subpath == ".") {
            jac::Value mainExport = jac::Value::undefined(self.context());
            if (exports.isString() || exports.isArray()) {
                mainExport = exports;
            }
            else if (exports.isObject()) {
                if (!objOnlyDots) {
                    mainExport = exports;
                }
                else {
                    auto obj = exports.to<jac::Object>();
                    if (obj.hasProperty(".")) {
                        mainExport = obj.get<jac::Value>(".");
                    }
                }
            }
            if (!mainExport.isUndefined()) {
                resolved = PACKAGE_TARGET_RESOLVE(packageURL, mainExport, std::nullopt, self);
            }
        }
        else if (exports.isObject() && objOnlyDots) {
            resolved = PACKAGE_IMPORTS_EXPORTS_RESOLVE(subpath, exports, packageURL, self);
        }

        if (resolved.status == PackageTargetStatus::InvalidTarget) {
            throw jac::Exception::create(jac::Exception::Type::TypeError, "invalid package target");
        }
        if (resolved.status != PackageTargetStatus::Resolved) {
            throw jac::Exception::create(jac::Exception::Type::Error, "package path not exported");
        }

        return resolved.path;
    }

    static std::optional<std::string> PACKAGE_SELF_RESOLVE(const std::string& packageName, const std::string& packageSubpath, NodeModuleLoaderFeature<Next>& self) {
        auto pjson = READ_PACKAGE_JSON(".", self);
        if (pjson && pjson->hasProperty("name")) {
            auto nameVal = pjson->template get<jac::Value>("name");
            if (nameVal.isString() && nameVal.toString().view() == packageName) {
                if (pjson->hasProperty("exports")) {
                    auto exports = pjson->template get<jac::Value>("exports");
                    if (!exports.isUndefined() && !exports.isNull()) {
                        return PACKAGE_EXPORTS_RESOLVE(".", packageSubpath, exports, self);
                    }
                }
            }
        }
        return std::nullopt;
    }

    static std::optional<jac::Object> READ_PACKAGE_JSON(const std::string& packageURL, NodeModuleLoaderFeature<Next>& self) {
        std::string pjsonURL = packageURL + "/package.json";
        if (!self.fs.isFileCode(pjsonURL)) {
            return std::nullopt;
        }

        std::string pjsonStr = self.fs.loadCode(pjsonURL);
        JSValue parsed = JS_ParseJSON2(self.context(), pjsonStr.c_str(), pjsonStr.size(), pjsonURL.c_str(), 0);
        if (JS_IsException(parsed)) {
            auto parseError = self.context().getException();
            throw jac::Exception::create(jac::Exception::Type::TypeError, "invalid package configuration " + pjsonURL + ": " + parseError.what());
        }
        if (!JS_IsObject(parsed) || JS_IsArray(self.context(), parsed)) {
            JS_FreeValue(self.context(), parsed);
            throw jac::Exception::create(jac::Exception::Type::TypeError, "package configuration must be an object: " + pjsonURL);
        }
        return jac::Object(self.context(), parsed);
    }

    static std::string PACKAGE_RESOLVE(const std::string& packageSpecifier, NodeModuleLoaderFeature<Next>& self) {
        if (packageSpecifier.size() == 0) {
            throw jac::Exception::create(jac::Exception::Type::TypeError, "invalid module specifier");
        }

        std::string packageName;
        std::string packageSubpath;
        size_t after = 0;

        if (packageSpecifier.starts_with("@")) {
            auto slashPos = packageSpecifier.find('/');
            if (slashPos == std::string::npos || slashPos == 1 || slashPos + 1 == packageSpecifier.size()) {
                throw jac::Exception::create(jac::Exception::Type::TypeError, "invalid module specifier");
            }
            after = slashPos + 1;
        }

        auto slashPos = packageSpecifier.find('/', after);
        if (slashPos == std::string::npos) {
            packageName = packageSpecifier;
            packageSubpath = ".";
        }
        else {
            packageName = packageSpecifier.substr(0, slashPos);
            packageSubpath = "." + packageSpecifier.substr(slashPos);
        }

        if (packageName.starts_with('.') || std::any_of(packageName.begin(), packageName.end(), [](char c){ return c == '\\' || c == '%'; })) {
            throw jac::Exception::create(jac::Exception::Type::TypeError, "invalid module specifier");
        }

        if (auto selfUrl = PACKAGE_SELF_RESOLVE(packageName, packageSubpath, self)) {
            return *selfUrl;
        }

        // XXX: we do not try parent directories for node_modules
        std::string packageURL = "node_modules/" + packageName;
        if (!self.fs.isDirectoryCode(packageURL)) {
            throw jac::Exception::create(jac::Exception::Type::Error, std::string("module not found: ") + packageSpecifier);
        }
        auto pjson = READ_PACKAGE_JSON(packageURL, self);

        if (pjson && pjson->hasProperty("exports")) {
            auto exports = pjson->template get<jac::Value>("exports");
            if (!exports.isUndefined() && !exports.isNull()) {
                return PACKAGE_EXPORTS_RESOLVE(packageURL, packageSubpath, exports, self);
            }
        }
        if (packageSubpath == ".") {
            if (pjson && pjson->hasProperty("main")) {
                auto mainVal = pjson->template get<jac::Value>("main");
                if (mainVal.isString()) {
                    std::string mainFile = mainVal.toString();
                    return packageURL + "/" + mainFile;
                }
            }
        }
        else {
            return packageURL + "/" + packageSubpath;
        }

        throw jac::Exception::create(jac::Exception::Type::Error, std::string("module not found: ") + packageSpecifier);
    }

    static std::string ESM_RESOLVE(const std::string& specifier, const std::string& parentURL, NodeModuleLoaderFeature<Next>& self) {
        // XXX: url specifiers not supported
        // JavaScript sources are always treated as modules

        std::string resolved;
        if (specifier.starts_with("./") || specifier.starts_with("../")) {
            resolved = self.path.normalize(self.path.join({ self.path.dirname(parentURL), specifier }));
        }
        else if (specifier.starts_with("/")) {
            resolved = self.path.normalize(specifier);
        }
        else if (specifier.starts_with("#")) {
            throw jac::Exception::create(jac::Exception::Type::Error, "not implemented (#module specifiers)");
        }
        else {
            resolved = PACKAGE_RESOLVE(specifier, self);
        }
        resolved = self.normalizeModulePath(std::move(resolved));

        size_t start = 0;
        size_t percentPos;
        while ((percentPos = resolved.find('%', start)) != std::string::npos) {
            if (percentPos + 2 >= resolved.size()) {
                break;
            }
            std::string_view hexStr = std::string_view(resolved).substr(percentPos + 1, 2);
            if (hexStr == "2F" || hexStr == "2f" || hexStr == "5C" || hexStr == "5c") {
                throw jac::Exception::create(jac::Exception::Type::TypeError, "encoded path separators are not allowed");
            }
            start = percentPos + 1;
        }

        if (self.fs.isDirectoryCode(resolved)) {
            throw jac::Exception::create(jac::Exception::Type::Error, "unsupported directory import: " + resolved);
        }
        if (!self.fs.existsCode(resolved) || !self.fs.isFileCode(resolved)) {
            throw jac::Exception::create(jac::Exception::Type::Error, std::string("module not found: ") + resolved);
        }

        return resolved;
    }

private:
    std::string normalizeModulePath(std::string filename) {
        filename = this->path.normalize(std::move(filename));
        if (!filename.starts_with("/") && !filename.starts_with("./") && !filename.starts_with("../")) {
            filename = "./" + filename;
        }
        return filename;
    }

    static JSModuleDef *moduleLoaderCbk(JSContext* ctx, const char *module_name, void *_self, JSValueConst attributes) {
        auto &self = *static_cast<NodeModuleLoaderFeature<Next>*>(_self);

        std::string filename = module_name;
        std::string buffer = self.fs.loadCode(filename);

        int jsonType = moduleAttributesJsonType(ctx, attributes);
        if (filename.ends_with(".json")) {
            if (jsonType == 0) {
                throw jac::Exception::create(jac::Exception::Type::TypeError, "module '" + filename + "' requires type 'json' or 'json5'");
            }
            JSValue val = JS_ParseJSON2(ctx, buffer.c_str(), buffer.size(), module_name,
                                        jsonType == 2 ? JS_PARSE_JSON_EXT : 0);
            if (JS_IsException(val)) {
                return nullptr;
            }
            return createJsonModule(ctx, module_name, val);
        }
        if (jsonType > 0) {
            throw jac::Exception::create(jac::Exception::Type::TypeError, "JSON import type is incompatible with module '" + filename + "'");
        }
        if (filename.ends_with(".cjs") || filename.ends_with(".wasm")) {
            throw jac::Exception::create(jac::Exception::Type::TypeError, "unsupported module format: " + filename);
        }

        self.resetWatchdog();
        JSValue val = JS_Eval(ctx, buffer.c_str(), buffer.size(), module_name,
                              JS_EVAL_TYPE_MODULE | JS_EVAL_FLAG_COMPILE_ONLY);
        if (JS_IsException(val)) {
            return nullptr;
        }

        Value bytecode(ctx, val);
        auto mdl = static_cast<JSModuleDef*>(JS_VALUE_GET_PTR(bytecode.getVal()));

        Object meta(ctx, JS_GetImportMeta(ctx, mdl));
        meta.set("url", filename);
        meta.set("main", false);

        return mdl;
    }

public:
    /**
     * @brief Evaluate a file
     *
     * @param path_ Path to the file
     * @return A promise that will be resolved when the module ends, and rejected if
     *         the module throws an exception.
     */
    Value evalFile(std::string path_) {
        std::string filename = normalizeModulePath(std::move(path_));
        if (filename.ends_with(".json")) {
            throw jac::Exception::create(jac::Exception::Type::TypeError, "JSON entry modules require an import type");
        }
        if (filename.ends_with(".cjs") || filename.ends_with(".wasm")) {
            throw jac::Exception::create(jac::Exception::Type::TypeError, "unsupported module format: " + filename);
        }
        auto buffer = this->fs.loadCode(filename);

        Value bytecode = this->eval(std::move(buffer), filename, EvalFlags::Module | EvalFlags::CompileOnly);
        auto mdl = static_cast<JSModuleDef*>(JS_VALUE_GET_PTR(bytecode.getVal()));

        Object meta(this->context(), JS_GetImportMeta(this->context(), mdl));
        meta.set("url", filename);
        meta.set("main", true);

        this->resetWatchdog();
        return Value(this->context(), JS_EvalFunction(this->context(), bytecode.loot().second));
    }

    /**
     * @brief Evaluate a file and run the event loop until the program exits
     *        or throws an exception.
     *
     * @param path_ Path to the file
     */
    void evalFileWithEventLoop(std::string path_) {
        Value promise = this->evalFile(path_);
        this->evalWithEventLoopCommon(promise);
    }

    void initialize() {
        Next::initialize();

        this->setModuleLoader(
            [this](JSContext* ctx, const char* name, JSValueConst attributes) {
                return moduleLoaderCbk(ctx, name, this, attributes);
            },
            [this](JSContext*, const char* baseName, const char* name) {
                return ESM_RESOLVE(name, baseName, *this);
            }
        );
    }
};


} // namespace jac
