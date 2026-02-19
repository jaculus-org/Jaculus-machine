#pragma once

#include <jac/machine/machine.h>
#include <jac/machine/values.h>

#include <iostream>
#include <cassert>
#include <sstream>
#include <optional>
#include <vector>
#include <algorithm>
#include <vector>
#include <string>
#include <set>


// XXX: experimental and not fully implemented and tested
//
// To avoid using URLs (i.e., "file://" in our case), we identify paths by initial "./".
// Because quickjs performs automatic of relative imports to current module URL, we can
// only differentiate relative imports ("./", "../") and package imports by initial characters
// of "module_name". We therefore expect the entry point to have a file path starting with "./".
//
// Known issues:
// - Relative imports in modules do not work because of quickjs's automatic "import path"
//   concatenation (and thus missing information whether the import is relative inside a module
//   or an import of package entry point). If exports are written consistently with the internal
//   structure of the package, it can be made to work (but all internal imports must be listed).


namespace jac {


bool strContains(const auto& str, const auto& substr) {
    return str.find(substr) != std::string::npos;
}

std::string_view substring(const std::string& str, size_t pos, size_t count = std::string::npos) {
    return std::string_view(str).substr(pos, count);
}


template<class Next>
class NodeModuleLoaderFeature : public Next {
public:
    // XXX: parentURL is always empty and thus ignored
    static inline const std::set<std::string_view> defaultConditions = { "jaculus", "node", "import" };  // XXX: different from spec

    static std::optional<std::string> PACKAGE_TARGET_RESOLVE(const std::string& packageURL, jac::Value target, std::optional<std::string> patternMatch, NodeModuleLoaderFeature<Next>& self) {
        // XXX: isImports is always false for now
        // XXX: conditions is always default for now

        if (target.isString()) {
            auto targetStr = target.toString();
            if (!targetStr.starts_with("./")) {
                throw jac::Exception::create(jac::Exception::Type::Error, "invalid package target");
            }
            auto prevSlash = targetStr.find_first_of("/\\");
            decltype(prevSlash) nextSlash;
            while ((nextSlash = targetStr.find_first_of("/\\", prevSlash + 1)) != std::string::npos) {
                auto segment = targetStr.substr(prevSlash + 1, nextSlash - prevSlash - 1);
                if (segment == "" || segment == "." || segment == ".." || segment == "node_modules") {
                    throw jac::Exception::create(jac::Exception::Type::Error, "invalid package target");
                }
                prevSlash = nextSlash;
            }
            std::string resolvedTarget = self.path.join({ packageURL, targetStr });  // XXX: "URL resolution"
            assert(strContains(resolvedTarget, packageURL));

            if (!patternMatch) {
                return resolvedTarget;
            }

            prevSlash = patternMatch->find_first_of("/\\");
            while ((nextSlash = patternMatch->find_first_of("/\\", prevSlash + 1)) != std::string::npos) {
                auto segment = substring(*patternMatch, prevSlash + 1, nextSlash - prevSlash - 1);
                if (segment == "" || segment == "." || segment == ".." || segment == "node_modules") {
                    throw jac::Exception::create(jac::Exception::Type::Error, "invalid module specifier");
                }
                prevSlash = nextSlash;
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
            return ss.str();  // TODO: "URL resolution"
        }
        else if (target.isArray()) {
            throw jac::Exception::create(jac::Exception::Type::Error, "array targets not implemented");
        }
        else if (target.isObject()) {  // check
            auto objectTarget = target.to<jac::Object>();
            auto keys = objectTarget.getOwnPropertyNames();
            for (auto& key : keys) {
                auto keyStr = key.toString();

                if (keyStr == "default" || defaultConditions.contains(keyStr)) {
                    auto targetValue = objectTarget.get<jac::Value>(key);
                    auto resolved = PACKAGE_TARGET_RESOLVE(packageURL, targetValue, patternMatch, self);
                    if (resolved) {
                        return resolved;
                    }
                }
            }
            return std::nullopt;
        }
        else if (target.isNull()) {
            return std::nullopt;
        }
        else {
            throw jac::Exception::create(jac::Exception::Type::Error, "invalid package target");
        }
    }

    static std::optional<std::string> PACKAGE_IMPORTS_EXPORTS_RESOLVE(const std::string& matchKey, jac::Object matchObj, const std::string& packageURL, NodeModuleLoaderFeature<Next>& self) {
        // XXX: isImports is always false for now
        // XXX: conditions is always default for now

        if (matchKey.ends_with("/")) {
            throw jac::Exception::create(jac::Exception::Type::Error, "invalid module specifier");
        }
        if (!strContains(matchKey, "*") && matchObj.hasProperty(matchKey)) {
            auto target = matchObj.get<jac::Value>(matchKey);
            return PACKAGE_TARGET_RESOLVE(packageURL, target, std::nullopt, self);
        }
        std::vector<jac::StringView> expansionKeys;
        {
            auto keys = matchObj.getOwnPropertyNames();
            for (auto& key : keys) {
                auto keyStr = key.toString();
                auto starPos = keyStr.find('*');
                if (starPos == std::string::npos) {
                    continue;
                }
                if (keyStr.find('*', starPos + 1) != std::string::npos) {
                    continue;
                }
                expansionKeys.emplace_back(std::move(keyStr));
            }
        }

        // FIXME: sort expansionKeys by specificity ("PATTERN_KEY_COMPARE" in docs)

        for (auto& expansionKey : expansionKeys) {
            size_t starPos = expansionKey.find('*');
            auto patternBase = expansionKey.substr(0, starPos);
            if (!matchKey.starts_with(patternBase)) {
                continue;
            }
            auto patternTrailer = expansionKey.substr(starPos + 1);
            if (patternTrailer.empty() || (matchKey.size() >= expansionKey.size() && matchKey.ends_with(patternTrailer))) {
                auto target = matchObj.get<jac::Value>(expansionKey);
                auto patternMatch = matchKey.substr(patternBase.size(), matchKey.size() - patternBase.size() - patternTrailer.size());
                return PACKAGE_TARGET_RESOLVE(packageURL, target, patternMatch, self);
            }
        }

        return std::nullopt;
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
                if (prop.toString().starts_with(".")) {
                    anyDot = true;
                }
                else {
                    anyNonDot = true;
                }
            }
            if (anyDot && anyNonDot) {
                throw jac::Exception::create(jac::Exception::Type::Error, "invalid package configuration");
            }
            objOnlyDots = anyDot;
        }

        std::optional<std::string> resolved = std::nullopt;
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
            assert(subpath.starts_with("./"));
            resolved = PACKAGE_IMPORTS_EXPORTS_RESOLVE(subpath, exports, packageURL, self);
        }

        if (!resolved) {
            throw jac::Exception::create(jac::Exception::Type::Error, "package path not exported");
        }

        return *resolved;
    }

    static std::optional<std::string> PACKAGE_SELF_RESOLVE(const std::string& packageName, const std::string& packageSubpath, NodeModuleLoaderFeature<Next>& self) {
        jac::Object pjson = READ_PACKAGE_JSON(".", self);
        if (pjson.hasProperty("name")) {
            auto nameVal = pjson.get<jac::Value>("name");
            if (nameVal.isString() && nameVal.toString() == packageName) {
                if (pjson.hasProperty("exports")) {
                    auto exports = pjson.get<jac::Value>("exports");
                    if (!exports.isUndefined() && !exports.isNull()) {
                        return PACKAGE_EXPORTS_RESOLVE(".", packageSubpath, exports, self);
                    }
                }
            }
        }
        return std::nullopt;
    }

    static jac::Object READ_PACKAGE_JSON(const std::string& packageURL, NodeModuleLoaderFeature<Next>& self) {
        std::string pjsonURL = packageURL + "/package.json";

        std::string pjsonStr = self.fs.loadCode(pjsonURL);
        return jac::Object(self.context(), JS_ParseJSON2(self.context(), pjsonStr.c_str(), pjsonStr.size(), pjsonURL.c_str(), 0));
    }

    static std::string PACKAGE_RESOLVE(const std::string& packageSpecifier, NodeModuleLoaderFeature<Next>& self) {
        if (packageSpecifier.size() == 0) {
            throw jac::Exception::create(jac::Exception::Type::Error, "invalid module specifier");
        }

        std::string packageName;
        std::string packageSubpath;
        size_t after = 0;

        if (packageSpecifier.starts_with("@")) {
            auto slashPos = packageSpecifier.find('/');
            if (slashPos == std::string::npos) {
                throw jac::Exception::create(jac::Exception::Type::Error, "invalid module specifier");
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
            throw jac::Exception::create(jac::Exception::Type::Error, "invalid module specifier");
        }

        if (auto selfUrl = PACKAGE_SELF_RESOLVE(packageName, packageSubpath, self)) {
            return *selfUrl;
        }

        // XXX: we do not try parent directories for node_modules - difficult because of qjs, also not needed for now

        std::string packageURL = "node_modules/" + packageName;  // XXX: resolve URL relative to parentURL
        if (!self.fs.isDirectoryCode(packageURL)) {
            throw jac::Exception::create(jac::Exception::Type::Error, std::string("module not found: ") + packageSpecifier);
        }
        jac::Object pjson = READ_PACKAGE_JSON(packageURL, self);

        if (pjson.hasProperty("exports")) {
            auto exports = pjson.get<jac::Value>("exports");
            if (!exports.isUndefined() && !exports.isNull()) {
                return PACKAGE_EXPORTS_RESOLVE(packageURL, packageSubpath, exports, self);
            }
        }
        if (packageSubpath == ".") {
            if (pjson.hasProperty("main")) {
                auto mainVal = pjson.get<jac::Value>("main");
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

    static std::string ESM_RESOLVE(const std::string& specifier, NodeModuleLoaderFeature<Next>& self) {
        // XXX: parentURL is always empty -> ignored
        // XXX: url specifiers not supported
        // XXX: format is always set to "module"

        std::string resolved;
        if (specifier.starts_with("./") || specifier.starts_with("../") || specifier.starts_with("/")) {
            resolved = specifier;
        }
        else if (specifier.starts_with("#")) {
            throw jac::Exception::create(jac::Exception::Type::Error, "not implemented (#module specifiers)");
        }
        else {
            resolved = PACKAGE_RESOLVE(specifier, self);
        }

        size_t start = 0;
        size_t percentPos;
        while ((percentPos = resolved.find('%', start)) != std::string::npos) {
            if (percentPos + 2 >= resolved.size()) {
                break;
            }
            std::string_view hexStr = std::string_view(resolved).substr(percentPos + 1, 2);
            if (hexStr == "2F" || hexStr == "2f" || hexStr == "5C" || hexStr == "5c") {
                throw jac::Exception::create(jac::Exception::Type::Error, "invalid module specifier");
            }
            start = percentPos + 1;
        }

        if (self.fs.isDirectoryCode(resolved)) {
            throw jac::Exception::create(jac::Exception::Type::Error, "unsupported directory import");
        }
        if (!self.fs.existsCode(resolved) || !self.fs.isFileCode(resolved)) {
            throw jac::Exception::create(jac::Exception::Type::Error, std::string("module not found: ") + resolved);
        }

        return resolved;
    }

private:
    static JSModuleDef *moduleLoaderCbk(JSContext* ctx, const char *module_name, void *_self) {
        auto &self = *static_cast<NodeModuleLoaderFeature<Next>*>(_self);

        std::string filename = ESM_RESOLVE(module_name, self);

        std::string buffer;
        try {
            buffer = self.fs.loadCode(filename);
        } catch (jac::Exception &e) {
            e.throwJS(ctx);
            return nullptr;
        }

        // compile and return module
        self.resetWatchdog();
        JSValue val = JS_Eval(ctx, buffer.c_str(), buffer.size(), module_name,
                              JS_EVAL_TYPE_MODULE | JS_EVAL_FLAG_COMPILE_ONLY);
        if (JS_IsException(val)) {
            return nullptr;
        }

        auto mdl = static_cast<JSModuleDef*>(JS_VALUE_GET_PTR(val));

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
        auto buffer = this->fs.loadCode(path_);

        Value val = this->eval(std::move(buffer), path_, EvalFlags::Module);
        return val;
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

        JS_SetModuleLoaderFunc(this->runtime(), nullptr, moduleLoaderCbk, this);
    }
};


} // namespace jac
