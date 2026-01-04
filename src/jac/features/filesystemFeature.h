#pragma once

#include <jac/machine/class.h>
#include <jac/machine/functionFactory.h>
#include <jac/machine/machine.h>

#include <filesystem>
#include <noal_func.h>

#include "types/file.h"


namespace jac {


struct FileProtoBuilder : public ProtoBuilder::Opaque<File>, public ProtoBuilder::Properties {
    static void addProperties(ContextRef ctx, Object proto) {
        addPropMember<std::string, &File::path_>(ctx, proto, "path", PropFlags::Enumerable);
        addMethodMember<bool(File::*)(), &File::isOpen>(ctx, proto, "isOpen", PropFlags::Enumerable);
        addMethodMember<void(File::*)(), &File::close>(ctx, proto, "close", PropFlags::Enumerable);
        addMethodMember<std::string(File::*)(int), &File::read>(ctx, proto, "read", PropFlags::Enumerable);
        addMethodMember<void(File::*)(std::string), &File::write>(ctx, proto, "write", PropFlags::Enumerable);
    }
};


template<class Next>
class FilesystemFeature : public Next {
private:

    std::filesystem::path _codeDir = ".";
    std::filesystem::path _workingDir = ".";

public:
    using FileClass = Class<FileProtoBuilder>;

    void setCodeDir(std::string path_) {
        this->_codeDir = std::filesystem::path(path_).lexically_normal();
    }

    void setWorkingDir(std::string path_) {
        this->_workingDir = std::filesystem::path(path_).lexically_normal();
    }

    class Path {
        FilesystemFeature& _feature;
    public:
        Path(FilesystemFeature& feature): _feature(feature) {}

        std::string normalize(std::string path_) {
            return std::filesystem::path(path_).lexically_normal().string();
        }

        std::string dirname(std::string path_) {
            auto res = std::filesystem::path(path_).parent_path().string();
            return res.empty() ? "." : res;
        }

        std::string basename(std::string path_) {
            return std::filesystem::path(path_).filename().string();
        }

        std::string join(std::vector<std::string> paths) {
            std::filesystem::path path_;
            for (auto& p : paths) {
                path_ /= p;
            }
            return path_.string();
        }
    };

private:
    class Fs {
        FilesystemFeature& _feature;
    public:
        Fs(FilesystemFeature& feature) : _feature(feature) {}

        std::string loadCode(std::string filename) {
            std::string buffer;
            File file(_feature._codeDir / filename, "r");
            std::string read = file.read();
            while (!read.empty()) {
                buffer += read;
                read = file.read();
            }
            return buffer;
        }

        bool existsCode(std::string path_) {
            return std::filesystem::exists(_feature._codeDir / path_);
        }

        bool isFileCode(std::string path_) {
            return std::filesystem::is_regular_file(_feature._codeDir / path_);
        }

        bool isDirectoryCode(std::string path_) {
            return std::filesystem::is_directory(_feature._codeDir / path_);
        }


        File open(std::string path_, std::string flags) {
            return File(_feature._workingDir / path_, flags);
        }

        bool exists(std::string path_) {
            return std::filesystem::exists(_feature._workingDir / path_);
        }

        bool isFile(std::string path_) {
            return std::filesystem::is_regular_file(_feature._workingDir / path_);
        }

        bool isDirectory(std::string path_) {
            return std::filesystem::is_directory(_feature._workingDir / path_);
        }

        void mkdir(std::string path_) {
            std::filesystem::create_directories(_feature._workingDir / path_);
        }

        std::vector<std::string> readdir(std::string path_) {
            std::vector<std::string> res;
            for (auto& p : std::filesystem::directory_iterator(_feature._workingDir / path_)) {
                res.push_back(p.path().filename().string());
            }
            return res;
        }

        void rm(std::string path_) {
            std::filesystem::remove(_feature._workingDir / path_);
        }

        void rmdir(std::string path_) {
            std::filesystem::remove_all(_feature._workingDir / path_);
        }
    };

public:
    Path path;
    Fs fs;

    FilesystemFeature() : path(*this), fs(*this) {
        FileClass::init("File");
    }

    void initialize() {
        Next::initialize();

        FileClass::initContext(this->context());

        FunctionFactory ff(this->context());

        Module& pathMod = this->newModule("path");
        pathMod.addExport("normalize", ff.newFunction(noal::function(&Path::normalize, &(this->path))));
        pathMod.addExport("dirname", ff.newFunction(noal::function(&Path::dirname, &(this->path))));
        pathMod.addExport("basename", ff.newFunction(noal::function(&Path::basename, &(this->path))));
        pathMod.addExport("join", ff.newFunctionVariadic([this](std::vector<ValueWeak> paths) {
            std::vector<std::string> paths_;
            for (auto& p : paths) {
                paths_.push_back(p.to<std::string>());
            }
            return this->path.join(paths_);
        }));

        Module& fsMod = this->newModule("fs");
        fsMod.addExport("open", ff.newFunction([this](std::string path_, std::string flags) {
            return FileClass::createInstance(this->context(), new File(this->fs.open(path_, flags)));
        }));
        fsMod.addExport("exists", ff.newFunction(noal::function(&Fs::exists, &(this->fs))));
        fsMod.addExport("isFile", ff.newFunction(noal::function(&Fs::isFile, &(this->fs))));
        fsMod.addExport("isDirectory", ff.newFunction(noal::function(&Fs::isDirectory, &(this->fs))));
        fsMod.addExport("mkdir", ff.newFunction(noal::function(&Fs::mkdir, &(this->fs))));
        fsMod.addExport("rm", ff.newFunction(noal::function(&Fs::rm, &(this->fs))));
        fsMod.addExport("rmdir", ff.newFunction(noal::function(&Fs::rmdir, &(this->fs))));
        fsMod.addExport("readdir", ff.newFunction(noal::function(&Fs::readdir, &(this->fs))));
    }
};


} // namespace jac
