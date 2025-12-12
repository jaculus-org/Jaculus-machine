#pragma once

#include <cassert>
#include <cstdlib>
#include <forward_list>
#include <list>
#include <map>
#include <memory>
#include <optional>
#include <set>
#include <string>
#include <tuple>
#include <utility>
#include <variant>
#include <vector>

#include "opcode.h"


namespace jac::cfg {


using RegId = int;
RegId newRegId();


using Identifier = std::string;
using MemberIdentifier = std::variant<Identifier, int32_t>;


struct Reg {
    ValueType type;
    RegId id;

    static Reg create(ValueType type_) {
        return { type_, newRegId() };
    }
    static Reg undefined() {
        return { ValueType::Void, 0 };
    }
};

/*
possible source:
    - literal
    - operator/call result
    - conversion from LVRef
*/
struct RValue {
    Reg reg;

    ValueType type() const {
        return reg.type;
    }

    RegId id() const {
        return reg.id;
    }

    operator Reg() const {
        return reg;
    }
};

/*
possible source:
    - variable
    - member access
*/
struct LVRef {
    Reg self;
    std::optional<Reg> memberIdent;  // for member access
    bool _const = false;

    bool isMember() const {
        return memberIdent.has_value();
    }

    bool isConst() const {
        return _const;
    }

    ValueType type() const {
        if (isMember()) {
            return ValueType::Any;
        }
        return self.type;
    }

    static LVRef direct(Reg self_, bool isConst) {
        return { self_, std::nullopt, isConst };
    }

    static LVRef mbr(Reg self_, Reg member_, bool isConst) {
        return { self_, member_, isConst };
    }

    LVRef(): self({ ValueType::Void, 0 }) {}
private:
    LVRef(Reg self_, std::optional<Reg> memberIdent_, bool isConst):
        self(self_), memberIdent(memberIdent_), _const(isConst)
    {}
};

struct Value {
    std::variant<RValue, LVRef> value;

    bool isRValue() const {
        return std::holds_alternative<RValue>(value);
    }
    LVRef& asLVRef() {
        return std::get<LVRef>(value);
    }
    RValue& asRValue() {
        return std::get<RValue>(value);
    }
};


struct Operation {
    Opcode op;

    Reg a;
    Reg b = { ValueType::Void, 0 };
    Reg res = { ValueType::Void, 0 };
};

struct ConstInit {
    RegId id;
    std::variant<int32_t, double, bool, std::string> value;

    ValueType type() const {
        if (std::holds_alternative<int32_t>(value)) {
            return ValueType::I32;
        }
        else if (std::holds_alternative<double>(value)) {
            return ValueType::F64;
        }
        else if (std::holds_alternative<bool>(value)) {
            return ValueType::Bool;
        }
        else if (std::holds_alternative<std::string>(value)) {
            return ValueType::StringConst;
        }
        abort();
    }
};

struct Call {
    std::variant<Identifier, Reg> obj;  // native/object
    bool isConstructor = false;
    std::vector<Reg> args;
    Reg res;

    bool isNative() const {
        return std::holds_alternative<Identifier>(obj);
    }
};

struct Instruction {
    std::variant<Operation, ConstInit, Call> op;

    bool isOperation() const {
        return std::holds_alternative<Operation>(op);
    }

    Operation& asOperation() {
        return std::get<Operation>(op);
    }

    Call& asCall() {
        return std::get<Call>(op);
    }

    Reg res() {
        if (auto op_ = std::get_if<Operation>(&op)) {
            return { op_->res.type, op_->res.id };
        }
        else if (auto call = std::get_if<Call>(&op)) {
            return call->res;
        }
        else if (auto init = std::get_if<ConstInit>(&op)) {
            return { init->type(), init->id };
        }
        assert(false);
    }
};

struct BasicBlock;
using BasicBlockPtr = BasicBlock*;

struct Terminator {
    enum Type {
        Jump,
        Branch,
        Return,
        Throw,
        None  // invalid jump type used for initialization
    };

    Type type;
    Reg value;
    BasicBlockPtr target;
    BasicBlockPtr other;

    static Terminator jump(BasicBlockPtr target) {
        return { Jump, {}, target, nullptr };
    }

    static Terminator branch(RValue condition, BasicBlockPtr target, BasicBlockPtr other) {
        return { Branch, condition.reg, target, other };
    }

    static Terminator ret() {
        return { Return, Reg::undefined(), nullptr, nullptr };
    }

    static Terminator retVal(RValue retValue) {
        return { Return, retValue.reg, nullptr, nullptr };
    }

    static Terminator throw_(RValue exception) {
        return { Throw, exception.reg, nullptr, nullptr };
    }

    static Terminator none() {
        return { None, {}, nullptr, nullptr };
    }
};

struct BasicBlock {
    std::list<Instruction> instructions;
    Terminator jump = Terminator::none();
};


struct Scope {
    std::map<Identifier, std::tuple<Reg, bool>> locals;

    LVRef addLocal(Identifier name, ValueType type, bool isConst) {
        auto reg = Reg::create(type);
        locals.emplace(name, std::forward_as_tuple(reg, isConst));
        return LVRef::direct(reg, isConst);
    }
    std::optional<LVRef> getLocal(Identifier name) {
        auto it = locals.find(name);
        if (it == locals.end()) {
            return std::nullopt;
        }
        auto& [ self, isConst ] = it->second;
        return LVRef::direct(self, isConst);
    }
    LVRef insertLocal(Identifier name, Reg reg, bool isConst) {
        locals.emplace(name, std::forward_as_tuple(reg, isConst));
        return LVRef::direct(reg, isConst);
    }
};

template<typename T>
struct ListPopper {
    std::forward_list<T>* list;

    ListPopper(std::forward_list<T>& list_): list(&list_) {}
    ListPopper(const ListPopper&) = delete;
    ListPopper(ListPopper&& other): list(other.list) { other.list = nullptr; }
    ListPopper& operator=(const ListPopper&) = delete;
    ListPopper& operator=(ListPopper&& other) {
        list = other.list;
        other.list = nullptr;
        return *this;
    }

    ~ListPopper() { list->pop_front(); }
};

struct Signature {
    std::vector<std::pair<Identifier, ValueType>> args;
    ValueType ret;
};
using SignaturePtr = std::shared_ptr<Signature>;

struct Function {
    BasicBlockPtr entry;
    std::list<std::unique_ptr<BasicBlock>> blocks;
    std::vector<Reg> args;
    ValueType ret;
    std::string _name;
    std::set<Identifier> requiredFunctions;

    std::string name() const { return _name; }
};

struct FunctionEmitter {
    const std::map<Identifier, SignaturePtr>& _otherSignatures;

    SignaturePtr signature;
    std::forward_list<Scope> scopes;
    std::list<std::pair<Identifier, Reg>> undefined;
    BasicBlockPtr activeBlock;
    std::forward_list<BasicBlockPtr> breakTargets;
    std::forward_list<BasicBlockPtr> continueTargets;

    Function data;

    FunctionEmitter(const std::map<Identifier, SignaturePtr>& otherSignatures):
        _otherSignatures(otherSignatures),
        scopes(1)
    {
        data.entry = createBlock();
        activeBlock = data.entry;
    }

    void setSignature(SignaturePtr sig) {
        if (signature) {
            throw std::runtime_error("Signature already set");
        }
        signature = sig;
        data.ret = sig->ret;
        for (const auto& [name, type] : sig->args) {
            auto ref = addLexical(name, type, false);
            data.args.push_back(ref.self);
        }
    }

    auto pushScope() { scopes.emplace_front(); return ListPopper(scopes); }
    LVRef addLexical(Identifier name, ValueType type, bool isConst) {
        if (scopes.front().locals.contains(name)) {
            throw std::runtime_error("Redeclaration of local variable: " + name);
        }
        return scopes.front().addLocal(name, type, isConst);
    }
    LVRef addUndefined(Identifier name, ValueType type) {
        auto reg = Reg::create(type);
        undefined.emplace_back(name, reg);
        return LVRef::direct(reg, false);
    }
    std::optional<LVRef> getLocal(Identifier name) {
        for (auto& scope : scopes) {
            if (auto ref = scope.getLocal(name); ref.has_value()) {
                return ref;
            }
        }
        return std::nullopt;
    }

    void emitInstruction(Instruction instruction) {
        activeBlock->instructions.push_back(instruction);
    }
    void emitInstruction(auto&& instruction) {
        emitInstruction(Instruction{ std::forward<decltype(instruction)>(instruction) });
    }

    void addRequiredFunction(Identifier name) {
        data.requiredFunctions.emplace(name);
    }
    SignaturePtr getSignature(Identifier name) {
        auto it = _otherSignatures.find(name);
        if (it == _otherSignatures.end()) {
            return nullptr;
        }
        return it->second;
    }

    BasicBlockPtr createBlock() {
        auto block = std::make_unique<BasicBlock>();
        data.blocks.push_back(std::move(block));
        return data.blocks.back().get();
    }
    BasicBlockPtr getActiveBlock() { return activeBlock; }
    void setActiveBlock(BasicBlockPtr block) { activeBlock = block; }

    auto pushBreakTarget(BasicBlockPtr block) {
        breakTargets.push_front(block);
        return ListPopper(breakTargets);
    }
    BasicBlockPtr getBreakTarget() {
        if (breakTargets.empty()) {
            throw std::runtime_error("No break target");
        }
        return breakTargets.front();
    }

    auto pushContinueTarget(BasicBlockPtr block) {
        continueTargets.push_front(block);
        return ListPopper(continueTargets);
    }
    BasicBlockPtr getContinueTarget() {
        if (continueTargets.empty()) {
            throw std::runtime_error("No continue target");
        }
        return continueTargets.front();
    }

    auto getEntry() const { return data.entry; }
    auto getEntry() { return data.entry; }

    void setEntry(BasicBlockPtr block) { data.entry = block; }

    Function output() {
        return std::move(data);
    }

    void setFunctionName(std::string name) {
        data._name = std::move(name);
    }
};


}  // namespace jac::cfg
