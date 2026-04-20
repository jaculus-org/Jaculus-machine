#pragma once

#include <cassert>
#include <cstdlib>
#include <forward_list>
#include <iostream>
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
#include <algorithm>

#include "tlssOpcode.h"


namespace jac::cfg::tless {


struct BasicBlock;
using BasicBlockPtr = BasicBlock*;
struct Instruction;
using InstructionPtr = Instruction*;
struct Terminator;
using TerminatorPtr = Terminator*;
struct BasicBlockBuilder;
using BasicBlockBuilderPtr = std::shared_ptr<BasicBlockBuilder>;


using RegId = int;
RegId newTmpId();


using Identifier = std::string;
using MemberIdentifier = std::variant<Identifier, int32_t>;


struct ValueType;
struct ValueTypeCompare {
    inline bool operator()(const ValueType& a, const ValueType& b) const;
};
using TypeSet = std::set<ValueType, ValueTypeCompare>;


struct ValueType {
    enum Primitive {  // TODO: extend
        Int32,
        Float64,
        Bool,
        String,
        Object,
        Void,
        Unknown
    };

    std::variant<Primitive, TypeSet> type;

private:
    ValueType(TypeSet types): type(types) {}
public:
    ValueType(Primitive prim): type(prim) {}

    static ValueType union_(TypeSet types) {
        TypeSet res;
        for (const auto& vt : types) {
            if (vt.isPrimitive()) {
                res.insert(vt.asPrimitive());
            }
            else {
                const auto& inner = std::get<TypeSet>(vt.type);
                for (const auto& t : inner) {
                    assert(t.isPrimitive());
                    res.insert(t.asPrimitive());
                }
            }
        }

        if (res.size() == 1) {
            return *res.begin();
        }

        return ValueType{ res };
    }

    bool isPrimitive() const {
        return std::holds_alternative<Primitive>(type);
    }

    Primitive asPrimitive() const {
        return std::get<Primitive>(type);
    }

    bool contains(const ValueType& other) const {
        if (isPrimitive()) {
            return other.isPrimitive() && asPrimitive() == other.asPrimitive();
        }
        const auto& thisSet = std::get<TypeSet>(type);
        if (other.isPrimitive()) {
            return thisSet.contains(other.asPrimitive());
        }
        const auto& otherSet = std::get<TypeSet>(other.type);
        return std::includes(thisSet.begin(), thisSet.end(), otherSet.begin(), otherSet.end(), ValueTypeCompare{});
    }
};


inline bool ValueTypeCompare::operator()(const ValueType& a, const ValueType& b) const {
    if (a.type.index() != b.type.index()) {
        return a.type.index() < b.type.index();
    }
    if (a.isPrimitive()) {
        return a.asPrimitive() < b.asPrimitive();
    }
    const auto& aSet = std::get<TypeSet>(a.type);
    const auto& bSet = std::get<TypeSet>(b.type);

    return std::lexicographical_compare(
        aSet.begin(), aSet.end(),
        bSet.begin(), bSet.end(),
        ValueTypeCompare{}
    );
}

struct RegInfo {
    RegId _id;

    InstructionPtr assign = nullptr;
    std::variant<std::monostate, InstructionPtr, BasicBlockPtr> use;  // used in an instruction or in a terminator

    RegInfo(RegId id_): _id(id_) {}
};

struct Reg : public std::shared_ptr<RegInfo> {
    static Reg createTmp() {
        return { std::make_shared<RegInfo>(newTmpId()) };
    }
    static Reg createVoid() {
        return { nullptr };
    }

    void setAssign(InstructionPtr instr) {
        assert(!this->get() || !this->get()->assign);
        this->get()->assign = instr;
    }

    void setUse(auto usePtr) {
        assert(!this->get() || std::holds_alternative<std::monostate>(this->get()->use));
        this->get()->use = usePtr;
    }

    RegId id() const {
        if (!this->get()) {
            return 0;
        }
        return this->get()->_id;
    }

    bool void_() const {
        return id() == 0;
    }
};

/*
possible source:
    - literal
    - operator/call result
    - conversion from LVRef
Local to current basic block
*/
struct RValue {
    int intermId;
};

/*
possible source:
    - variable -> varId
    - member access -> {objectReg, accessorReg}
*/
struct LVRef {
    std::variant<int, std::pair<RValue, RValue>> self;  // varId or {object, accessor}
    bool _const = false;

    bool isMember() const {
        return std::holds_alternative<std::pair<RValue, RValue>>(self);
    }

    bool isConst() const {
        return _const;
    }

    int varId() const {
        assert(!isMember());
        return std::get<int>(self);
    }

    std::pair<RValue, RValue> member() const {
        assert(isMember());
        return std::get<std::pair<RValue, RValue>>(self);
    }

    static LVRef direct(int varId, bool isConst) {
        return LVRef(varId, isConst);
    }

    static LVRef mbr(RValue self_, RValue member_) {
        return LVRef(self_, member_);
    }

    LVRef(): self({ 0 }) {}
private:
    LVRef(int varId, bool isConst): self(varId), _const(isConst) {}
    LVRef(RValue self_, RValue member_): self(std::make_pair(self_, member_)), _const(false) {}
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

    std::vector<Reg> args;
    std::vector<Reg> res;
};

struct PoolConst {
    int id;
};

struct ConstInit {
    Reg reg;
    std::variant<int32_t, double, bool, std::string, PoolConst> value;
};

struct Instruction {
    std::variant<Operation, ConstInit> op;
    BasicBlockPtr parentBlock = nullptr;

    bool isOperation() const {
        return std::holds_alternative<Operation>(op);
    }

    Operation& asOperation() {
        return std::get<Operation>(op);
    }

    ConstInit& asConstInit() {
        return std::get<ConstInit>(op);
    }

    std::vector<Reg> res() {  // return view
        if (auto op_ = std::get_if<Operation>(&op)) {
            return op_->res;
        }
        else if (auto init = std::get_if<ConstInit>(&op)) {
            return { init->reg };
        }
        assert(false);
    }
};


struct Terminator {
    enum Type {
        Jump,
        Branch,
        Return,
        Throw,
        None  // invalid terminator
    };

    Type type;
    Reg value;
    BasicBlockPtr target;
    BasicBlockPtr other;
    std::vector<Reg> args;

    static Terminator jump(BasicBlockPtr target, std::vector<Reg> args = {}) {
        return { Jump, {}, target, nullptr, std::move(args) };
    }

    static Terminator branch(Reg condition, BasicBlockPtr target, BasicBlockPtr other, std::vector<Reg> args = {}) {
        return { Branch, condition, target, other, std::move(args) };
    }

    static Terminator ret() {
        return { Return, Reg::createVoid(), nullptr, nullptr };
    }

    static Terminator retVal(Reg retValue) {
        return { Return, retValue, nullptr, nullptr };
    }

    static Terminator throw_(Reg exception) {
        return { Throw, exception, nullptr, nullptr };
    }

    static Terminator none(std::vector<Reg> args = {}) {
        return { None, {}, nullptr, nullptr, std::move(args) };
    }
};

struct BasicBlock {
    Terminator terminator = Terminator::none();
    std::vector<std::unique_ptr<Instruction>> instructions;
    std::set<BasicBlockPtr> predecessors;
    std::vector<Reg> args;
};

struct VarToRegMap {
    std::map<int, Reg> data;

    Reg get(int var) const {
        auto it = data.find(var);
        if (it == data.end()) {
            throw std::runtime_error("Variable not mapped to register");
        }
        return it->second;
    }

    void update(int var, Reg reg) {
        data[var] = reg;
    }

    void update(const VarToRegMap& other) {
        for (const auto& [ var, reg ] : other.data) {
            data[var] = reg;
        }
    }

    std::vector<int> vars() const {
        std::vector<int> res;
        res.reserve(data.size());
        for (const auto& [ var, reg ] : data) {
            res.push_back(var);
        }
        return res;
    }

    std::vector<int> diff(const VarToRegMap& other) const {
        std::vector<int> res;
        for (const auto& [ var, reg ] : data) {
            auto it = other.data.find(var);
            if (it == other.data.end() || it->second.id() != reg.id()) {
                res.push_back(var);
            }
        }
        return res;
    }

    std::vector<Reg> getRegs(const std::vector<int>& vars) const {
        std::vector<Reg> res;
        res.reserve(vars.size());
        for (const auto& var : vars) {
            res.push_back(get(var));
        }
        return res;
    }

    std::vector<Reg> getAllRegs() const {
        std::vector<Reg> res;
        res.reserve(data.size());
        for (const auto& [ var, reg ] : data) {
            res.push_back(reg);
        }
        return res;
    }

    std::vector<std::pair<int, Reg>> getAllVarsExcept(const std::vector<int>& vars) const {
        std::set<int> varSet(vars.begin(), vars.end());
        std::vector<std::pair<int, Reg>> res;
        for (const auto& [ var, reg ] : data) {
            if (!varSet.contains(var)) {
                res.emplace_back(var, reg);
            }
        }
        return res;
    }

    static VarToRegMap remapVars(const auto& vars) {
        VarToRegMap res;
        for (const auto& var : vars) {
            res.data[var] = Reg::createTmp();
        }
        return res;
    }
};

struct BasicBlockBuilder {
    BasicBlockPtr block;
    VarToRegMap varToReg;
    std::vector<Reg> interm;  // vector of intermediate values in the block - in order of "evaluation stack" (for easier renaming when splitting blocks)
                              // size of interm should be same on entry and exit of block
private:
    void addPredecessor(BasicBlockPtr pred) {
        block->predecessors.insert(pred);
    }
    void removePredecessor(BasicBlockPtr pred) {
        block->predecessors.erase(pred);
    }
public:
    BasicBlockBuilder(BasicBlockPtr block_): block(block_) {}

    auto& term() {
        return block->terminator;
    }

    ~BasicBlockBuilder() {
        return;
        assert(block == nullptr);
    }

    const std::set<BasicBlockPtr>& predecessors = block->predecessors;
    const std::vector<std::unique_ptr<Instruction>>& instructions = block->instructions;
    const std::vector<Reg>& args = block->args;

    void pushInstruction(Instruction instr) {
        instr.parentBlock = block;
        block->instructions.push_back(std::make_unique<Instruction>(std::move(instr)));
        auto ptr = block->instructions.back().get();
        if (auto op = std::get_if<Operation>(&ptr->op)) {
            for (auto& reg : op->args) {
                reg.setUse(ptr);
            }
            for (auto& reg : op->res) {
                reg.setAssign(ptr);
            }
        }
        else if (const auto init = std::get_if<ConstInit>(&ptr->op)) {
            init->reg.setAssign(ptr);
        }
    }

    void fixTerminatorRegUses() {
        for (auto& reg : term().args) {
            reg.setUse(block);
        }
        if (!term().value.void_()) {
            term().value.setUse(block);
        }
    }

    auto getFullArgs(const auto& vars, const auto& addArgs) {
        std::vector<Reg> fullArgs;
        fullArgs.reserve(vars.size() + interm.size() + addArgs.size());
        {
            auto regs = varToReg.getRegs(vars);
            fullArgs.insert(fullArgs.end(), regs.begin(), regs.end());
        }
        fullArgs.insert(fullArgs.end(), interm.begin(), interm.end());
        fullArgs.insert(fullArgs.end(), addArgs.begin(), addArgs.end());
        return fullArgs;
    }

    void setJump(BasicBlockBuilder& target, std::vector<Reg> addArgs = {}) {
        assert(block->terminator.type == Terminator::None);
        auto vars = target.varToReg.vars();
        assert(target.args.size() == (vars.size() + addArgs.size() + interm.size()));

        block->terminator = Terminator::jump(target.block, getFullArgs(vars, addArgs));
        target.addPredecessor(block);
        fixTerminatorRegUses();
    }

    void setBranch(Reg condition, BasicBlockBuilder& trueBlock, BasicBlockBuilder& falseBlock, std::vector<Reg> addArgs = {}) {
        assert(block->terminator.type == Terminator::None);
        auto trueVars = trueBlock.varToReg.vars();

        std::cout << "true args: " << trueBlock.args.size() << "; used args:" << (trueVars.size() + addArgs.size() + interm.size()) << std::endl;
        assert(trueVars == falseBlock.varToReg.vars());
        assert(trueBlock.args.size() == falseBlock.args.size());
        assert(trueBlock.args.size() == (trueVars.size() + addArgs.size() + interm.size()));

        block->terminator = Terminator::branch(condition, trueBlock.block, falseBlock.block, getFullArgs(trueVars, addArgs));
        trueBlock.addPredecessor(block);
        falseBlock.addPredecessor(block);
        fixTerminatorRegUses();
    }

    void setRetVal(Reg retValue) {
        assert(block->terminator.type == Terminator::None);
        block->terminator = Terminator::retVal(retValue);
        fixTerminatorRegUses();
    }

    void setReturn() {
        assert(block->terminator.type == Terminator::None);
        block->terminator = Terminator::ret();
    }

    void setThrow(Reg exception) {
        assert(block->terminator.type == Terminator::None);
        block->terminator = Terminator::throw_(exception);
        fixTerminatorRegUses();
    }

    RValue pushInterm(Reg reg) {
        interm.push_back(reg);
        return { static_cast<int>(interm.size() - 1) };
    }

    Reg popInterm(RValue val) {
        std::cout << "Popping interm " << val.intermId << "; interm size: " << interm.size() << std::endl;
        assert(val.intermId == static_cast<int>(interm.size() - 1));
        auto reg = interm.back();
        interm.pop_back();
        return reg;
    }
};


struct Variable {
    int id;
    bool isConst;
};


struct Scope {
    std::map<Identifier, Variable> locals;

    Variable addLocal(Identifier name, bool isConst) {
        static int id = 0;
        auto [it, succ] = locals.emplace(name, Variable{ ++id, isConst });
        if (!succ) {
            throw std::runtime_error("Redeclaration of local variable: " + name);
        }
        return it->second;
    }
    std::optional<Variable> getLocal(Identifier name) {
        auto it = locals.find(name);
        if (it == locals.end()) {
            return std::nullopt;
        }
        return it->second;
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
    std::vector<Identifier> args;
    std::vector<Identifier> closureVars;
    std::vector<Identifier> globalVars;
};
using SignaturePtr = std::shared_ptr<Signature>;

struct Function;

struct Constant {
    std::variant<std::unique_ptr<Function>> value;
};


struct Function {
    BasicBlockPtr entry;
    std::list<std::unique_ptr<BasicBlock>> blocks;
    std::string _name;
    std::vector<Constant> constPool;

    std::string name() const { return _name; }
};

struct FunctionEmitter {
    FunctionEmitter* parent;

    SignaturePtr signature;
    std::forward_list<Scope> scopes;
    Scope* argsScope;
    BasicBlockBuilderPtr activeBlock;
    std::forward_list<std::pair<BasicBlockBuilderPtr, std::vector<int>*>> breakTargets;     // bbl, list of vars
    std::forward_list<std::pair<BasicBlockBuilderPtr, std::vector<int>*>> continueTargets;

    Function data;

    FunctionEmitter(FunctionEmitter* parent_):
        parent(parent_),
        scopes(1)
    {
        auto block = createBlock();
        data.entry = block->block;
        setActiveBlock(block);
    }

    void setSignature(SignaturePtr sig) {
        if (signature) {
            throw std::runtime_error("Signature already set");
        }
        signature = sig;
        for (size_t argIndex = 0; argIndex < sig->args.size(); ++argIndex) {
            const auto& name = sig->args[argIndex];
            RValue indexVal = emitConst(static_cast<int32_t>(argIndex));
            auto var = scopes.front().addLocal(name, false);
            Reg reg = Reg::createTmp();
            getActiveBlock()->varToReg.data[var.id] = reg;
            emitInstruction(Operation{
                .op = Opcode::GetArgRef,
                .args = { popInterm(indexVal) },
                .res = { reg }
            });
        }
        for (size_t closureIndex = 0; closureIndex < sig->closureVars.size(); ++closureIndex) {
            const auto& name = sig->closureVars[closureIndex];
            RValue indexVal = emitConst(static_cast<int32_t>(closureIndex));
            auto var = scopes.front().addLocal(name, false);
            Reg reg = Reg::createTmp();
            getActiveBlock()->varToReg.data[var.id] = reg;
            emitInstruction(Operation{
                .op = Opcode::GetClosureRef,
                .args = { popInterm(indexVal) },
                .res = { reg }
            });
        }
        for (const auto & name : sig->globalVars) {
            RValue nameVal = emitConst(name);
            auto var = scopes.front().addLocal(name, false);
            Reg reg = Reg::createTmp();
            getActiveBlock()->varToReg.data[var.id] = reg;
            emitInstruction(Operation{
                .op = Opcode::GetGlobalRef,
                .args = { popInterm(nameVal) },
                .res = { reg }
            });
        }
        argsScope = &scopes.front();
    }

    Reg createSlot() {
        auto reg = Reg::createTmp();
        emitInstruction(Operation{
            .op = Opcode::CreateSlot,
            .args = {},
            .res = { reg }
        });
        return reg;
    }

    Reg createGlobalSlot(const Identifier& name) {
        auto reg = Reg::createTmp();
        RValue nameVal = emitConst(name);
        emitInstruction(Operation{
            .op = Opcode::CreateGlobalSlot,
            .args = { popInterm(nameVal) },
            .res = { reg }
        });
        return reg;
    }

    void enterScope() { scopes.emplace_front(); }
    void exitScope(bool killVars) {
        std::cout << "Exiting scope with " << scopes.front().locals.size() << " locals\n";
        for (const auto& [ name, var ] : scopes.front().locals) {
            auto it = getActiveBlock()->varToReg.data.find(var.id);
            std::cout << "  var " << name << " (id " << var.id << ") mapped to reg " << (it != getActiveBlock()->varToReg.data.end() ? std::to_string(it->second.id()) : "none") << "\n";
            if (killVars) {
                assert(it != getActiveBlock()->varToReg.data.end());
                emitInstruction(Operation{
                    .op = Opcode::Kill,
                    .args = { it->second },
                    .res = { }
                });
                getActiveBlock()->varToReg.data.erase(it);
            }
            else {
                assert(it == getActiveBlock()->varToReg.data.end());
            }
        }
        scopes.pop_front();
    }

    LVRef addLexical(Identifier name, bool isConst) {
        Reg reg = createSlot();
        auto var = scopes.front().addLocal(name, isConst);
        getActiveBlock()->varToReg.data[var.id] = reg;
        return LVRef::direct(var.id, isConst);
    }

    LVRef addGlobal(Identifier name, bool isConst) {
        Reg reg = createGlobalSlot(name);
        auto var = scopes.front().addLocal(name, isConst);
        getActiveBlock()->varToReg.data[var.id] = reg;
        return LVRef::direct(var.id, isConst);
    }

    std::optional<LVRef> getVar(Identifier name) {
        for (auto& scope : scopes) {
            if (auto var = scope.getLocal(name); var.has_value()) {
                return LVRef::direct(var->id, var->isConst);
            }
        }
        return std::nullopt;
    }

    void emitInstruction(Instruction instruction) {
        activeBlock->pushInstruction(instruction);
    }
    void emitInstruction(auto&& instruction) {
        emitInstruction(Instruction{ std::forward<decltype(instruction)>(instruction) });
    }

    [[nodiscard]] RValue emitConst(auto value) {
        auto reg = Reg::createTmp();
        emitInstruction(ConstInit{
            .reg = reg,
            .value = value
        });
        return pushInterm(reg);
    }
    [[nodiscard]] RValue emitUndefined() {
        auto reg = Reg::createTmp();
        emitInstruction(Operation{
            .op = Opcode::CreateUndefined,
            .args = { },
            .res = { reg }
        });
        return pushInterm(reg);
    }

    RValue pushInterm(Reg reg) {
        return getActiveBlock()->pushInterm(reg);
    }
    Reg popInterm(RValue val) {
        return getActiveBlock()->popInterm(val);
    }
private:
    BasicBlockPtr createBlockInternal() {
        auto block = std::make_unique<BasicBlock>();
        auto ptr = block.get();
        data.blocks.push_back(std::move(block));
        return ptr;
    }
public:
    BasicBlockBuilderPtr createBlock() {
        return std::make_shared<BasicBlockBuilder>(createBlockInternal());
    }

    BasicBlockBuilderPtr createBlock(const VarToRegMap& varToReg, int extraArgs, int inheritedInterm = 0) {
        return createBlock(varToReg.vars(), extraArgs, inheritedInterm);
    }

    BasicBlockBuilderPtr createBlock(const auto& vars, int extraArgs, int inheritedInterm = 0) {
        std::cout << "Creating block with vars: ";
        for (const auto& var : vars) {
            std::cout << var << " ";        }
        std::cout << "and extra args: " << extraArgs;
        std::cout << "and interms: " << inheritedInterm << std::endl;
        auto block = std::make_shared<BasicBlockBuilder>(createBlockInternal());
        block->varToReg = VarToRegMap::remapVars(vars);

        block->block->args.reserve(vars.size() + extraArgs + inheritedInterm);
        {
            auto regs = block->varToReg.getRegs(vars);
            block->block->args.insert(block->block->args.end(), regs.begin(), regs.end());
        }
        for (int i = 0; i < inheritedInterm; i++) {
            auto reg = Reg::createTmp();
            block->block->args.push_back(reg);
            block->pushInterm(reg);
        }
        for (int i = 0; i < extraArgs; i++) {
            block->block->args.push_back(Reg::createTmp());
        }

        return block;
    }

    BasicBlockBuilderPtr getActiveBlock() { return activeBlock; }
    void setActiveBlock(BasicBlockBuilderPtr block) { activeBlock = block; }

    VarToRegMap createVarToReg() {
        VarToRegMap newMap;
        for (auto& scope : scopes) {
            for (const auto& loc : scope.locals) {
                newMap.data[loc.second.id] = getActiveBlock()->varToReg.get(loc.second.id);
            }
        }
        return newMap;
    }

    auto pushBreakTarget(BasicBlockBuilderPtr block, std::vector<int>* vars) {
        breakTargets.emplace_front(block, vars);
        return ListPopper(breakTargets);
    }
    auto getBreakTarget() {
        if (breakTargets.empty()) {
            throw std::runtime_error("No break target");
        }
        return breakTargets.front();
    }

    auto pushContinueTarget(BasicBlockBuilderPtr block, std::vector<int>* vars) {
        continueTargets.emplace_front(block, vars);
        return ListPopper(continueTargets);
    }
    auto getContinueTarget() {
        if (continueTargets.empty()) {
            throw std::runtime_error("No continue target");
        }
        return continueTargets.front();
    }

    auto getEntry() const { return data.entry; }
    auto getEntry() { return data.entry; }

    void setEntry(BasicBlockPtr block) { data.entry = block; }

    PoolConst addPoolConstant(auto&& constant) {
        data.constPool.push_back({ std::move(constant) });
        return { static_cast<int>(data.constPool.size() - 1) };
    }

    Function output() {
        return std::move(data);
    }

    void setFunctionName(std::string name) {
        data._name = std::move(name);
    }
};


}  // namespace jac::cfg
