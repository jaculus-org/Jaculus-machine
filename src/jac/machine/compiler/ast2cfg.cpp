#include "ast2cfg.h"
#include "ast.h"
#include "cfg.h"
#include "opcode.h"
#include <ranges>


namespace jac::cfg {


enum class ShortCircuitKind {
    And,
    Or
};

const std::unordered_map<ast::BinaryExpression::Op, ShortCircuitKind> shortCircuitOps = {
    { ast::BinaryExpression::Op::LogAnd, ShortCircuitKind::And },
    { ast::BinaryExpression::Op::LogOr, ShortCircuitKind::Or }
};

const std::unordered_map<ast::BinaryExpression::Op, Opcode> binaryOps = {
    { ast::BinaryExpression::Op::BitOr, Opcode::BitOr },
    { ast::BinaryExpression::Op::BitXor, Opcode::BitXor },
    { ast::BinaryExpression::Op::BitAnd, Opcode::BitAnd },
    { ast::BinaryExpression::Op::Eq, Opcode::Eq },
    { ast::BinaryExpression::Op::Neq, Opcode::Neq },
    { ast::BinaryExpression::Op::StrictEq, Opcode::StrictEq },
    { ast::BinaryExpression::Op::StrictNeq, Opcode::StrictNeq },
    { ast::BinaryExpression::Op::Lt, Opcode::Lt },
    { ast::BinaryExpression::Op::Lte, Opcode::Lte },
    { ast::BinaryExpression::Op::Gt, Opcode::Gt },
    { ast::BinaryExpression::Op::Gte, Opcode::Gte },
    // { "in", ... },
    // { "instanceof", ... },
    { ast::BinaryExpression::Op::LShift, Opcode::LShift },
    { ast::BinaryExpression::Op::RShift, Opcode::RShift },
    { ast::BinaryExpression::Op::URShift, Opcode::URShift },
    { ast::BinaryExpression::Op::Add, Opcode::Add },
    { ast::BinaryExpression::Op::Sub, Opcode::Sub },
    { ast::BinaryExpression::Op::Mul, Opcode::Mul },
    { ast::BinaryExpression::Op::Div, Opcode::Div },
    { ast::BinaryExpression::Op::Rem, Opcode::Rem },
    { ast::BinaryExpression::Op::Exp, Opcode::Pow }
};

const std::unordered_map<ast::Assignment::Op, ShortCircuitKind> shortCircuitAssignmentOps = {
    { ast::Assignment::Op::LogAndAssign, ShortCircuitKind::And },
    { ast::Assignment::Op::LogOrAssign, ShortCircuitKind::Or }
};

const std::unordered_map<ast::Assignment::Op, Opcode> arithAssignmentOps = {
    { ast::Assignment::Op::AddAssign, Opcode::Add },
    { ast::Assignment::Op::SubAssign, Opcode::Sub },
    { ast::Assignment::Op::MulAssign, Opcode::Mul },
    { ast::Assignment::Op::DivAssign, Opcode::Div },
    { ast::Assignment::Op::RemAssign, Opcode::Rem },
    { ast::Assignment::Op::BitAndAssign, Opcode::BitAnd },
    { ast::Assignment::Op::BitOrAssign, Opcode::BitOr },
    { ast::Assignment::Op::BitXorAssign, Opcode::BitXor },
    { ast::Assignment::Op::LShiftAssign, Opcode::LShift },
    { ast::Assignment::Op::RShiftAssign, Opcode::RShift },
    { ast::Assignment::Op::URShiftAssign, Opcode::URShift }
};

const std::unordered_map<ast::UnaryExpression::Op, Opcode> unaryOps = {
    { ast::UnaryExpression::Op::LogNot, Opcode::BoolNot },
    { ast::UnaryExpression::Op::BitNot, Opcode::BitNot },
    { ast::UnaryExpression::Op::Plus, Opcode::UnPlus },
    { ast::UnaryExpression::Op::Minus, Opcode::UnMinus },
    { ast::UnaryExpression::Op::Await, Opcode::Await },
    // "typeof",
    // "void",
    // "delete",
};

void handleException(Reg ex, Reg hadEx, FunctionEmitter& func);
[[nodiscard]] Reg emitCheckedOp(Opcode op, std::vector<Reg> args, FunctionEmitter& func);
void emitCheckedVoidOp(Opcode op, std::vector<Reg> args, FunctionEmitter& func);

[[nodiscard]] RValue materialize(LVRef lv, FunctionEmitter& func) {
    if (!lv.isMember()) {
        Reg v = Reg::createTmp();
        auto newSlotReg = Reg::createTmp();
        auto ex = Reg::createTmp();
        auto hadEx = Reg::createTmp();
        auto varId = lv.varId();

        func.emitInstruction(Operation{
            .op = Opcode::Load,
            .args = { func.getActiveBlock()->varToReg.get(lv.varId()) },
            .res = { v, newSlotReg, ex, hadEx }
        });
        func.getActiveBlock()->varToReg.update(varId, newSlotReg);
        auto resR = func.pushInterm(v);
        handleException(ex, hadEx, func);
        return resR;
    }

    auto [obj, acc] = lv.member();

    auto accReg = func.popInterm(acc);
    auto objReg = func.popInterm(obj);
    Reg res = emitCheckedOp(Opcode::GetMember, { objReg, accReg }, func);

    return func.pushInterm(res);
}

[[nodiscard]] RValue materialize(Value val, FunctionEmitter& func) {
    if (val.isRValue()) {
        return val.asRValue();
    }
    return materialize(val.asLVRef(), func);
}


std::pair<Reg, Reg> emitDup(Reg val, FunctionEmitter& func) {
    auto res1 = Reg::createTmp();
    auto res2 = Reg::createTmp();

    func.emitInstruction(Operation{
        .op = Opcode::Dup,
        .args = { val },
        .res = { res1, res2 }
    });

    return { res1, res2 };
}

void emitKill(Reg val, FunctionEmitter& func) {
    func.emitInstruction(Operation{
        .op = Opcode::Kill,
        .args = { val },
        .res = { }
    });
}

[[nodiscard]] std::pair<LVRef, LVRef> dupMemberTarget(LVRef target, FunctionEmitter& func) {
    auto [obj, acc] = target.member();

    auto accReg = func.popInterm(acc);
    auto objReg = func.popInterm(obj);

    auto [obj1, obj2] = emitDup(objReg, func);
    auto [acc1, acc2] = emitDup(accReg, func);

    auto obj2R = func.pushInterm(obj2);
    auto acc2R = func.pushInterm(acc2);
    auto obj1R = func.pushInterm(obj1);
    auto acc1R = func.pushInterm(acc1);

    return { LVRef::mbr(obj1R, acc1R), LVRef::mbr(obj2R, acc2R) };
}

void emitKillVars(const auto& vars, FunctionEmitter& func) {
    for (const auto& [var, reg] : vars) {
        emitKill({ reg }, func);
        func.getActiveBlock()->varToReg.data.erase(var);
    }
}

void emitKillLiveVars(FunctionEmitter& func) {
    for (const auto& [varId, reg] : func.getActiveBlock()->varToReg.data) {
        emitKill({ reg }, func);
    }
    func.getActiveBlock()->varToReg.data.clear();
}

void handleException(Reg ex, Reg hadEx, FunctionEmitter& func) {
    auto exR = func.pushInterm(ex);

    auto trueBlock = func.createBlock(func.getActiveBlock()->varToReg, 0, func.getActiveBlock()->interm.size());
    auto falseBlock = func.createBlock(func.getActiveBlock()->varToReg, 0, func.getActiveBlock()->interm.size());
    func.getActiveBlock()->setBranch(hadEx, *trueBlock, *falseBlock);

    func.setActiveBlock(trueBlock);
    auto toThrow = func.popInterm(exR);
    for (auto& reg : std::ranges::reverse_view(trueBlock->interm)) {
        emitKill({ reg }, func);
    }
    emitKillLiveVars(func);
    func.emitThrow(toThrow);

    func.setActiveBlock(falseBlock);
    emitKill(func.popInterm(exR), func);
}

// Emits `op` and routes its (ex, hadEx) outputs through handleException.
// Returns the primary result as an ordinary Reg.
[[nodiscard]] Reg emitCheckedOp(Opcode op, std::vector<Reg> args, FunctionEmitter& func) {
    Reg res = Reg::createTmp();
    Reg ex = Reg::createTmp();
    Reg hadEx = Reg::createTmp();
    func.emitInstruction(Operation{
        .op = op,
        .args = std::move(args),
        .res = { res, ex, hadEx }
    });
    auto resR = func.pushInterm(res);
    handleException(ex, hadEx, func);
    return func.popInterm(resR);
}

// Same as emitCheckedOp, for operations with no primary result value (e.g. SetMember).
void emitCheckedVoidOp(Opcode op, std::vector<Reg> args, FunctionEmitter& func) {
    Reg ex = Reg::createTmp();
    Reg hadEx = Reg::createTmp();
    func.emitInstruction(Operation{
        .op = op,
        .args = std::move(args),
        .res = { ex, hadEx }
    });
    handleException(ex, hadEx, func);
}

[[nodiscard]] RValue emitAsRV(const ast::Expression& node, FunctionEmitter& func);
[[nodiscard]] LVRef emitAsLV(const ast::Expression& node, FunctionEmitter& func);


void emitAssign(LVRef target, Reg value, FunctionEmitter& func) {
    if (target.isMember()) {
        auto [obj, acc] = target.member();

        auto accReg = func.popInterm(acc);
        auto objReg = func.popInterm(obj);

        emitCheckedVoidOp(Opcode::SetMember, { objReg, accReg, value }, func);
    }
    else {
        auto newSlotReg = Reg::createTmp();
        auto ex = Reg::createTmp();
        auto hadEx = Reg::createTmp();
        auto varId = target.varId();
        func.emitInstruction(Operation{
            .op = Opcode::Store,
            .args = { value, func.getActiveBlock()->varToReg.get(varId) },
            .res = { newSlotReg, ex, hadEx }
        });
        func.getActiveBlock()->varToReg.update(varId, newSlotReg);
        handleException(ex, hadEx, func);
    }
}

[[nodiscard]] RValue emitAssignAndKeep(LVRef target, Reg toAssign, Reg toKeep, FunctionEmitter& func) {
    if (target.isMember()) {
        auto [obj, acc] = target.member();
        auto accReg = func.popInterm(acc);
        auto objReg = func.popInterm(obj);
        auto kept = func.pushInterm(toKeep);
        emitCheckedVoidOp(Opcode::SetMember, { objReg, accReg, toAssign }, func);
        return kept;
    }

    auto toKeepR = func.pushInterm(toKeep);
    emitAssign(target, toAssign, func);
    return toKeepR;
}


[[nodiscard]] RValue emitBinaryArithmetic(RValue lhs, RValue rhs, Opcode op, FunctionEmitter& func) {
    auto rhsReg = func.popInterm(rhs);
    auto lhsReg = func.popInterm(lhs);

    // Strict (in)equality never invokes ToPrimitive/user code, so it cannot throw.
    if (op == Opcode::StrictEq || op == Opcode::StrictNeq) {
        Reg res = Reg::createTmp();
        func.emitInstruction(Operation{
            .op = op,
            .args = { lhsReg, rhsReg },
            .res = { res }
        });
        return func.pushInterm(res);
    }

    Reg res = emitCheckedOp(op, { lhsReg, rhsReg }, func);
    return func.pushInterm(res);
}


template<typename F, typename G>
[[nodiscard]] RValue emitShortCircuit(RValue lhs, std::vector<Reg> passthrough, F evalRhs, G processRes, ShortCircuitKind kind, FunctionEmitter& func) {
    static_assert(std::is_same_v<decltype(evalRhs()), RValue>, "evalRhs must return RValue");
    static_assert(std::is_invocable_v<G, RValue, bool, std::vector<RValue>>);

    auto preBlock = func.getActiveBlock();
    int extraArgCount = static_cast<int>(passthrough.size()) + 1;
    auto postBlock = func.createBlock(preBlock->varToReg, 1, preBlock->interm.size() - 1);
    auto skipBlock = func.createBlock(preBlock->varToReg, extraArgCount, preBlock->interm.size() - 1);  // target when expression short circuits
    auto elseBlock = func.createBlock(preBlock->varToReg, extraArgCount, preBlock->interm.size() - 1);  // target otherwise

    auto lhs1 = Reg::createTmp();
    auto lhs2 = Reg::createTmp();
    func.emitInstruction(Operation{
        .op = Opcode::Dup,
        .args = { func.popInterm(lhs) },
        .res = { lhs1, lhs2 }
    });

    std::vector<Reg> extraArgs = passthrough;
    extraArgs.push_back(lhs1);
    if (kind == ShortCircuitKind::Or) {
        preBlock->setBranch(lhs2, *skipBlock, *elseBlock, extraArgs);
    }
    else if (kind == ShortCircuitKind::And) {
        preBlock->setBranch(lhs2, *elseBlock, *skipBlock, extraArgs);
    }

    auto passthroughArgsOf = [&](const BasicBlockBuilderPtr& block) {
        auto n = block->args.size();
        std::vector<Reg> regs;
        regs.reserve(passthrough.size());
        for (size_t i = 0; i < passthrough.size(); i++) {
            regs.push_back(block->args[n - extraArgCount + i]);
        }
        return regs;
    };

    func.setActiveBlock(skipBlock);
    {
        for (Reg r : passthroughArgsOf(skipBlock)) {
            emitKill(r, func);
        }
        RValue lhs3 = processRes({ func.pushInterm(skipBlock->args.back()) }, true, {});
        func.getActiveBlock()->setJump(*postBlock, { func.popInterm(lhs3) });
    }

    func.setActiveBlock(elseBlock);
    {
        emitKill({ elseBlock->args.back() }, func);

        std::vector<RValue> passthroughVals;
        for (Reg r : passthroughArgsOf(elseBlock)) {
            passthroughVals.push_back(func.pushInterm(r));
        }

        RValue rhs = evalRhs();
        RValue rhs1 = processRes(rhs, false, passthroughVals);
        func.getActiveBlock()->setJump(*postBlock, { func.popInterm(rhs1) });
    }

    func.setActiveBlock(postBlock);

    return { func.pushInterm(postBlock->args.back()) };
}


[[nodiscard]] RValue emitCallObj(Value obj, ast::Arguments* args_, FunctionEmitter& func, bool isConstructor) {
    std::vector<RValue> args;
    Opcode op;
    if (args_) {
        args.reserve(args_->argCount() + 1 + (isConstructor ? 0 : 1));
    }
    else {
        args.reserve(1 + (isConstructor ? 0 : 1));
    }

    if (isConstructor) {  // ctor (`this` created by runtime)
        args.push_back(materialize(obj, func));
        op = Opcode::Construct;
    }
    else if (!obj.isRValue() && obj.asLVRef().isMember()) {  // method, obj
        auto [this_, ident] = obj.asLVRef().member();

        auto identReg = func.popInterm(ident);
        auto thisReg = func.popInterm(this_);

        Reg thisCopy1 = Reg::createTmp();
        Reg thisCopy2 = Reg::createTmp();
        func.emitInstruction(Operation{
            .op = Opcode::Dup,
            .args = { thisReg },
            .res = { thisCopy1, thisCopy2 }
        });

        auto receiver = func.pushInterm(thisCopy2);
        Reg methodReg = emitCheckedOp(Opcode::GetMember, { thisCopy1, identReg }, func);

        args.push_back(receiver);
        args.push_back(func.pushInterm(methodReg));
        op = Opcode::CallMethod;
    }
    else {  // function
        args.push_back(materialize(obj, func));
        op = Opcode::Call;
    }

    Reg res = { Reg::createTmp() };

    if (args_) {
        if (args_->spread()) {
            throw IRGenError("Spread arguments are not supported");
        }
        for (size_t i = 0; i < args_->argCount(); i++) {
            auto arg = emitAsRV(*args_->argGet(i), func);
            args.push_back(arg);
        }
    }

    std::vector<Reg> argRegs(args.size());
    for (size_t i = args.size(); i != 0; i--) {
        argRegs[i - 1] = func.popInterm(args[i - 1]);
    }

    auto ex = Reg::createTmp();
    auto hadEx = Reg::createTmp();
    func.emitInstruction(Operation{
        .op = op,
        .args = std::move(argRegs),
        .res = { res, ex, hadEx }
    });

    auto resR = func.pushInterm(res);
    handleException(ex, hadEx, func);
    return resR;
}


[[nodiscard]] LVRef mbrAccess(RValue obj, RValue acc, FunctionEmitter& func) {
    return LVRef::mbr(obj, acc);
}

[[nodiscard]] LVRef emitAsLV(const ast::Expression& node, FunctionEmitter& func) {
    using Types = TypeList<ast::Identifier, ast::MemberAccessExpression, ast::Expression>;

    return ast::visitNode<Types>(node, overloaded{
        [&](const ast::Identifier& expr) -> LVRef {
            auto local = func.getVar(expr.name);
            if (!local) {
                throw IRGenError("Identifier referenced before declaration (" + expr.name + ")");
            }
            return *local;
        },
        [&](const ast::MemberAccessExpression& expr) -> LVRef {
            RValue obj = emitAsRV(*expr.object(), func);
            RValue accR = emitAsRV(*expr.property(), func);

            return { mbrAccess(obj, accR, func) };
        },
        [&](const ast::Expression&) -> LVRef {
            throw IRGenError("Assignment target is not a valid left-hand side expression");
        }
    });
}


[[nodiscard]] RValue emitAsRV(const ast::Identifier& ident, FunctionEmitter& func) {
    LVRef val = emitAsLV(ident, func);
    return materialize(val, func);
}

[[nodiscard]] RValue emitAsRV(const ast::Literal& lit, FunctionEmitter &func) {
    return std::visit(overloaded{
        [&](const ast::Literal::Null&) -> RValue {
            throw IRGenError("Null literals are not supported");
        },
        [&](auto value) -> RValue {
            return func.emitConst(value);
        }
    }, lit.value);
}

[[nodiscard]] RValue emitAsRV(const ast::BinaryExpression& expr, FunctionEmitter& func) {
    if (auto it = shortCircuitOps.find(expr.op); it != shortCircuitOps.end()) {
        auto lhsRes = emitAsRV(*expr.left(), func);

        return emitShortCircuit(lhsRes, {},
            [&]() {
                return emitAsRV(*expr.right(), func);
            },
            [&](RValue x, bool, const std::vector<RValue>&) { return x; },
            it->second, func
        );
    }

    auto it = binaryOps.find(expr.op);
    if (it == binaryOps.end()) {
        throw IRGenError("Unsupported binary operator");
    }

    auto lop = emitAsRV(*expr.left(), func);
    auto rop = emitAsRV(*expr.right(), func);
    Opcode op = it->second;

    return { emitBinaryArithmetic(lop, rop, op, func) };
}

[[nodiscard]] RValue emitAsRV(const ast::ConditionalExpression& expr, FunctionEmitter& func) {
    auto condVal = emitAsRV(*expr.test(), func);

    auto preBlock = func.getActiveBlock();
    auto trueBlock = func.createBlock(preBlock->varToReg, 0, preBlock->interm.size() - 1);
    auto falseBlock = func.createBlock(preBlock->varToReg, 0, preBlock->interm.size() - 1);
    auto postBlock = func.createBlock(preBlock->varToReg, 1, preBlock->interm.size() - 1);

    preBlock->setBranch(func.popInterm(condVal), *trueBlock, *falseBlock);

    auto emitBranch = [&](BasicBlockBuilderPtr block, const auto& expr_) -> std::pair<BasicBlockBuilderPtr, RValue> {
        func.setActiveBlock(block);
        auto branchRes = emitAsRV(expr_, func);

        return { func.getActiveBlock(), branchRes };
    };

    auto [ trueCont, trueRes ] = emitBranch(trueBlock, *expr.consequent());
    auto [ falseCont, falseRes ] = emitBranch(falseBlock, *expr.alternate());

    trueCont->setJump(*postBlock, { trueCont->popInterm(trueRes) });
    falseCont->setJump(*postBlock, { falseCont->popInterm(falseRes) });

    func.setActiveBlock(postBlock);
    return func.pushInterm(postBlock->args.back());
}

[[nodiscard]] RValue emitAsRV(const ast::UnaryExpression& expr, FunctionEmitter& func) {
    auto arg = emitAsRV(*expr.expression(), func);

    assert((expr.op != ast::UnaryExpression::Op::Await || func.data.isAsync)
           && "Await emitted in non-async function");

    auto it = unaryOps.find(expr.op);
    if (it == unaryOps.end()) {
        throw IRGenError("Unsupported unary operator '" + std::to_string(expr.op) + "'");
    }
    Opcode op = it->second;
    Reg argReg = func.popInterm(arg);

    if (op == Opcode::BoolNot) {
        // ToBoolean never invokes user code, so BoolNot cannot throw.
        Reg res = Reg::createTmp();
        func.emitInstruction(Operation{
            .op = op,
            .args = { argReg },
            .res = { res }
        });
        return func.pushInterm(res);
    }

    Reg res = emitCheckedOp(op, { argReg }, func);
    return func.pushInterm(res);
}

[[nodiscard]] RValue emitAsRV(const ast::UpdateExpression& expr, FunctionEmitter& func) {
    LVRef val = emitAsLV(*expr.expression(), func);

    LVRef writeTarget = val;
    RValue lhsVal;
    if (val.isMember()) {
        auto [readTarget, wTarget] = dupMemberTarget(val, func);
        lhsVal = materialize(readTarget, func);
        writeTarget = wTarget;
    }
    else {
        lhsVal = materialize(val, func);
    }

    Reg lop = func.popInterm(lhsVal);
    Reg rop = func.popInterm(func.emitConst(static_cast<int32_t>(1)));

    Reg res;
    if (expr.kind == ast::UpdateExpression::Op::PostInc || expr.kind == ast::UpdateExpression::Op::PostDec) {
        std::tie(lop, res) = emitDup(lop, func);
    }

    Reg valPost;

    switch (expr.kind) {
        case ast::UpdateExpression::Op::PreInc:
            valPost = emitCheckedOp(Opcode::Add, { lop, rop }, func);
            break;
        case ast::UpdateExpression::Op::PostInc: {
            auto original = func.pushInterm(res);
            valPost = emitCheckedOp(Opcode::Add, { lop, rop }, func);
            res = func.popInterm(original);
            break;
        }
        case ast::UpdateExpression::Op::PreDec:
            valPost = emitCheckedOp(Opcode::Sub, { lop, rop }, func);
            break;
        case ast::UpdateExpression::Op::PostDec: {
            auto original = func.pushInterm(res);
            valPost = emitCheckedOp(Opcode::Sub, { lop, rop }, func);
            res = func.popInterm(original);
            break;
        }
        default:
            assert(false);
    }

    if (expr.kind == ast::UpdateExpression::Op::PreInc || expr.kind == ast::UpdateExpression::Op::PreDec) {
        std::tie(valPost, res) = emitDup(valPost, func);
    }
    return emitAssignAndKeep(writeTarget, valPost, res, func);
}

[[nodiscard]] RValue emitAsRV(const ast::Function& astFn, FunctionEmitter& em) {
    auto sig = jac::cfg::getSignature(astFn);
    if (!sig) {
        throw std::runtime_error("Failed to get function signature");
    }

    std::vector<Identifier> closureVars;
    for (const auto& id : sig->closureVars) {
        auto local = em.getVar(id);
        if (!local) {
            throw IRGenError("Closure variable '" + id + "' not found in function '" + astFn.name()->name + "'");
        }

        if (local->isGlobal()) {
            sig->globalVars.push_back(id);
            continue;
        }
        closureVars.push_back(id);
    }
    sig->closureVars = closureVars;

    auto cfgFuncEm = jac::cfg::ast2cfg(astFn, sig, &em);
    auto c = em.addPoolConstant(std::make_unique<Function>(cfgFuncEm.output()));

    auto code = Reg::createTmp();
    em.emitInstruction(ConstInit{
        .reg = code,
        .value = c
    });
    std::vector<Reg> args = { code };
    for (const auto& id : sig->closureVars) {
        auto local = em.getVar(id);
        if (!local) {
            throw IRGenError("Closure variable '" + id + "' not found in function '" + astFn.name()->name + "'");
        }
        auto& v2r = em.getActiveBlock()->varToReg;
        auto reg = v2r.get(local->varId());
        auto [s1, s2] = emitDup({ reg }, em);
        v2r.update(local->varId(), s2);
        args.push_back(s1);
    }

    auto closure = Reg::createTmp();
    em.emitInstruction(Operation{
        .op = Opcode::MakeClosure,
        .args = args,
        .res = { closure }
    });
    return em.pushInterm(closure);
}

[[nodiscard]] RValue emitAsRV(const ast::NewCallExpression& expr, FunctionEmitter& func) {
    auto ctor = emitAsRV(*expr.callee(), func);
    return emitCallObj({ ctor }, expr.arguments(), func, true);
}

[[nodiscard]] RValue emitAsRV(const ast::CommaExpression& expr, FunctionEmitter& func) {
    for (size_t i = 0; i + 1 < expr.itemCount(); i++) {
        auto res = emitAsRV(*expr.itemGet(i), func);
        emitKill(func.popInterm(res), func);
    }
    if (expr.itemCount()) {
        return emitAsRV(*expr.itemGet(expr.itemCount() - 1), func);
    }
    throw IRGenError("Empty expression");
}

[[nodiscard]] RValue emitMemberShortCircuitAssign(LVRef target, const ast::Expression& rhsExpr, ShortCircuitKind kind, FunctionEmitter& func) {
    auto [readTarget, writeTarget] = dupMemberTarget(target, func);
    auto [wobj, wacc] = writeTarget.member();

    RValue lhsVal = materialize(readTarget, func);

    auto lhsReg = func.popInterm(lhsVal);
    auto waccReg = func.popInterm(wacc);
    auto wobjReg = func.popInterm(wobj);
    auto lhsVal2 = func.pushInterm(lhsReg);

    return emitShortCircuit(lhsVal2, { wobjReg, waccReg },
        [&]() {
            return emitAsRV(rhsExpr, func);
        },
        [&](RValue val, bool skipped, const std::vector<RValue>& passthrough) -> RValue {
            if (skipped) {
                return val;
            }
            auto [val1, val2] = emitDup(func.popInterm(val), func);
            auto accReg = func.popInterm(passthrough[1]);
            auto objReg = func.popInterm(passthrough[0]);
            auto kept = func.pushInterm(val2);
            emitCheckedVoidOp(Opcode::SetMember, { objReg, accReg, val1 }, func);
            return kept;
        },
        kind, func
    );
}

[[nodiscard]] RValue emitAsRV(const ast::Assignment& assign, FunctionEmitter& func) {
    LVRef target = emitAsLV(*assign.left(), func);

    if (assign.op == ast::Assignment::Op::Assign) {
        auto rhs = emitAsRV(*assign.right(), func);
        auto [ rhs1, rhs2 ] = emitDup(func.popInterm(rhs), func);
        return emitAssignAndKeep(target, rhs1, rhs2, func);
    }
    if (auto it = arithAssignmentOps.find(assign.op); it != arithAssignmentOps.end()) {
        Opcode op = it->second;

        if (target.isMember()) {
            auto [readTarget, writeTarget] = dupMemberTarget(target, func);
            auto lhs = materialize(readTarget, func);
            auto rhs = emitAsRV(*assign.right(), func);

            auto rhsReg = func.popInterm(rhs);
            auto lhsReg = func.popInterm(lhs);
            Reg res = emitCheckedOp(op, { lhsReg, rhsReg }, func);
            auto [ res1, res2 ] = emitDup(res, func);
            return emitAssignAndKeep(writeTarget, res1, res2, func);
        }

        auto rhs = emitAsRV(*assign.right(), func);
        RValue targetR = materialize(target, func);

        auto lhsReg = func.popInterm(targetR);
        auto rhsReg = func.popInterm(rhs);
        Reg res = emitCheckedOp(op, { lhsReg, rhsReg }, func);
        auto [ res1, res2 ] = emitDup(res, func);
        return emitAssignAndKeep(target, res1, res2, func);
    }
    if (auto it = shortCircuitAssignmentOps.find(assign.op); it != shortCircuitAssignmentOps.end()) {
        if (target.isMember()) {
            return emitMemberShortCircuitAssign(target, *assign.right(), it->second, func);
        }

        RValue targetR = materialize(target, func);

        return emitShortCircuit(targetR, {}, [&]() {
                return emitAsRV(*assign.right(), func);
            },
            [&](RValue val, bool skipped, const std::vector<RValue>&) {
                if (skipped) {
                    return val;
                }
                auto [val1, val2] = emitDup(func.popInterm(val), func);
                return emitAssignAndKeep(target, val1, val2, func);
            },
            it->second, func
        );
    }
    throw IRGenError("Unsupported assignment operator");
}

[[nodiscard]] RValue emitAsRV(const ast::MemberAccessExpression& member, FunctionEmitter& func) {
    LVRef obj = emitAsLV(member, func);
    return materialize(obj, func);
}

[[nodiscard]] RValue emitAsRV(const ast::TaggedTemplateExpression&, FunctionEmitter& func) {
    throw IRGenError("Tagged template expressions are not supported");
}

[[nodiscard]] RValue emitAsRV(const ast::CallExpression& call, FunctionEmitter& func) {
    auto obj = emitAsLV(*call.callee(), func);
    return emitCallObj({ obj }, call.arguments(), func, false);
}

[[nodiscard]] RValue emitAsRV(const ast::ThisExpression&, FunctionEmitter& func) {
    throw IRGenError("The 'this' keyword is not supported");
}


[[nodiscard]] RValue emitAsRV(const ast::Expression& node, FunctionEmitter& func) {
    return ast::visitNode<ast::ExpressionTypes>(node, overloaded{
        [&](const auto& expr) -> RValue {
            return emitAsRV(expr, func);
        }
    });
}


bool emitStmt(const ast::Statement& statement, FunctionEmitter& func);


bool emitStmt(const ast::ExpressionStatement& stmt, FunctionEmitter& func) {
    auto v = emitAsRV(*stmt.expression(), func);
    emitKill(func.popInterm(v), func);
    return false;
}

void preDeclareVariables(const ast::StatementList& list, FunctionEmitter& func) {
    for (const auto& [ident, info] : list.hoistedDeclarations) {
        func.addLexical(ident, info.isConst);
    }
}

bool emitStmt(const ast::LexicalDeclaration& stmt, FunctionEmitter& func) {
    for (size_t i = 0; i < stmt.bindingCount(); i++) {
        const auto& binding = stmt.bindingGet(i);

        RValue rhs;
        if (binding->initializer()) {
            rhs = emitAsRV(*binding->initializer(), func);
        }
        else {
            rhs = func.emitUndefined();
        }
        auto ref = func.getVar(binding->target()->name);
        emitAssign(*ref, func.popInterm(rhs), func);
    }
    return false;
}

bool emitStmt(const ast::IterationStatement& stmt, FunctionEmitter& func) {
    auto preBlock = func.getActiveBlock();

    assert(preBlock->term().args.size() == 0);
    auto initBlock = func.createBlock(func.getActiveBlock()->varToReg, 0);  // XXX: emit into preBlock?
    BasicBlockBuilderPtr condBlock;
    BasicBlockBuilderPtr updateBlock;
    BasicBlockBuilderPtr statementBlock;
    BasicBlockBuilderPtr postBlock;

    func.enterScope();

    // loop entry
    if (!stmt.isDoWhile()) {
        preBlock->setJump(*initBlock);
    }

    // init block
    func.setActiveBlock(initBlock);
    if (stmt.init()) {
        ast::visitNode<TypeList<ast::Statement, ast::Expression>>(*stmt.init(), overloaded{
            [&](const ast::Statement& s) {
                emitStmt(s, func);
            },
            [&](const ast::Expression& e) {
                auto res = emitAsRV(e, func);
                emitKill(func.popInterm(res), func);
            }
        });
    }
    auto innerVars = func.getActiveBlock()->varToReg.vars();

    postBlock = func.createBlock(func.getActiveBlock()->varToReg, 0);
    condBlock = func.createBlock(func.getActiveBlock()->varToReg, 0);
    updateBlock = func.createBlock(func.getActiveBlock()->varToReg, 0);
    statementBlock = func.createBlock(func.getActiveBlock()->varToReg, 0);

    func.getActiveBlock()->setJump(*condBlock);

    if (stmt.isDoWhile()) {
        preBlock->setJump(*statementBlock);
    }

    // condition block
    func.setActiveBlock(condBlock);
    if (auto cond = stmt.condition()) {
        auto res = emitAsRV(*cond, func);
        func.getActiveBlock()->setBranch(func.popInterm(res), *statementBlock, *postBlock);
    }
    else {
        func.getActiveBlock()->setJump(*statementBlock);
    }

    // statement block
    func.setActiveBlock(statementBlock);
    if (auto bodyStmt = stmt.statement()) {
        auto __ = func.pushBreakTarget(postBlock, &innerVars);
        auto ___ = func.pushContinueTarget(updateBlock, &innerVars);
        emitStmt(*bodyStmt, func);
    }
    if (func.getActiveBlock()->term().type == Terminator::None) {
        func.getActiveBlock()->setJump(*updateBlock);
    }

    // update block
    func.setActiveBlock(updateBlock);
    if (auto update = stmt.update()) {
        auto res = emitAsRV(*update, func);
        emitKill(func.popInterm(res), func);
    }
    func.getActiveBlock()->setJump(*condBlock);

    // post block
    func.setActiveBlock(postBlock);
    func.exitScope(true);
    return false;
}

bool emitStmt(const ast::ContinueStatement& stmt, FunctionEmitter& func) {
    if (stmt.label()) {
        throw IRGenError("Labeled continue statements are not supported");
    }
    auto [ target, vars ] = func.getContinueTarget();
    assert(target->args.size() == vars->size());
    emitKillVars(func.getActiveBlock()->varToReg.getAllVarsExcept(*vars), func);

    func.getActiveBlock()->setJump(*target);
    return true;
}

bool emitStmt(const ast::BreakStatement& stmt, FunctionEmitter& func) {
    if (stmt.label()) {
        throw IRGenError("Labeled break statements are not supported");
    }
    auto [ target, vars ] = func.getBreakTarget();
    assert(target->args.size() == vars->size());
    emitKillVars(func.getActiveBlock()->varToReg.getAllVarsExcept(*vars), func);

    func.getActiveBlock()->setJump(*target);
    return true;
}

bool emitStmt(const ast::ReturnStatement& stmt, FunctionEmitter& func) {
    if (!stmt.expression()) {
        emitKillLiveVars(func);
        func.emitReturn();
        return true;
    }

    auto arg = emitAsRV(*stmt.expression(), func);

    emitKillLiveVars(func);
    func.emitReturn(func.popInterm(arg));
    return true;
}

bool emitStmt(const ast::ThrowStatement& stmt, FunctionEmitter& func) {
    auto val = emitAsRV(*stmt.expression(), func);

    emitKillLiveVars(func);
    func.emitThrow(func.popInterm(val));
    return true;
}

bool emitStmt(const ast::EmptyStatement&, FunctionEmitter&) {
    return false;
}

bool emitStmt(const ast::DebuggerStatement& stmt, FunctionEmitter& func) {
    throw IRGenError("Debugger statements are not supported");
}

bool emitStmt(const ast::HoistableDeclaration& stmt, FunctionEmitter& func) {
    // FIXME: incorrect identifier handling
    RValue val = emitAsRV(*stmt.function(), func);
    if (!stmt.function()->name()) {
        throw IRGenError("Function declarations must have a name");
    }
    auto ref = func.getVar(stmt.function()->name()->name);
    emitAssign(*ref, func.popInterm(val), func);
    return false;
}

bool emitStmt(const ast::IfStatement& stmt, FunctionEmitter& func) {
    auto preBlock = func.getActiveBlock();
    auto ifBlock = func.createBlock(preBlock->varToReg, 0);
    auto elseBlock = func.createBlock(preBlock->varToReg, 0);
    auto postBlock = func.createBlock(preBlock->varToReg, 0);

    // condition block
    auto res = emitAsRV(*stmt.condition(), func);
    func.getActiveBlock()->setBranch(func.popInterm(res), *ifBlock, *elseBlock);

    // if block
    func.setActiveBlock(ifBlock);
    emitStmt(*stmt.consequent(), func);

    if (func.getActiveBlock()->term().type == Terminator::None) {
        func.getActiveBlock()->setJump(*postBlock);
    }

    // else block
    func.setActiveBlock(elseBlock);
    if (auto alt = stmt.alternate()) {
        emitStmt(*alt, func);
    }
    if (func.getActiveBlock()->term().type == Terminator::None) {
        func.getActiveBlock()->setJump(*postBlock);
    }

    func.setActiveBlock(postBlock);
    return false;
}

// returns true if the block contains a "terminating" statement
bool emitStmt(const ast::StatementList& list, FunctionEmitter& func, bool skipPreDeclaration = false) {
    bool hasTerminator = false;
    {
        func.enterScope();
        if (!skipPreDeclaration) {
            preDeclareVariables(list, func);
        }

        for (size_t i = 0; i < list.statementCount(); i++) {
            auto stmt = list.statementGet(i);
            if (emitStmt(*stmt, func)) {
                hasTerminator = true;
                break;
            }
        }
    }
    // func.reduceCurrentVarToReg();
    func.exitScope(!hasTerminator);
    return hasTerminator;
}


bool emitStmt(const ast::Statement& statement, FunctionEmitter& func) {
    return ast::visitNode<ast::StatementTypes>(statement, overloaded{
        [&](const auto& expr) -> bool {
            return emitStmt(expr, func);
        }
    });
}


SignaturePtr getSignature(const ast::Function& decl) {
    auto sig = std::make_shared<Signature>();

    const auto& params = decl.parameters();
    if (params->restParameter()) {
        return nullptr;
    }

    for (size_t i = 0; i < params->parameterCount(); i++) {
        const auto& arg = params->parameterGet(i);
        sig->args.emplace_back(arg->target()->name);
    }

    for (const auto& closure : decl.closureVars) {
        sig->closureVars.push_back(closure);
    }

    for (const auto& global : decl.globalVars) {
        sig->globalVars.push_back(global);
    }

    return sig;
}

FunctionEmitter ast2cfg(const ast::Function& decl, SignaturePtr sig, FunctionEmitter* parent) {
    if (!decl.name()) {
        throw IRGenError("Function declarations must have a name");
    }

    FunctionEmitter out(parent);
    out.setSignature(sig);
    out.setFunctionName(decl.name()->name);
    out.data.isAsync = decl.isAsync;

    if (decl.body()) {
        emitStmt(*decl.body(), out);
    }

    if (out.getActiveBlock()->term().type == Terminator::None) {
        emitKillLiveVars(out);
        out.emitReturn();
    }

    return out;
}

FunctionEmitter ast2cfg(const ast::Script& s) {
    FunctionEmitter out(nullptr);
    auto sig = std::make_shared<Signature>();
    for (const auto& global : s.globalVars) {
        sig->globalVars.push_back(global);
    }
    out.setSignature(sig);
    out.setFunctionName("<module>");
    out.data.isAsync = false;

    for (const auto& [ident, info] : s.body()->hoistedDeclarations) {
        bool isLet = !info.isConst && !info.isVar && !info.isFunction;
        out.addGlobal(ident, info.isConst, isLet);
    }

    if (s.body()) {
        emitStmt(*s.body(), out, true);
    }

    if (out.getActiveBlock()->term().type == Terminator::None) {
        emitKillLiveVars(out);
        out.emitReturn();
    }

    return out;
}


FunctionEmitter ast2cfg(const ast::Module& m) {
    FunctionEmitter out(nullptr);
    auto sig = std::make_shared<Signature>();
    for (const auto& global : m.globalVars) {
        sig->globalVars.push_back(global);
    }
    out.setSignature(sig);
    out.setFunctionName("<module>");
    out.data.isAsync = true;

    if (m.body()) {
        emitStmt(*m.body(), out, false);
    }

    if (out.getActiveBlock()->term().type == Terminator::None) {
        emitKillLiveVars(out);
        out.emitReturn();
    }

    return out;
}


}  // namespace jac::cfg
