#include "tlessAst2cfg.h"
#include "ast.h"
#include "tlessCfg.h"
#include "tlssOpcode.h"


namespace jac::cfg::tless {


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
    // { "===", ... },
    // { "!==", ... },
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
    // "typeof",
    // "void",
    // "delete",
    // "await"
};

[[nodiscard]] RValue materialize(LVRef lv, FunctionEmitter& func) {
    if (!lv.isMember()) {
        Reg v = Reg::createTmp();
        auto newSlotReg = Reg::createTmp();
        auto varId = lv.varId();

        func.emitInstruction(Operation{
            .op = Opcode::Load,
            .args = { func.getActiveBlock()->varToReg.get(lv.varId()) },
            .res = { v, newSlotReg }
        });
        func.getActiveBlock()->varToReg.update(varId, newSlotReg);
        return func.pushInterm(v);
    }

    auto res = Reg::createTmp();
    auto [obj, acc] = lv.member();

    auto accReg = func.popInterm(acc);
    auto objReg = func.popInterm(obj);
    func.emitInstruction(Operation{
        .op = Opcode::GetMember,
        .args = { objReg, accReg },
        .res = { res }
    });

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


[[nodiscard]] RValue emitAsRV(const ast::Expression& node, FunctionEmitter& func);
[[nodiscard]] LVRef emitAsLV(const ast::Expression& node, FunctionEmitter& func);



void emitAssign(LVRef target, Reg value, FunctionEmitter& func) {
    if (target.isMember()) {
        auto [obj, acc] = target.member();

        auto accReg = func.popInterm(acc);
        auto objReg = func.popInterm(obj);

        func.emitInstruction(Operation{
            .op = Opcode::SetMember,
            .args = { objReg, accReg, value },
            .res = { }
        });
    }
    else {
        auto newSlotReg = Reg::createTmp();
        auto varId = target.varId();
        func.emitInstruction(Operation{
            .op = Opcode::Store,
            .args = { value, func.getActiveBlock()->varToReg.get(varId) },
            .res = { newSlotReg }
        });
        func.getActiveBlock()->varToReg.update(varId, newSlotReg);
    }
}


[[nodiscard]] RValue emitBinaryArithmetic(RValue lhs, RValue rhs, Opcode op, FunctionEmitter& func) {
    auto res = Reg::createTmp();
    auto rhsReg = func.popInterm(rhs);
    auto lhsReg = func.popInterm(lhs);

    func.emitInstruction(Operation{
        .op = op,
        .args = {
            lhsReg,
            rhsReg
        },
        .res = { res }
    });
    return func.pushInterm(res);
}


// evalRhs(void) emits evaluation of the right-hand side expression and returns the resulting RValue.
// setRes(res, skipped) is a function that processes the result of the evaluation; skipped is true if the expression short-circuited.
template<typename F, typename G>
[[nodiscard]] RValue emitShortCircuit(RValue lhs, F evalRhs, G processRes, ShortCircuitKind kind, FunctionEmitter& func) {
    static_assert(std::is_same_v<decltype(evalRhs()), RValue>, "evalRhs must return RValue");
    static_assert(std::is_invocable_v<G, RValue, bool>);

    auto preBlock = func.getActiveBlock();
    auto postBlock = func.createBlock(preBlock->varToReg, 1, preBlock->interm.size() - 1);
    auto skipBlock = func.createBlock(preBlock->varToReg, 1, preBlock->interm.size() - 1);  // target when expression short circuits
    auto elseBlock = func.createBlock(preBlock->varToReg, 1, preBlock->interm.size() - 1);  // target otherwise

    auto lhs1 = Reg::createTmp();
    auto lhs2 = Reg::createTmp();
    func.emitInstruction(Operation{
        .op = Opcode::Dup,
        .args = { func.popInterm(lhs) },
        .res = { lhs1, lhs2 }
    });

    if (kind == ShortCircuitKind::Or) {
        preBlock->setBranch(lhs1, *skipBlock, *elseBlock, { lhs2 });
    }
    else if (kind == ShortCircuitKind::And) {
        preBlock->setBranch(lhs1, *elseBlock, *skipBlock, { lhs2 });
    }

    func.setActiveBlock(skipBlock);

    RValue lhs3 = processRes({ func.pushInterm(func.getActiveBlock()->args.back()) }, true);
    func.getActiveBlock()->setJump(*postBlock, { func.popInterm(lhs3) });

    func.setActiveBlock(elseBlock);
    emitKill({ func.getActiveBlock()->args.back() }, func);
    RValue rhs = evalRhs();
    RValue rhs1 = processRes(rhs, false);
    func.getActiveBlock()->setJump(*postBlock, { func.popInterm(rhs1) });

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

        args.push_back(this_);
        args.push_back(ident);
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

    func.emitInstruction(Operation{
        .op = op,
        .args = std::move(argRegs),
        .res = { res }
    });

    return func.pushInterm(res);
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

        return emitShortCircuit(lhsRes,
            [&]() {
                return emitAsRV(*expr.right(), func);
            },
            [&](RValue x, bool) { return x; },
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

    auto it = unaryOps.find(expr.op);
    if (it == unaryOps.end()) {
        throw IRGenError("Unsupported unary operator '" + std::to_string(expr.op) + "'");
    }
    Opcode op = it->second;

    Reg res = { Reg::createTmp() };

    func.emitInstruction(Operation{
        .op = op,
        .args = { func.popInterm(arg) },
        .res = { res }
    });

    return func.pushInterm(res);
}

[[nodiscard]] RValue emitAsRV(const ast::UpdateExpression& expr, FunctionEmitter& func) {
    LVRef val = emitAsLV(*expr.expression(), func);

    Reg lop = func.popInterm(materialize(val, func));
    Reg rop = func.popInterm(func.emitConst(static_cast<int32_t>(1)));

    Reg res;
    if (expr.kind == ast::UpdateExpression::Op::PostInc || expr.kind == ast::UpdateExpression::Op::PostDec) {
        std::tie(lop, res) = emitDup(lop, func);
    }

    Reg valPost = Reg::createTmp();

    switch (expr.kind) {
        case ast::UpdateExpression::Op::PreInc:
        case ast::UpdateExpression::Op::PostInc:
            func.emitInstruction(Operation{
                .op = Opcode::Add,
                .args = { lop, rop },
                .res = { valPost }
            });
            break;
        case ast::UpdateExpression::Op::PreDec:
        case ast::UpdateExpression::Op::PostDec:
            func.emitInstruction(Operation{
                .op = Opcode::Sub,
                .args = { lop, rop },
                .res = { valPost }
            });
            break;
        default:
            assert(false);
    }

    if (expr.kind == ast::UpdateExpression::Op::PreInc || expr.kind == ast::UpdateExpression::Op::PreDec) {
        std::tie(valPost, res) = emitDup(valPost, func);
    }
    emitAssign(val, valPost, func);

    return func.pushInterm(res);
}

[[nodiscard]] RValue emitAsRV(const ast::Function& astFn, FunctionEmitter& em) {
    auto sig = jac::cfg::tless::getSignature(astFn);
    if (!sig) {
        throw std::runtime_error("Failed to get function signature");
    }
    auto cfgFuncEm = jac::cfg::tless::ast2cfg(astFn, sig, &em);
    auto c = em.addPoolConstant(std::make_unique<Function>(cfgFuncEm.output()));

    auto code = Reg::createTmp();
    em.emitInstruction(ConstInit{
        .reg = code,
        .value = c
    });
    std::vector<Reg> args = { code };
    for (const auto& id : astFn.closureVars) {
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

[[nodiscard]] RValue emitAsRV(const ast::Assignment& assign, FunctionEmitter& func) {
    LVRef target = emitAsLV(*assign.left(), func);

    if (assign.op == ast::Assignment::Op::Assign) {
        auto rhs = emitAsRV(*assign.right(), func);
        auto [ rhs1, rhs2 ] = emitDup(func.popInterm(rhs), func);
        emitAssign(target, rhs1, func);
        return func.pushInterm(rhs2);
    }
    if (auto it = arithAssignmentOps.find(assign.op); it != arithAssignmentOps.end()) {
        auto rhs = emitAsRV(*assign.right(), func);

        Opcode op = it->second;
        RValue targetR = materialize(target, func);

        Reg res = Reg::createTmp();
        auto lhsReg = func.popInterm(targetR);
        auto rhsReg = func.popInterm(rhs);
        func.emitInstruction(Operation{
            .op = op,
            .args = {
                lhsReg,
                rhsReg
            },
            .res = { res }
        });
        auto [ res1, res2 ] = emitDup(res, func);
        emitAssign(target, res1, func);
        return func.pushInterm(res2);
    }
    if (auto it = shortCircuitAssignmentOps.find(assign.op); it != shortCircuitAssignmentOps.end()) {
        RValue targetR = materialize(target, func);

        return emitShortCircuit(targetR, [&]() {
                return emitAsRV(*assign.right(), func);
            },
            [&](RValue val, bool skipped) {
                if (skipped) {
                    return val;
                }
                auto [val1, val2] = emitDup(func.popInterm(val), func);
                emitAssign(target, val1, func);  // XXX: check member assignment
                return func.pushInterm(val2);
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
    for (const auto& [ident, isConst] : list.hoistedDeclarations) {
        func.addLexical(ident, isConst);
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
        func.getActiveBlock()->setReturn();
        return true;
    }

    auto arg = emitAsRV(*stmt.expression(), func);

    emitKillLiveVars(func);
    func.getActiveBlock()->setRetVal(func.popInterm(arg));
    return true;
}

bool emitStmt(const ast::ThrowStatement& stmt, FunctionEmitter& func) {
    auto val = emitAsRV(*stmt.expression(), func);

    emitKillLiveVars(func);
    func.getActiveBlock()->setThrow(func.popInterm(val));
    return true;
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

    if (decl.body()) {
        emitStmt(*decl.body(), out);
    }

    if (out.getActiveBlock()->term().type == Terminator::None) {
        out.getActiveBlock()->setReturn();
        emitKillLiveVars(out);
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

    for (const auto& [ident, isConst] : s.body()->hoistedDeclarations) {
        out.addGlobal(ident, isConst);
    }

    if (s.body()) {
        emitStmt(*s.body(), out, true);
    }

    if (out.getActiveBlock()->term().type == Terminator::None) {
        out.getActiveBlock()->setReturn();
        emitKillLiveVars(out);
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

    if (m.body()) {
        emitStmt(*m.body(), out, false);
    }

    if (out.getActiveBlock()->term().type == Terminator::None) {
        out.getActiveBlock()->setReturn();
        emitKillLiveVars(out);
    }

    return out;
}


}  // namespace jac::cfg
