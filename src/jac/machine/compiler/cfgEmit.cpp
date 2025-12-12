#include "cfgEmit.h"
#include "ast.h"
#include "cfg.h"
#include "opcode.h"


namespace jac::cfg {


enum class ShortCircuitKind {
    And,
    Or
};

const std::unordered_map<std::string_view, ShortCircuitKind> shortCircuitOps = {
    { "&&", ShortCircuitKind::And },
    { "||", ShortCircuitKind::Or }
};

const std::unordered_map<std::string_view, Opcode> binaryOps = {
    { "|", Opcode::BitOr },
    { "^", Opcode::BitXor },
    { "&", Opcode::BitAnd },
    { "==", Opcode::Eq },
    { "!=", Opcode::Neq },
    // { "===", ... },
    // { "!==", ... },
    { "<", Opcode::Lt },
    { "<=", Opcode::Lte },
    { ">", Opcode::Gt },
    { ">=", Opcode::Gte },
    // { "in", ... },
    // { "instanceof", ... },
    { "<<", Opcode::LShift },
    { ">>", Opcode::RShift },
    { ">>>", Opcode::URShift },
    { "+", Opcode::Add },
    { "-", Opcode::Sub },
    { "*", Opcode::Mul },
    { "/", Opcode::Div },
    { "%", Opcode::Rem },
    { "**", Opcode::Pow }
};

const std::unordered_map<std::string_view, ShortCircuitKind> shortCircuitAssignmentOps = {
    { "&&=", ShortCircuitKind::And },
    { "||=", ShortCircuitKind::Or }
};

const std::unordered_map<std::string_view, Opcode> arithAssignmentOps = {
    { "+=", Opcode::Add },
    { "-=", Opcode::Sub },
    { "*=", Opcode::Mul },
    { "/=", Opcode::Div },
    { "%=", Opcode::Rem },
    { "&=", Opcode::BitAnd },
    { "|=", Opcode::BitOr },
    { "^=", Opcode::BitXor },
    { "<<=", Opcode::LShift },
    { ">>=", Opcode::RShift },
    { ">>>=", Opcode::URShift }
};

const std::unordered_map<std::string_view, Opcode> unaryOps = {
    { "!", Opcode::BoolNot },
    { "~", Opcode::BitNot },
    { "+", Opcode::UnPlus },
    { "-", Opcode::UnMinus },
    // "typeof",
    // "void",
    // "delete",
    // "await"
};

const std::unordered_map<std::string_view, ValueType> types = {
    { "int32", ValueType::I32 },
    { "float64", ValueType::F64 },
    { "boolean", ValueType::Bool },
    { "object", ValueType::Object },
    { "void", ValueType::Void },
    { "any", ValueType::Any }
};

ValueType getType(std::string_view name) {
    auto it = types.find(name);
    if (it == types.end()) {
        throw IRGenError("Invalid type literal");
    }
    return it->second;
}

void emitDup(RValue val, FunctionEmitter& func) {
    func.emitInstruction({Operation{
        .op = Opcode::Dup,
        .a = val
    }});
}

void emitPushFree(RValue val, FunctionEmitter& func) {
    func.emitInstruction({Operation{
        .op = Opcode::PushUnref,
        .a = val
    }});
}

void emitPushFree(Value val, FunctionEmitter& func) {
    if (val.isRValue()) {
        emitPushFree(val.asRValue(), func);
    }
}

[[nodiscard]] RValue emitConst(auto value, FunctionEmitter& func) {
    ConstInit init = ConstInit{
        .id = newRegId(),
        .value = value
    };
    func.emitInstruction(init);
    RValue initR = { init.type(), init.id };
    return initR;
}

[[nodiscard]] RValue giveSimple(LVRef lv, FunctionEmitter&) {
    if (lv.isMember()) {
        throw IRGenError("Cannot move member");
    }
    return { lv.self };
}

[[nodiscard]] RValue emitCastAndFree(RValue val, ValueType type, FunctionEmitter& func) {
    if (val.type() == type) {
        return val;
    }
    RValue res = { Reg::create(type) };
    func.emitInstruction({Operation{
        .op = Opcode::Set,
        .a = val,
        .res = res
    }});
    emitPushFree(val, func);
    return res;
}

[[nodiscard]] RValue materialize(LVRef lv, FunctionEmitter& func) {
    if (!lv.isMember()) {
        RValue v = { lv.self };
        emitDup(v, func);
        return v;
    }

    emitPushFree({ *lv.memberIdent }, func);  // XXX: broken if lv is materialized multiple times

    // TODO: solve conversion of parent/accessor types
    RValue res = { Reg::create(lv.type()) };
    func.emitInstruction({Operation{
        .op = Opcode::GetMember,
        .a = lv.self,
        .b = *lv.memberIdent,
        .res = res
    }});

    return res;
}

[[nodiscard]] RValue materialize(Value val, FunctionEmitter& func) {
    if (val.isRValue()) {
        return val.asRValue();
    }
    return materialize(val.asLVRef(), func);
}


[[nodiscard]] RValue emitAsRV(const ast::Expression& node, FunctionEmitter& func);
[[nodiscard]] LVRef emitAsLV(const ast::Expression& node, FunctionEmitter& func);



// TODO: think about conversion set once more
[[nodiscard]] RValue emitAssign(LVRef target, RValue value, FunctionEmitter& func) {
    if (target.isMember()) {
        // TODO: allow direct assignment of different types in member case
        RValue parent = { target.self };
        RValue ident = { *target.memberIdent };

        emitPushFree(ident, func);  // XXX: broken if target is used multiple times

        func.emitInstruction({Operation{
            .op = Opcode::SetMember,
            .a = ident,
            .b = value,
            .res = parent
        }});
        return value;
    }
    else {
        if (target.type() != value.type()) {
            emitPushFree(value, func);
        }

        RValue targetR = giveSimple(target, func);
        func.emitInstruction({Operation{
            .op = Opcode::Set,
            .a = value,
            .res = targetR
        }});
        return targetR;
    }
}


[[nodiscard]] RValue emitBinaryArithmetic(RValue lhs, RValue rhs, Opcode op, FunctionEmitter& func) {
    ValueType resType = resultType(op, lhs.type(), rhs.type());
    RValue res = { Reg::create(resType) };

    RValue lopRType;
    RValue ropRType;

    if (isArithmetic(op) || isBitwise(op)) {
        lopRType = emitCastAndFree(lhs, resType, func);
        ropRType = emitCastAndFree(rhs, resType, func);
    }
    else {
        assert(isComparison(op));
        ValueType commonType = commonUpcast(lhs.type(), rhs.type());
        lopRType = emitCastAndFree(lhs, commonType, func);
        ropRType = emitCastAndFree(rhs, commonType, func);
    }

    emitPushFree(lopRType, func);
    emitPushFree(ropRType, func);

    func.emitInstruction({Operation{
        .op = op,
        .a = lopRType,
        .b = ropRType,
        .res = res
    }});
    return res;
}



// evalRhs(void) emits evaluation of the right-hand side expression and returns the resulting RValue.
// setRes(res, skipped) is a function that processes the result of the evaluation; skipped is true if the expression short-circuited.
template<typename F, typename G>
RValue emitShortCircuit(RValue lhs, F evalRhs, G processRes, ShortCircuitKind kind, FunctionEmitter& func) {
    static_assert(std::is_same_v<decltype(evalRhs()), RValue>, "evalRhs must return RValue");
    static_assert(std::is_invocable_v<G, RValue, bool>);

    if (lhs.type() != ValueType::Bool) {
        throw IRGenError("Short circuit expressions support only boolean operands");
    }

    auto preBlock = func.getActiveBlock();
    auto skipBlock = func.createBlock();  // target when expression short circuits
    auto elseBlock = func.createBlock();  // target otherwise
    auto postBlock = func.createBlock();

    postBlock->jump = preBlock->jump;
    skipBlock->jump = Terminator::jump(postBlock);
    elseBlock->jump = Terminator::jump(postBlock);

    // evaluate lhs
    RValue lhsBool = lhs;

    if (kind == ShortCircuitKind::Or) {
        preBlock->jump = Terminator::branch(lhsBool, skipBlock, elseBlock);
    }
    else if (kind == ShortCircuitKind::And) {
        preBlock->jump = Terminator::branch(lhsBool, elseBlock, skipBlock);
    }
    func.setActiveBlock(skipBlock);
    processRes(lhsBool, true);

    func.setActiveBlock(elseBlock);
    emitPushFree(lhsBool, func);
    RValue rhs = evalRhs();
    processRes(rhs, false);

    func.setActiveBlock(postBlock);

    return rhs;
}


[[nodiscard]] RValue emitCallObj(Value obj, ast::Arguments* args_, FunctionEmitter& func, bool isConstructor) {
    RValue res = { Reg::create(ValueType::Any) };

    RValue objR = materialize(obj, func);
    emitPushFree(objR, func);

    std::vector<Reg> args;
    if (args_) {
        args.reserve(args_->argCount() + 1);
    }
    else {
        args.reserve(1);
    }

    if (obj.isRValue() || isConstructor) {
        args.push_back(Reg::undefined());
    }
    else {
        args.push_back(obj.asLVRef().self);
    }

    if (args_) {
        if (args_->spread()) {
            throw IRGenError("Spread arguments are not supported");
        }
        for (size_t i = 0; i < args_->argCount(); i++) {
            auto arg = emitAsRV(*args_->argGet(i), func);

            if (arg.type() == ValueType::StringConst) {
                arg = emitCastAndFree(arg, ValueType::Any, func);
            }
            emitPushFree(arg, func);

            args.push_back(arg);
        }
    }

    func.emitInstruction({Call{
        .obj = objR,
        .isConstructor = isConstructor,
        .args = args,
        .res = res
    }});

    return { res };
}


[[nodiscard]] RValue emitCallNative(Identifier ident, ast::Arguments* args_, FunctionEmitter& func) {
    auto sig = func.getSignature(ident);
    if (!sig) {
        throw IRGenError("Function not found: " + ident);
    }

    RValue res = { Reg::undefined() };
    if (sig->ret != ValueType::Void) {
        res = { Reg::create(sig->ret) };
    }

    std::vector<Reg> args;
    if (args_) {
        if (args_->spread()) {
            throw IRGenError("Spread arguments are not supported");
        }

        args.reserve(args_->argCount());
        for (size_t i = 0; i < args_->argCount(); i++) {
            auto arg = emitAsRV(*args_->argGet(i), func);
            emitPushFree(arg, func);

            args.push_back(arg);
        }
    }

    func.emitInstruction({Call{
        .obj = ident,
        .args = args,
        .res = res
    }});
    func.addRequiredFunction(ident);

    return res;
}


[[nodiscard]] LVRef mbrAccess(RValue obj, RValue acc, FunctionEmitter& func) {
    emitPushFree(obj, func);
    if (obj.type() != ValueType::Object && obj.type() != ValueType::Any) {
        RValue conv = { Reg::create(ValueType::Any) };
        func.emitInstruction({Operation{
            .op = Opcode::Set,
            .a = obj,
            .res = conv
        }});
        emitPushFree(conv, func);
        obj = conv;
    }

    LVRef res = LVRef::mbr(obj, acc, false);

    return res;
}

[[nodiscard]] LVRef emitAsLV(const ast::Expression& node, FunctionEmitter& func) {
    using Types = TypeList<ast::Identifier, ast::MemberAccessExpression, ast::Expression>;

    return ast::visitNode<Types>(node, overloaded{
        [&](const ast::Identifier& expr) -> LVRef {
            auto local = func.getLocal(expr.name);
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
            return emitConst(value, func);
        }
    }, lit.value);
}

[[nodiscard]] RValue emitAsRV(const ast::BinaryExpression& expr, FunctionEmitter& func) {
    if (auto it = shortCircuitOps.find(expr.op); it != shortCircuitOps.end()) {
        auto lhsRes = emitAsRV(*expr.left(), func);

        RValue res = { Reg::create(ValueType::Bool) };
        emitShortCircuit(lhsRes, [&]() {
            return emitAsRV(*expr.right(), func);
        },
        [&](RValue val, bool) {
            func.emitInstruction({Operation{
                .op = Opcode::Set,
                .a = val,
                .res = res
            }});
        },
        it->second, func);
        return { res };
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
    emitPushFree(condVal, func);

    auto preBlock = func.getActiveBlock();
    auto trueBlock = func.createBlock();
    auto falseBlock = func.createBlock();
    auto postBlock = func.createBlock();

    postBlock->jump = preBlock->jump;
    trueBlock->jump = Terminator::jump(postBlock);
    falseBlock->jump = Terminator::jump(postBlock);
    preBlock->jump = Terminator::branch(condVal, trueBlock, falseBlock);

    auto emitBranch = [&](BasicBlockPtr block, const auto& expr_) -> std::pair<BasicBlockPtr, RValue> {
        func.setActiveBlock(block);
        auto branchRes = emitAsRV(expr_, func);

        return { func.getActiveBlock(), branchRes };
    };

    auto [ trueCont, trueRes ] = emitBranch(trueBlock, *expr.consequent());
    auto [ falseCont, falseRes ] = emitBranch(falseBlock, *expr.alternate());

    RValue res;
    if (trueRes.type() == falseRes.type()) {
        res = RValue{ Reg::create(trueRes.type()) };
    }
    else {
        res = RValue{ Reg::create(ValueType::Any) };
    }

    auto emitSet = [&](BasicBlockPtr block, RValue resR) {
        func.setActiveBlock(block);
        func.emitInstruction({Operation{
            .op = Opcode::Set,
            .a = resR,
            .res = res
        }});
    };

    emitSet(trueCont, trueRes);
    emitSet(falseCont, falseRes);

    func.setActiveBlock(postBlock);
    return { res };
}

[[nodiscard]] RValue emitAsRV(const ast::UnaryExpression& expr, FunctionEmitter& func) {
    auto arg = emitAsRV(*expr.expression(), func);

    auto it = unaryOps.find(expr.op);
    if (it == unaryOps.end()) {
        throw IRGenError("Unsupported unary operator '" + std::string(expr.op) + "'");
    }
    Opcode op = it->second;

    auto resType = resultType(op, arg.type(), ValueType::Void);

    RValue argConv = emitCastAndFree(arg, resType, func);

    if (op == Opcode::UnPlus) {
        return { argConv };
    }

    emitPushFree(argConv, func);
    RValue res = { Reg::create(resType) };

    func.emitInstruction({Operation{
        .op = op,
        .a = argConv,
        .res = res
    }});

    return { res };
}

[[nodiscard]] RValue emitAsRV(const ast::UpdateExpression& expr, FunctionEmitter& func) {
    LVRef val = emitAsLV(*expr.expression(), func);

    RValue lop = materialize(val, func);
    RValue rop;
    if (lop.type() == ValueType::I32) {
        rop = emitConst(static_cast<int32_t>(1), func);
    }
    else if (lop.type() == ValueType::F64) {
        rop = emitConst(static_cast<double>(1), func);
    }
    else {
        lop = emitCastAndFree(lop, ValueType::Any, func);
        RValue off = emitConst(static_cast<int32_t>(1), func);
        rop = emitCastAndFree(off, ValueType::Any, func);
    }
    emitPushFree(rop, func);

    RValue valPre;
    if (expr.kind == ast::UpdateExpression::Op::PostInc || expr.kind == ast::UpdateExpression::Op::PostDec) {
        valPre = { Reg::create(lop.type()) };
        func.emitInstruction({Operation{
            .op = Opcode::Set,
            .a = lop,
            .res = valPre
        }});
    }
    else {
        emitPushFree(lop, func);
    }

    RValue valPost = { Reg::create(lop.type()) };

    switch (expr.kind) {
        case ast::UpdateExpression::Op::PreInc:
        case ast::UpdateExpression::Op::PostInc:
            func.emitInstruction({Operation{
                .op = Opcode::Add,
                .a = lop,
                .b = rop,
                .res = valPost
            }});
            break;
        case ast::UpdateExpression::Op::PreDec:
        case ast::UpdateExpression::Op::PostDec:
            func.emitInstruction({Operation{
                .op = Opcode::Sub,
                .a = lop,
                .b = rop,
                .res = valPost
            }});
            break;
        default:
            assert(false);
    }

    (void) emitAssign(val, valPost, func);

    if (expr.kind == ast::UpdateExpression::Op::PostInc || expr.kind == ast::UpdateExpression::Op::PostDec) {
        emitPushFree(valPost, func);
        return { valPre };
    }
    return { valPost };
}

[[nodiscard]] RValue emitAsRV(const ast::Function&, FunctionEmitter& func) {
    throw IRGenError("Function expressions are not supported");
}

[[nodiscard]] RValue emitAsRV(const ast::NewCallExpression& expr, FunctionEmitter& func) {
    auto ctor = emitAsRV(*expr.callee(), func);
    return emitCallObj({ ctor }, expr.arguments(), func, true);
}

[[nodiscard]] RValue emitAsRV(const ast::CommaExpression& expr, FunctionEmitter& func) {
    for (size_t i = 0; i + 1 < expr.itemCount(); i++) {
        auto v = emitAsRV(*expr.itemGet(i), func);
        emitPushFree(v, func);
    }
    if (!expr.itemCount()) {
        return emitAsRV(*expr.itemGet(expr.itemCount() - 1), func);
    }
    throw IRGenError("Empty expression");
}

[[nodiscard]] RValue emitAsRV(const ast::Assignment& assign, FunctionEmitter& func) {
    LVRef target = emitAsLV(*assign.left(), func);

    if (assign.op == "=") {
        auto rhs = emitAsRV(*assign.right(), func);
        return emitAssign(target, rhs, func);
    }
    if (auto it = arithAssignmentOps.find(assign.op); it != arithAssignmentOps.end()) {
        auto rhs = emitAsRV(*assign.right(), func);

        Opcode op = it->second;
        RValue targetR = materialize(target, func);

        RValue res = emitBinaryArithmetic(targetR, rhs, op, func);
        return emitAssign(target, res, func);
    }
    if (auto it = shortCircuitAssignmentOps.find(assign.op); it != shortCircuitAssignmentOps.end()) {
        RValue targetR = materialize(target, func);

        emitShortCircuit(targetR, [&]() {
            return emitAsRV(*assign.right(), func);
        },
        [&](RValue val, bool skipped) {
            if (skipped) {
                return;
            }
            (void) emitAssign(target, val, func);
            if (target.isMember()) {
                func.emitInstruction({Operation{
                    .op = Opcode::Set,
                    .a = val,
                    .res = targetR
                }});
            }
        },
        it->second, func);

        return targetR;
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
    if (auto* ident = dynamic_cast<ast::Identifier*>(call.callee())) {
        if (!func.getLocal(ident->name)) {
            return { emitCallNative(ident->name, call.arguments(), func) };
        }
    }
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


ValueType resolveType(const ast::TypeAnnotation& typeAnn) {
    return getType(typeAnn.name);
}


bool emitStmt(const ast::Statement& statement, FunctionEmitter& func);


bool emitStmt(const ast::ExpressionStatement& stmt, FunctionEmitter& func) {
    auto v = emitAsRV(*stmt.expression(), func);
    if (v.type() == ValueType::Void) {
        return false;
    }
    emitPushFree(v, func);
    return false;
}

bool emitStmt(const ast::LexicalDeclaration& stmt, FunctionEmitter& func) {
    for (size_t i = 0; i < stmt.bindingCount(); i++) {
        const auto& binding = stmt.bindingGet(i);
        auto type = resolveType(*binding->typeAnnotation());

        RValue rhs;
        if (binding->initializer()) {
            rhs = emitAsRV(*binding->initializer(), func);
        }

        auto ref = func.addLexical(binding->target()->name, type, false);

        if (binding->initializer()) {
            emitPushFree(emitAssign(ref, rhs, func), func);
        }
    }
    return false;
}

bool emitStmt(const ast::IterationStatement& stmt, FunctionEmitter& func) {
    auto preBlock = func.getActiveBlock();
    auto initBlock = func.createBlock();
    auto preCondBlock = func.createBlock();
    auto postCondBlock = func.createBlock();
    auto updateBlock = func.createBlock();
    auto statementBlock = func.createBlock();
    auto postBlock = func.createBlock();

    postBlock->jump = preBlock->jump;
    preBlock->jump = Terminator::jump(initBlock);
    initBlock->jump = Terminator::jump(preCondBlock);
    statementBlock->jump = Terminator::jump(postCondBlock);
    updateBlock->jump = Terminator::jump(preCondBlock);

    postCondBlock->jump = Terminator::jump(updateBlock);
    preCondBlock->jump = Terminator::jump(statementBlock);

    auto _ = func.pushScope();

    // init block
    if (stmt.init()) {
        func.setActiveBlock(initBlock);
        ast::visitNode<TypeList<ast::Statement, ast::Expression>>(*stmt.init(), overloaded{
            [&](const ast::Statement& s) {
                emitStmt(s, func);
            },
            [&](const ast::Expression& e) {
                RValue v = emitAsRV(e, func);
                emitPushFree(v, func);
            }
        });
    }

    // pre-condition block
    if (auto preCond = stmt.preCondition()) {
        func.setActiveBlock(preCondBlock);
        auto res = emitAsRV(*preCond, func);
        auto test = emitCastAndFree(res, ValueType::Bool, func);
        emitPushFree(test, func);

        func.getActiveBlock()->jump = Terminator::branch(test, statementBlock, postBlock);
    }

    // statement block
    if (auto bodyStmt = stmt.statement()) {
        func.setActiveBlock(statementBlock);
        auto __ = func.pushBreakTarget(postBlock);
        auto ___ = func.pushContinueTarget(updateBlock);
        emitStmt(*bodyStmt, func);
    }

    // update block
    if (auto update = stmt.update()) {
        func.setActiveBlock(updateBlock);
        auto v = emitAsRV(*update, func);
        emitPushFree(v, func);
    }

    // post-condition block
    if (auto postCond = stmt.postCondition()) {
        func.setActiveBlock(postCondBlock);
        auto res = emitAsRV(*postCond, func);
        auto test = emitCastAndFree(res, ValueType::Bool, func);
        emitPushFree(test, func);

        func.getActiveBlock()->jump = Terminator::branch(test, updateBlock, postBlock);
    }

    func.setActiveBlock(postBlock);
    return false;
}

bool emitStmt(const ast::ContinueStatement& stmt, FunctionEmitter& func) {
    if (stmt.label()) {
        throw IRGenError("Labeled continue statements are not supported");
    }
    if (auto target = func.getContinueTarget()) {
        func.getActiveBlock()->jump = Terminator::jump(target);
    }
    else {
        throw IRGenError("Continue statement without target");
    }
    return true;
}

bool emitStmt(const ast::BreakStatement& stmt, FunctionEmitter& func) {
    if (stmt.label()) {
        throw IRGenError("Labeled break statements are not supported");
    }
    if (auto target = func.getBreakTarget()) {
        func.getActiveBlock()->jump = Terminator::jump(target);
    }
    else {
        throw IRGenError("Break statement without target");
    }
    return true;
}

bool emitStmt(const ast::ReturnStatement& stmt, FunctionEmitter& func) {
    if (!stmt.expression()) {
        func.getActiveBlock()->jump = Terminator::ret();
        return true;
    }

    auto arg = emitAsRV(*stmt.expression(), func);
    RValue conv = emitCastAndFree(arg, func.signature->ret, func);

    func.getActiveBlock()->jump = Terminator::retVal(conv);
    return true;
}

bool emitStmt(const ast::ThrowStatement& stmt, FunctionEmitter& func) {
    auto val = emitAsRV(*stmt.expression(), func);
    RValue anyVal = emitCastAndFree(val, ValueType::Any, func);

    func.getActiveBlock()->jump = Terminator::throw_(anyVal);
    return true;
}

bool emitStmt(const ast::DebuggerStatement& stmt, FunctionEmitter& func) {
    throw IRGenError("Debugger statements are not supported");
}

bool emitStmt(const ast::HoistableDeclaration& stmt, FunctionEmitter& func) {
    throw IRGenError("Function declarations are not supported");
}

bool emitStmt(const ast::IfStatement& stmt, FunctionEmitter& func) {
    auto preBlock = func.getActiveBlock();
    auto ifBlock = func.createBlock();
    auto elseBlock = func.createBlock();  // XXX: set to null if no else block
    auto postBlock = func.createBlock();

    postBlock->jump = preBlock->jump;
    ifBlock->jump = Terminator::jump(postBlock);
    elseBlock->jump = Terminator::jump(postBlock);

    // condition block
    auto res = emitAsRV(*stmt.condition(), func);
    auto test = emitCastAndFree(res, ValueType::Bool, func);
    emitPushFree(test, func);

    func.getActiveBlock()->jump = Terminator::branch(test, ifBlock, elseBlock);

    // if block
    func.setActiveBlock(ifBlock);
    emitStmt(*stmt.consequent(), func);

    // else block
    if (auto alt = stmt.alternate()) {
        func.setActiveBlock(elseBlock);
        emitStmt(*alt, func);
    }

    func.setActiveBlock(postBlock);
    return false;
}

// returns true if the block contains a "terminating" statement
bool emitStmt(const ast::StatementList& list, FunctionEmitter& func) {
    auto _ = func.pushScope();
    for (size_t i = 0; i < list.statementCount(); i++) {
        auto stmt = list.statementGet(i);
        if (emitStmt(*stmt, func)) {
            return true;
        }
    }

    return false;
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
    if (!decl.returnType()) {
        return nullptr;
    }
    sig->ret = resolveType(*decl.returnType());

    const auto& params = decl.parameters();
    if (params->restParameter()) {
        return nullptr;
    }

    for (size_t i = 0; i < params->parameterCount(); i++) {
        const auto& arg = params->parameterGet(i);
        if (!arg->typeAnnotation()) {
            return nullptr;
        }

        sig->args.emplace_back(arg->target()->name, resolveType(*arg->typeAnnotation()));
    }

    return sig;
}

FunctionEmitter emit(const ast::Function& decl, SignaturePtr sig, const std::map<cfg::Identifier, cfg::SignaturePtr>& otherSignatures) {
    if (!decl.name()) {
        throw IRGenError("Function declarations must have a name");
    }

    FunctionEmitter out(otherSignatures);
    out.setSignature(sig);
    out.setFunctionName(decl.name()->name);

    emitStmt(*decl.body(), out);

    if (out.getActiveBlock()->jump.type == Terminator::None) {
        out.getActiveBlock()->jump = Terminator::ret();
    }

    return out;
}


}  // namespace jac::cfg
