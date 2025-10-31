#include "cfgEmit.h"
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


const ast::PrimaryExpression& newExpGetPrimary(const ast::NewExpression& newExp) {
    const auto member = std::get_if<ast::MemberExpression>(&(newExp.value));
    if (!member) {
        throw IRGenError("Only member expressions are supported");
    }
    const auto primary = std::get_if<ast::PrimaryExpression>(&(member->value));
    if (!primary) {
        throw IRGenError("Only primary expressions are supported");
    }

    return *primary;
}


const ast::PrimaryExpression& lhsGetPrimary(const ast::LeftHandSideExpression& lhs) {
    const auto newExp = std::get_if<ast::NewExpression>(&(lhs.value));
    if (newExp) {
        return newExpGetPrimary(*newExp);
    }

    throw IRGenError("Only new expressions are supported");
}

const std::optional<Identifier> memberGetIdentifier(const ast::MemberExpression& member) {
    auto primary = std::get_if<ast::PrimaryExpression>(&(member.value));
    if (!primary) {
        return std::nullopt;
    }
    auto identifier = std::get_if<ast::IdentifierReference>(&(primary->value));
    if (!identifier) {
        return std::nullopt;
    }

    return identifier->identifier.name;
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

[[nodiscard]] RValue emit(const ast::NumericLiteral& lit, FunctionEmitter &func) {
    return std::visit([&func](auto value) {
        return emitConst(value, func);
    }, lit.value);
}

[[nodiscard]] RValue emit(const ast::Literal& lit, FunctionEmitter &func) {
    struct visitor {
        FunctionEmitter& func;

        RValue operator()(const ast::NullLiteral&) {
            throw IRGenError("Null literals are not supported");
        }
        RValue operator()(const ast::BooleanLiteral& lit) {
            return emitConst(lit.value, func);
        }
        RValue operator()(const ast::NumericLiteral& lit) {
            return emit(lit, func);
        }
        RValue operator()(const ast::StringLiteral& lit) {
            return emitConst(lit.value, func);
        }
    };

    return std::visit(visitor{func}, lit.value);
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


[[nodiscard]] RValue emit(const ast::Expression& expr, FunctionEmitter& func);
[[nodiscard]] Value emit(const ast::AssignmentExpression& expr, FunctionEmitter& func);
[[nodiscard]] Value emit(const ast::MemberExpression& member, FunctionEmitter& func);


[[nodiscard]] Value emit(const ast::PrimaryExpression& primary, FunctionEmitter& func) {
    struct visitor {
        FunctionEmitter& func;

        Value operator()(const ast::ThisExpr&) {
            throw IRGenError("This expressions are not supported");
        }
        Value operator()(const ast::IdentifierReference& identifier) {
            Identifier ident = identifier.identifier.name;
            auto local = func.getLocal(ident);
            if (!local) {
                throw IRGenError("Identifier referenced before declaration (" + identifier.identifier.name + ")");
            }
            return { *local };
        }
        Value operator()(const ast::Literal& literal) {
            return { emit(literal, func) };
        }
        Value operator()(const ast::ArrayLiteral&) {
            throw IRGenError("Array literals are not supported");
        }
        Value operator()(const ast::ObjectLiteral&) {
            throw IRGenError("Object literals are not supported");
        }
        Value operator()(const ast::FunctionExpression&) {
            throw IRGenError("Function expressions are not supported");
        }
        Value operator()(const ast::ClassExpression&) {
            throw IRGenError("Class expressions are not supported");
        }
        Value operator()(const ast::GeneratorExpression&) {
            throw IRGenError("Generator expressions are not supported");
        }
        Value operator()(const ast::AsyncFunctionExpression&) {
            throw IRGenError("Async function expressions are not supported");
        }
        Value operator()(const ast::AsyncGeneratorExpression&) {
            throw IRGenError("Async generator expressions are not supported");
        }
        Value operator()(const ast::RegularExpressionLiteral&) {
            throw IRGenError("Regular expression literals are not supported");
        }
        Value operator()(const ast::TemplateLiteral&) {
            throw IRGenError("Template literals are not supported");
        }
        Value operator()(const ast::ParenthesizedExpression& expr) {
            return { emit(expr.expression, func) };
        }
    };

    return std::visit(visitor{func}, primary.value);
}

[[nodiscard]] RValue emitCallNative(Identifier ident, const ast::Arguments& args_, FunctionEmitter& func) {
    auto sig = func.getSignature(ident);
    if (!sig) {
        throw IRGenError("Function not found: " + ident);
    }

    RValue res = { Reg::undefined() };
    if (sig->ret != ValueType::Void) {
        res = { Reg::create(sig->ret) };
    }

    std::vector<Reg> args;
    args.reserve(args_.arguments.size());
    for (const auto& [ spread, expr ] : args_.arguments) {  // TODO: check arguments count
        if (spread) {
            throw IRGenError("Spread arguments are not supported");
        }
        Value arg = emit(*expr, func);
        auto argR = materialize(arg, func);
        emitPushFree(argR, func);

        args.push_back(argR);
    }

    func.emitInstruction({Call{
        .obj = ident,
        .args = args,
        .res = res
    }});
    func.addRequiredFunction(ident);

    return res;
}


[[nodiscard]] RValue emitCallObj(Value obj, const ast::Arguments& args_, FunctionEmitter& func, bool isConstructor) {
    RValue res = { Reg::create(ValueType::Any) };

    RValue objR = materialize(obj, func);
    emitPushFree(objR, func);

    std::vector<Reg> args;
    args.reserve(args_.arguments.size() + 1);
    if (obj.isRValue() || isConstructor) {
        args.push_back(Reg::undefined());
    }
    else {
        args.push_back(obj.asLVRef().self);
    }
    for (const auto& [ spread, expr ] : args_.arguments) {
        if (spread) {
            throw IRGenError("Spread arguments are not supported");
        }
        Value arg = emit(*expr, func);
        RValue argR = materialize(arg, func);

        if (argR.type() == ValueType::StringConst) {
            argR = emitCastAndFree(argR, ValueType::Any, func);
        }
        emitPushFree(argR, func);

        args.push_back(argR);
    }

    func.emitInstruction({Call{
        .obj = objR,
        .isConstructor = isConstructor,
        .args = args,
        .res = res
    }});

    return { res };
}


[[nodiscard]] LVRef mbrAccess(Value obj, RValue acc, FunctionEmitter& func) {
    RValue objR;
    if (obj.isRValue()) {
        objR = obj.asRValue();
        emitPushFree(objR, func);
    }
    else {
        LVRef objLV = obj.asLVRef();
        if (objLV.isMember()) {
            objR = materialize(obj, func);
            emitPushFree(objR, func);
        }
        else {
            objR = { objLV.self };
        }
    }

    if (objR.type() != ValueType::Object && objR.type() != ValueType::Any) {
        RValue conv = { Reg::create(ValueType::Any) };
        func.emitInstruction({Operation{
            .op = Opcode::Set,
            .a = objR,
            .res = conv
        }});
        emitPushFree(conv, func);
        objR = conv;
    }

    LVRef res = LVRef::mbr(objR, acc, false);

    return res;
}


[[nodiscard]] Value emit(const ast::CallExpression& call, FunctionEmitter& func) {
    struct visitor {
        FunctionEmitter& func;

        Value operator()(const ast::SuperCall&) {
            throw IRGenError("Super calls are not supported");
        }
        Value operator()(const ast::ImportCall&) {
            throw IRGenError("Import calls are not supported");
        }
        Value operator()(const std::pair<ast::MemberExpression, ast::Arguments>& call) {
            const auto& [mem, args_] = call;

            if (auto ident = memberGetIdentifier(mem); ident) {
                if (!func.getLocal(*ident)) {
                    return { emitCallNative(*ident, args_, func) };
                }
            }

            Value obj = emit(mem, func);
            return { emitCallObj(obj, args_, func, false) };

        }
        Value operator()(const std::pair<ast::CallExpressionPtr, ast::Arguments>&) { // call
            throw IRGenError("Call -> args are not supported");
        }
        Value operator()(const std::pair<ast::CallExpressionPtr, ast::Expression>& callBracket) { // brackets
            const auto& [call_, acc] = callBracket;
            Value obj = emit(*call_, func);
            RValue accR = emit(acc, func);

            return { mbrAccess(obj, accR, func) };
        }
        Value operator()(const std::pair<ast::CallExpressionPtr, ast::IdentifierName>& callDot) { // .
            const auto& [call_, ident] = callDot;
            Value obj = emit(*call_, func);
            RValue identR = emitConst(ident, func);

            return { mbrAccess({ obj }, identR, func) };
        }
        Value operator()(const std::pair<ast::CallExpressionPtr, ast::PrivateIdentifier>&) { // .private
            throw IRGenError("Call -> .private are not supported");
        }
        Value operator()(const std::pair<ast::CallExpressionPtr, ast::TemplateLiteral>&) { // template
            throw IRGenError("Call -> template literals are not supported");
        }
    };

    return std::visit(visitor{func}, call.value);
}


[[nodiscard]] Value emit(const ast::MemberExpression& member, FunctionEmitter& func) {
    struct visitor {
        FunctionEmitter& func;

        Value operator()(const ast::SuperProperty&) {
            throw IRGenError("Super properties are not supported");
        }

        Value operator()(const ast::MetaProperty&) {
            throw IRGenError("Meta properties are not supported");
        }

        Value operator()(const ast::PrimaryExpression& primary) {
            return emit(primary, func);
        }

        Value operator()(const std::pair<ast::MemberExpressionPtr, ast::Expression>& memBracket) { // brackets
            const auto& [mem, acc] = memBracket;
            Value obj = emit(*mem, func);
            RValue accR = emit(acc, func);

            return { mbrAccess(obj, accR, func) };
        }

        Value operator()(const std::pair<ast::MemberExpressionPtr, ast::IdentifierName>& memDot) { // .
            const auto& [mem, ident] = memDot;
            Value obj = emit(*mem, func);
            RValue identR = emitConst(ident, func);

            return { mbrAccess(obj, identR, func) };
        }

        Value operator()(const std::pair<ast::MemberExpressionPtr, ast::PrivateIdentifier>&) { // .private
            throw IRGenError("Member -> .private are not supported");
        }

        Value operator()(const std::pair<ast::MemberExpressionPtr, ast::TemplateLiteral>&) { // template
            throw IRGenError("Member -> template literals are not supported");
        }

        Value operator()(const std::pair<ast::MemberExpressionPtr, ast::Arguments>& ctorCall) { // new
            const auto& [mem, args_] = ctorCall;

            Value obj = emit(*mem, func);
            return { emitCallObj(obj, args_, func, true) };
        }
    };

    return std::visit(visitor{func}, member.value);
}


[[nodiscard]] Value emit(const ast::NewExpression& newExp, FunctionEmitter& func) {
    struct visitor {
        FunctionEmitter& func;

        Value operator()(const ast::MemberExpression& member) {
            return emit(member, func);
        }
        Value operator()(const ast::NewExpressionPtr& new_) {
            Value obj = emit(*new_, func);
            return { emitCallObj(obj, ast::Arguments{}, func, true) };
        }
    };

    return std::visit(visitor{func}, newExp.value);
}


[[nodiscard]] Value emit(const ast::LeftHandSideExpression& lhs, FunctionEmitter& func) {
    struct visitor {
        FunctionEmitter& func;

        Value operator()(const ast::CallExpression& call) {
            return { emit(call, func) };
        }
        Value operator()(const ast::NewExpression& newExp) {
            return emit(newExp, func);
        }
    };

    return std::visit(visitor{func}, lhs.value);
}


[[nodiscard]] Value emit(const ast::UnaryExpression& expr, FunctionEmitter& func);


[[nodiscard]] Value emit(const ast::UpdateExpression& expr, FunctionEmitter& func) {
    struct visitor {
        FunctionEmitter& func;

        Value operator()(const ast::UnaryExpressionPtr& unary) {
            return emit(*unary, func);
        }

        Value operator()(const ast::LeftHandSideExpressionPtr& lhs) {
            return emit(*lhs, func);
        }
    };

    auto val = std::visit(visitor{func}, expr.value);

    if (expr.kind == ast::UpdateKind::None) {
        return val;
    }
    if (val.isRValue()) {
        throw IRGenError("The operand of an update expression must be a left-hand side expression");
    }

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
    if (expr.kind == ast::UpdateKind::PostInc || expr.kind == ast::UpdateKind::PostDec) {
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
        case ast::UpdateKind::PreInc:
        case ast::UpdateKind::PostInc:
            func.emitInstruction({Operation{
                .op = Opcode::Add,
                .a = lop,
                .b = rop,
                .res = valPost
            }});
            break;
        case ast::UpdateKind::PreDec:
        case ast::UpdateKind::PostDec:
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

    (void) emitAssign(val.asLVRef(), valPost, func);

    if (expr.kind == ast::UpdateKind::PostInc || expr.kind == ast::UpdateKind::PostDec) {
        emitPushFree(valPost, func);
        return { valPre };
    }
    return { valPost };
}


Value emit(const ast::UnaryExpression& expr, FunctionEmitter& func) {
    if (std::holds_alternative<ast::UpdateExpression>(expr.value)) {
        return emit(std::get<ast::UpdateExpression>(expr.value), func);
    }

    const auto& un = std::get<std::pair<ast::UnaryExpressionPtr, std::string_view>>(expr.value);
    Value arg = emit(*(un.first), func);

    auto it = unaryOps.find(un.second);
    if (it == unaryOps.end()) {
        throw IRGenError("Unsupported unary operator '" + std::string(un.second) + "'");
    }
    Opcode op = it->second;

    RValue argR = materialize(arg, func);
    auto resType = resultType(op, argR.type(), ValueType::Void);

    RValue argConv = emitCastAndFree(argR, resType, func);

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


[[nodiscard]] Value emit(const ast::BinaryExpression& expr, FunctionEmitter& func) {
    struct visitor {
        FunctionEmitter& func;

        Value operator()(const std::tuple<
                                ast::BinaryExpressionPtr,
                                ast::BinaryExpressionPtr,
                                std::string_view
                            >& binExpr) {
            if (auto it = shortCircuitOps.find(std::get<2>(binExpr)); it != shortCircuitOps.end()) {
                auto& [lhs, rhs, _] = binExpr;

                auto lhsRes = emit(*lhs, func);
                RValue lhsR = materialize(lhsRes, func);

                RValue res = { Reg::create(ValueType::Bool) };
                emitShortCircuit(lhsR, [&]() {
                    auto rhsVal = emit(*rhs, func);
                    return materialize(rhsVal, func);
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

            auto it = binaryOps.find(std::get<2>(binExpr));
            if (it == binaryOps.end()) {
                throw IRGenError("Unsupported binary operator");
            }

            Value lop = emit(*std::get<0>(binExpr), func);
            Value rop = emit(*std::get<1>(binExpr), func);
            Opcode op = it->second;

            RValue lopR = materialize(lop, func);
            RValue ropR = materialize(rop, func);

            return { emitBinaryArithmetic(lopR, ropR, op, func) };
        }
        Value operator()(const ast::UnaryExpressionPtr& unary) {
            return emit(*unary, func);
        }
    };

    return std::visit(visitor{func}, expr.value);
}


[[nodiscard]] Value emit(const ast::ConditionalExpression& expr, FunctionEmitter& func) {
    struct visitor {
        FunctionEmitter& func;

        Value operator()(const ast::BinaryExpression& xpr) {
            return emit(xpr, func);
        }
        Value operator()(const std::tuple<  // ternary conditional operator
                                ast::BinaryExpression,
                                ast::AssignmentExpressionPtr,
                                ast::AssignmentExpressionPtr
                            >&xpr) {
            const auto& [cond, trueExpr, falseExpr] = xpr;

            auto condVal = emit(cond, func);
            RValue condR = materialize(condVal, func);
            emitPushFree(condR, func);

            auto preBlock = func.getActiveBlock();
            auto trueBlock = func.createBlock();
            auto falseBlock = func.createBlock();
            auto postBlock = func.createBlock();

            postBlock->jump = preBlock->jump;
            trueBlock->jump = Terminator::jump(postBlock);
            falseBlock->jump = Terminator::jump(postBlock);
            preBlock->jump = Terminator::branch(condR, trueBlock, falseBlock);

            auto emitBranch = [&](BasicBlockPtr block, const auto& expr) -> std::pair<BasicBlockPtr, RValue> {
                func.setActiveBlock(block);
                Value branchRes = emit(expr, func);
                RValue resR = materialize(branchRes, func);

                return { func.getActiveBlock(), resR };
            };

            auto [ trueCont, trueRes ] = emitBranch(trueBlock, *trueExpr);
            auto [ falseCont, falseRes ] = emitBranch(falseBlock, *falseExpr);

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
    };

    return std::visit(visitor{func}, expr.value);
}


[[nodiscard]] RValue emit(const ast::Assignment& assign, FunctionEmitter& func);


[[nodiscard]] Value emit(const ast::AssignmentExpression& expr, FunctionEmitter& func) {
    struct visitor {
        FunctionEmitter& func;

        Value operator()(const ast::ConditionalExpression& cond) {
            return emit(cond, func);
        }
        Value operator()(const ast::YieldExpression&) {
            throw IRGenError("Yield expressions are not supported");
        }
        Value operator()(const ast::ArrowFunction&) {
            throw IRGenError("Arrow functions are not supported");
        }
        Value operator()(const ast::AsyncArrowFunction&) {
            throw IRGenError("Async arrow functions are not supported");
        }
        Value operator()(const ast::Assignment& assign) {
            return { emit(assign, func) };
        }
    };

    return std::visit(visitor{func}, expr.value);
}


[[nodiscard]] RValue emit(const ast::Assignment& assign, FunctionEmitter& func) {
    struct visitor {
        FunctionEmitter& func;

        Value operator()(const ast::MemberExpression& expr) {
            return emit(expr, func);
        }
        Value operator()(const ast::NewExpressionPtr&) {
            throw IRGenError("Assignment patterns are not supported");
        }
    };

    if (!std::holds_alternative<ast::NewExpression>(assign.lhs.value)) {
        throw IRGenError("Only new expressions are supported in assignments");
    }
    Value target = std::visit(visitor{func}, std::get<ast::NewExpression>(assign.lhs.value).value);
    if (target.isRValue()) {
        throw IRGenError("Invalid assignment target");
    }

    if (assign.op == "=") {
        Value rhs = emit(*assign.rhs, func);
        RValue rhsR = materialize(rhs, func);

        auto targetLV = target.asLVRef();
        return emitAssign(targetLV, rhsR, func);
    }
    if (auto it = arithAssignmentOps.find(assign.op); it != arithAssignmentOps.end()) {
        Value rhs = emit(*assign.rhs, func);
        RValue rhsR = materialize(rhs, func);

        Opcode op = it->second;
        RValue targetR = materialize(target, func);

        RValue res = emitBinaryArithmetic(targetR, rhsR, op, func);
        return emitAssign(target.asLVRef(), res, func);
    }
    if (auto it = shortCircuitAssignmentOps.find(assign.op); it != shortCircuitAssignmentOps.end()) {
        RValue targetR = materialize(target, func);

        emitShortCircuit(targetR, [&]() {
            Value rhs = emit(*assign.rhs, func);
            return materialize(rhs, func);
        },
        [&](RValue val, bool skipped) {
            if (skipped) {
                return;
            }
            (void) emitAssign(target.asLVRef(), val, func);
            if (target.asLVRef().isMember()) {
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


void emit(const ast::LexicalDeclaration& lexical, FunctionEmitter& func) {
    struct visitor {
        FunctionEmitter& func;
        ValueType type;

        void operator()(const ast::BindingIdentifier& decl) {
            func.addLexical(decl.identifier.name, type, false);
            // do nothing
        }
        void operator()(const std::pair<ast::BindingIdentifier, ast::InitializerPtr>& assign) {
            const auto& [binding, initializer] = assign;

            Value rhs = emit(*initializer, func);
            LVRef target = func.addLexical(binding.identifier.name, type, false);

            RValue rhsR = materialize(rhs, func);

            if (target.type() != rhsR.type()) {
                emitPushFree(rhsR, func);
            }

            func.emitInstruction({Operation{
                .op = Opcode::Set,
                .a = rhsR,
                .res = giveSimple(target, func)
            }});

            emitPushFree(giveSimple(target, func), func);
        }
        void operator()(const std::pair<ast::BindingPattern, ast::InitializerPtr>&) {
            throw IRGenError("Binding patterns are not supported");
        }
    };

    for (const ast::LexicalBinding& binding : lexical.bindings) {
        if (!binding.type) {
            throw IRGenError("Lexical bindings must have a type");
        }
        std::visit(visitor{ func, getType(binding.type->type) }, binding.value);
    }
}


void emit(const ast::Declaration& declaration, FunctionEmitter& func) {
    if (!std::holds_alternative<ast::LexicalDeclaration>(declaration.value)) {
        throw IRGenError("Only lexical declarations are supported");
    }

    emit(std::get<ast::LexicalDeclaration>(declaration.value), func);
}


[[nodiscard]] RValue emit(const ast::Expression& expr, FunctionEmitter& func) {
    for (size_t i = 0; i + 1 < expr.items.size(); i++) {
        Value v = emit(*expr.items[i], func);
        emitPushFree(v, func);
    }
    if (!expr.items.empty()) {
        Value last = emit(*expr.items.back(), func);
        return materialize(last, func);
    }
    throw IRGenError("Empty expression");
}


bool emit(const ast::Statement& statement, FunctionEmitter& func);


void emit(const ast::IfStatement& stmt, FunctionEmitter& func) {
    auto preBlock = func.getActiveBlock();
    auto ifBlock = func.createBlock();
    auto elseBlock = func.createBlock();  // XXX: set to null if no else block
    auto postBlock = func.createBlock();

    postBlock->jump = preBlock->jump;
    ifBlock->jump = Terminator::jump(postBlock);
    elseBlock->jump = Terminator::jump(postBlock);

    // condition block
    RValue cond = emit(stmt.expression, func);
    RValue res = emitCastAndFree(cond, ValueType::Bool, func);
    emitPushFree(res, func);

    func.getActiveBlock()->jump = Terminator::branch(res, ifBlock, elseBlock);

    // if block
    func.setActiveBlock(ifBlock);
    emit(*stmt.consequent, func);

    // else block
    if (stmt.alternate) {
        func.setActiveBlock(elseBlock);
        emit(**stmt.alternate, func);
    }

    func.setActiveBlock(postBlock);
}


void emit(const ast::DoWhileStatement& stmt, FunctionEmitter& func) {
    auto preBlock = func.getActiveBlock();
    auto condBlock = func.createBlock();
    auto loopBlock = func.createBlock();
    auto postBlock = func.createBlock();

    postBlock->jump = preBlock->jump;
    preBlock->jump = Terminator::jump(loopBlock);
    loopBlock->jump = Terminator::jump(condBlock);

    // condition block
    func.setActiveBlock(condBlock);
    RValue cond = emit(stmt.expression, func);
    RValue res = emitCastAndFree(cond, ValueType::Bool, func);
    emitPushFree(res, func);

    func.getActiveBlock()->jump = Terminator::branch(res, loopBlock, postBlock);

    // loop block
    func.setActiveBlock(loopBlock);
    {
        auto _ = func.pushBreakTarget(postBlock);
        auto __ = func.pushContinueTarget(condBlock);
        emit(*stmt.statement, func);
    }

    func.setActiveBlock(postBlock);
}


void emit(const ast::WhileStatement& stmt, FunctionEmitter& func) {
    auto preBlock = func.getActiveBlock();
    auto condBlock = func.createBlock();
    auto loopBlock = func.createBlock();
    auto postBlock = func.createBlock();

    postBlock->jump = preBlock->jump;
    preBlock->jump = Terminator::jump(condBlock);
    loopBlock->jump = Terminator::jump(condBlock);

    // condition block
    func.setActiveBlock(condBlock);
    RValue cond = emit(stmt.expression, func);
    RValue res = emitCastAndFree(cond, ValueType::Bool, func);
    emitPushFree(res, func);

    func.getActiveBlock()->jump = Terminator::branch(res, loopBlock, postBlock);

    // loop block
    func.setActiveBlock(loopBlock);
    {
        auto _ = func.pushBreakTarget(postBlock);
        auto __ = func.pushContinueTarget(condBlock);
        emit(*stmt.statement, func);
    }

    func.setActiveBlock(postBlock);
}


void emit(const ast::ForStatement& stmt, FunctionEmitter& func) {
    struct InitVisitor {
        FunctionEmitter& func;

        void operator()(std::monostate) {
            // do nothing
        }
        void operator()(const ast::Expression& expr) {
            RValue v = emit(expr, func);
            emitPushFree(v, func);
        }
        void operator()(const ast::VariableDeclarationList&) {
            throw IRGenError("Variable declarations are not supported");
        }
        void operator()(const ast::LexicalDeclaration& decl) {
            emit(decl, func);
        }
    };

    BasicBlockPtr preBlock = func.getActiveBlock();
    BasicBlockPtr initBlock = func.createBlock();
    BasicBlockPtr condBlock = func.createBlock();
    BasicBlockPtr loopBlock = func.createBlock();
    BasicBlockPtr updateBlock = func.createBlock();
    BasicBlockPtr postBlock = func.createBlock();

    postBlock->jump = preBlock->jump;
    preBlock->jump = Terminator::jump(initBlock);
    initBlock->jump = Terminator::jump(condBlock);
    loopBlock->jump = Terminator::jump(updateBlock);
    updateBlock->jump = Terminator::jump(condBlock);

    auto _ = func.pushScope();

    // init block
    func.setActiveBlock(initBlock);
    std::visit(InitVisitor{func}, stmt.init);

    // condition block
    if (stmt.condition) {
        func.setActiveBlock(condBlock);
        RValue cond = emit(*stmt.condition, func);
        RValue res = emitCastAndFree(cond, ValueType::Bool, func);
        emitPushFree(res, func);

        func.getActiveBlock()->jump = Terminator::branch(res, loopBlock, postBlock);
    }
    else {
        func.getActiveBlock()->jump = Terminator::jump(loopBlock);
    }

    // update block
    if (stmt.update) {
        func.setActiveBlock(updateBlock);
        RValue v = emit(*stmt.update, func);
        emitPushFree(v, func);
    }

    // loop block
    func.setActiveBlock(loopBlock);
    {
        auto __ = func.pushBreakTarget(postBlock);
        auto ___ = func.pushContinueTarget(updateBlock);
        emit(*stmt.statement, func);
    }

    func.setActiveBlock(postBlock);
}


void emit(const ast::IterationStatement& stmt, FunctionEmitter& func) {
    struct visitor {
        FunctionEmitter& func;

        void operator()(const ast::DoWhileStatement& stmt) {
            emit(stmt, func);
        }
        void operator()(const ast::WhileStatement& stmt) {
            emit(stmt, func);
        }
        void operator()(const ast::ForStatement& stmt) {
            emit(stmt, func);
        }
        void operator()(const ast::ForInOfStatement&) {
            throw IRGenError("For-in/of statements are not supported");
        }
    };

    // XXX: move continue target here?
    std::visit(visitor{func}, stmt.value);
}


void emit(const ast::BreakableStatement& stmt, FunctionEmitter& func) {
    struct visitor {
        FunctionEmitter& func;

        void operator()(const ast::IterationStatement& stmt) {
            emit(stmt, func);
        }
        void operator()(const ast::SwitchStatement&) {
            throw IRGenError("Switch statements are not supported");
        }
    };

    // XXX: move break target here?
    std::visit(visitor{func}, stmt.value);
}


void emit(const ast::ReturnStatement& stmt, FunctionEmitter& func) {
    if (!stmt.expression) {
        func.getActiveBlock()->jump = Terminator::ret();
        return;
    }

    Value arg = { emit(*stmt.expression, func) };
    RValue argR = materialize(arg, func);
    RValue conv = emitCastAndFree(argR, func.signature->ret, func);

    func.getActiveBlock()->jump = Terminator::retVal(conv);
}

void emit(const ast::ContinueStatement& stmt, FunctionEmitter& func) {
    if (stmt.label) {
        throw IRGenError("Labeled continue statements are not supported");
    }
    if (auto target = func.getContinueTarget()) {
        func.getActiveBlock()->jump = Terminator::jump(target);
    }
    else {
        throw IRGenError("Continue statement without target");
    }
}

void emit(const ast::BreakStatement& stmt, FunctionEmitter& func) {
    if (stmt.label) {
        throw IRGenError("Labeled break statements are not supported");
    }
    if (auto target = func.getBreakTarget()) {
        func.getActiveBlock()->jump = Terminator::jump(target);
    }
    else {
        throw IRGenError("Break statement without target");
    }
}

void emit(const ast::ThrowStatement& stmt, FunctionEmitter& func) {
    RValue val = emit(stmt.expression, func);
    RValue anyVal = emitCastAndFree(val, ValueType::Any, func);

    func.getActiveBlock()->jump = Terminator::throw_(anyVal);
}


bool emit(const ast::Block& block, FunctionEmitter& func);


bool emit(const ast::Statement& statement, FunctionEmitter& func) {
    struct visitor {
        FunctionEmitter& func;

        bool operator()(const ast::BlockStatement& stmt) {
            return emit(stmt, func);
        }
        bool operator()(const ast::VariableStatement&) {
            throw IRGenError("Variable statements are not supported");
        }
        bool operator()(const ast::EmptyStatement&) {
            throw IRGenError("Empty statements are not supported");
        }
        bool operator()(const ast::ExpressionStatement& stmt) {
            RValue v = emit(stmt.expression, func);
            if (v.type() == ValueType::Void) {
                return false;
            }
            emitPushFree(v, func);
            return false;
        }
        bool operator()(const ast::IfStatement& stmt) {
            emit(stmt, func);
            return false;
        }
        bool operator()(const ast::BreakableStatement& stmt) {
            emit(stmt, func);
            return false;
        }
        bool operator()(const ast::ContinueStatement& stmt) {
            emit(stmt, func);
            return false;
        }
        bool operator()(const ast::BreakStatement& stmt) {
            emit(stmt, func);
            return true;
        }
        bool operator()(const ast::ReturnStatement& stmt) {
            emit(stmt, func);
            return true;
        }
        bool operator()(const ast::WithStatement&) {
            throw IRGenError("With statements are not supported");
        }
        bool operator()(const ast::LabeledStatement&) {
            throw IRGenError("Labeled statements are not supported");
        }
        bool operator()(const ast::ThrowStatement& stmt) {
            emit(stmt, func);
            return true;
        }
        bool operator()(const ast::TryStatement&) {
            throw IRGenError("Try statements are not supported");
        }
        bool operator()(const ast::DebuggerStatement&) {
            throw IRGenError("Debugger statements are not supported");
        }
    };

    return std::visit(visitor{func}, statement.value);
}


// returns true if the block contains a "terminating" statement
bool emit(const ast::StatementList& list, FunctionEmitter& func) {
    struct visitor {
        FunctionEmitter& func;
        bool operator()(const ast::Statement& statement) {
            return emit(statement, func);
        }
        bool operator()(const ast::Declaration& declaration) {
            emit(declaration, func);
            return false;
        }
    };

    for (const ast::StatementListItem& statement : list.items) {
        if (std::visit(visitor{func}, statement.value)) {
            return true;
        }
    }

    return false;
}


bool emit(const ast::Block& block, FunctionEmitter& func) {
    if (block.statementList) {
        auto _ = func.pushScope();
        return emit(*block.statementList, func);
    }
    return false;
}


SignaturePtr getSignature(const ast::FunctionDeclaration& decl) {
    auto sig = std::make_shared<Signature>();
    if (!decl.returnType) {
        return nullptr;
    }
    sig->ret = getType(decl.returnType->type);

    for (const ast::FormalParameter& arg : decl.parameters.parameterList) {
        if (!std::holds_alternative<ast::BindingIdentifier>(arg.value)) {
            return nullptr;
        }
        const auto& binding = std::get<ast::BindingIdentifier>(arg.value);

        if (!arg.type) {
            return nullptr;
        }

        sig->args.emplace_back(binding.identifier.name, getType(arg.type->type));
    }

    return sig;
}

FunctionEmitter emit(const ast::FunctionDeclaration& decl, SignaturePtr sig, const std::map<cfg::Identifier, cfg::SignaturePtr>& otherSignatures) {
    if (!decl.name) {
        throw IRGenError("Function declarations must have a name");
    }

    FunctionEmitter out(otherSignatures);
    out.setSignature(sig);
    out.setFunctionName(decl.name->identifier.name);

    emit(decl.body->statementList, out);

    if (out.getActiveBlock()->jump.type == Terminator::None) {
        out.getActiveBlock()->jump = Terminator::ret();
    }

    return out;
}


}  // namespace jac::cfg
