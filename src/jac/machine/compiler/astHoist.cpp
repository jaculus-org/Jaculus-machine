#include <ranges>

#include "ast.h"


namespace jac::ast {

struct HoistPassState {
    std::vector<StatementList*> scopes;

    void enterScope(StatementList* scope) {
        scopes.push_back(scope);
    }

    void exitScope() {
        scopes.pop_back();
    }

    void declareIdentifier(const IdentifierName& name, bool isConst, bool lexical) {
        if (!lexical) {
            throw std::runtime_error("Variable declarations not supported");
        }
        for (auto& scope : std::ranges::reverse_view(scopes)) {
            if (scope->hasDeclarationOf(name)) {
                throw std::runtime_error("Redeclaration of identifier: " + name);
            }
            scope->addHoistedDeclaration(name, isConst);
        }
    }
};

struct ResolvePassState {
    std::vector<StatementList*> scopes;
    std::vector<std::unordered_set<IdentifierName>> undefinedIdentifiers;
    const ResolvePassState* parentFunctionState;
    const std::unordered_set<IdentifierName>* parameterNames;

    void enterScope(StatementList* scope) {
        scopes.push_back(scope);
        undefinedIdentifiers.emplace_back();
    }

    void exitScope() {
        auto prev = std::move(undefinedIdentifiers.back());

        undefinedIdentifiers.pop_back();
        undefinedIdentifiers.back().insert(prev.begin(), prev.end());
        scopes.pop_back();
    }

    void useIdentifier(const IdentifierName& name) {
        for (auto scope : std::ranges::reverse_view(scopes)) {
            if (scope->hasDeclarationOf(name)) {
                return;
            }
        }
        if (parameterNames != nullptr && parameterNames->contains(name)) {
            return;
        }
        undefinedIdentifiers.back().insert(name);
    }

    bool declaredInParentScopes(const IdentifierName& name) const {
        for (auto parent = parentFunctionState; parent != nullptr; parent = parent->parentFunctionState) {
            for (auto scope : std::ranges::reverse_view(parent->scopes)) {
                if (scope->hasDeclarationOf(name)) {
                    return true;
                }
            }
            if (parent->parameterNames != nullptr && parent->parameterNames->contains(name)) {
                return true;
            }
        }
        return false;
    }
};


void hoistStmt(Statement&, HoistPassState& state);
void resolveStmt(Statement&, ResolvePassState& state);
void resolveExpr(Expression&, ResolvePassState& state);
void hoistFunctionImpl(Function& fn, const ResolvePassState* parentFunctionState);


void resolveArguments(Arguments& args, ResolvePassState& state) {
    if (args.spread()) {
        resolveExpr(*args.spread(), state);
    }
    for (size_t i = 0; i < args.argCount(); i++) {
        resolveExpr(*args.argGet(i), state);
    }
}


void hoistStmt(ExpressionStatement&, HoistPassState& state) {
    // No declarations in a plain expression statement.
}

void hoistStmt(StatementList& stmtList, HoistPassState& state, bool skipEnter = false) {
    if (!skipEnter && stmtList.kind == StatementList::Block) {
        state.enterScope(&stmtList);
    }

    for (size_t i = 0; i < stmtList.statementCount(); i++) {
        auto stmt = stmtList.statementGet(i);
        hoistStmt(*stmt, state);
    }

    if (!skipEnter && stmtList.kind == StatementList::Block) {
        state.exitScope();
    }
}

void hoistStmt(LexicalDeclaration& decl, HoistPassState& state) {
    for (size_t i = 0; i < decl.bindingCount(); i++) {
        const auto& binding = decl.bindingGet(i);
        state.declareIdentifier(binding->target()->name, decl.isConst, true);
    }
}

void hoistStmt(IterationStatement& iter, HoistPassState& state) {
    if (iter.init()) {
        if (auto initDecl = dynamic_cast<LexicalDeclaration*>(iter.init())) {
            hoistStmt(*initDecl, state);
        }
        else if (dynamic_cast<Expression*>(iter.init())) {
            // Initializer expressions do not contain declarations in the current scope.
        }
        else {
            throw std::runtime_error("Invalid for loop initializer");
        }
    }

    hoistStmt(*iter.statement(), state);
}

void hoistStmt(ContinueStatement&, HoistPassState& state) {
    // nothing to do
}

void hoistStmt(BreakStatement&, HoistPassState& state) {
    // nothing to do
}

void hoistStmt(ReturnStatement&, HoistPassState& state) {
    // nothing to do
}

void hoistStmt(ThrowStatement&, HoistPassState& state) {
    // nothing to do
}

void hoistStmt(DebuggerStatement&, HoistPassState& state) {
    // nothing to do
}

void hoistStmt(HoistableDeclaration& decl, HoistPassState& state) {
    state.declareIdentifier(decl.function()->name()->name, false, true);
}

void hoistStmt(IfStatement& if_, HoistPassState& state) {
    hoistStmt(*if_.consequent(), state);
    if (if_.alternate()) {
        hoistStmt(*if_.alternate(), state);
    }
}

void hoistStmt(Statement& stmt, HoistPassState& state) {
    visitNode<StatementTypes>(stmt, overloaded{
        [&](auto& s) {
            hoistStmt(s, state);
        }
    });
}


void resolveStmt(ExpressionStatement& expr, ResolvePassState& state) {
    resolveExpr(*expr.expression(), state);
}

void resolveStmt(StatementList& stmtList, ResolvePassState& state, bool skipEnter = false) {
    if (!skipEnter && stmtList.kind == StatementList::Block) {
        state.enterScope(&stmtList);
    }

    for (size_t i = 0; i < stmtList.statementCount(); i++) {
        auto stmt = stmtList.statementGet(i);
        resolveStmt(*stmt, state);
    }

    if (!skipEnter && stmtList.kind == StatementList::Block) {
        state.exitScope();
    }
}

void resolveStmt(LexicalDeclaration& decl, ResolvePassState& state) {
    for (size_t i = 0; i < decl.bindingCount(); i++) {
        const auto& binding = decl.bindingGet(i);
        if (binding->initializer()) {
            resolveExpr(*binding->initializer(), state);
        }
    }
}

void resolveStmt(IterationStatement& iter, ResolvePassState& state) {
    if (iter.init()) {
        if (auto initDecl = dynamic_cast<LexicalDeclaration*>(iter.init())) {
            resolveStmt(*initDecl, state);
        }
        else if (auto initExpr = dynamic_cast<Expression*>(iter.init())) {
            resolveExpr(*initExpr, state);
        }
        else {
            throw std::runtime_error("Invalid for loop initializer");
        }
    }
    if (iter.condition()) {
        resolveExpr(*iter.condition(), state);
    }
    if (iter.update()) {
        resolveExpr(*iter.update(), state);
    }

    resolveStmt(*iter.statement(), state);
}

void resolveStmt(ContinueStatement&, ResolvePassState& state) {
    // nothing to do
}

void resolveStmt(BreakStatement&, ResolvePassState& state) {
    // nothing to do
}

void resolveStmt(ReturnStatement& ret, ResolvePassState& state) {
    if (ret.expression()) {
        resolveExpr(*ret.expression(), state);
    }
}

void resolveStmt(ThrowStatement& throw_, ResolvePassState& state) {
    if (throw_.expression()) {
        resolveExpr(*throw_.expression(), state);
    }
}

void resolveStmt(DebuggerStatement&, ResolvePassState& state) {
    // nothing to do
}

void resolveStmt(HoistableDeclaration& decl, ResolvePassState& state) {
    hoistFunctionImpl(*decl.function(), &state);
    for (const auto& id : decl.function()->closureVars) {
        state.useIdentifier(id);
    }
}

void resolveStmt(IfStatement& if_, ResolvePassState& state) {
    resolveExpr(*if_.condition(), state);
    resolveStmt(*if_.consequent(), state);
    if (if_.alternate()) {
        resolveStmt(*if_.alternate(), state);
    }
}

void resolveStmt(Statement& stmt, ResolvePassState& state) {
    visitNode<StatementTypes>(stmt, overloaded{
        [&](auto& s) {
            resolveStmt(s, state);
        }
    });
}


void resolveExpr(Identifier& ident, ResolvePassState& state) {
    state.useIdentifier(ident.name);
}

void resolveExpr(Literal&, ResolvePassState& state) {
    // nothing to do
}

void resolveExpr(BinaryExpression& expr, ResolvePassState& state) {
    resolveExpr(*expr.left(), state);
    resolveExpr(*expr.right(), state);
}

void resolveExpr(ConditionalExpression& cond, ResolvePassState& state) {
    resolveExpr(*cond.test(), state);
    resolveExpr(*cond.consequent(), state);
    resolveExpr(*cond.alternate(), state);
}

void resolveExpr(UnaryExpression& unary, ResolvePassState& state) {
    resolveExpr(*unary.expression(), state);
}

void resolveExpr(UpdateExpression& update, ResolvePassState& state) {
    resolveExpr(*update.expression(), state);
}

void resolveExpr(Function& fn, ResolvePassState& state) {
    hoistFunctionImpl(fn, &state);
    for (const auto& id : fn.closureVars) {
        state.useIdentifier(id);
    }
}

void resolveExpr(NewCallExpression& new_, ResolvePassState& state) {
    resolveExpr(*new_.callee(), state);
    if (new_.arguments()) {
        resolveArguments(*new_.arguments(), state);
    }
}

void resolveExpr(CommaExpression& comma, ResolvePassState& state) {
    for (size_t i = 0; i < comma.itemCount(); i++) {
        resolveExpr(*comma.itemGet(i), state);
    }
}

void resolveExpr(Assignment& assign, ResolvePassState& state) {
    resolveExpr(*assign.left(), state);
    resolveExpr(*assign.right(), state);
}

void resolveExpr(MemberAccessExpression& member, ResolvePassState& state) {
    resolveExpr(*member.object(), state);
    resolveExpr(*member.property(), state);
}

void resolveExpr(TaggedTemplateExpression& tag, ResolvePassState& state) {
    resolveExpr(*tag.tag(), state);
}

void resolveExpr(CallExpression& call, ResolvePassState& state) {
    resolveExpr(*call.callee(), state);
    resolveArguments(*call.arguments(), state);
}

void resolveExpr(ThisExpression&, ResolvePassState& state) {
    // nothing to do
}


void resolveExpr(Expression& expr, ResolvePassState& state) {
    visitNode<ExpressionTypes>(expr, overloaded{
        [&](auto& e) {
            resolveExpr(e, state);
        }
    });
}


void hoistFunctionImpl(Function& fn, const ResolvePassState* parentFunctionState) {
    auto body = fn.body();
    HoistPassState hoistState{ {} };

    hoistState.enterScope(body);

    hoistStmt(*body, hoistState, true);

    std::unordered_set<IdentifierName> parameterNames;
    auto& params = *fn.parameters();
    for (size_t i = 0; i < params.parameterCount(); i++) {
        const auto& param = params.parameterGet(i);
        parameterNames.insert(param->target()->name);
    }

    ResolvePassState resolveState{ {}, {}, parentFunctionState, &parameterNames };
    resolveState.enterScope(body);

    for (size_t i = 0; i < params.parameterCount(); i++) {
        const auto& param = params.parameterGet(i);
        if (param->initializer()) {
            resolveExpr(*param->initializer(), resolveState);
        }
    }

    resolveStmt(*body, resolveState, true);

    fn.closureVars.clear();
    fn.globalVars.clear();
    for (const auto& id : resolveState.undefinedIdentifiers.back()) {
        if (resolveState.declaredInParentScopes(id)) {
            fn.closureVars.push_back(id);
        }
        else {
            fn.globalVars.push_back(id);
        }
    }
}


void hoistFunction(Function& fn) {
    hoistFunctionImpl(fn, nullptr);
}


std::unordered_set<IdentifierName> hoistScript(Script& script) {
    auto body = script.body();
    HoistPassState hoistState{ {} };

    hoistState.enterScope(body);
    hoistStmt(*body, hoistState, true);

    ResolvePassState resolveState{ {}, {}, nullptr, nullptr };
    resolveState.enterScope(body);
    resolveStmt(*body, resolveState, true);

    script.globalVars.clear();
    for (const auto& id : resolveState.undefinedIdentifiers.back()) {
        script.globalVars.push_back(id);
    }

    return std::move(resolveState.undefinedIdentifiers.back());
}


std::unordered_set<IdentifierName> hoistModule(Module& module) {
    auto body = module.body();
    HoistPassState hoistState{ {} };

    hoistState.enterScope(body);
    hoistStmt(*body, hoistState, true);

    ResolvePassState resolveState{ {}, {}, nullptr, nullptr };
    resolveState.enterScope(body);
    resolveStmt(*body, resolveState, true);

    module.globalVars.clear();
    for (const auto& id : resolveState.undefinedIdentifiers.back()) {
        module.globalVars.push_back(id);
    }

    return std::move(resolveState.undefinedIdentifiers.back());
}


}  // namespace jac::ast
