#pragma once

#include "scanner.h"
#include "../../util.h"

#include <cassert>
#include <memory>
#include <span>
#include <variant>
#include <vector>


namespace jac::ast {


struct ASTNode;
using ASTNodePtr = std::unique_ptr<ASTNode>;
struct ASTNode {
    std::vector<ASTNodePtr> children;

    ASTNode() = default;
    ASTNode(std::vector<ASTNodePtr> children_) : children(std::move(children_)) {}

    ASTNode(ASTNode&&) = default;
    virtual ~ASTNode() = default;
};
using IdentifierName = std::string;


template<typename... Args>
std::vector<ASTNodePtr> makeChildren(Args&&... args) {
    std::vector<ASTNodePtr> children;
    children.reserve(sizeof...(args));
    (children.emplace_back(std::forward<Args>(args)), ...);
    return children;
}


struct Expression : public ASTNode {
    Expression(std::vector<ASTNodePtr> children_) : ASTNode(std::move(children_)) {}
};
using ExpressionPtr = std::unique_ptr<Expression>;


struct Statement : public ASTNode {
    Statement(std::vector<ASTNodePtr> children_) : ASTNode(std::move(children_)) {}
};
using StatementPtr = std::unique_ptr<Statement>;

struct TypeAnnotation : public ASTNode {
    IdentifierName name;

    TypeAnnotation(IdentifierName name_) : name(std::move(name_)) {}
};
using TypeAnnotationPtr = std::unique_ptr<TypeAnnotation>;


struct Identifier : public Expression {
    IdentifierName name;

    Identifier(IdentifierName name_):
        Expression({}),
        name(std::move(name_))
    {
        assert(!name.empty());
    }

    bool isPrivate() const {
        return name[0] == '#';
    }
};
using IdentifierPtr = std::unique_ptr<Identifier>;


struct BindingElement : public ASTNode {
    BindingElement(IdentifierPtr target, TypeAnnotationPtr type_, ExpressionPtr initializer_):
        ASTNode(makeChildren(std::move(target), std::move(type_), std::move(initializer_)))
    {}

    Identifier* target() const {
        return dynamic_cast<Identifier*>(children[0].get());
    }

    TypeAnnotation* typeAnnotation() const {
        return dynamic_cast<TypeAnnotation*>(children[1].get());
    }
    Expression* initializer() const {
        return dynamic_cast<Expression*>(children[2].get());
    }
};
using BindingElementPtr = std::unique_ptr<BindingElement>;


struct Literal : public Expression {
    struct Null {};

    std::variant<Null, bool, double, int32_t, std::string> value;

    explicit Literal(auto v):
        Expression({}),
        value(std::move(v))
    {}
};
using LiteralPtr = std::unique_ptr<Literal>;


struct BinaryExpression : public Expression {
    // TODO: switch to enum op
    enum Op {
        Add,
        Subtract,
        Multiply,
        Divide
    };

    std::string op;

    BinaryExpression(ExpressionPtr left_, ExpressionPtr right_, std::string_view op_):
        Expression(makeChildren(std::move(left_), std::move(right_))),
        op(op_)
    {}

    Expression* left() const {
        return dynamic_cast<Expression*>(children[0].get());
    }
    Expression* right() const {
        return dynamic_cast<Expression*>(children[1].get());
    }
};
using BinaryExpressionPtr = std::unique_ptr<BinaryExpression>;


struct ConditionalExpression : public Expression {
    ConditionalExpression(ExpressionPtr test_, ExpressionPtr consequent_, ExpressionPtr alternate_):
        Expression(makeChildren(std::move(test_), std::move(consequent_), std::move(alternate_)))
    {}

    Expression* test() const {
        return dynamic_cast<Expression*>(children[0].get());
    }
    Expression* consequent() const {
        return dynamic_cast<Expression*>(children[1].get());
    }
    Expression* alternate() const {
        return dynamic_cast<Expression*>(children[2].get());
    }
};


struct UnaryExpression : public Expression {
    // TODO: switch to enum op
    enum Op {
        Negate,
        Not
    };

    std::string op;

    UnaryExpression(ExpressionPtr expr_, std::string_view op_):
        Expression(makeChildren(std::move(expr_))),
        op(op_)
    {}

    Expression* expression() const {
        return dynamic_cast<Expression*>(children[0].get());
    }
};
using UnaryExpressionPtr = std::unique_ptr<UnaryExpression>;


struct UpdateExpression : public Expression {
    enum class Op { // TODO: switch to enum
        PreInc,
        PreDec,
        PostInc,
        PostDec
    };

    Op kind;

    UpdateExpression(ASTNodePtr expr_, Op kind_):
        Expression(makeChildren(std::move(expr_))),
        kind(kind_)
    {}

    Expression* expression() const {
        return dynamic_cast<Expression*>(children[0].get());
    }
};
using UpdateExpressionPtr = std::unique_ptr<UpdateExpression>;


struct FormalParameters : public ASTNode {
    FormalParameters(BindingElementPtr restParam_, std::vector<BindingElementPtr> params_) {
        children.reserve(1 + params_.size());

        children.emplace_back(std::move(restParam_));
        for (auto& param : params_) {
            children.emplace_back(std::move(param));
        }
    }

    BindingElement* restParameter() const {
        return dynamic_cast<BindingElement*>(children[0].get());
    }
    size_t parameterCount() const {
        return children.size() - 1;
    }
    BindingElement* parameterGet(size_t index) const {
        return dynamic_cast<BindingElement*>(children[index + 1].get());
    }
};
using FormalParametersPtr = std::unique_ptr<FormalParameters>;


struct ExpressionStatement : public Statement {
    ExpressionStatement(ExpressionPtr expr_):
        Statement(makeChildren(std::move(expr_)))
    {}

    Expression* expression() const {
        return dynamic_cast<Expression*>(children[0].get());
    }
};
using ExpressionStatementPtr = std::unique_ptr<ExpressionStatement>;


struct StatementList : public Statement {
    enum Kind {
        Normal,
        Block
    };

    Kind kind;
    StatementList(Kind kind_, std::vector<StatementPtr> statements_):
        Statement({}),
        kind(kind_)
    {
        children.reserve(statements_.size());
        for (auto& stmt : statements_) {
            children.emplace_back(std::move(stmt));
        }
    }

    size_t statementCount() const {
        return children.size();
    }
    Statement* statementGet(size_t index) const {
        return dynamic_cast<Statement*>(children[index].get());
    }
};
using StatementListPtr = std::unique_ptr<StatementList>;


struct LexicalDeclaration : public Statement {
    bool isConst;

    LexicalDeclaration(bool isConst_, std::vector<BindingElementPtr> bindings_):
        Statement({}),
        isConst(isConst_)
    {
        children.reserve(bindings_.size());
        for (auto& binding : bindings_) {
            children.emplace_back(std::move(binding));
        }
    }

    size_t bindingCount() const {
        return children.size();
    }
    BindingElement* bindingGet(size_t index) const {
        return dynamic_cast<BindingElement*>(children[index].get());
    }
};
using LexicalDeclarationPtr = std::unique_ptr<LexicalDeclaration>;


struct Function : public Expression {
    bool isGenerator;
    bool isAsync;
    std::string_view code;

    Function(bool isGenerator_, bool isAsync_, IdentifierPtr name, FormalParametersPtr params_, TypeAnnotationPtr returnType_, StatementListPtr body_, std::string_view code_):
        Expression(makeChildren(std::move(name), std::move(params_), std::move(returnType_), std::move(body_))),
        isGenerator(isGenerator_),
        isAsync(isAsync_),
        code(code_)
    {}

    Identifier* name() const {
        return dynamic_cast<Identifier*>(children[0].get());
    }
    FormalParameters* parameters() const {
        return dynamic_cast<FormalParameters*>(children[1].get());
    }
    TypeAnnotation* returnType() const {
        return dynamic_cast<TypeAnnotation*>(children[2].get());
    }
    StatementList* body() const {
        return dynamic_cast<StatementList*>(children[3].get());
    }
};
using FunctionPtr = std::unique_ptr<Function>;


struct IterationStatement : public Statement {
    enum Kind {
        While,
        For,
        DoWhile
    };

    Kind kind;
private:
    IterationStatement(Kind kind_, ASTNodePtr init_, ExpressionPtr condition_, ExpressionPtr update_, StatementPtr statement_):
        Statement(makeChildren(std::move(init_) , std::move(condition_), std::move(update_), std::move(statement_))),
        kind(kind_)
    {}
public:

    IterationStatement(IterationStatement&&) = default;

    ASTNode* init() const {
        return children[0].get();
    }
    Expression* preCondition() const {
        if (kind == DoWhile) {
            return nullptr;
        }
        return dynamic_cast<Expression*>(children[1].get());
    }
    Expression* postCondition() const {
        if (kind != DoWhile) {
            return nullptr;
        }
        return dynamic_cast<Expression*>(children[1].get());
    }
    Expression* update() const {
        return dynamic_cast<Expression*>(children[2].get());
    }
    Statement* statement() const {
        return dynamic_cast<Statement*>(children[3].get());
    }


    static IterationStatement while_(ExpressionPtr condition_, StatementPtr statement_) {
        return IterationStatement(While, nullptr, std::move(condition_), nullptr, std::move(statement_));
    }
    static IterationStatement for_(ASTNodePtr init_, ExpressionPtr condition_, ExpressionPtr update_, StatementPtr statement_) {
        return IterationStatement(For, std::move(init_), std::move(condition_), std::move(update_), std::move(statement_));
    }
    static IterationStatement doWhile(StatementPtr statement_, ExpressionPtr condition_) {
        return IterationStatement(DoWhile, nullptr, std::move(condition_), nullptr, std::move(statement_));
    }
};
using IterationStatementPtr = std::unique_ptr<IterationStatement>;


struct ContinueStatement : public Statement {
    ContinueStatement(IdentifierPtr label_):
        Statement(makeChildren(std::move(label_)))
    {}

    Identifier* label() const {
        return dynamic_cast<Identifier*>(children[0].get());
    }
};
using ContinueStatementPtr = std::unique_ptr<ContinueStatement>;

struct BreakStatement : public Statement {
    BreakStatement(IdentifierPtr label_):
        Statement(makeChildren(std::move(label_)))
    {}

    Identifier* label() const {
        return dynamic_cast<Identifier*>(children[0].get());
    }
};
using BreakStatementPtr = std::unique_ptr<BreakStatement>;


struct ReturnStatement : public Statement {
    ReturnStatement(ExpressionPtr expr_):
        Statement(makeChildren(std::move(expr_)))
    {}

    Expression* expression() const {
        return dynamic_cast<Expression*>(children[0].get());
    }
};
using ReturnStatementPtr = std::unique_ptr<ReturnStatement>;


struct ThrowStatement : public Statement {
    ThrowStatement(ExpressionPtr expr_):
        Statement(makeChildren(std::move(expr_)))
    {}

    Expression* expression() const {
        return dynamic_cast<Expression*>(children[0].get());
    }
};
using ThrowStatementPtr = std::unique_ptr<ThrowStatement>;


struct DebuggerStatement : public Statement {
    DebuggerStatement():
        Statement({})
    {}
};
using DebuggerStatementPtr = std::unique_ptr<DebuggerStatement>;


struct HoistableDeclaration : public Statement {
    HoistableDeclaration(FunctionPtr function_):
        Statement(makeChildren(std::move(function_)))
    {}

    Function* function() const {
        return dynamic_cast<Function*>(children[0].get());
    }
};
using HoistableDeclarationPtr = std::unique_ptr<HoistableDeclaration>;


struct IfStatement : public Statement {
    IfStatement(ExpressionPtr condition_, StatementPtr consequent_, StatementPtr alternate_):
        Statement(makeChildren(std::move(condition_), std::move(consequent_), std::move(alternate_)))
    {}

    Expression* condition() const {
        return dynamic_cast<Expression*>(children[0].get());
    }
    Statement* consequent() const {
        return dynamic_cast<Statement*>(children[1].get());
    }
    Statement* alternate() const {
        return dynamic_cast<Statement*>(children[2].get());
    }
};
using IfStatementPtr = std::unique_ptr<IfStatement>;


struct EmptyStatement : public Statement {
    EmptyStatement() : Statement({}) {}
};
using EmptyStatementPtr = std::unique_ptr<EmptyStatement>;


struct CoverParenthesizedExpressionAndArrowParameterList : public ASTNode {
    CoverParenthesizedExpressionAndArrowParameterList(std::vector<ASTNodePtr> children_, ASTNodePtr rest):
        ASTNode(std::move(children_))
    {
        children.shrink_to_fit();
        children.emplace_back(std::move(rest));
    }

    ASTNode* rest() const {
        return children.back().get();
    }
};
using CoverParenthesizedExpressionAndArrowParameterListPtr = std::unique_ptr<CoverParenthesizedExpressionAndArrowParameterList>;


struct Arguments : public ASTNode {
    Arguments(std::vector<ExpressionPtr> args_, ExpressionPtr spread_) {
        children.reserve(args_.size() + 1);
        for (auto& arg : args_) {
            children.emplace_back(std::move(arg));
        }
        children.emplace_back(std::move(spread_));
    }

    Expression* spread() const {
        return dynamic_cast<Expression*>(children.back().get());
    }

    size_t argCount() const {
        return children.size() - 1;
    }

    Expression* argGet(size_t index) const {
        return dynamic_cast<Expression*>(children[index].get());
    }
};
using ArgumentsPtr = std::unique_ptr<Arguments>;


struct CoverCallExpressionAndAsyncArrowHead : public ASTNode {
    CoverCallExpressionAndAsyncArrowHead(ExpressionPtr callee_, ArgumentsPtr arguments_):
        ASTNode(makeChildren(std::move(callee_), std::move(arguments_)))
    {}
};
using CoverCallExpressionAndAsyncArrowHeadPtr = std::unique_ptr<CoverCallExpressionAndAsyncArrowHead>;


struct NewCallExpression : public Expression {
    NewCallExpression(ExpressionPtr callee_, ArgumentsPtr arguments_):
        Expression(makeChildren(std::move(callee_), std::move(arguments_)))
    {}

    Expression* callee() const {
        return dynamic_cast<Expression*>(children[0].get());
    }
    Arguments* arguments() const {
        return dynamic_cast<Arguments*>(children[1].get());
    }
};
using NewCallExpressionPtr = std::unique_ptr<NewCallExpression>;


struct CommaExpression : public Expression {
    CommaExpression(std::vector<ExpressionPtr> items_):
        Expression({})
    {
        children.reserve(items_.size());
        for (auto& item : items_) {
            children.emplace_back(std::move(item));
        }
    }

    CommaExpression(std::vector<ASTNodePtr> items_):
        Expression(std::move(items_))
    {}

    auto itemCount() const {
        return children.size();
    }
    Expression* itemGet(size_t index) const {
        return dynamic_cast<Expression*>(children[index].get());
    }
};


struct Script : public ASTNode {
    Script(StatementListPtr body_):
        ASTNode(makeChildren(std::move(body_)))
    {}

    StatementList* body() const {
        return dynamic_cast<StatementList*>(children[0].get());
    }
};
using ScriptPtr = std::unique_ptr<Script>;


struct Assignment : public Expression {
    std::string op;

    Assignment(ExpressionPtr left_, ExpressionPtr right_, std::string_view op_):
        Expression(makeChildren(std::move(left_), std::move(right_))),
        op(op_)
    {}

    Expression* left() const {
        return dynamic_cast<Expression*>(children[0].get());
    }
    Expression* right() const {
        return dynamic_cast<Expression*>(children[1].get());
    }
};
using AssignmentPtr = std::unique_ptr<Assignment>;


struct MemberAccessExpression : public Expression {
    MemberAccessExpression(ExpressionPtr object_, ExpressionPtr property_):
        Expression(makeChildren(std::move(object_), std::move(property_)))
    {}

    Expression* object() const {
        return dynamic_cast<Expression*>(children[0].get());
    }
    Expression* property() const {
        return dynamic_cast<Expression*>(children[1].get());
    }
};
using MemberAccessExpressionPtr = std::unique_ptr<MemberAccessExpression>;


struct TaggedTemplateExpression : public Expression {
    TaggedTemplateExpression(ExpressionPtr tag_, ExpressionPtr templateLiteral_):
        Expression(makeChildren(std::move(tag_), std::move(templateLiteral_)))
    {}

    Expression* tag() const {
        return dynamic_cast<Expression*>(children[0].get());
    }
    Expression* templateLiteral() const {
        return dynamic_cast<Expression*>(children[1].get());
    }
};
using TaggedTemplateExpressionPtr = std::unique_ptr<TaggedTemplateExpression>;


struct CallExpression : public Expression {
    CallExpression(ExpressionPtr callee_, ArgumentsPtr arguments_):
        Expression(makeChildren(std::move(callee_), std::move(arguments_)))
    {}

    Expression* callee() const {
        return dynamic_cast<Expression*>(children[0].get());
    }
    Arguments* arguments() const {
        return dynamic_cast<Arguments*>(children[1].get());
    }
};
using CallExpressionPtr = std::unique_ptr<CallExpression>;


struct ThisExpression : public Expression {
    ThisExpression():
        Expression({})
    {}
};
using ThisExpressionPtr = std::unique_ptr<ThisExpression>;



struct Yield {
    const bool value = true;
};
struct Await {
    const bool value = true;
};
struct In {
    const bool value = true;
};
struct Return {
    const bool value = true;
};


template<typename... Stacks>
struct PushPopper {
    std::tuple<Stacks...> stacks;

    PushPopper(auto... stacks_):
        stacks(stacks_.first...)
    {
        (stacks_.first.push_back(stacks_.second), ...);
    }

    ~PushPopper() {
        std::apply([](auto&... stacks_) {
            (stacks_.pop_back(), ...);
        }, stacks);
    }
};
template<typename... Args>
PushPopper(Args... args) -> PushPopper<decltype(args.first)...>;


class ParserState {
    std::span<lex::Token> _tokens;
    std::span<lex::Token>::iterator _pos;

    lex::Token _errorToken = lex::Token(0, 0, "", lex::Token::NoToken);
    std::string_view _errorMessage;

    std::vector<bool> yieldStack;
    std::vector<bool> awaitStack;
    std::vector<bool> inStack;
    std::vector<bool> returnStack;

    template<typename Arg>
    auto& getStack() {
        if constexpr (std::is_same_v<Arg, Yield>) {
            return yieldStack;
        }
        else if constexpr (std::is_same_v<Arg, Await>) {
            return awaitStack;
        }
        else if constexpr (std::is_same_v<Arg, In>) {
            return inStack;
        }
        else if constexpr (std::is_same_v<Arg, Return>) {
            return returnStack;
        }
    }
public:
    ParserState(std::span<lex::Token> tokens):
        _tokens(tokens),
        _pos(_tokens.begin())
    {}

    void error(std::string_view message) {
        if (_tokens.empty()) {
            _errorMessage = message;
            return;
        }
        if (_pos == _tokens.end()) { // TODO: fix token position
            _errorToken = _tokens.back();
            _errorMessage = message;
            return;
        }
        if (_errorToken.text.begin() > _pos->text.begin()) {
            return;
        }
        _errorToken = current();
        _errorMessage = message;
    }

    lex::Token current() {
        if (_tokens.empty()) {
            return lex::Token(0, 0, "", lex::Token::NoToken);
        }
        if (_pos == _tokens.end()) {
            auto pos = _tokens.back().text.end();
            return lex::Token(_tokens.back().line + 1, 0, std::string_view(pos, pos), lex::Token::NoToken);
        }
        return *_pos;
    }

    void advance() {
        if (_pos == _tokens.end()) {
            error("Unexpected end of input");
            return;
        }
        ++_pos;
    }

    void backtrack() {
        assert(_pos != _tokens.begin());
        --_pos;
    }

    bool isEnd() {
        return _pos == _tokens.end();
    }

    auto getPosition() const {
        return _pos;
    }

    void restorePosition(auto position) {
        _pos = position;
    }

    std::string_view getErrorMessage() const {
        return _errorMessage;
    }

    lex::Token getErrorToken() const {
        return _errorToken;
    }

    bool getYield() const {
        if (yieldStack.empty()) {
            return false;
        }
        return yieldStack.back();
    }
    bool getAwait() const {
        if (awaitStack.empty()) {
            return false;
        }
        return awaitStack.back();
    }
    bool getIn() const {
        if (inStack.empty()) {
            return false;
        }
        return inStack.back();
    }
    bool getReturn() const {
        if (returnStack.empty()) {
            return false;
        }
        return returnStack.back();
    }

    template<auto... Args>
    auto pushTemplate() {
        auto make = [&](auto arg) {
            auto& stack = getStack<std::decay_t<decltype(arg)>>();
            return std::pair<decltype(stack), bool>(stack, arg.value);
        };
        return PushPopper(make(Args)...);
    }
};


ExpressionPtr parseAssignmentExpression(ParserState& state);
StatementPtr parseStatement(ParserState& state);
StatementListPtr parseStatementList(ParserState& state);
ExpressionPtr parseExpression(ParserState& state);
std::optional<IdentifierName> parseIdentifierName(ParserState& state, bool allowReserved);
IdentifierPtr parseIdentifier(ParserState& state);
IdentifierPtr parseSpecialIdentifier(ParserState& state, bool allowYield, bool allowAwait);
IdentifierPtr parseIdentifierReference(ParserState& state);
IdentifierPtr parseBindingIdentifier(ParserState& state);
IdentifierPtr parseLabelIdentifier(ParserState& state);
IdentifierPtr parsePrivateIdentifier(ParserState& state);
ThisExpressionPtr parseThisExpr(ParserState& state);
LiteralPtr parseNullLiteral(ParserState& state);
LiteralPtr parseBooleanLiteral(ParserState& state);
LiteralPtr parseNumericLiteral(ParserState& state);
LiteralPtr parseStringLiteral(ParserState& state);
LiteralPtr parseLiteral(ParserState& state);
ExpressionPtr parseUnaryExpression(ParserState& state);
ExpressionPtr parseLeftHandSideExpression(ParserState& state);
ExpressionPtr parseUpdateExpression(ParserState& state);
ExpressionPtr parseUnaryExpression(ParserState& state);
ExpressionPtr parseBinaryExpression(ParserState& state);
TypeAnnotationPtr parseTypeAnnotation(ParserState& state);
ExpressionPtr parseInitializer(ParserState& state);
BindingElementPtr parseSingleNameBinding(ParserState& state);
auto parseBindingPattern(ParserState&);
BindingElementPtr parseBindingElement(ParserState& state);
auto parseBindingRestElement(ParserState&);
constexpr auto parseFormalParameter = parseBindingElement;
auto parseFormalParameters(ParserState& state);
StatementListPtr parseFunctionBody(ParserState& state);
StatementListPtr parseBlock(ParserState& state);
BindingElementPtr parseLexicalBinding(ParserState& state);
LexicalDeclarationPtr parseLexicalDeclaration(ParserState& state);
auto parseVariableStatement(ParserState&);
EmptyStatementPtr parseEmptyStatement(ParserState& state);
ExpressionStatementPtr parseExpressionStatement(ParserState& state);
ExpressionPtr parseExpressionParenthesised(ParserState& state);
IterationStatementPtr parseDoWhileStatement(ParserState& state);
IterationStatementPtr parseWhileStatement(ParserState& state);
IterationStatementPtr parseForInOfStatement(ParserState&);
IterationStatementPtr parseForStatement(ParserState& state);
IterationStatementPtr parseIterationStatement(ParserState& state);
auto parseSwitchStatement(ParserState&);
StatementPtr parseBreakableStatement(ParserState& state);
ContinueStatementPtr parseContinueStatement(ParserState& state);
BreakStatementPtr parseBreakStatement(ParserState& state);
ReturnStatementPtr parseReturnStatement(ParserState& state);
ThrowStatementPtr parseThrowStatement(ParserState& state);
auto parseWithStatement(ParserState&);
FunctionPtr parseFunction(ParserState& state, TriState identifierRequired);
FunctionPtr parseFunctionDeclaration(ParserState& state, bool Default);
auto parseLabeledStatement(ParserState&);
auto parseTryStatement(ParserState&);
DebuggerStatementPtr parseDebuggerStatement(ParserState& state);
IfStatementPtr parseIfStatement(ParserState& state);
constexpr auto parseBlockStatement = parseBlock;
StatementPtr parseStatement(ParserState& state);
auto parseGeneratorDeclaration(ParserState&, bool);
auto parseAsyncFunctionDeclaration(ParserState&, bool);
auto parseAsyncGeneratorDeclaration(ParserState&, bool);
HoistableDeclarationPtr parseHoistableDeclaration(ParserState& state, bool Default);
auto parseClassDeclaration(ParserState&, bool);
StatementPtr parseDeclaration(ParserState& state);
StatementPtr parseStatementListItem(ParserState& state);
auto parseRegularExpressionLiteral(ParserState&);
auto parseTemplateLiteral(ParserState&, bool);
CoverParenthesizedExpressionAndArrowParameterListPtr parseCoverParenthesizedExpressionAndArrowParameterList(ParserState& state);
ExpressionPtr parsePrimaryExpression(ParserState& state);
auto parseSuperProperty(ParserState&);
auto parseMetaProperty(ParserState&);
ArgumentsPtr parseArguments(ParserState& state);
ExpressionPtr parseMemberExpression(ParserState& state, int& initNewCount);
ExpressionPtr parseNewExpression(ParserState& state);
auto parseSuperCall(ParserState&);
auto parseImportCall(ParserState&);
CoverCallExpressionAndAsyncArrowHeadPtr parseCoverCallExpressionAndAsyncArrowHead(ParserState& state);
ExpressionPtr parseCallExpression(ParserState& state);
ExpressionPtr parseLeftHandSideExpression(ParserState& state);
auto parseYieldExpression(ParserState&);
auto parseArrowFunction(ParserState&);
auto parseAsyncArrowFunction(ParserState&);
ScriptPtr parseScript(ParserState& state);
ExpressionPtr parseConditionalExpression(ParserState& state);
AssignmentPtr parseAssignment(ParserState& state);
ExpressionPtr parseAssignmentExpression(ParserState& state);
ExpressionPtr parseExpression(ParserState& state);
StatementListPtr parseStatementList(ParserState& state);


using MiscTypes = TypeList<
    TypeAnnotation, BindingElement, FormalParameters, Arguments, CoverParenthesizedExpressionAndArrowParameterList,
    CoverCallExpressionAndAsyncArrowHead, Script,
    ASTNode, Expression, Statement
>;

using ExpressionTypes = TypeList<
    Identifier, Literal, BinaryExpression, ConditionalExpression, UnaryExpression, UpdateExpression, Function,
    NewCallExpression, CommaExpression, Assignment, MemberAccessExpression, TaggedTemplateExpression, CallExpression,
    ThisExpression
>;

using StatementTypes = TypeList<
    ExpressionStatement, StatementList, LexicalDeclaration, IterationStatement, ContinueStatement, BreakStatement,
    ReturnStatement, ThrowStatement, DebuggerStatement, HoistableDeclaration, IfStatement
>;


namespace detail {


template<typename Last, typename... Ts>
struct Tail {
    using type = Last;
    using rest = TypeList<Ts...>;
};


template<typename First, typename... Ts>
decltype(auto) visitNodeImpl(auto&& node, auto&& func, TypeList<First, Ts...>) {
    using NodeT = std::remove_reference_t<decltype(node)>;
    using Target = std::conditional_t<std::is_const_v<NodeT>, const First, First>;

    if (auto ptr = dynamic_cast<Target*>(&node)) {
        RETURN_IF_NOT_VOID(func(*ptr));
    }
    if constexpr (sizeof...(Ts) > 0) {
        RETURN_IF_NOT_VOID(visitNodeImpl(node, func, TypeList<Ts...>{}));
    } else {
        throw std::runtime_error("Unhandled ASTNode type");
    }
}


static constexpr auto id = [](auto&& res) { return res; };


}  // namespace detail



template<typename Types = ConcatTypeLists<ExpressionTypes, StatementTypes, MiscTypes>>
decltype(auto) visitNode(auto&& node, auto&& func) {
    RETURN_IF_NOT_VOID(detail::visitNodeImpl(node, func, Types{}));
}


}  // namespace jac::ast
