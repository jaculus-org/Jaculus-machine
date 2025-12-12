#include "ast.h"

#include "../../util.h"

#include <cmath>
#include <memory>
#include <string_view>


namespace jac::ast {


ExpressionPtr parseAssignmentExpression(ParserState& state);
StatementPtr parseStatement(ParserState& state);
StatementListPtr parseStatementList(ParserState& state);
ExpressionPtr parseExpression(ParserState& state);


// TODO: check identifiers reserved words, await/yield
std::optional<IdentifierName> parseIdentifierName(ParserState& state, bool allowReserved) {
    // TODO: support unicode escape sequences
    if (state.current().kind == lex::Token::IdentifierName || (allowReserved && state.current().kind == lex::Token::Keyword)) {
        IdentifierName id;
        id = state.current().text;
        state.advance();
        return id;
    }

    return std::nullopt;
}


IdentifierPtr parseIdentifier(ParserState& state) {
    if (auto id = parseIdentifierName(state, false)) {
        return std::make_unique<Identifier>(*id);
    }
    return nullptr;
}


IdentifierPtr parseSpecialIdentifier(ParserState& state, bool allowYield, bool allowAwait) {
    if (auto id = parseIdentifier(state)) {
        return id;
    }

    if (auto current = state.current(); current.kind == lex::Token::Keyword) {
        if ((allowYield && current.text == "yield")
         || (allowAwait && current.text == "await")) {
            state.advance();
            return std::make_unique<Identifier>(IdentifierName(current.text));
        }
    }

    return nullptr;
}


IdentifierPtr parseIdentifierReference(ParserState& state) {
    return parseSpecialIdentifier(state, !state.getYield(), !state.getAwait());
}


IdentifierPtr parseBindingIdentifier(ParserState& state) {
    // TODO: strict mode - cannot be "arguments" or "eval"
    return parseSpecialIdentifier(state, true, true);
}


IdentifierPtr parseLabelIdentifier(ParserState& state) {
    return parseSpecialIdentifier(state, !state.getYield(), !state.getAwait());
}


IdentifierPtr parsePrivateIdentifier(ParserState& state) {
    auto start = state.getPosition();
    if (auto id = parseIdentifierName(state, true)) {
        if (id->size() < 2 || (*id)[0] != '#') {
            state.restorePosition(start);
            state.error("Private identifier must start with #");
            return nullptr;
        }
        return std::make_unique<Identifier>(*id);
    }

    return nullptr;
}


ThisExpressionPtr parseThisExpr(ParserState& state) {
    if (state.current().kind == lex::Token::Keyword && state.current().text == "this") {
        state.advance();
        return std::make_unique<ThisExpression>();
    }

    return nullptr;
}


LiteralPtr parseNullLiteral(ParserState& state) {
    if (state.current().kind == lex::Token::Keyword && state.current().text == "null") {
        state.advance();
        return std::make_unique<Literal>(Literal::Null{});
    }

    return nullptr;
}


LiteralPtr parseBooleanLiteral(ParserState& state) {
    if (state.current().kind == lex::Token::Keyword) {
        if (state.current().text == "true") {
            state.advance();
            return std::make_unique<Literal>(true);
        }
        if (state.current().text == "false") {
            state.advance();
            return std::make_unique<Literal>(false);
        }
    }

    return nullptr;
}


LiteralPtr parseNumericLiteral(ParserState& state) {
    // TODO: check correctness, maybe process in lexer?
    if (state.current().kind != lex::Token::NumericLiteral) {
        return nullptr;
    }
    std::string_view text = state.current().text;
    std::int32_t num = 0;
    double dnum = 0;

    bool legacyOctal = text[0] == '0';
    int base = 10;
    if (text.size() >= 2 && text[0] == '0') {
        char baseChar = std::tolower(text[1]);
        if (baseChar == 'b') {
            base = 2;
        } else if (baseChar == 'o') {
            base = 8;
        } else if (baseChar == 'x') {
            base = 16;
        }
        if (base != 10) {
            text = text.substr(2);
            legacyOctal = false;
            if (text.empty()) {
                state.error("Invalid numeric literal");
                return nullptr;
            }
        }
    }

    int exponent = 0;
    bool decimalPoint = false;
    bool isFloatingPoint = false;
    auto suffixStart = text.end();

    for (auto it = text.begin(); it != text.end(); ++it) {
        char c = *it;
        if (c == '_') {
            continue;
        }

        if (std::isdigit(c)) {
            if (decimalPoint) {
                exponent -= 1;
            }
            legacyOctal &= c < '8';

            if (!isFloatingPoint) {
                int64_t tmp = static_cast<int64_t>(num) * base + (c - '0');
                if (static_cast<std::int32_t>(tmp) != tmp) {
                    isFloatingPoint = true;
                    dnum = tmp;
                }
                else {
                    num = tmp;
                }
            }
            else {
                dnum = dnum * base + (c - '0');
            }
        }
        else if (std::isxdigit(c) && base == 16) {
            if (decimalPoint) {
                exponent -= 1;
            }
            if (!isFloatingPoint) {
                int64_t tmp = static_cast<int64_t>(num) * base + (std::tolower(c) - 'a' + 10);
                if (static_cast<std::int32_t>(tmp) != tmp) {
                    isFloatingPoint = true;
                    dnum = tmp;
                }
                else {
                    num = tmp;
                }
            }
            else {
                dnum = dnum * base + (std::tolower(c) - 'a' + 10);
            }
        }
        else if (c == '.') {
            decimalPoint = true;
            isFloatingPoint = true;
            dnum = num;
        }
        else if (suffixStart == text.end()) {
            suffixStart = it;
            break;
        }
    }

    if (legacyOctal && num != 0) {
        // TODO: fix base
        state.error("Legacy octal literals are not supported");
        return nullptr;
    }

    if (suffixStart != text.end() && std::tolower(*suffixStart) == 'e' && base == 10) {
        std::string_view exponentText = text.substr(suffixStart - text.begin() + 1);
        int exp = 0;
        bool negative = false;
        if (exponentText.empty()) {
            state.error("Invalid numeric literal");
            return nullptr;
        }
        if (exponentText[0] == '+' || exponentText[0] == '-') {
            negative = exponentText[0] == '-';
            exponentText = exponentText.substr(1);
        }
        if (exponentText.empty()) {
            state.error("Invalid numeric literal");
            return nullptr;
        }
        for (char c : exponentText) {
            if (c == '_') {
                continue;
            }
            exp = exp * 10 + (c - '0');
        }

        if (negative) {
            exp = -exp;
        }


        exponent += exp;
    }
    else if (suffixStart != text.end()) {
        // TODO: different suffix - bigint
    }

    if (exponent != 0) {
        if (isFloatingPoint) {
            dnum *= std::pow(10, exponent);
        }
        else {
            // TODO: check overflow
            num *= std::pow(10, exponent);
        }
    }

    state.advance();
    if (isFloatingPoint) {
        return std::make_unique<Literal>(dnum);
    }
    return std::make_unique<Literal>(num);
}


LiteralPtr parseStringLiteral(ParserState& state) {
    if (state.current().kind != lex::Token::StringLiteral) {
        return nullptr;
    }
    std::string_view text = state.current().text;
    text = text.substr(1, text.size() - 2);  // remove quotes

    int length = 0;
    for (char c : text) {
        if (c != '\\') {
            length += 1;
        }
        // TODO: handle more complex escape sequences
    }

    std::string str;
    str.reserve(length);

    for (auto it = text.begin(); it != text.end(); ++it) {
        if (*it != '\\') {
            str.push_back(*it);
            continue;
        }
        else {
            ++it;
            if (it == text.end()) {
                state.error("Invalid escape sequence");
                return nullptr;
            }
            char c = *it;
            switch (c) {
                case 'b':
                    str.push_back('\b');
                    break;
                case 'f':
                    str.push_back('\f');
                    break;
                case 'n':
                    str.push_back('\n');
                    break;
                case 'r':
                    str.push_back('\r');
                    break;
                case 't':
                    str.push_back('\t');
                    break;
                case 'v':
                    str.push_back('\v');
                    break;
                case '0':
                    str.push_back('\0');
                    break;
                case '\'':
                case '"':
                case '\\':
                default:
                    str.push_back(c);
                    break;
            }
        }
    }

    state.advance();
    return std::make_unique<Literal>(str);
}


LiteralPtr parseLiteral(ParserState& state) {
    // TODO: check all literals
    if (auto null = parseNullLiteral(state)) {
        return null;
    }
    if (auto boolean = parseBooleanLiteral(state)) {
        return boolean;
    }
    if (auto numeric = parseNumericLiteral(state)) {
        return numeric;
    }
    if (auto string = parseStringLiteral(state)) {
        return string;
    }

    return nullptr;
}


ExpressionPtr parseUpdateExpression(ParserState& state) {
    // TODO: check that "AssignmentTargetType" is simple while parsing
    if (state.current().kind == lex::Token::Punctuator) {
        std::string_view op = state.current().text;
        if (op == "++" || op == "--") {
            state.advance();
            if (auto expr = parseUnaryExpression(state)) {
                return std::make_unique<UpdateExpression>(std::move(expr), (op == "++" ? UpdateExpression::Op::PreInc : UpdateExpression::Op::PreDec));
            }
            state.backtrack();
        }
    }


    if (auto expr = parseLeftHandSideExpression(state)) {
        if (state.current().kind == lex::Token::Punctuator) {
            std::string_view op = state.current().text;
            if (op == "++" || op == "--") {
                state.advance();
                return std::make_unique<UpdateExpression>(std::move(expr), (op == "++" ? UpdateExpression::Op::PostInc : UpdateExpression::Op::PostDec));
            }
        }
        return expr;
    }

    return nullptr;
}


ExpressionPtr parseUnaryExpression(ParserState& state) {
    if (state.current().kind == lex::Token::Punctuator || state.current().kind == lex::Token::Keyword) { // parse prefix unary
        std::string_view op = state.current().text;
        if (unaryOperator.contains(op) && (state.getAwait() || op != "await")) {
            state.advance();
            if (auto expr = parseUnaryExpression(state)) {
                return std::make_unique<UnaryExpression>(std::move(expr), op);
            }
            state.backtrack();
        }
    }

    if (auto update = parseUpdateExpression(state)) {
        return update;
    }

    return nullptr;
}


ExpressionPtr parseBinaryExpression(ParserState& state) {
    // Shunting Yard algorithm
    using Element = std::variant<
        ExpressionPtr,
        std::string_view
    >;

    std::vector<Element> output;
    std::vector<std::string_view> operators;

    auto start = state.getPosition();
    while (true) {
        auto unary = parseUnaryExpression(state);
        if (!unary) {
            return nullptr;
        }
        output.emplace_back(std::move(unary));
        auto next = state.current();
        if (next.kind != lex::Token::Punctuator) {
            break;
        }
        auto precedence = binaryPrecedence.find(next.text);
        if (precedence == binaryPrecedence.end()) {
            break;
        }
        while (!operators.empty()) {
            auto top = operators.back();
            auto topPrecedence = binaryPrecedence.at(top);
            if (topPrecedence < precedence->second
              || (rightAssociative.contains(top) && precedence->second == topPrecedence)) {
                break;
            }
            operators.pop_back();
            output.emplace_back(top);
        }
        operators.push_back(next.text);
        state.advance();
    }
    while (!operators.empty()) {
        output.emplace_back(operators.back());
        operators.pop_back();
    }

    struct Popper {
        std::vector<Element>& output;
        ParserState& state;
        std::span<lex::Token>::iterator start;

        ExpressionPtr pop() {
            if (output.empty()) {
                state.restorePosition(start);
                return nullptr;
            }
            auto elem = std::move(output.back());
            output.pop_back();
            if (auto expr = std::get_if<ExpressionPtr>(&elem)) {
                return std::move(*expr);
            }
            if (auto op = std::get_if<std::string_view>(&elem)) {
                auto rhs = pop();
                if (!rhs) {
                    state.restorePosition(start);
                    return nullptr;
                }
                auto lhs = pop();
                if (!lhs) {
                    state.restorePosition(start);
                    return nullptr;
                }
                return std::make_unique<BinaryExpression>(
                    std::move(lhs),
                    std::move(rhs),
                    *op
                );
            }

            return nullptr;
        }
    };

    Popper popper{output, state, start};
    auto res = popper.pop();
    if (!res) {
        state.restorePosition(start);
        return nullptr;
    }
    assert(output.empty());
    return res;
}


TypeAnnotationPtr parseTypeAnnotation(ParserState& state) {
    if (state.current().kind == lex::Token::Punctuator && state.current().text == ":") {
        state.advance();
        if (state.current().kind == lex::Token::Keyword && !allowedKWTypes.contains(state.current().text)) {
            state.error("Invalid type annotation");
            return nullptr;
        }
        if (auto id = parseIdentifierName(state, true)) {
            return std::make_unique<TypeAnnotation>(*id);
        }
    }

    return nullptr;
}


ExpressionPtr parseInitializer(ParserState& state) {
    if (state.current().kind == lex::Token::Punctuator && state.current().text == "=") {
        state.advance();
        if (auto initializer = (state.pushTemplate<In{true}>(), parseAssignmentExpression(state))) {
            return initializer;
        }
        state.backtrack();
    }

    return nullptr;
}


BindingElementPtr parseSingleNameBinding(ParserState& state) {
    auto id = parseBindingIdentifier(state);
    if (!id) {
        return nullptr;
    }
    auto annotation = parseTypeAnnotation(state);
    auto initializer = parseInitializer(state);

    return std::make_unique<BindingElement>(std::move(id), std::move(annotation), std::move(initializer));
}


auto parseBindingPattern(ParserState&) {
    // XXX: ignore for now
    return nullptr;
}


BindingElementPtr parseBindingElement(ParserState& state) {
    // TODO: vs LexicalBinding
    if (auto single = parseSingleNameBinding(state)) {
        return single;
    }
    if (auto pattern = parseBindingPattern(state)) {
        // TODO
        // auto annotation = parseTypeAnnotation(state);
        // auto initializer = parseInitializer(state);

        return pattern;
    }

    return nullptr;
}


auto parseBindingRestElement(ParserState&) {
    // XXX: ignore for now
    return nullptr;
}


auto parseFormalParameters(ParserState& state) {
    std::vector<BindingElementPtr> params;
    BindingElementPtr rest;

    bool canContinue = true;
    while (canContinue) {
        if (auto param = parseFormalParameter(state)) {
            params.emplace_back(std::move(param));
            if (state.current().kind == lex::Token::Punctuator && state.current().text == ",") {
                state.advance();
                continue;
            }
            else {
                canContinue = false;
            }
        }
        break;
    }

    if (canContinue) {
        rest = parseBindingRestElement(state);
    }

    return std::make_unique<FormalParameters>(std::move(rest), std::move(params));
}


StatementListPtr parseFunctionBody(ParserState& state) {
    if (auto stmts = (state.pushTemplate<Return{true}>(), parseStatementList(state))) {
        return stmts;
    }
    return nullptr;
}


StatementListPtr parseBlock(ParserState& state) {
    if (state.current().kind != lex::Token::Punctuator || state.current().text != "{") {
        state.error("Expected {");
        return nullptr;
    }
    state.advance();

    auto start = state.getPosition();

    auto list = parseStatementList(state);

    if (state.current().kind != lex::Token::Punctuator || state.current().text != "}") {
        state.restorePosition(start);
        state.error("Expected }");
        return nullptr;
    }
    state.advance();

    if (list) {
        list->kind = StatementList::Kind::Block;
        return list;
    }

    return std::make_unique<StatementList>(StatementList::Kind::Block, std::vector<StatementPtr>{});
}


BindingElementPtr parseLexicalBinding(ParserState& state) {
    if (auto id = parseBindingIdentifier(state)) {
        auto annotation = parseTypeAnnotation(state);
        auto initializer = parseInitializer(state);
        return std::make_unique<BindingElement>(std::move(id), std::move(annotation), std::move(initializer));
    }

    auto start = state.getPosition();
    if (auto pattern = parseBindingPattern(state)) {
        auto annotation = parseTypeAnnotation(state);
        if (auto initializer = parseInitializer(state)) {
            return std::make_unique<BindingElement>(std::move(pattern), std::move(annotation), std::move(initializer));
        }
    }

    state.restorePosition(start);
    return nullptr;
}


LexicalDeclarationPtr parseLexicalDeclaration(ParserState& state) {
    auto start = state.getPosition();
    if (state.current().kind != lex::Token::Keyword) {
        state.error("Expected let or const");
        return nullptr;
    }
    bool isConst;
    if (state.current().text == "let") {
        isConst = false;
    }
    else if (state.current().text == "const") {
        isConst = true;
    }
    else {
        state.error("Expected let or const");
        return nullptr;
    }
    state.advance();

    std::vector<BindingElementPtr> bindings;

    while (true) {
        if (auto binding = parseLexicalBinding(state)) {
            bindings.emplace_back(std::move(binding));
        }
        else {
            state.restorePosition(start);
            state.error("Invalid lexical declaration");
            return nullptr;
        }


        if (state.current().kind == lex::Token::Punctuator && state.current().text == ",") {
            state.advance();
            continue;
        }
        if (state.current().kind == lex::Token::Punctuator && state.current().text == ";") {
            state.advance();
            break;
        }
        state.error("Unexpected token");
        state.restorePosition(start);
        return nullptr;
    }

    return std::make_unique<LexicalDeclaration>(isConst, std::move(bindings));
}


auto parseVariableStatement(ParserState&) {
    // XXX: ignore for now
    return nullptr;
}


ExpressionStatementPtr parseExpressionStatement(ParserState& state) {
    auto start = state.getPosition();
    if (auto expr = (state.pushTemplate<In{false}>(), parseExpression(state))) {
        if (state.current().kind != lex::Token::Punctuator || state.current().text != ";") {
            state.error("Expected ;");
            state.restorePosition(start);
            return nullptr;
        }
        state.advance();
        return std::make_unique<ExpressionStatement>(std::move(expr));
    }

    return nullptr;
}

ExpressionPtr parseExpressionParenthesised(ParserState& state) {
    auto start = state.getPosition();
    if (state.current().kind != lex::Token::Punctuator || state.current().text != "(") {
        state.error("Expected (");
        return nullptr;
    }
    state.advance();

    auto expr = parseExpression(state);
    if (!expr) {
        state.error("Expected expression");
        state.restorePosition(start);
        return nullptr;
    }

    if (state.current().kind != lex::Token::Punctuator || state.current().text != ")") {
        state.error("Expected )");
        state.restorePosition(start);
        return nullptr;
    }
    state.advance();

    return expr;
}


IterationStatementPtr parseDoWhileStatement(ParserState& state) {
    auto start = state.getPosition();
    if (state.current().kind != lex::Token::Keyword || state.current().text != "do") {
        return nullptr;
    }
    state.advance();

    auto statement = parseStatement(state);
    if (!statement) {
        state.error("Expected statement");
        state.restorePosition(start);
        return nullptr;
    }

    if (state.current().kind != lex::Token::Keyword || state.current().text != "while") {
        state.error("Expected while");
        state.restorePosition(start);
        return nullptr;
    }
    state.advance();

    auto expr = (state.pushTemplate<In{true}>(), parseExpressionParenthesised(state));
    if (!expr) {
        state.error("Expected expression");
        state.restorePosition(start);
        return nullptr;
    }

    if (state.current().kind != lex::Token::Punctuator || state.current().text != ";") {
        state.error("Expected ;");
        state.restorePosition(start);
        return nullptr;
    }
    state.advance();

    return std::make_unique<IterationStatement>(IterationStatement::doWhile(std::move(statement), std::move(expr)));
}


IterationStatementPtr parseWhileStatement(ParserState& state) {
    auto start = state.getPosition();
    if (state.current().kind != lex::Token::Keyword || state.current().text != "while") {
        return nullptr;
    }
    state.advance();

    auto expr = (state.pushTemplate<In{true}>(), parseExpressionParenthesised(state));
    if (!expr) {
        state.restorePosition(start);
        return nullptr;
    }

    auto statement = parseStatement(state);
    if (!statement) {
        state.error("Expected statement");
        state.restorePosition(start);
        return nullptr;
    }

    return std::make_unique<IterationStatement>(IterationStatement::while_(std::move(expr), std::move(statement)));
}


IterationStatementPtr parseForInOfStatement(ParserState&) {
    return nullptr;
}


IterationStatementPtr parseForStatement(ParserState& state) {
    auto start = state.getPosition();
    if (state.current().kind != lex::Token::Keyword || state.current().text != "for") {
        return nullptr;
    }
    state.advance();

    if (state.current().kind != lex::Token::Punctuator || state.current().text != "(") {
        state.error("Expected (");
        state.restorePosition(start);
        return nullptr;
    }
    state.advance();

    ASTNodePtr init;

    if (state.current().kind == lex::Token::Keyword && state.current().text == "var") {
        throw std::runtime_error("Variable declarations in for loop are not supported");
    }
    if (state.current().kind == lex::Token::Keyword && (state.current().text == "let" || state.current().text == "const")) {
        if (auto decl = (state.pushTemplate<In{false}>(), parseLexicalDeclaration(state))) {
            init = std::move(decl);
        }
        else {
            state.restorePosition(start);
            return nullptr;
        }
        // semicolon as part of the declaration statement
    }
    else {
        if (auto expr = (state.pushTemplate<In{false}>(), parseExpression(state))) {
            init = std::move(expr);
        }
        if (state.current().kind != lex::Token::Punctuator || state.current().text != ";") {
            state.error("Expected ;");
            state.restorePosition(start);
            return nullptr;
        }
        state.advance();
    }


    auto condition = (state.pushTemplate<In{true}>(), parseExpression(state));

    if (state.current().kind != lex::Token::Punctuator || state.current().text != ";") {
        state.error("Expected ;");
        state.restorePosition(start);
        return nullptr;
    }
    state.advance();


    auto update = (state.pushTemplate<In{true}>(), parseExpression(state));

    if (state.current().kind != lex::Token::Punctuator || state.current().text != ")") {
        state.error("Expected )");
        state.restorePosition(start);
        return nullptr;
    }
    state.advance();

    auto statement = parseStatement(state);
    if (!statement) {
        state.error("Expected statement");
        state.restorePosition(start);
        return nullptr;
    }

    return std::make_unique<IterationStatement>(IterationStatement::for_(std::move(init), std::move(condition), std::move(update), std::move(statement)));
}


IterationStatementPtr parseIterationStatement(ParserState& state) {
    if (auto doWhile = parseDoWhileStatement(state)) {
        return doWhile;
    }
    if (auto whileStmt = parseWhileStatement(state)) {
        return whileStmt;
    }
    if (auto forStmt = parseForStatement(state)) {
        return forStmt;
    }
    if (auto forInOf = parseForInOfStatement(state)) {
        return forInOf;
    }

    return nullptr;
}


auto parseSwitchStatement(ParserState&) {
    // XXX: ignore for now
    return nullptr;
}


StatementPtr parseBreakableStatement(ParserState& state) {
    if (auto iter = parseIterationStatement(state)) {
        return  iter;
    }
    if (auto sw = parseSwitchStatement(state)) {
        return  sw;
    }

    return nullptr;
}


ContinueStatementPtr parseContinueStatement(ParserState& state) {
    if (state.current().kind != lex::Token::Keyword || state.current().text != "continue") {
        return nullptr;
    }
    state.advance();

    auto label = parseLabelIdentifier(state);

    if (state.current().kind != lex::Token::Punctuator || state.current().text != ";") {
        state.error("Expected ;");
        return nullptr;
    }

    state.advance();
    return std::make_unique<ContinueStatement>(std::move(label));
}


BreakStatementPtr parseBreakStatement(ParserState& state) {
    if (state.current().kind != lex::Token::Keyword || state.current().text != "break") {
        return nullptr;
    }
    state.advance();

    auto label = parseLabelIdentifier(state);

    if (state.current().kind != lex::Token::Punctuator || state.current().text != ";") {
        state.error("Expected ;");
        return nullptr;
    }
    state.advance();
    return std::make_unique<BreakStatement>(std::move(label));
}


ReturnStatementPtr parseReturnStatement(ParserState& state) {
    if (state.current().kind != lex::Token::Keyword || state.current().text != "return") {
        return nullptr;
    }
    state.advance();

    auto start = state.getPosition();
    auto expr = (state.pushTemplate<In{true}>(), parseExpression(state));

    if (state.current().kind != lex::Token::Punctuator || state.current().text != ";") {
        state.error("Expected ;");
        state.restorePosition(start);
        return nullptr;
    }

    state.advance();
    return std::make_unique<ReturnStatement>(std::move(expr));
}


ThrowStatementPtr parseThrowStatement(ParserState& state) {
    if (state.current().kind != lex::Token::Keyword || state.current().text != "throw") {
        return nullptr;
    }
    state.advance();

    auto start = state.getPosition();
    auto expr = (state.pushTemplate<In{true}>(), parseExpression(state));

    if (state.current().kind != lex::Token::Punctuator || state.current().text != ";") {
        state.error("Expected ;");
        state.restorePosition(start);
        return nullptr;
    }

    state.advance();
    return std::make_unique<ThrowStatement>(std::move(expr));
}


auto parseWithStatement(ParserState&) {
    return nullptr;
}


FunctionPtr parseFunction(ParserState& state, TriState identifierRequired) {
    auto start = state.getPosition();
    if (state.current().kind != lex::Token::Keyword || state.current().text != "function") {
        return nullptr;
    }
    state.advance();

    IdentifierPtr name;
    if (identifierRequired != TriState::False) {
        if (auto id = parseBindingIdentifier(state)) {
            name = std::move(id);
        }
        else if (identifierRequired == TriState::True) {
            state.error("Expected identifier");
            state.restorePosition(start);
            return nullptr;
        }
    }

    if (state.current().kind != lex::Token::Punctuator || state.current().text != "(") {
        state.error("Expected (");
        state.restorePosition(start);
        return nullptr;
    }
    state.advance();

    auto params = (state.pushTemplate<Yield{false}, Await{false}>(), parseFormalParameters(state));
    if (!params) {
        state.restorePosition(start);
        return nullptr;
    }

    if (state.current().kind != lex::Token::Punctuator || state.current().text != ")") {
        state.error("Expected )");
        state.restorePosition(start);
        return nullptr;
    }
    state.advance();

    auto returnType = parseTypeAnnotation(state);

    if (state.current().kind != lex::Token::Punctuator || state.current().text != "{") {
        state.error("Expected {");
        state.restorePosition(start);
        return nullptr;
    }
    state.advance();

    auto body = (state.pushTemplate<Yield{false}, Await{false}>(), parseFunctionBody(state));

    if (state.current().kind != lex::Token::Punctuator || state.current().text != "}") {
        state.error("Expected }");
        state.restorePosition(start);
        return nullptr;
    }

    state.advance();

    return std::make_unique<Function>(
        false, false, std::move(name), std::move(params), std::move(returnType),
        std::move(body), std::string_view(start->text.begin(), state.current().text.begin())
    );
}


FunctionPtr parseFunctionDeclaration(ParserState& state, bool Default) {
    return parseFunction(state, Default ? TriState::False : TriState::True);
}


auto parseLabeledStatement(ParserState&) {
    // XXX: ignore for now
    return nullptr;
}


auto parseTryStatement(ParserState&) {
    // XXX: ignore for now
    return nullptr;
}


DebuggerStatementPtr parseDebuggerStatement(ParserState& state) {
    if (state.current().kind != lex::Token::Keyword || state.current().text != "debugger") {
        return nullptr;
    }
    state.advance();
    if (state.current().kind != lex::Token::Punctuator || state.current().text != ";") {
        state.error("Expected ;");
        return nullptr;
    }
    state.advance();
    return std::make_unique<DebuggerStatement>();
}


IfStatementPtr parseIfStatement(ParserState& state) {
    auto start = state.getPosition();
    if (state.current().kind != lex::Token::Keyword || state.current().text != "if") {
        return nullptr;
    }
    state.advance();

    auto expr = (state.pushTemplate<In{true}>(), parseExpressionParenthesised(state));
    if (!expr) {
        state.restorePosition(start);
        return nullptr;
    }

    auto consequent = parseStatement(state);
    if (!consequent) {
        state.error("Expected statement");
        state.restorePosition(start);
        return nullptr;
    }

    StatementPtr alternate;
    if (state.current().kind == lex::Token::Keyword && state.current().text == "else") {
        state.advance();
        alternate = parseStatement(state);

        if (!alternate) {
            state.error("Expected statement");
            state.restorePosition(start);
            return nullptr;
        }
    }

    return std::make_unique<IfStatement>(std::move(expr),  std::move(consequent), std::move(alternate));
}


EmptyStatementPtr parseEmptyStatement(ParserState& state) {
    if (state.current().kind == lex::Token::Punctuator && state.current().text == ";") {
        state.advance();
        return std::make_unique<EmptyStatement>();
    }

    return nullptr;
}


StatementPtr parseStatement(ParserState& state) {
    if (auto block = parseBlockStatement(state)) {
        return block;
    }
    if (auto variable = parseVariableStatement(state)) {
        return variable;
    }
    if (auto empty = parseEmptyStatement(state)) {
        return empty;
    }
    if (auto expression = parseExpressionStatement(state)) {
        return expression;
    }
    if (auto if_ = parseIfStatement(state)) {
        return if_;
    }
    if (auto breakable = parseBreakableStatement(state)) {
        return breakable;
    }
    if (auto continue_ = parseContinueStatement(state)) {
        return continue_;
    }
    if (auto break_ = parseBreakStatement(state)) {
        return break_;
    }
    if (state.getReturn()) {
        if (auto ret = parseReturnStatement(state)) {
            return ret;
        }
    }
    if (auto with = parseWithStatement(state)) {
        return with;
    }
    if (auto labeled = parseLabeledStatement(state)) {
        return labeled;
    }
    if (auto throw_ = parseThrowStatement(state)) {
        return throw_;
    }
    if (auto try_ = parseTryStatement(state)) {
        return try_;
    }
    if (auto debugger = parseDebuggerStatement(state)) {
        return debugger;
    }

    return nullptr;
}


auto parseGeneratorDeclaration(ParserState&, bool) {
    // XXX: ignore for now
    return nullptr;
}


auto parseAsyncFunctionDeclaration(ParserState&, bool) {
    // XXX: ignore for now
    return nullptr;
}


auto parseAsyncGeneratorDeclaration(ParserState&, bool) {
    // XXX: ignore for now
    return nullptr;
}


HoistableDeclarationPtr parseHoistableDeclaration(ParserState& state, bool Default) {
    if (auto function = parseFunctionDeclaration(state, Default)) {
        return std::make_unique<HoistableDeclaration>(std::move(function));
    }
    if (auto generator = parseGeneratorDeclaration(state, Default)) {
        return std::make_unique<HoistableDeclaration>(std::move(generator));
    }
    if (auto asyncFunction = parseAsyncFunctionDeclaration(state, Default)) {
        return std::make_unique<HoistableDeclaration>(std::move(asyncFunction));
    }
    if (auto asyncGenerator = parseAsyncGeneratorDeclaration(state, Default)) {
        return std::make_unique<HoistableDeclaration>(std::move(asyncGenerator));
    }
    return nullptr;
}


auto parseClassDeclaration(ParserState&, bool) {
    // XXX: ignore for now
    return nullptr;
}


StatementPtr parseDeclaration(ParserState& state) {
    if (auto hoistable = parseHoistableDeclaration(state, false)) {
        return hoistable;
    }
    if (auto klass = parseClassDeclaration(state, false)) {
        return klass;
    }
    if (auto lexical = (state.pushTemplate<In{true}>(), parseLexicalDeclaration(state))) {
        return lexical;
    }
    return nullptr;
}


StatementPtr parseStatementListItem(ParserState& state) {
    if (auto statement = parseStatement(state)) {
        return statement;
    }
    if (auto declaration = parseDeclaration(state)) {
        return declaration;
    }

    return nullptr;
}


auto parseRegularExpressionLiteral(ParserState&) {
    return nullptr;
}


auto parseTemplateLiteral(ParserState&, bool) {
    return nullptr;
}


CoverParenthesizedExpressionAndArrowParameterListPtr parseCoverParenthesizedExpressionAndArrowParameterList(ParserState& state) {
    auto start = state.getPosition();
    if (state.current().kind != lex::Token::Punctuator || state.current().text != "(") {
        return nullptr;
    }
    state.advance();

    std::vector<ASTNodePtr> result;
    ASTNodePtr rest;
    bool canContinue = true;

    if (auto expr = (state.pushTemplate<In{true}>(), parseExpression(state))) {
        result.emplace_back(std::move(expr));
        if (state.current().kind != lex::Token::Punctuator || state.current().text != ",") {
            canContinue = false;
        }
        else {
            state.advance();
        }
    }

    if (canContinue) {
        if (state.current().kind == lex::Token::Punctuator && state.current().text == "...") {
            state.advance();
            if (auto ident = parseBindingIdentifier(state)) {
                rest = std::move(ident);
            }
            if (auto pattern = parseBindingPattern(state)) {
                rest = std::move(pattern);
            }
            else {
                state.error("Invalid binding");
                state.restorePosition(start);
                return nullptr;
            }
        }
    }

    if (state.current().kind != lex::Token::Punctuator || state.current().text != ")") {
        state.error("Expected )");
        state.restorePosition(start);
        return nullptr;
    }
    state.advance();

    return std::make_unique<CoverParenthesizedExpressionAndArrowParameterList>(std::move(result), std::move(rest));
}


ExpressionPtr refineParenthesizedExpression(ParserState& state, CoverParenthesizedExpressionAndArrowParameterList& cover) {
    if (cover.rest()) {
        return nullptr;
    }
    for (size_t i = 0; i < cover.children.size() - 1; ++i) {
        if (!dynamic_cast<Expression*>(cover.children[i].get())) {
            return nullptr;
        }
    }

    auto children = std::move(cover.children);
    children.pop_back();
    if (children.size() == 1) {
        return ExpressionPtr(static_cast<Expression*>(children[0].release()));  // NOLINT
    }
    return std::make_unique<CommaExpression>(std::move(children));
}


ExpressionPtr parsePrimaryExpression(ParserState& state) {
    if (auto thisExpr = parseThisExpr(state)) {
        return thisExpr;
    }
    if (auto identifier = parseIdentifierReference(state)) {
        return identifier;
    }
    if (auto lit = parseLiteral(state)) {
        return lit;
    }
    if (auto cover = parseCoverParenthesizedExpressionAndArrowParameterList(state)) {
        // TODO: move to AssignmentExpression?
        if (auto refined = refineParenthesizedExpression(state, *cover)) {
            return refined;
        }
        state.error("Invalid parenthesized expression");
        return nullptr;
    }

    // TODO: rest of the cases
    return nullptr;
}


auto parseSuperProperty(ParserState&) {
    // XXX: ignore for now
    return nullptr;
}


auto parseMetaProperty(ParserState&) {
    // XXX: ignore for now
    return nullptr;
}


ArgumentsPtr parseArguments(ParserState& state) {
    auto start = state.getPosition();
    if (state.current().kind != lex::Token::Punctuator || state.current().text != "(") {
        return nullptr;
    }
    state.advance();

    std::vector<ExpressionPtr> args;
    if (state.current().kind == lex::Token::Punctuator && state.current().text == ")") {
        state.advance();
        return std::make_unique<Arguments>(std::move(args), nullptr);
    }

    bool canContinue = true;
    while (true) {
        if (auto expr = (state.pushTemplate<In{true}>(), parseAssignmentExpression(state))) {
            args.emplace_back(std::move(expr));
        }
        else {
            break;
        }

        if (state.current().kind != lex::Token::Punctuator || state.current().text != ",") {
            canContinue = false;
            break;
        }
        state.advance();
    }

    ExpressionPtr spread;
    if (canContinue) {
        if (state.current().kind == lex::Token::Punctuator && state.current().text == "...") {
            state.advance();
            spread = (state.pushTemplate<In{true}>(), parseAssignmentExpression(state));
            if (!spread) {
                state.error("Expected expression after ...");
                state.restorePosition(start);
                return nullptr;
            }
        }
    }

    if (state.current().kind != lex::Token::Punctuator || state.current().text != ")") {
        state.error("Expected )");
        state.restorePosition(start);
        return nullptr;
    }
    state.advance();

    return std::make_unique<Arguments>(std::move(args), std::move(spread));
}


ExpressionPtr parseMemberExpression(ParserState& state, int& initNewCount) {
    auto fullStart = state.getPosition();

    int newCount = initNewCount;
    if (state.current().kind == lex::Token::Keyword && state.current().text == "new") {
        state.advance();
        newCount++;
    }

    ExpressionPtr prev;
    if (auto super = parseSuperProperty(state)) {
        prev = std::move(super);
    }
    else if (auto meta = parseMetaProperty(state)) {
        prev = std::move(meta);
    }
    else if (auto primary = parsePrimaryExpression(state)) {
        prev = std::move(primary);
    }

    if (!prev) {
        return nullptr;
    }

    do {
        auto start = state.getPosition();

        if (state.current().kind == lex::Token::Punctuator && state.current().text == "[") {
            state.advance();
            if (auto expr = (state.pushTemplate<In{true}>(), parseExpression(state))) {
                if (state.current().kind == lex::Token::Punctuator && state.current().text == "]") {
                    state.advance();
                    prev = std::make_unique<MemberAccessExpression>(std::move(prev), std::move(expr));
                    continue;
                }
                state.error("Expected ]");
            }
            state.restorePosition(start);
        }
        if (state.current().kind == lex::Token::Punctuator && state.current().text == ".") {
            state.advance();
            constexpr auto asLiteral = [](IdentifierPtr id) { return std::make_unique<Literal>(std::move(id->name)); };
            if (auto identifier = parseIdentifier(state)) {
                prev = std::make_unique<MemberAccessExpression>(std::move(prev), asLiteral(std::move(identifier)));
                continue;
            }
            if (auto priv = parsePrivateIdentifier(state)) {
                prev = std::make_unique<MemberAccessExpression>(std::move(prev), asLiteral(std::move(priv)));
                continue;
            }
            state.backtrack();
        }
        if (auto tplate = parseTemplateLiteral(state, true)) {
            prev = std::make_unique<TaggedTemplateExpression>(std::move(prev), std::move(tplate));
            continue;
        }
        if (newCount > 0) {
            if (auto args = parseArguments(state)) {
                prev = std::make_unique<NewCallExpression>(std::move(prev), std::move(args));
                newCount--;
                continue;
            }
        }

        state.restorePosition(start);
        break;
    } while (true);

    if (newCount > initNewCount) {
        state.error("Missing arguments for new expression");
        state.restorePosition(fullStart);
        return nullptr;
    }
    initNewCount = newCount;

    return prev;
}


ExpressionPtr parseNewExpression(ParserState& state) {
    int openNewCount = 0;
    while (state.current().kind == lex::Token::Keyword && state.current().text == "new") {
        state.advance();
        openNewCount++;
    }

    if (auto member = parseMemberExpression(state, openNewCount)) {
        while (openNewCount > 0) {
            member = std::make_unique<NewCallExpression>(std::move(member), nullptr);
            openNewCount--;
        }
        return member;
    }

    return nullptr;
}


auto parseSuperCall(ParserState&) {
    // XXX: ignore for now
    return nullptr;
}


auto parseImportCall(ParserState&) {
    // XXX: ignore for now
    return nullptr;
}


CoverCallExpressionAndAsyncArrowHeadPtr parseCoverCallExpressionAndAsyncArrowHead(ParserState& state) {
    auto start = state.getPosition();
    int dummy = 0;
    if (auto member = parseMemberExpression(state, dummy)) {
        if (auto args = parseArguments(state)) {
            return std::make_unique<CoverCallExpressionAndAsyncArrowHead>(std::move(member), std::move(args));
        }
    }

    state.restorePosition(start);
    return nullptr;
}


ExpressionPtr refineCallExpression(ParserState& state, CoverCallExpressionAndAsyncArrowHead& cover) {
    if (cover.children.size() != 2) {
        return nullptr;
    }
    auto children = std::move(cover.children);
    assert(dynamic_cast<Expression*>(children[0].get()));
    assert(dynamic_cast<Arguments*>(children[1].get()));

    auto callee = ExpressionPtr(static_cast<Expression*>(children[0].release()));  // NOLINT
    auto args = ArgumentsPtr(static_cast<Arguments*>(children[1].release()));  // NOLINT

    return std::make_unique<CallExpression>(std::move(callee), std::move(args));
}


ExpressionPtr parseCallExpression(ParserState& state) {
    ExpressionPtr prev;

    if (auto super = parseSuperCall(state)) {
        prev = std::move(super);
    }
    else if (auto import = parseImportCall(state)) {
        prev = std::move(import);
    }
    else if (auto cover = parseCoverCallExpressionAndAsyncArrowHead(state)) {
        // TODO: move elsewhere?
        if (auto refined = refineCallExpression(state, *cover)) {
            prev = std::move(refined);
        }
        else {
            state.error("Invalid call expression");
            return nullptr;
        }
    }

    if (!prev) {
        return nullptr;
    }

    do {
        auto start = state.getPosition();

        if (auto args = parseArguments(state)) {
            prev = std::make_unique<CallExpression>(std::move(prev), std::move(args));
            continue;
        }
        if (state.current().kind == lex::Token::Punctuator && state.current().text == "[") {
            state.advance();
            if (auto expr = (state.pushTemplate<In{true}>(), parseExpression(state))) {
                if (state.current().kind == lex::Token::Punctuator && state.current().text == "]") {
                    state.advance();
                    prev = std::make_unique<MemberAccessExpression>(std::move(prev), std::move(expr));
                    continue;
                }
                state.error("Expected ]");
            }
            state.restorePosition(start);
        }
        if (state.current().kind == lex::Token::Punctuator && state.current().text == ".") {
            state.advance();
            constexpr auto asLiteral = [](IdentifierPtr id) { return std::make_unique<Literal>(std::move(id->name)); };
            if (auto identifier = parseIdentifier(state)) {
                prev = std::make_unique<MemberAccessExpression>(std::move(prev), asLiteral(std::move(identifier)));
                continue;
            }
            if (auto identifier = parsePrivateIdentifier(state)) {
                prev = std::make_unique<MemberAccessExpression>(std::move(prev), asLiteral(std::move(identifier)));
                continue;
            }
            state.backtrack();
        }
        if (auto tplate = parseTemplateLiteral(state, true)) {
            prev = std::make_unique<TaggedTemplateExpression>(std::move(prev), std::move(tplate));
            continue;
        }

        state.restorePosition(start);
        break;
    } while (true);

    return prev;
}


ExpressionPtr parseLeftHandSideExpression(ParserState& state) {
    // new Function()()
    if (auto call = parseCallExpression(state)) {
        return call;
    }
    if (auto newExpr = parseNewExpression(state)) {
        return newExpr;
    }
    // TODO: rest

    return nullptr;
}


auto parseYieldExpression(ParserState&) {
    // XXX: ignore for now
    return nullptr;
}


auto parseArrowFunction(ParserState&) {
    // XXX: ignore for now
    return nullptr;
}


auto parseAsyncArrowFunction(ParserState&) {
    // XXX: ignore for now
    return nullptr;
}


ScriptPtr parseScript(ParserState& state) {
    if (state.isEnd()) {
        return std::make_unique<Script>(std::make_unique<StatementList>(StatementList::Normal, std::vector<StatementPtr>{}));
    }

    auto _ = state.pushTemplate<Yield{false}, Await{false}, Return{false}>();

    if (auto statementList = parseStatementList(state)) {
        return std::make_unique<Script>(std::move(statementList));
    }

    return nullptr;
}


ExpressionPtr parseConditionalExpression(ParserState& state) {
    auto start = state.getPosition();
    if (auto shortCircuit = parseBinaryExpression(state)) {
        if (state.current().kind == lex::Token::Punctuator && state.current().text == "?") {
            state.advance();
            if (auto consequent = parseAssignmentExpression(state)) {
                if (state.current().kind == lex::Token::Punctuator && state.current().text == ":") {
                    state.advance();
                    if (auto alternate = parseAssignmentExpression(state)) {
                        return std::make_unique<ConditionalExpression>(std::move(shortCircuit), std::move(consequent), std::move(alternate));
                    }
                }
            }
            state.restorePosition(start);
            return nullptr;
        }
        return shortCircuit;
    }
    return nullptr;
}


AssignmentPtr parseAssignment(ParserState& state) {
    auto start = state.getPosition();
    auto lhs = parseLeftHandSideExpression(state);
    if (!lhs) {
        return nullptr;
    }

    if (state.current().kind != lex::Token::Punctuator || !assignmentOperator.contains(state.current().text)) {
        state.restorePosition(start);
        state.error("Expected assignment operator");
        return nullptr;
    }

    std::string_view op = state.current().text;
    state.advance();

    auto rhs = parseAssignmentExpression(state);
    if (!rhs) {
        state.restorePosition(start);
        return nullptr;
    }

    return std::make_unique<Assignment>(std::move(lhs), std::move(rhs), op);
}


ExpressionPtr parseAssignmentExpression(ParserState& state) {
    if (auto yield = parseYieldExpression(state)) {
        return yield;
    }
    if (auto arrow = parseArrowFunction(state)) {
        return arrow;
    }
    if (auto asyncArrow = parseAsyncArrowFunction(state)) {
        return asyncArrow;
    }
    if (auto assignment = parseAssignment(state)) {
        return assignment;
    }
    if (auto cond = parseConditionalExpression(state)) {
        return cond;
    }
    return nullptr;
}


ExpressionPtr parseExpression(ParserState& state) {
    std::vector<ExpressionPtr> items;

    auto last = state.getPosition();
    while (true) {
        if (auto assignment = parseAssignmentExpression(state)) {
            items.emplace_back(std::move(assignment));
            last = state.getPosition();
        }
        else {
            break;
        }

        if (state.current().kind != lex::Token::Punctuator || state.current().text != ",") {
            break;
        }
        state.advance();
    }
    state.restorePosition(last);
    if (items.empty()) {
        return nullptr;
    }
    if (items.size() == 1) {
        return std::move(items[0]);
    }
    return std::make_unique<CommaExpression>(std::move(items));
}


StatementListPtr parseStatementList(ParserState& state) {
    std::vector<StatementPtr> items;

    while (true) {
        if (auto item = parseStatementListItem(state)) {
            items.emplace_back(std::move(item));
        }
        else {
            break;
        }
    }
    if (items.empty()) {
        return nullptr;
    }
    return std::make_unique<StatementList>(StatementList::Normal, std::move(items));
}


}  // namespace jac::ast
