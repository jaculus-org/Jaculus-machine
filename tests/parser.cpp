#include <catch2/catch_test_macros.hpp>
#include <catch2/generators/catch_generators.hpp>

#include <cmath>
#include <cstdint>
#include <string>
#include <variant>

#include <jac/machine/compiler/ast.h>


template<>
struct Catch::StringMaker<jac::lex::Token> {
    static std::string convert(jac::lex::Token const& value) {
        return std::string("Token(") + std::to_string(value.line) + ", " + std::to_string(value.column) + ", " + std::string(value.text) + ", " + std::to_string(value.kind) + ")";
    }
};


bool floatEq(auto a, auto b) {
    return std::fabs(a - b) < 0.001;
}

template<typename T>
bool isLit(const auto* node, T expected) {
    const auto& lit = dynamic_cast<const jac::ast::Literal&>(*node);
    if (!std::holds_alternative<T>(lit.value)) {
        return false;
    }
    return std::get<T>(lit.value) == expected;
}

bool isIdent(const auto* node, auto expected) {
    const auto& ident = dynamic_cast<const jac::ast::Identifier&>(*node);
    return ident.name == expected;
}


using TokenVector = std::vector<jac::lex::Token>;


TEST_CASE("NumericLiteral", "[parser]") {

    SECTION("123") {
        auto tokens = TokenVector{
            jac::lex::Token(1, 1, "123", jac::lex::Token::NumericLiteral)
        };

        jac::ast::ParserState state(tokens);

        auto result = jac::ast::parseNumericLiteral(state);
        CAPTURE(state.getErrorMessage());
        CAPTURE(state.getErrorToken());
        REQUIRE(state.isEnd());
        REQUIRE(result);

        REQUIRE(isLit<int32_t>(result.get(), 123));
    }

    SECTION("123.456") {
        auto tokens = TokenVector{
            jac::lex::Token(1, 1, "123.456", jac::lex::Token::NumericLiteral)
        };

        jac::ast::ParserState state(tokens);

        auto result = jac::ast::parseNumericLiteral(state);
        CAPTURE(state.getErrorMessage());
        CAPTURE(state.getErrorToken());
        REQUIRE(state.isEnd());
        REQUIRE(result);

        REQUIRE(std::holds_alternative<double>(result->value));
        REQUIRE(std::get<double>(result->value) == 123.456);
    }

    SECTION("123.456e-7") {
        auto tokens = TokenVector{
            jac::lex::Token(1, 1, "123.456e-7", jac::lex::Token::NumericLiteral)
        };

        jac::ast::ParserState state(tokens);

        auto result = jac::ast::parseNumericLiteral(state);
        CAPTURE(state.getErrorMessage());
        CAPTURE(state.getErrorToken());
        REQUIRE(state.isEnd());
        REQUIRE(result);

        REQUIRE(std::holds_alternative<double>(result->value));
        REQUIRE(floatEq(std::get<double>(result->value), 123.456e-7));
    }

    SECTION("123.456e+7") {
        auto tokens = TokenVector{
            jac::lex::Token(1, 1, "123.456e+7", jac::lex::Token::NumericLiteral)
        };

        jac::ast::ParserState state(tokens);

        auto result = jac::ast::parseNumericLiteral(state);
        CAPTURE(state.getErrorMessage());
        CAPTURE(state.getErrorToken());
        REQUIRE(state.isEnd());
        REQUIRE(result);

        REQUIRE(std::holds_alternative<double>(result->value));
        REQUIRE(floatEq(std::get<double>(result->value), 123.456e+7));
    }

    SECTION("12_3.45_6e+7") {
        auto tokens = TokenVector{
            jac::lex::Token(1, 1, "12_3.45_6e+7", jac::lex::Token::NumericLiteral)
        };

        jac::ast::ParserState state(tokens);

        auto result = jac::ast::parseNumericLiteral(state);
        CAPTURE(state.getErrorMessage());
        CAPTURE(state.getErrorToken());
        REQUIRE(state.isEnd());
        REQUIRE(result);

        REQUIRE(std::holds_alternative<double>(result->value));
        REQUIRE(floatEq(std::get<double>(result->value), 123.456e+7));
    }

    SECTION("0x0123") {
        auto tokens = TokenVector{
            jac::lex::Token(1, 1, "0x0123", jac::lex::Token::NumericLiteral)
        };

        jac::ast::ParserState state(tokens);

        auto result = jac::ast::parseNumericLiteral(state);
        CAPTURE(state.getErrorMessage());
        CAPTURE(state.getErrorToken());
        REQUIRE(state.isEnd());
        REQUIRE(result);

        REQUIRE(isLit<int32_t>(result.get(), 291));
    }

    SECTION("0o0123") {
        auto tokens = TokenVector{
            jac::lex::Token(1, 1, "0o0123", jac::lex::Token::NumericLiteral)
        };

        jac::ast::ParserState state(tokens);

        auto result = jac::ast::parseNumericLiteral(state);
        CAPTURE(state.getErrorMessage());
        CAPTURE(state.getErrorToken());
        REQUIRE(state.isEnd());
        REQUIRE(result);

        REQUIRE(isLit<int32_t>(result.get(), 83));
    }

    SECTION("0b1010") {
        auto tokens = TokenVector{
            jac::lex::Token(1, 1, "0b1010", jac::lex::Token::NumericLiteral)
        };

        jac::ast::ParserState state(tokens);

        auto result = jac::ast::parseNumericLiteral(state);
        CAPTURE(state.getErrorMessage());
        CAPTURE(state.getErrorToken());
        REQUIRE(state.isEnd());
        REQUIRE(result);

        REQUIRE(isLit<int32_t>(result.get(), 10));
    }

    SECTION("0123456789") {
        auto tokens = TokenVector{
            jac::lex::Token(1, 1, "0123456789", jac::lex::Token::NumericLiteral)
        };

        jac::ast::ParserState state(tokens);

        auto result = jac::ast::parseNumericLiteral(state);
        CAPTURE(state.getErrorMessage());
        CAPTURE(state.getErrorToken());
        REQUIRE(state.isEnd());
        REQUIRE(result);

        REQUIRE(isLit<int32_t>(result.get(), 123456789));
    }

    SECTION("0x0123456789abcdef") {
        auto tokens = TokenVector{
            jac::lex::Token(1, 1, "0x0123456789abcdef", jac::lex::Token::NumericLiteral)
        };

        jac::ast::ParserState state(tokens);

        auto result = jac::ast::parseNumericLiteral(state);
        CAPTURE(state.getErrorMessage());
        CAPTURE(state.getErrorToken());
        REQUIRE(state.isEnd());
        REQUIRE(result);

        REQUIRE(std::holds_alternative<double>(result->value));
        REQUIRE(floatEq(std::get<double>(result->value), 0x0123456789abcdef));
    }

    SECTION("0x0123456789ABCDEF") {
        auto tokens = TokenVector{
            jac::lex::Token(1, 1, "0x0123456789ABCDEF", jac::lex::Token::NumericLiteral)
        };

        jac::ast::ParserState state(tokens);

        auto result = jac::ast::parseNumericLiteral(state);
        CAPTURE(state.getErrorMessage());
        CAPTURE(state.getErrorToken());
        REQUIRE(state.isEnd());
        REQUIRE(result);

        REQUIRE(std::holds_alternative<double>(result->value));
        REQUIRE(floatEq(std::get<double>(result->value), 0x0123456789ABCDEF));
    }

    SECTION("0") {
        auto tokens = TokenVector{
            jac::lex::Token(1, 1, "0", jac::lex::Token::NumericLiteral)
        };

        jac::ast::ParserState state(tokens);

        auto result = jac::ast::parseNumericLiteral(state);
        CAPTURE(state.getErrorMessage());
        CAPTURE(state.getErrorToken());
        REQUIRE(state.isEnd());
        REQUIRE(result);

        REQUIRE(isLit<int32_t>(result.get(), 0));
    }

    SECTION("invalid") {
        auto tokens = TokenVector{
            jac::lex::Token(1, 1, "123.456e", jac::lex::Token::NumericLiteral)
        };

        jac::ast::ParserState state(tokens);

        auto result = jac::ast::parseNumericLiteral(state);
        CAPTURE(state.getErrorMessage());
        CAPTURE(state.getErrorToken());
        REQUIRE(!result);
    }

    SECTION("invalid 2") {
        auto tokens = TokenVector{
            jac::lex::Token(1, 1, "0x", jac::lex::Token::NumericLiteral)
        };

        jac::ast::ParserState state(tokens);

        auto result = jac::ast::parseNumericLiteral(state);
        CAPTURE(state.getErrorMessage());
        CAPTURE(state.getErrorToken());
        REQUIRE(!result);
    }
}


TEST_CASE("BooleanLiteral", "[parser]") {

    SECTION("true") {
        auto tokens = TokenVector{
            jac::lex::Token(1, 1, "true", jac::lex::Token::Keyword)
        };

        jac::ast::ParserState state(tokens);

        auto result = jac::ast::parseBooleanLiteral(state);
        CAPTURE(state.getErrorMessage());
        CAPTURE(state.getErrorToken());
        REQUIRE(state.isEnd());
        REQUIRE(result);

        REQUIRE(isLit<bool>(result.get(), true));
    }

    SECTION("false") {
        auto tokens = TokenVector{
            jac::lex::Token(1, 1, "false", jac::lex::Token::Keyword)
        };

        jac::ast::ParserState state(tokens);

        auto result = jac::ast::parseBooleanLiteral(state);
        CAPTURE(state.getErrorMessage());
        CAPTURE(state.getErrorToken());
        REQUIRE(state.isEnd());
        REQUIRE(result);

        REQUIRE(isLit<bool>(result.get(), false));
    }

    SECTION("tru") {
        auto tokens = TokenVector{
            jac::lex::Token(1, 1, "tru", jac::lex::Token::IdentifierName)
        };

        jac::ast::ParserState state(tokens);

        auto result = jac::ast::parseBooleanLiteral(state);
        CAPTURE(state.getErrorMessage());
        CAPTURE(state.getErrorToken());
        REQUIRE(!result);
    }
}


TEST_CASE("Identifier", "[parser]") {

    SECTION("label") {
        auto tokens = TokenVector{
            jac::lex::Token(1, 1, "label", jac::lex::Token::IdentifierName)
        };

        jac::ast::ParserState state(tokens);

        auto result = jac::ast::parseIdentifier(state);
        CAPTURE(state.getErrorMessage());
        CAPTURE(state.getErrorToken());
        REQUIRE(state.isEnd());
        REQUIRE(result);
        REQUIRE(result->name == "label");
    }

    SECTION("var") {
        auto tokens = TokenVector{
            jac::lex::Token(1, 1, "var", jac::lex::Token::Keyword)
        };

        jac::ast::ParserState state(tokens);

        auto result = jac::ast::parseIdentifier(state);
        CAPTURE(state.getErrorMessage());
        CAPTURE(state.getErrorToken());
        REQUIRE(!result);
    }
}


TEST_CASE("IdentifierReference", "[parser]") {

    SECTION("label") {
        auto tokens = TokenVector{
            jac::lex::Token(1, 1, "label", jac::lex::Token::IdentifierName)
        };

        jac::ast::ParserState state(tokens);

        auto result = jac::ast::parseIdentifierReference(state);
        CAPTURE(state.getErrorMessage());
        CAPTURE(state.getErrorToken());
        REQUIRE(state.isEnd());
        REQUIRE(result);
    }

    SECTION("yield fail") {
        auto tokens = TokenVector{
            jac::lex::Token(1, 1, "yield", jac::lex::Token::Keyword)
        };

        jac::ast::ParserState state(tokens);
        auto _ = state.pushTemplate<jac::ast::Yield{true}>();

        auto result = jac::ast::parseIdentifierReference(state);
        CAPTURE(state.getErrorMessage());
        CAPTURE(state.getErrorToken());
        REQUIRE(!result);
    }
}


TEST_CASE("BindingIdentifier", "[parser]") {

    SECTION("label") {
        auto tokens = TokenVector{
            jac::lex::Token(1, 1, "label", jac::lex::Token::IdentifierName)
        };

        jac::ast::ParserState state(tokens);

        auto result = jac::ast::parseBindingIdentifier(state);
        CAPTURE(state.getErrorMessage());
        CAPTURE(state.getErrorToken());
        REQUIRE(state.isEnd());
        REQUIRE(result);
        REQUIRE(result->name == "label");
    }

    SECTION("yield") {
        auto tokens = TokenVector{
            jac::lex::Token(1, 1, "yield", jac::lex::Token::Keyword)
        };

        jac::ast::ParserState state(tokens);

        auto result = jac::ast::parseBindingIdentifier(state);
        CAPTURE(state.getErrorMessage());
        CAPTURE(state.getErrorToken());
        REQUIRE(state.isEnd());
        REQUIRE(result);
        REQUIRE(result->name == "yield");
    }
}


TEST_CASE("LabelIdentifier", "[parser]") {

    SECTION("label") {
        auto tokens = TokenVector{
            jac::lex::Token(1, 1, "label", jac::lex::Token::IdentifierName)
        };

        jac::ast::ParserState state(tokens);

        auto result = jac::ast::parseLabelIdentifier(state);
        CAPTURE(state.getErrorMessage());
        CAPTURE(state.getErrorToken());
        REQUIRE(state.isEnd());
        REQUIRE(result);
    }

    SECTION("yield fail") {
        auto tokens = TokenVector{
            jac::lex::Token(1, 1, "yield", jac::lex::Token::Keyword)
        };

        jac::ast::ParserState state(tokens);
        auto _ = state.pushTemplate<jac::ast::Yield{true}>();

        auto result = jac::ast::parseLabelIdentifier(state);
        CAPTURE(state.getErrorMessage());
        CAPTURE(state.getErrorToken());
        REQUIRE(!result);
    }
}


TEST_CASE("PrivateIdentifier", "[parser]") {

    SECTION("label") {
        auto tokens = TokenVector{
            jac::lex::Token(1, 1, "label", jac::lex::Token::IdentifierName)
        };

        jac::ast::ParserState state(tokens);

        auto result = jac::ast::parsePrivateIdentifier(state);
        CAPTURE(state.getErrorMessage());
        CAPTURE(state.getErrorToken());
        REQUIRE(!result);
    }

    SECTION("#label") {
        auto tokens = TokenVector{
            jac::lex::Token(1, 1, "#label", jac::lex::Token::IdentifierName)
        };

        jac::ast::ParserState state(tokens);

        auto result = jac::ast::parsePrivateIdentifier(state);
        CAPTURE(state.getErrorMessage());
        CAPTURE(state.getErrorToken());
        REQUIRE(state.isEnd());
        REQUIRE(result);
        REQUIRE(result->name == "#label");
    }
}


TEST_CASE("NullLiteral", "[parser]") {

    SECTION("null") {
        auto tokens = TokenVector{
            jac::lex::Token(1, 1, "null", jac::lex::Token::Keyword)
        };

        jac::ast::ParserState state(tokens);

        auto result = jac::ast::parseNullLiteral(state);
        CAPTURE(state.getErrorMessage());
        CAPTURE(state.getErrorToken());
        REQUIRE(state.isEnd());
        REQUIRE(result);
    }

    SECTION("nul") {
        auto tokens = TokenVector{
            jac::lex::Token(1, 1, "nul", jac::lex::Token::IdentifierName)
        };

        jac::ast::ParserState state(tokens);

        auto result = jac::ast::parseNullLiteral(state);
        CAPTURE(state.getErrorMessage());
        CAPTURE(state.getErrorToken());
        REQUIRE(!result);
    }
}


TEST_CASE("ThisExpr", "[parser]") {

    SECTION("this") {
        auto tokens = TokenVector{
            jac::lex::Token(1, 1, "this", jac::lex::Token::Keyword)
        };

        jac::ast::ParserState state(tokens);

        auto result = jac::ast::parseThisExpr(state);
        CAPTURE(state.getErrorMessage());
        CAPTURE(state.getErrorToken());
        REQUIRE(state.isEnd());
        REQUIRE(result);

        REQUIRE(dynamic_cast<jac::ast::ThisExpression*>(result.get()) != nullptr);
    }
}


TEST_CASE("StringLiteral", "[parser]") {

    SECTION("\"hello\"") {
        auto tokens = TokenVector{
            jac::lex::Token(1, 1, "\"hello\"", jac::lex::Token::StringLiteral)
        };

        jac::ast::ParserState state(tokens);

        auto result = jac::ast::parseStringLiteral(state);
        CAPTURE(state.getErrorMessage());
        CAPTURE(state.getErrorToken());
        REQUIRE(state.isEnd());
        REQUIRE(result);

        REQUIRE(isLit<std::string>(result.get(), "hello"));
    }

    SECTION("\"hello\\nworld\"") {
        auto tokens = TokenVector{
            jac::lex::Token(1, 1, "\"hello\\nworld\"", jac::lex::Token::StringLiteral)
        };

        jac::ast::ParserState state(tokens);

        auto result = jac::ast::parseStringLiteral(state);
        CAPTURE(state.getErrorMessage());
        CAPTURE(state.getErrorToken());
        REQUIRE(state.isEnd());
        REQUIRE(result);

        REQUIRE(isLit<std::string>(result.get(), "hello\nworld"));
    }
}


TEST_CASE("Literal", "[parser]") {

    SECTION("NumericLiteral") {
        auto tokens = TokenVector{
            jac::lex::Token(1, 1, "123", jac::lex::Token::NumericLiteral)
        };

        jac::ast::ParserState state(tokens);

        auto result = jac::ast::parseLiteral(state);
        CAPTURE(state.getErrorMessage());
        CAPTURE(state.getErrorToken());
        REQUIRE(state.isEnd());
        REQUIRE(result);

        REQUIRE(isLit<int32_t>(result.get(), 123));
    }

    SECTION("BooleanLiteral") {
        auto tokens = TokenVector{
            jac::lex::Token(1, 1, "true", jac::lex::Token::Keyword)
        };

        jac::ast::ParserState state(tokens);

        auto result = jac::ast::parseLiteral(state);
        CAPTURE(state.getErrorMessage());
        CAPTURE(state.getErrorToken());
        REQUIRE(state.isEnd());
        REQUIRE(result);

        REQUIRE(isLit<bool>(result.get(), true));
    }

    SECTION("NullLiteral") {
        auto tokens = TokenVector{
            jac::lex::Token(1, 1, "null", jac::lex::Token::Keyword)
        };

        jac::ast::ParserState state(tokens);

        auto result = jac::ast::parseLiteral(state);
        CAPTURE(state.getErrorMessage());
        CAPTURE(state.getErrorToken());
        REQUIRE(state.isEnd());
        REQUIRE(result);
        REQUIRE(std::holds_alternative<jac::ast::Literal::Null>(result->value));
    }

    SECTION("StringLiteral") {
        auto tokens = TokenVector{
            jac::lex::Token(1, 1, "\"hello\"", jac::lex::Token::StringLiteral)
        };

        jac::ast::ParserState state(tokens);

        auto result = jac::ast::parseLiteral(state);
        CAPTURE(state.getErrorMessage());
        CAPTURE(state.getErrorToken());
        REQUIRE(state.isEnd());
        REQUIRE(result);

        REQUIRE(isLit<std::string>(result.get(), "hello"));
    }

    SECTION("invalid") {
        auto tokens = TokenVector{
            jac::lex::Token(1, 1, "123.456e", jac::lex::Token::NumericLiteral)
        };

        jac::ast::ParserState state(tokens);

        auto result = jac::ast::parseLiteral(state);
        CAPTURE(state.getErrorMessage());
        CAPTURE(state.getErrorToken());
        REQUIRE(!result);
    }
}


TEST_CASE("LexicalDeclaration", "[parser]") {

    SECTION("let x;") {
        auto tokens = TokenVector{
            jac::lex::Token(1, 1, "let", jac::lex::Token::Keyword),
            jac::lex::Token(1, 5, "x", jac::lex::Token::IdentifierName),
            jac::lex::Token(1, 7, ";", jac::lex::Token::Punctuator)
        };

        jac::ast::ParserState state(tokens);

        auto result = jac::ast::parseLexicalDeclaration(state);
        CAPTURE(state.getErrorMessage());
        CAPTURE(state.getErrorToken());
        REQUIRE(state.isEnd());
        REQUIRE(result);
        REQUIRE(result->isConst == false);
        REQUIRE(result->bindingCount() == 1);
        auto binding = result->bindingGet(0);
        REQUIRE(binding->target()->name == "x");
        REQUIRE(binding->typeAnnotation() == nullptr);
        REQUIRE(binding->initializer() == nullptr);
    }

    SECTION("const x = 123;") {
        auto tokens = TokenVector{
            jac::lex::Token(1, 1, "const", jac::lex::Token::Keyword),
            jac::lex::Token(1, 7, "x", jac::lex::Token::IdentifierName),
            jac::lex::Token(1, 9, "=", jac::lex::Token::Punctuator),
            jac::lex::Token(1, 11, "123", jac::lex::Token::NumericLiteral),
            jac::lex::Token(1, 14, ";", jac::lex::Token::Punctuator)
        };

        jac::ast::ParserState state(tokens);

        auto result = jac::ast::parseLexicalDeclaration(state);
        CAPTURE(state.getErrorMessage());
        CAPTURE(state.getErrorToken());
        REQUIRE(state.isEnd());
        REQUIRE(result);
        REQUIRE(result->isConst == true);
        REQUIRE(result->bindingCount() == 1);
        auto binding = result->bindingGet(0);
        REQUIRE(binding->target()->name == "x");
        REQUIRE(binding->typeAnnotation() == nullptr);
        REQUIRE(binding->initializer() != nullptr);

        REQUIRE(isLit<int32_t>(binding->initializer(), 123));
    }

    SECTION("const x = abc + def;") {
        auto tokens = TokenVector{
            jac::lex::Token(1, 1, "const", jac::lex::Token::Keyword),
            jac::lex::Token(1, 7, "x", jac::lex::Token::IdentifierName),
            jac::lex::Token(1, 9, "=", jac::lex::Token::Punctuator),
            jac::lex::Token(1, 11, "abc", jac::lex::Token::IdentifierName),
            jac::lex::Token(1, 15, "+", jac::lex::Token::Punctuator),
            jac::lex::Token(1, 17, "def", jac::lex::Token::IdentifierName),
            jac::lex::Token(1, 20, ";", jac::lex::Token::Punctuator)
        };

        jac::ast::ParserState state(tokens);

        auto result = jac::ast::parseLexicalDeclaration(state);
        CAPTURE(state.getErrorMessage());
        CAPTURE(state.getErrorToken());
        REQUIRE(state.isEnd());
        REQUIRE(result);
        REQUIRE(result->isConst == true);
        REQUIRE(result->bindingCount() == 1);
        auto binding = result->bindingGet(0);
        REQUIRE(binding->target()->name == "x");
        REQUIRE(binding->typeAnnotation() == nullptr);
        REQUIRE(binding->initializer() != nullptr);
        auto initializer = dynamic_cast<jac::ast::BinaryExpression*>(binding->initializer());
        REQUIRE(initializer != nullptr);

        auto lhs = dynamic_cast<jac::ast::Identifier*>(initializer->left());
        REQUIRE(lhs != nullptr);
        REQUIRE(lhs->name == "abc");

        auto rhs = dynamic_cast<jac::ast::Identifier*>(initializer->right());
        REQUIRE(rhs != nullptr);
        REQUIRE(rhs->name == "def");
    }
}


TEST_CASE("UnaryExpression", "[parser]") {

    SECTION("+ - 123") {
        auto tokens = TokenVector{
            jac::lex::Token(1, 1, "+", jac::lex::Token::Punctuator),
            jac::lex::Token(1, 3, "-", jac::lex::Token::Punctuator),
            jac::lex::Token(1, 5, "123", jac::lex::Token::NumericLiteral)
        };

        jac::ast::ParserState state(tokens);

        auto result = jac::ast::parseUnaryExpression(state);
        CAPTURE(state.getErrorMessage());
        CAPTURE(state.getErrorToken());
        REQUIRE(state.isEnd());
        REQUIRE(result);

        auto& first_un = dynamic_cast<jac::ast::UnaryExpression&>(*result);
        REQUIRE(first_un.op == "+");

        auto& second_un = dynamic_cast<jac::ast::UnaryExpression&>(*first_un.expression());
        REQUIRE(second_un.op == "-");

        REQUIRE(isLit<int32_t>(second_un.expression(), 123));
    }

    SECTION("- a++") {
        auto tokens = TokenVector{
            jac::lex::Token(1, 1, "-", jac::lex::Token::Punctuator),
            jac::lex::Token(1, 3, "a", jac::lex::Token::IdentifierName),
            jac::lex::Token(1, 4, "++", jac::lex::Token::Punctuator)
        };

        jac::ast::ParserState state(tokens);

        auto result = jac::ast::parseUnaryExpression(state);
        CAPTURE(state.getErrorMessage());
        CAPTURE(state.getErrorToken());
        REQUIRE(state.isEnd());
        REQUIRE(result);

        auto& un = dynamic_cast<jac::ast::UnaryExpression&>(*result);
        REQUIRE(un.op == "-");

        auto& update = dynamic_cast<jac::ast::UpdateExpression&>(*un.expression());
        REQUIRE(update.kind == jac::ast::UpdateExpression::Op::PostInc);

        auto& id = dynamic_cast<jac::ast::Identifier&>(*update.expression());
        REQUIRE(id.name == "a");
    }
}


TEST_CASE("BinaryExpression", "[parser]") {

    SECTION("1 + 2") {
        auto tokens = TokenVector{
            jac::lex::Token(1, 1, "1", jac::lex::Token::NumericLiteral),
            jac::lex::Token(1, 3, "+", jac::lex::Token::Punctuator),
            jac::lex::Token(1, 5, "2", jac::lex::Token::NumericLiteral)
        };

        jac::ast::ParserState state(tokens);

        auto result = jac::ast::parseBinaryExpression(state);
        CAPTURE(state.getErrorMessage());
        CAPTURE(state.getErrorToken());
        REQUIRE(state.isEnd());
        REQUIRE(result);

        auto& addExp = dynamic_cast<jac::ast::BinaryExpression&>(*result);
        REQUIRE(addExp.op == "+");

        REQUIRE(isLit<int32_t>(addExp.left(), 1));
        REQUIRE(isLit<int32_t>(addExp.right(), 2));
    }

    SECTION("1 + 2 * 3") {
        auto tokens = TokenVector{
            jac::lex::Token(1, 1, "1", jac::lex::Token::NumericLiteral),
            jac::lex::Token(1, 3, "+", jac::lex::Token::Punctuator),
            jac::lex::Token(1, 5, "2", jac::lex::Token::NumericLiteral),
            jac::lex::Token(1, 7, "*", jac::lex::Token::Punctuator),
            jac::lex::Token(1, 9, "3", jac::lex::Token::NumericLiteral)
        };

        jac::ast::ParserState state(tokens);

        auto result = jac::ast::parseBinaryExpression(state);
        CAPTURE(state.getErrorMessage());
        CAPTURE(state.getErrorToken());
        REQUIRE(state.isEnd());
        REQUIRE(result);

        auto& addExp = dynamic_cast<jac::ast::BinaryExpression&>(*result);
        REQUIRE(addExp.op == "+");
        REQUIRE(isLit<int32_t>(addExp.left(), 1));

        auto& mulExp = dynamic_cast<jac::ast::BinaryExpression&>(*addExp.right());
        REQUIRE(mulExp.op == "*");
        REQUIRE(isLit<int32_t>(mulExp.left(), 2));
        REQUIRE(isLit<int32_t>(mulExp.right(), 3));
    }

    SECTION("1 + 2 * 3 * 4 - 5") {
        auto tokens = TokenVector{
            jac::lex::Token(1, 1, "1", jac::lex::Token::NumericLiteral),
            jac::lex::Token(1, 3, "+", jac::lex::Token::Punctuator),
            jac::lex::Token(1, 5, "2", jac::lex::Token::NumericLiteral),
            jac::lex::Token(1, 7, "*", jac::lex::Token::Punctuator),
            jac::lex::Token(1, 9, "3", jac::lex::Token::NumericLiteral),
            jac::lex::Token(1, 11, "*", jac::lex::Token::Punctuator),
            jac::lex::Token(1, 13, "4", jac::lex::Token::NumericLiteral),
            jac::lex::Token(1, 15, "-", jac::lex::Token::Punctuator),
            jac::lex::Token(1, 17, "5", jac::lex::Token::NumericLiteral)
        };

        jac::ast::ParserState state(tokens);

        auto result = jac::ast::parseBinaryExpression(state);
        CAPTURE(state.getErrorMessage());
        CAPTURE(state.getErrorToken());
        REQUIRE(state.isEnd());
        REQUIRE(result);

        auto& subExp = dynamic_cast<jac::ast::BinaryExpression&>(*result);
        REQUIRE(subExp.op == "-");
        REQUIRE(isLit<int32_t>(subExp.right(), 5));

        auto& addExp = dynamic_cast<jac::ast::BinaryExpression&>(*subExp.left());
        REQUIRE(addExp.op == "+");
        REQUIRE(isLit<int32_t>(addExp.left(), 1));

        auto& firstMulExp = dynamic_cast<jac::ast::BinaryExpression&>(*addExp.right());
        REQUIRE(firstMulExp.op == "*");
        REQUIRE(isLit<int32_t>(firstMulExp.right(), 4));

        auto& secondMulExp = dynamic_cast<jac::ast::BinaryExpression&>(*firstMulExp.left());
        REQUIRE(secondMulExp.op == "*");
        REQUIRE(isLit<int32_t>(secondMulExp.left(), 2));
        REQUIRE(isLit<int32_t>(secondMulExp.right(), 3));
    }

    SECTION("1 + (2 * 3)") {
        auto tokens = TokenVector{
            jac::lex::Token(1, 1, "1", jac::lex::Token::NumericLiteral),
            jac::lex::Token(1, 3, "+", jac::lex::Token::Punctuator),
            jac::lex::Token(1, 5, "(", jac::lex::Token::Punctuator),
            jac::lex::Token(1, 6, "2", jac::lex::Token::NumericLiteral),
            jac::lex::Token(1, 8, "*", jac::lex::Token::Punctuator),
            jac::lex::Token(1, 10, "3", jac::lex::Token::NumericLiteral),
            jac::lex::Token(1, 12, ")", jac::lex::Token::Punctuator)
        };

        jac::ast::ParserState state(tokens);

        auto result = jac::ast::parseBinaryExpression(state);
        CAPTURE(state.getErrorMessage());
        CAPTURE(state.getErrorToken());
        REQUIRE(state.isEnd());
        REQUIRE(result);

        auto& addExp = dynamic_cast<jac::ast::BinaryExpression&>(*result);
        REQUIRE(addExp.op == "+");
        REQUIRE(isLit<int32_t>(addExp.left(), 1));

        auto& mulExp = dynamic_cast<jac::ast::BinaryExpression&>(*addExp.right());
        REQUIRE(mulExp.op == "*");
        REQUIRE(isLit<int32_t>(mulExp.left(), 2));
        REQUIRE(isLit<int32_t>(mulExp.right(), 3));
    }

    SECTION("2 * 4 / 2 % 3") {
        auto tokens = TokenVector{
            jac::lex::Token(1, 1, "2", jac::lex::Token::NumericLiteral),
            jac::lex::Token(1, 3, "*", jac::lex::Token::Punctuator),
            jac::lex::Token(1, 5, "4", jac::lex::Token::NumericLiteral),
            jac::lex::Token(1, 7, "/", jac::lex::Token::Punctuator),
            jac::lex::Token(1, 9, "2", jac::lex::Token::NumericLiteral),
            jac::lex::Token(1, 11, "%", jac::lex::Token::Punctuator),
            jac::lex::Token(1, 13, "3", jac::lex::Token::NumericLiteral)
        };

        jac::ast::ParserState state(tokens);

        auto result = jac::ast::parseBinaryExpression(state);
        CAPTURE(state.getErrorMessage());
        CAPTURE(state.getErrorToken());
        REQUIRE(state.isEnd());
        REQUIRE(result);

        auto& modExp = dynamic_cast<jac::ast::BinaryExpression&>(*result);
        REQUIRE(modExp.op == "%");
        REQUIRE(isLit<int32_t>(modExp.right(), 3));

        auto& divExp = dynamic_cast<jac::ast::BinaryExpression&>(*modExp.left());
        REQUIRE(divExp.op == "/");
        REQUIRE(isLit<int32_t>(divExp.right(), 2));

        auto& mulExp = dynamic_cast<jac::ast::BinaryExpression&>(*divExp.left());
        REQUIRE(mulExp.op == "*");
        REQUIRE(isLit<int32_t>(mulExp.left(), 2));
        REQUIRE(isLit<int32_t>(mulExp.right(), 4));
    }

    SECTION("1 + + 2 - 3") {
        auto tokens = TokenVector{
            jac::lex::Token(1, 1, "1", jac::lex::Token::NumericLiteral),
            jac::lex::Token(1, 3, "+", jac::lex::Token::Punctuator),
            jac::lex::Token(1, 5, "+", jac::lex::Token::Punctuator),
            jac::lex::Token(1, 7, "2", jac::lex::Token::NumericLiteral),
            jac::lex::Token(1, 9, "-", jac::lex::Token::Punctuator),
            jac::lex::Token(1, 11, "3", jac::lex::Token::NumericLiteral)
        };

        jac::ast::ParserState state(tokens);

        auto result = jac::ast::parseBinaryExpression(state);
        CAPTURE(state.getErrorMessage());
        CAPTURE(state.getErrorToken());
        REQUIRE(state.isEnd());
        REQUIRE(result);

        auto& subExp = dynamic_cast<jac::ast::BinaryExpression&>(*result);
        REQUIRE(subExp.op == "-");
        REQUIRE(isLit<int32_t>(subExp.right(), 3));

        auto& addExp = dynamic_cast<jac::ast::BinaryExpression&>(*subExp.left());
        REQUIRE(addExp.op == "+");
        REQUIRE(isLit<int32_t>(addExp.left(), 1));

        auto& unaryExp = dynamic_cast<jac::ast::UnaryExpression&>(*addExp.right());
        REQUIRE(unaryExp.op == "+");
        REQUIRE(isLit<int32_t>(unaryExp.expression(), 2));
    }

    SECTION("1 +") {
        auto tokens = TokenVector{
            jac::lex::Token(1, 1, "1", jac::lex::Token::NumericLiteral),
            jac::lex::Token(1, 3, "+", jac::lex::Token::Punctuator)
        };

        jac::ast::ParserState state(tokens);

        auto result = jac::ast::parseBinaryExpression(state);
        CAPTURE(state.getErrorMessage());
        CAPTURE(state.getErrorToken());
        REQUIRE(!result);
    }

    SECTION("1 * 2 +") {
        auto tokens = TokenVector{
            jac::lex::Token(1, 1, "1", jac::lex::Token::NumericLiteral),
            jac::lex::Token(1, 3, "*", jac::lex::Token::Punctuator),
            jac::lex::Token(1, 5, "2", jac::lex::Token::NumericLiteral),
            jac::lex::Token(1, 7, "+", jac::lex::Token::Punctuator)
        };

        jac::ast::ParserState state(tokens);

        auto result = jac::ast::parseBinaryExpression(state);
        CAPTURE(state.getErrorMessage());
        CAPTURE(state.getErrorToken());
        REQUIRE(!result);
    }

    SECTION("1 + 2 (3)") {
        auto tokens = TokenVector{
            jac::lex::Token(1, 1, "1", jac::lex::Token::NumericLiteral),
            jac::lex::Token(1, 3, "+", jac::lex::Token::Punctuator),
            jac::lex::Token(1, 5, "2", jac::lex::Token::NumericLiteral),
            jac::lex::Token(1, 7, "(", jac::lex::Token::Punctuator),
            jac::lex::Token(1, 8, "3", jac::lex::Token::NumericLiteral),
            jac::lex::Token(1, 10, ")", jac::lex::Token::Punctuator)
        };
        jac::ast::ParserState state(tokens);

        auto result = jac::ast::parseBinaryExpression(state);
        CAPTURE(state.getErrorMessage());
        CAPTURE(state.getErrorToken());
        REQUIRE(result);
    }
}


TEST_CASE("ConditionalExpression", "[parser]") {

    SECTION("a && b ? c : d + e") {
        auto tokens = TokenVector{
            jac::lex::Token(1, 1, "a", jac::lex::Token::IdentifierName),
            jac::lex::Token(1, 3, "&&", jac::lex::Token::Punctuator),
            jac::lex::Token(1, 6, "b", jac::lex::Token::IdentifierName),
            jac::lex::Token(1, 8, "?", jac::lex::Token::Punctuator),
            jac::lex::Token(1, 10, "c", jac::lex::Token::IdentifierName),
            jac::lex::Token(1, 12, ":", jac::lex::Token::Punctuator),
            jac::lex::Token(1, 14, "d", jac::lex::Token::IdentifierName),
            jac::lex::Token(1, 16, "+", jac::lex::Token::Punctuator),
            jac::lex::Token(1, 18, "e", jac::lex::Token::IdentifierName)
        };

        jac::ast::ParserState state(tokens);

        auto result = jac::ast::parseConditionalExpression(state);
        CAPTURE(state.getErrorMessage());
        CAPTURE(state.getErrorToken());
        REQUIRE(state.isEnd());
        REQUIRE(result);

        auto& condExp = dynamic_cast<jac::ast::ConditionalExpression&>(*result);
        auto& logicalExp = dynamic_cast<jac::ast::BinaryExpression&>(*condExp.test());
        REQUIRE(logicalExp.op == "&&");
        REQUIRE(isIdent(logicalExp.left(), "a"));
        REQUIRE(isIdent(logicalExp.right(), "b"));

        REQUIRE(isIdent(condExp.consequent(), "c"));

        auto& addExp = dynamic_cast<jac::ast::BinaryExpression&>(*condExp.alternate());
        REQUIRE(addExp.op == "+");
        REQUIRE(isIdent(addExp.left(), "d"));
        REQUIRE(isIdent(addExp.right(), "e"));
    }
}


TEST_CASE("FunctionDeclaration", "[parser]") {

    SECTION("function f() {}") {
        auto tokens = TokenVector{
            jac::lex::Token(1, 1, "function", jac::lex::Token::Keyword),
            jac::lex::Token(1, 10, "f", jac::lex::Token::IdentifierName),
            jac::lex::Token(1, 11, "(", jac::lex::Token::Punctuator),
            jac::lex::Token(1, 12, ")", jac::lex::Token::Punctuator),
            jac::lex::Token(1, 14, "{", jac::lex::Token::Punctuator),
            jac::lex::Token(1, 15, "}", jac::lex::Token::Punctuator)
        };

        jac::ast::ParserState state(tokens);

        auto result = jac::ast::parseFunctionDeclaration(state, false);
        CAPTURE(state.getErrorMessage());
        CAPTURE(state.getErrorToken());
        REQUIRE(state.isEnd());
        REQUIRE(result);

        REQUIRE(result->name()->name == "f");
        REQUIRE(result->parameters());
        REQUIRE(result->parameters()->parameterCount() == 0);
        REQUIRE(!result->parameters()->restParameter());
        REQUIRE(!result->body());
    }

    SECTION("function fun(a, b) { const x = 3; let y = 4; return x + y; }") {
        auto tokens = TokenVector{
            jac::lex::Token(1, 1, "function", jac::lex::Token::Keyword),
            jac::lex::Token(1, 10, "fun", jac::lex::Token::IdentifierName),
            jac::lex::Token(1, 13, "(", jac::lex::Token::Punctuator),
            jac::lex::Token(1, 14, "a", jac::lex::Token::IdentifierName),
            jac::lex::Token(1, 15, ",", jac::lex::Token::Punctuator),
            jac::lex::Token(1, 17, "b", jac::lex::Token::IdentifierName),
            jac::lex::Token(1, 18, ")", jac::lex::Token::Punctuator),
            jac::lex::Token(1, 20, "{", jac::lex::Token::Punctuator),
            jac::lex::Token(1, 22, "const", jac::lex::Token::Keyword),
            jac::lex::Token(1, 28, "x", jac::lex::Token::IdentifierName),
            jac::lex::Token(1, 30, "=", jac::lex::Token::Punctuator),
            jac::lex::Token(1, 32, "3", jac::lex::Token::NumericLiteral),
            jac::lex::Token(1, 33, ";", jac::lex::Token::Punctuator),
            jac::lex::Token(1, 35, "let", jac::lex::Token::Keyword),
            jac::lex::Token(1, 39, "y", jac::lex::Token::IdentifierName),
            jac::lex::Token(1, 41, "=", jac::lex::Token::Punctuator),
            jac::lex::Token(1, 43, "4", jac::lex::Token::NumericLiteral),
            jac::lex::Token(1, 44, ";", jac::lex::Token::Punctuator),
            jac::lex::Token(1, 46, "return", jac::lex::Token::Keyword),
            jac::lex::Token(1, 53, "x", jac::lex::Token::IdentifierName),
            jac::lex::Token(1, 55, "+", jac::lex::Token::Punctuator),
            jac::lex::Token(1, 57, "y", jac::lex::Token::IdentifierName),
            jac::lex::Token(1, 58, ";", jac::lex::Token::Punctuator),
            jac::lex::Token(1, 60, "}", jac::lex::Token::Punctuator)
        };

        jac::ast::ParserState state(tokens);

        auto result = jac::ast::parseFunctionDeclaration(state, false);
        CAPTURE(state.getErrorMessage());
        CAPTURE(state.getErrorToken());
        REQUIRE(state.isEnd());
        REQUIRE(result);

        REQUIRE(result->name()->name == "fun");
        REQUIRE(result->parameters());

        // params
        auto& params = *result->parameters();
        REQUIRE(params.parameterCount() == 2);

        REQUIRE(isIdent(params.parameterGet(0)->target(), "a"));

        auto& bindingB = *params.parameterGet(1);
        REQUIRE(isIdent(bindingB.target(), "b"));

        REQUIRE(!result->parameters()->restParameter());

        // statements
        REQUIRE(result->body());
        auto bodyIt = result->body()->children.begin();
        REQUIRE(bodyIt != result->body()->children.end());

        auto& lexDeclX = dynamic_cast<jac::ast::LexicalDeclaration&>(**bodyIt);
        REQUIRE(lexDeclX.isConst);
        REQUIRE(lexDeclX.bindingCount() == 1);
        auto bindingX = lexDeclX.bindingGet(0);
        REQUIRE(bindingX->target()->name == "x");
        REQUIRE(isLit<int32_t>(bindingX->initializer(), 3));
        ++bodyIt;
        REQUIRE(bodyIt != result->body()->children.end());

        auto& lexDeclY = dynamic_cast<jac::ast::LexicalDeclaration&>(**bodyIt);
        REQUIRE(!lexDeclY.isConst);
        REQUIRE(lexDeclY.bindingCount() == 1);
        auto bindingY = lexDeclY.bindingGet(0);
        REQUIRE(bindingY->target()->name == "y");
        REQUIRE(isLit<int32_t>(bindingY->initializer(), 4));
        ++bodyIt;
        REQUIRE(bodyIt != result->body()->children.end());

        auto& returnStmt = dynamic_cast<jac::ast::ReturnStatement&>(**bodyIt);
        auto& addExp = dynamic_cast<jac::ast::BinaryExpression&>(*returnStmt.expression());
        REQUIRE(addExp.op == "+");
        REQUIRE(isIdent(addExp.left(), "x"));
        REQUIRE(isIdent(addExp.right(), "y"));
    }

    SECTION("function fun(a, b=3) {}") {
        auto tokens = TokenVector{
            jac::lex::Token(1, 1, "function", jac::lex::Token::Keyword),
            jac::lex::Token(1, 10, "fun", jac::lex::Token::IdentifierName),
            jac::lex::Token(1, 13, "(", jac::lex::Token::Punctuator),
            jac::lex::Token(1, 14, "a", jac::lex::Token::IdentifierName),
            jac::lex::Token(1, 15, ",", jac::lex::Token::Punctuator),
            jac::lex::Token(1, 17, "b", jac::lex::Token::IdentifierName),
            jac::lex::Token(1, 18, "=", jac::lex::Token::Punctuator),
            jac::lex::Token(1, 19, "3", jac::lex::Token::NumericLiteral),
            jac::lex::Token(1, 20, ")", jac::lex::Token::Punctuator),
            jac::lex::Token(1, 22, "{", jac::lex::Token::Punctuator),
            jac::lex::Token(1, 23, "}", jac::lex::Token::Punctuator)
        };

        jac::ast::ParserState state(tokens);

        auto result = jac::ast::parseFunctionDeclaration(state, false);
        CAPTURE(state.getErrorMessage());
        CAPTURE(state.getErrorToken());
        REQUIRE(state.isEnd());
        REQUIRE(result);

        REQUIRE(result->name()->name == "fun");
        REQUIRE(result->parameters());

        // params
        auto params = result->parameters();
        REQUIRE(params->parameterCount() == 2);
        REQUIRE(isIdent(params->parameterGet(0)->target(), "a"));

        auto& bindingB = *params->parameterGet(1);
        REQUIRE(isIdent(bindingB.target(), "b"));
        REQUIRE(isLit<int32_t>(bindingB.initializer(), 3));

        REQUIRE(!params->restParameter());

        REQUIRE(!result->body());
    }
}


TEST_CASE("Assignment", "[parser]") {

    SECTION("x = 4") {
        auto tokens = TokenVector{
            jac::lex::Token(1, 1, "x", jac::lex::Token::IdentifierName),
            jac::lex::Token(1, 3, "=", jac::lex::Token::Punctuator),
            jac::lex::Token(1, 5, "4", jac::lex::Token::NumericLiteral)
        };

        jac::ast::ParserState state(tokens);

        auto result = jac::ast::parseAssignment(state);
        CAPTURE(state.getErrorMessage());
        CAPTURE(state.getErrorToken());
        REQUIRE(state.isEnd());
        REQUIRE(result);

        REQUIRE(result->op == "=");
        auto& lhs = dynamic_cast<jac::ast::Identifier&>(*result->left());
        REQUIRE(lhs.name == "x");
        REQUIRE(isLit<int32_t>(result->right(), 4));
    }

    SECTION("x = x / 4") {
        auto tokens = TokenVector{
            jac::lex::Token(1, 1, "x", jac::lex::Token::IdentifierName),
            jac::lex::Token(1, 3, "=", jac::lex::Token::Punctuator),
            jac::lex::Token(1, 5, "x", jac::lex::Token::IdentifierName),
            jac::lex::Token(1, 7, "/", jac::lex::Token::Punctuator),
            jac::lex::Token(1, 9, "4", jac::lex::Token::NumericLiteral)
        };

        jac::ast::ParserState state(tokens);

        auto result = jac::ast::parseAssignment(state);
        CAPTURE(state.getErrorMessage());
        CAPTURE(state.getErrorToken());
        REQUIRE(state.isEnd());
        REQUIRE(result);

        REQUIRE(result->op == "=");
        auto& lhs = dynamic_cast<jac::ast::Identifier&>(*result->left());
        REQUIRE(lhs.name == "x");

        auto& divExp = dynamic_cast<jac::ast::BinaryExpression&>(*result->right());
        REQUIRE(divExp.op == "/");
        REQUIRE(isIdent(divExp.left(), "x"));
        REQUIRE(isLit<int32_t>(divExp.right(), 4));
    }

    SECTION("x += y -= 4") {
        auto tokens = TokenVector{
            jac::lex::Token(1, 1, "x", jac::lex::Token::IdentifierName),
            jac::lex::Token(1, 3, "+=", jac::lex::Token::Punctuator),
            jac::lex::Token(1, 6, "y", jac::lex::Token::IdentifierName),
            jac::lex::Token(1, 8, "-=", jac::lex::Token::Punctuator),
            jac::lex::Token(1, 11, "4", jac::lex::Token::NumericLiteral)
        };

        jac::ast::ParserState state(tokens);

        auto result = jac::ast::parseAssignment(state);
        CAPTURE(state.getErrorMessage());
        CAPTURE(state.getErrorToken());
        REQUIRE(state.isEnd());
        REQUIRE(result);

        REQUIRE(result->op == "+=");
        auto& lhs = dynamic_cast<jac::ast::Identifier&>(*result->left());
        REQUIRE(lhs.name == "x");

        auto& rightAssign = dynamic_cast<jac::ast::Assignment&>(*result->right());
        REQUIRE(rightAssign.op == "-=");

        auto& rightLhs = dynamic_cast<jac::ast::Identifier&>(*rightAssign.left());
        REQUIRE(rightLhs.name == "y");
        REQUIRE(isLit<int32_t>(rightAssign.right(), 4));
    }
}


TEST_CASE("Expression", "[parser]") {

    SECTION("x < 10") {
        auto tokens = TokenVector{
            jac::lex::Token(1, 1, "x", jac::lex::Token::IdentifierName),
            jac::lex::Token(1, 3, "<", jac::lex::Token::Punctuator),
            jac::lex::Token(1, 5, "10", jac::lex::Token::NumericLiteral)
        };

        jac::ast::ParserState state(tokens);

        auto result = jac::ast::parseExpression(state);

        CAPTURE(state.getErrorMessage());
        CAPTURE(state.getErrorToken());
        REQUIRE(state.isEnd());
        REQUIRE(result);

        auto& binaryExp = dynamic_cast<jac::ast::BinaryExpression&>(*result);
        REQUIRE(binaryExp.op == "<");
        REQUIRE(isIdent(binaryExp.left(), "x"));
        REQUIRE(isLit<int32_t>(binaryExp.right(), 10));
    }

    SECTION("x++") {
        auto tokens = TokenVector{
            jac::lex::Token(1, 1, "x", jac::lex::Token::IdentifierName),
            jac::lex::Token(1, 2, "++", jac::lex::Token::Punctuator)
        };

        jac::ast::ParserState state(tokens);

        auto result = jac::ast::parseExpression(state);

        CAPTURE(state.getErrorMessage());
        CAPTURE(state.getErrorToken());
        REQUIRE(state.isEnd());
        REQUIRE(result);

        auto& updateExp = dynamic_cast<jac::ast::UpdateExpression&>(*result);
        REQUIRE(updateExp.kind == jac::ast::UpdateExpression::Op::PostInc);
        REQUIRE(isIdent(updateExp.expression(), "x"));
    }

    SECTION("empty expr") {
        auto tokens = TokenVector{};

        jac::ast::ParserState state(tokens);

        auto result = jac::ast::parseExpression(state);

        CAPTURE(state.getErrorMessage());
        CAPTURE(state.getErrorToken());
        REQUIRE(state.isEnd());
        REQUIRE(!result);
    }

    SECTION("true literal expr") {
        auto tokens = TokenVector{
            jac::lex::Token(1, 1, "true", jac::lex::Token::Keyword)
        };

        jac::ast::ParserState state(tokens);

        auto result = jac::ast::parseExpression(state);

        CAPTURE(state.getErrorMessage());
        CAPTURE(state.getErrorToken());
        REQUIRE(state.isEnd());
        REQUIRE(result);

        REQUIRE(isLit<bool>(result.get(), true));
    }
}


TEST_CASE("BlockStatement", "[parser]") {

    SECTION("{}") {
        auto tokens = TokenVector{
            jac::lex::Token(1, 1, "{", jac::lex::Token::Punctuator),
            jac::lex::Token(1, 2, "}", jac::lex::Token::Punctuator)
        };

        jac::ast::ParserState state(tokens);

        auto result = jac::ast::parseBlockStatement(state);
        CAPTURE(state.getErrorMessage());
        CAPTURE(state.getErrorToken());
        REQUIRE(state.isEnd());
        REQUIRE(result);

        REQUIRE(result->children.size() == 0);
        REQUIRE(result->kind == jac::ast::StatementList::Kind::Block);
    }

    SECTION("{ let x = 3; x <<= x % 4; }") {
        auto tokens = TokenVector{
            jac::lex::Token(1, 1, "{", jac::lex::Token::Punctuator),
            jac::lex::Token(1, 3, "let", jac::lex::Token::Keyword),
            jac::lex::Token(1, 7, "x", jac::lex::Token::IdentifierName),
            jac::lex::Token(1, 9, "=", jac::lex::Token::Punctuator),
            jac::lex::Token(1, 11, "3", jac::lex::Token::NumericLiteral),
            jac::lex::Token(1, 12, ";", jac::lex::Token::Punctuator),
            jac::lex::Token(1, 14, "x", jac::lex::Token::IdentifierName),
            jac::lex::Token(1, 16, "<<=", jac::lex::Token::Punctuator),
            jac::lex::Token(1, 19, "x", jac::lex::Token::IdentifierName),
            jac::lex::Token(1, 21, "%", jac::lex::Token::Punctuator),
            jac::lex::Token(1, 23, "4", jac::lex::Token::NumericLiteral),
            jac::lex::Token(1, 24, ";", jac::lex::Token::Punctuator),
            jac::lex::Token(1, 26, "}", jac::lex::Token::Punctuator)
        };

        jac::ast::ParserState state(tokens);

        auto result = jac::ast::parseBlockStatement(state);
        CAPTURE(state.getErrorMessage());
        CAPTURE(state.getErrorToken());
        REQUIRE(state.isEnd());
        REQUIRE(result);

        REQUIRE(result->kind == jac::ast::StatementList::Kind::Block);
        REQUIRE(result->children.size() == 2);

        auto& declStmt = dynamic_cast<jac::ast::LexicalDeclaration&>(*result->children[0]);
        REQUIRE(!declStmt.isConst);
        REQUIRE(declStmt.bindingCount() == 1);
        auto bindingX = declStmt.bindingGet(0);
        REQUIRE(bindingX->target()->name == "x");
        REQUIRE(isLit<int32_t>(bindingX->initializer(), 3));

        auto& exprStmt = dynamic_cast<jac::ast::ExpressionStatement&>(*result->children[1]);
        auto& assignExp = dynamic_cast<jac::ast::Assignment&>(*exprStmt.expression());
        REQUIRE(assignExp.op == "<<=");
        auto& lhs = dynamic_cast<jac::ast::Identifier&>(*assignExp.left());
        REQUIRE(lhs.name == "x");
    }
}


TEST_CASE("Script", "[parser]") {
    SECTION("x;") {
        auto tokens = TokenVector{
            jac::lex::Token(1, 1, "x", jac::lex::Token::IdentifierName),
            jac::lex::Token(1, 2, ";", jac::lex::Token::Punctuator)
        };

        jac::ast::ParserState state(tokens);

        auto result = jac::ast::parseScript(state);
        CAPTURE(state.getErrorMessage());
        CAPTURE(state.getErrorToken());
        REQUIRE(state.isEnd());
        REQUIRE(result);

        REQUIRE(result->body());
        REQUIRE(result->body()->children.size() == 1);
        REQUIRE(result->body()->kind == jac::ast::StatementList::Kind::Normal);
        auto& exprStmt = dynamic_cast<jac::ast::ExpressionStatement&>(*result->body()->children[0]);
        REQUIRE(isIdent(exprStmt.expression(), "x"));
    }
}


TEST_CASE("Call expression", "[parser]") {

    SECTION("f()") {
        auto tokens = TokenVector{
            jac::lex::Token(1, 1, "f", jac::lex::Token::IdentifierName),
            jac::lex::Token(1, 2, "(", jac::lex::Token::Punctuator),
            jac::lex::Token(1, 3, ")", jac::lex::Token::Punctuator)
        };

        jac::ast::ParserState state(tokens);

        auto result = jac::ast::parseCallExpression(state);
        CAPTURE(state.getErrorMessage());
        CAPTURE(state.getErrorToken());
        REQUIRE(state.isEnd());
        REQUIRE(result);
    }

    SECTION("func(1, 'abc', variable)") {
        auto tokens = TokenVector{
            jac::lex::Token(1, 1, "func", jac::lex::Token::IdentifierName),
            jac::lex::Token(1, 9, "(", jac::lex::Token::Punctuator),
            jac::lex::Token(1, 10, "1", jac::lex::Token::NumericLiteral),
            jac::lex::Token(1, 11, ",", jac::lex::Token::Punctuator),
            jac::lex::Token(1, 13, "'abc'", jac::lex::Token::StringLiteral),
            jac::lex::Token(1, 18, ",", jac::lex::Token::Punctuator),
            jac::lex::Token(1, 20, "variable", jac::lex::Token::IdentifierName),
            jac::lex::Token(1, 28, ")", jac::lex::Token::Punctuator)
        };

        jac::ast::ParserState state(tokens);

        auto result = jac::ast::parseCallExpression(state);
        CAPTURE(state.getErrorMessage());
        CAPTURE(state.getErrorToken());
        REQUIRE(state.isEnd());
        REQUIRE(result);

        auto& callExp = dynamic_cast<jac::ast::CallExpression&>(*result);
        REQUIRE(isIdent(callExp.callee(), "func"));

        REQUIRE(callExp.arguments());
        REQUIRE(callExp.arguments()->argCount() == 3);
        auto& args = *callExp.arguments();

        REQUIRE(isLit<int32_t>(args.argGet(0), 1));
        REQUIRE(isLit<std::string>(args.argGet(1), "abc"));
        REQUIRE(isIdent(args.argGet(2), "variable"));

        REQUIRE(!callExp.arguments()->spread());
    }

    SECTION("call(true)") {
        auto tokens = TokenVector{
            jac::lex::Token(1, 1, "call", jac::lex::Token::IdentifierName),
            jac::lex::Token(1, 5, "(", jac::lex::Token::Punctuator),
            jac::lex::Token(1, 6, "true", jac::lex::Token::Keyword),
            jac::lex::Token(1, 10, ")", jac::lex::Token::Punctuator)
        };

        jac::ast::ParserState state(tokens);

        auto result = jac::ast::parseCallExpression(state);
        CAPTURE(state.getErrorMessage());
        CAPTURE(state.getErrorToken());
        REQUIRE(state.isEnd());
        REQUIRE(result);

        auto& callExp = dynamic_cast<jac::ast::CallExpression&>(*result);
        REQUIRE(isIdent(callExp.callee(), "call"));

        REQUIRE(callExp.arguments());
        REQUIRE(callExp.arguments()->argCount() == 1);
        REQUIRE(isLit<bool>(callExp.arguments()->argGet(0), true));

        REQUIRE(!callExp.arguments()->spread());
    }

    SECTION("call(fun(true))") {
        auto tokens = TokenVector{
            jac::lex::Token(1, 1, "call", jac::lex::Token::IdentifierName),
            jac::lex::Token(1, 5, "(", jac::lex::Token::Punctuator),
            jac::lex::Token(1, 6, "fun", jac::lex::Token::IdentifierName),
            jac::lex::Token(1, 9, "(", jac::lex::Token::Punctuator),
            jac::lex::Token(1, 10, "true", jac::lex::Token::Keyword),
            jac::lex::Token(1, 14, ")", jac::lex::Token::Punctuator),
            jac::lex::Token(1, 16, ")", jac::lex::Token::Punctuator)
        };

        jac::ast::ParserState state(tokens);

        auto result = jac::ast::parseCallExpression(state);
        CAPTURE(state.getErrorMessage());
        CAPTURE(state.getErrorToken());
        REQUIRE(state.isEnd());
        REQUIRE(result);

        auto& callExp = dynamic_cast<jac::ast::CallExpression&>(*result);
        REQUIRE(isIdent(callExp.callee(), "call"));
        REQUIRE(callExp.arguments());
        REQUIRE(!callExp.arguments()->spread());

        auto& args = *callExp.arguments();
        REQUIRE(args.argCount() == 1);

        auto& innerCall = dynamic_cast<jac::ast::CallExpression&>(*args.argGet(0));
        REQUIRE(isIdent(innerCall.callee(), "fun"));
        REQUIRE(innerCall.arguments());
        REQUIRE(!innerCall.arguments()->spread());
        auto& innerArgs = *innerCall.arguments();
        REQUIRE(innerArgs.argCount() == 1);
        REQUIRE(isLit<bool>(innerArgs.argGet(0), true));
    }
}


TEST_CASE("If statement", "[parser]") {

    SECTION("if (x) {}") {
        auto tokens = TokenVector{
            jac::lex::Token(1, 1, "if", jac::lex::Token::Keyword),
            jac::lex::Token(1, 4, "(", jac::lex::Token::Punctuator),
            jac::lex::Token(1, 5, "x", jac::lex::Token::IdentifierName),
            jac::lex::Token(1, 6, ")", jac::lex::Token::Punctuator),
            jac::lex::Token(1, 8, "{", jac::lex::Token::Punctuator),
            jac::lex::Token(1, 9, "}", jac::lex::Token::Punctuator)
        };

        jac::ast::ParserState state(tokens);

        auto result = jac::ast::parseIfStatement(state);
        CAPTURE(state.getErrorMessage());
        CAPTURE(state.getErrorToken());
        REQUIRE(state.isEnd());
        REQUIRE(result);

        REQUIRE(result->consequent());
        REQUIRE_FALSE(result->alternate());
    }

    SECTION("if (x) { y; } else { z; }") {
        auto tokens = TokenVector{
            jac::lex::Token(1, 1, "if", jac::lex::Token::Keyword),
            jac::lex::Token(1, 4, "(", jac::lex::Token::Punctuator),
            jac::lex::Token(1, 5, "x", jac::lex::Token::IdentifierName),
            jac::lex::Token(1, 6, ")", jac::lex::Token::Punctuator),
            jac::lex::Token(1, 8, "{", jac::lex::Token::Punctuator),
            jac::lex::Token(1, 10, "y", jac::lex::Token::IdentifierName),
            jac::lex::Token(1, 11, ";", jac::lex::Token::Punctuator),
            jac::lex::Token(1, 13, "}", jac::lex::Token::Punctuator),
            jac::lex::Token(1, 15, "else", jac::lex::Token::Keyword),
            jac::lex::Token(1, 20, "{", jac::lex::Token::Punctuator),
            jac::lex::Token(1, 22, "z", jac::lex::Token::IdentifierName),
            jac::lex::Token(1, 23, ";", jac::lex::Token::Punctuator),
            jac::lex::Token(1, 25, "}", jac::lex::Token::Punctuator)
        };

        jac::ast::ParserState state(tokens);

        auto result = jac::ast::parseIfStatement(state);
        CAPTURE(state.getErrorMessage());
        CAPTURE(state.getErrorToken());
        REQUIRE(state.isEnd());
        REQUIRE(result);

        REQUIRE(result->consequent());
        REQUIRE(result->alternate());
    }

    SECTION("if (a == 4) b; else c;") {
        auto tokens = TokenVector{
            jac::lex::Token(1, 1, "if", jac::lex::Token::Keyword),
            jac::lex::Token(1, 4, "(", jac::lex::Token::Punctuator),
            jac::lex::Token(1, 5, "a", jac::lex::Token::IdentifierName),
            jac::lex::Token(1, 7, "==", jac::lex::Token::Punctuator),
            jac::lex::Token(1, 10, "4", jac::lex::Token::NumericLiteral),
            jac::lex::Token(1, 11, ")", jac::lex::Token::Punctuator),
            jac::lex::Token(1, 13, "b", jac::lex::Token::IdentifierName),
            jac::lex::Token(1, 14, ";", jac::lex::Token::Punctuator),
            jac::lex::Token(1, 16, "else", jac::lex::Token::Keyword),
            jac::lex::Token(1, 21, "c", jac::lex::Token::IdentifierName),
            jac::lex::Token(1, 22, ";", jac::lex::Token::Punctuator)
        };

        jac::ast::ParserState state(tokens);

        auto result = jac::ast::parseIfStatement(state);
        CAPTURE(state.getErrorMessage());
        CAPTURE(state.getErrorToken());
        REQUIRE(state.isEnd());
        REQUIRE(result);

        REQUIRE(result->consequent());
        REQUIRE(result->alternate());
    }
}


TEST_CASE("Loop statement", "[parser]") {

    SECTION("while (x) {}") {
        auto tokens = TokenVector{
            jac::lex::Token(1, 1, "while", jac::lex::Token::Keyword),
            jac::lex::Token(1, 7, "(", jac::lex::Token::Punctuator),
            jac::lex::Token(1, 8, "x", jac::lex::Token::IdentifierName),
            jac::lex::Token(1, 9, ")", jac::lex::Token::Punctuator),
            jac::lex::Token(1, 11, "{", jac::lex::Token::Punctuator),
            jac::lex::Token(1, 12, "}", jac::lex::Token::Punctuator)
        };

        jac::ast::ParserState state(tokens);

        auto result = jac::ast::parseWhileStatement(state);
        CAPTURE(state.getErrorMessage());
        CAPTURE(state.getErrorToken());
        REQUIRE(state.isEnd());
        REQUIRE(result);

        REQUIRE(!result->init());
        REQUIRE(result->statement());
        REQUIRE(result->preCondition());
        REQUIRE(!result->postCondition());
        REQUIRE(!result->update());

        REQUIRE(isIdent(result->preCondition(), "x"));
        REQUIRE(dynamic_cast<jac::ast::StatementList&>(*result->statement()).children.empty());

    }

    SECTION("while (x) { y; }") {
        auto tokens = TokenVector{
            jac::lex::Token(1, 1, "while", jac::lex::Token::Keyword),
            jac::lex::Token(1, 7, "(", jac::lex::Token::Punctuator),
            jac::lex::Token(1, 8, "x", jac::lex::Token::IdentifierName),
            jac::lex::Token(1, 9, ")", jac::lex::Token::Punctuator),
            jac::lex::Token(1, 11, "{", jac::lex::Token::Punctuator),
            jac::lex::Token(1, 13, "y", jac::lex::Token::IdentifierName),
            jac::lex::Token(1, 14, ";", jac::lex::Token::Punctuator),
            jac::lex::Token(1, 16, "}", jac::lex::Token::Punctuator)
        };

        jac::ast::ParserState state(tokens);

        auto result = jac::ast::parseWhileStatement(state);
        CAPTURE(state.getErrorMessage());
        CAPTURE(state.getErrorToken());
        REQUIRE(state.isEnd());
        REQUIRE(result);

        REQUIRE(!result->init());
        REQUIRE(result->statement());
        REQUIRE(result->preCondition());
        REQUIRE(!result->postCondition());
        REQUIRE(!result->update());

        REQUIRE(isIdent(result->preCondition(), "x"));
        auto& stmtList = dynamic_cast<jac::ast::StatementList&>(*result->statement());
        REQUIRE(stmtList.children.size() == 1);
        auto& exprStmt = dynamic_cast<jac::ast::ExpressionStatement&>(*stmtList.children[0]);
        REQUIRE(isIdent(exprStmt.expression(), "y"));
    }

    SECTION("while (true);") {
        auto tokens = TokenVector{
            jac::lex::Token(1, 1, "while", jac::lex::Token::Keyword),
            jac::lex::Token(1, 7, "(", jac::lex::Token::Punctuator),
            jac::lex::Token(1, 8, "true", jac::lex::Token::Keyword),
            jac::lex::Token(1, 12, ")", jac::lex::Token::Punctuator),
            jac::lex::Token(1, 13, ";", jac::lex::Token::Punctuator)
        };

        jac::ast::ParserState state(tokens);

        auto result = jac::ast::parseWhileStatement(state);
        CAPTURE(state.getErrorMessage());
        CAPTURE(state.getErrorToken());
        REQUIRE(state.isEnd());
        REQUIRE(result);

        REQUIRE(!result->init());
        REQUIRE(result->statement());
        REQUIRE(result->preCondition());
        REQUIRE(!result->postCondition());
        REQUIRE(!result->update());

        REQUIRE(isLit<bool>(result->preCondition(), true));
        REQUIRE(dynamic_cast<jac::ast::EmptyStatement*>(result->statement()));
    }

    SECTION("do { x; } while (y > 0);") {
        auto tokens = TokenVector{
            jac::lex::Token(1, 1, "do", jac::lex::Token::Keyword),
            jac::lex::Token(1, 4, "{", jac::lex::Token::Punctuator),
            jac::lex::Token(1, 6, "x", jac::lex::Token::IdentifierName),
            jac::lex::Token(1, 7, ";", jac::lex::Token::Punctuator),
            jac::lex::Token(1, 9, "}", jac::lex::Token::Punctuator),
            jac::lex::Token(1, 11, "while", jac::lex::Token::Keyword),
            jac::lex::Token(1, 17, "(", jac::lex::Token::Punctuator),
            jac::lex::Token(1, 18, "y", jac::lex::Token::IdentifierName),
            jac::lex::Token(1, 20, ">", jac::lex::Token::Punctuator),
            jac::lex::Token(1, 22, "0", jac::lex::Token::NumericLiteral),
            jac::lex::Token(1, 23, ")", jac::lex::Token::Punctuator),
            jac::lex::Token(1, 24, ";", jac::lex::Token::Punctuator)
        };

        jac::ast::ParserState state(tokens);

        auto result = jac::ast::parseDoWhileStatement(state);
        CAPTURE(state.getErrorMessage());
        CAPTURE(state.getErrorToken());
        REQUIRE(state.isEnd());
        REQUIRE(result);

        REQUIRE(result->statement());
        REQUIRE(result->postCondition());
        REQUIRE(!result->preCondition());
        REQUIRE(!result->init());
        REQUIRE(!result->update());

        auto& stmtList = dynamic_cast<jac::ast::StatementList&>(*result->statement());
        REQUIRE(stmtList.children.size() == 1);
        auto& exprStmt = dynamic_cast<jac::ast::ExpressionStatement&>(*stmtList.children[0]);
        REQUIRE(isIdent(exprStmt.expression(), "x"));

        auto& binaryExp = dynamic_cast<jac::ast::BinaryExpression&>(*result->postCondition());
        REQUIRE(binaryExp.op == ">");
        REQUIRE(isIdent(binaryExp.left(), "y"));
        REQUIRE(isLit<int32_t>(binaryExp.right(), 0));
    }

    SECTION("for (let x = 0; x < 10; x++) { y; }") {
        auto tokens = TokenVector{
            jac::lex::Token(1, 1, "for", jac::lex::Token::Keyword),
            jac::lex::Token(1, 5, "(", jac::lex::Token::Punctuator),
            jac::lex::Token(1, 6, "let", jac::lex::Token::Keyword),
            jac::lex::Token(1, 10, "x", jac::lex::Token::IdentifierName),
            jac::lex::Token(1, 12, "=", jac::lex::Token::Punctuator),
            jac::lex::Token(1, 14, "0", jac::lex::Token::NumericLiteral),
            jac::lex::Token(1, 15, ";", jac::lex::Token::Punctuator),
            jac::lex::Token(1, 17, "x", jac::lex::Token::IdentifierName),
            jac::lex::Token(1, 19, "<", jac::lex::Token::Punctuator),
            jac::lex::Token(1, 21, "10", jac::lex::Token::NumericLiteral),
            jac::lex::Token(1, 23, ";", jac::lex::Token::Punctuator),
            jac::lex::Token(1, 25, "x", jac::lex::Token::IdentifierName),
            jac::lex::Token(1, 26, "++", jac::lex::Token::Punctuator),
            jac::lex::Token(1, 28, ")", jac::lex::Token::Punctuator),
            jac::lex::Token(1, 30, "{", jac::lex::Token::Punctuator),
            jac::lex::Token(1, 32, "y", jac::lex::Token::IdentifierName),
            jac::lex::Token(1, 33, ";", jac::lex::Token::Punctuator),
            jac::lex::Token(1, 35, "}", jac::lex::Token::Punctuator)
        };

        jac::ast::ParserState state(tokens);

        auto result = jac::ast::parseForStatement(state);
        CAPTURE(state.getErrorMessage());
        CAPTURE(state.getErrorToken());
        REQUIRE(state.isEnd());
        REQUIRE(result);

        REQUIRE(result->init());
        REQUIRE(result->preCondition());
        REQUIRE(!result->postCondition());
        REQUIRE(result->update());
        REQUIRE(result->statement());

        auto& declStmt = dynamic_cast<jac::ast::LexicalDeclaration&>(*result->init());
        REQUIRE(!declStmt.isConst);
        REQUIRE(declStmt.bindingCount() == 1);
        auto bindingX = declStmt.bindingGet(0);
        REQUIRE(bindingX->target()->name == "x");
        REQUIRE(isLit<int32_t>(bindingX->initializer(), 0));

        auto& binaryExp = dynamic_cast<jac::ast::BinaryExpression&>(*result->preCondition());
        REQUIRE(binaryExp.op == "<");
        REQUIRE(isIdent(binaryExp.left(), "x"));
        REQUIRE(isLit<int32_t>(binaryExp.right(), 10));

        auto& updateExp = dynamic_cast<jac::ast::UpdateExpression&>(*result->update());
        REQUIRE(updateExp.kind == jac::ast::UpdateExpression::Op::PostInc);
        REQUIRE(isIdent(updateExp.expression(), "x"));

        auto& stmtList = dynamic_cast<jac::ast::StatementList&>(*result->statement());
        REQUIRE(stmtList.children.size() == 1);
        auto& exprStmt = dynamic_cast<jac::ast::ExpressionStatement&>(*stmtList.children[0]);
        REQUIRE(isIdent(exprStmt.expression(), "y"));
    }

    SECTION("for (;;) { }") {
        auto tokens = TokenVector{
            jac::lex::Token(1, 1, "for", jac::lex::Token::Keyword),
            jac::lex::Token(1, 5, "(", jac::lex::Token::Punctuator),
            jac::lex::Token(1, 6, ";", jac::lex::Token::Punctuator),
            jac::lex::Token(1, 7, ";", jac::lex::Token::Punctuator),
            jac::lex::Token(1, 8, ")", jac::lex::Token::Punctuator),
            jac::lex::Token(1, 10, "{", jac::lex::Token::Punctuator),
            jac::lex::Token(1, 12, "}", jac::lex::Token::Punctuator)
        };

        jac::ast::ParserState state(tokens);

        auto result = jac::ast::parseForStatement(state);
        CAPTURE(state.getErrorMessage());
        CAPTURE(state.getErrorToken());
        REQUIRE(state.isEnd());
        REQUIRE(result);

        REQUIRE(!result->init());
        REQUIRE(!result->preCondition());
        REQUIRE(!result->postCondition());
        REQUIRE(!result->update());
        REQUIRE(result->statement());

        auto& stmtList = dynamic_cast<jac::ast::StatementList&>(*result->statement());
        REQUIRE(stmtList.children.size() == 0);
    }
}



TEST_CASE("Statement", "[parser]") {

    SECTION("report(fun(true));") {
        auto tokens = TokenVector{
            jac::lex::Token(1, 1, "report", jac::lex::Token::IdentifierName),
            jac::lex::Token(1, 8, "(", jac::lex::Token::Punctuator),
            jac::lex::Token(1, 9, "fun", jac::lex::Token::IdentifierName),
            jac::lex::Token(1, 13, "(", jac::lex::Token::Punctuator),
            jac::lex::Token(1, 14, "true", jac::lex::Token::Keyword),
            jac::lex::Token(1, 18, ")", jac::lex::Token::Punctuator),
            jac::lex::Token(1, 20, ")", jac::lex::Token::Punctuator),
            jac::lex::Token(1, 21, ";", jac::lex::Token::Punctuator)
        };

        jac::ast::ParserState state(tokens);

        auto result = jac::ast::parseStatement(state);
        CAPTURE(state.getErrorMessage());
        CAPTURE(state.getErrorToken());
        REQUIRE(state.isEnd());
        REQUIRE(result);

        auto& exprStmt = dynamic_cast<jac::ast::ExpressionStatement&>(*result);
        auto& callExp = dynamic_cast<jac::ast::CallExpression&>(*exprStmt.expression());
        REQUIRE(isIdent(callExp.callee(), "report"));
    }
}


TEST_CASE("Member access", "[parser]") {

    SECTION("a.b") {
        auto tokens = TokenVector{
            jac::lex::Token(1, 1, "a", jac::lex::Token::IdentifierName),
            jac::lex::Token(1, 2, ".", jac::lex::Token::Punctuator),
            jac::lex::Token(1, 3, "b", jac::lex::Token::IdentifierName)
        };

        jac::ast::ParserState state(tokens);

        auto result = jac::ast::parseLeftHandSideExpression(state);
        CAPTURE(state.getErrorMessage());
        CAPTURE(state.getErrorToken());
        REQUIRE(state.isEnd());
        REQUIRE(result);

        auto& memberExp = dynamic_cast<jac::ast::MemberAccessExpression&>(*result);
        REQUIRE(isIdent(memberExp.object(), "a"));
        REQUIRE(isLit<std::string>(memberExp.property(), "b"));
    }

    SECTION("return a.b;") {
        auto tokens = TokenVector{
            jac::lex::Token(1, 1, "return", jac::lex::Token::Keyword),
            jac::lex::Token(1, 8, "a", jac::lex::Token::IdentifierName),
            jac::lex::Token(1, 9, ".", jac::lex::Token::Punctuator),
            jac::lex::Token(1, 10, "b", jac::lex::Token::IdentifierName),
            jac::lex::Token(1, 11, ";", jac::lex::Token::Punctuator)
        };

        jac::ast::ParserState state(tokens);

        auto _ = state.pushTemplate<jac::ast::Return{true}>();
        auto result = jac::ast::parseStatement(state);
        CAPTURE(state.getErrorMessage());
        CAPTURE(state.getErrorToken());
        REQUIRE(state.isEnd());
        REQUIRE(result);
    }

    SECTION("a.b.c") {
        auto tokens = TokenVector{
            jac::lex::Token(1, 1, "a", jac::lex::Token::IdentifierName),
            jac::lex::Token(1, 2, ".", jac::lex::Token::Punctuator),
            jac::lex::Token(1, 3, "b", jac::lex::Token::IdentifierName),
            jac::lex::Token(1, 4, ".", jac::lex::Token::Punctuator),
            jac::lex::Token(1, 5, "c", jac::lex::Token::IdentifierName)
        };

        jac::ast::ParserState state(tokens);

        auto result = jac::ast::parseLeftHandSideExpression(state);
        CAPTURE(state.getErrorMessage());
        CAPTURE(state.getErrorToken());
        REQUIRE(state.isEnd());
        REQUIRE(result);

        auto& memberExp1 = dynamic_cast<jac::ast::MemberAccessExpression&>(*result);
        REQUIRE(isLit<std::string>(memberExp1.property(), "c"));
        auto& memberExp2 = dynamic_cast<jac::ast::MemberAccessExpression&>(*memberExp1.object());
        REQUIRE(isLit<std::string>(memberExp2.property(), "b"));
        REQUIRE(isIdent(memberExp2.object(), "a"));
    }

    SECTION("a.b.c[\"test\"]") {
        auto tokens = TokenVector{
            jac::lex::Token(1, 1, "a", jac::lex::Token::IdentifierName),
            jac::lex::Token(1, 2, ".", jac::lex::Token::Punctuator),
            jac::lex::Token(1, 3, "b", jac::lex::Token::IdentifierName),
            jac::lex::Token(1, 4, ".", jac::lex::Token::Punctuator),
            jac::lex::Token(1, 5, "c", jac::lex::Token::IdentifierName),
            jac::lex::Token(1, 6, "[", jac::lex::Token::Punctuator),
            jac::lex::Token(1, 7, "\"test\"", jac::lex::Token::StringLiteral),
            jac::lex::Token(1, 13, "]", jac::lex::Token::Punctuator)
        };

        jac::ast::ParserState state(tokens);

        auto result = jac::ast::parseLeftHandSideExpression(state);
        CAPTURE(state.getErrorMessage());
        CAPTURE(state.getErrorToken());
        REQUIRE(state.isEnd());
        REQUIRE(result);

        auto& memberExp1 = dynamic_cast<jac::ast::MemberAccessExpression&>(*result);
        REQUIRE(isLit<std::string>(memberExp1.property(), "test"));
        auto& memberExp2 = dynamic_cast<jac::ast::MemberAccessExpression&>(*memberExp1.object());
        REQUIRE(isLit<std::string>(memberExp2.property(), "c"));
        auto& memberExp3 = dynamic_cast<jac::ast::MemberAccessExpression&>(*memberExp2.object());
        REQUIRE(isLit<std::string>(memberExp3.property(), "b"));
        REQUIRE(isIdent(memberExp3.object(), "a"));
    }
}

TEST_CASE("Type annotation", "[parser]") {

    SECTION(": void") {
        auto tokens = TokenVector{
            jac::lex::Token(1, 2, ":", jac::lex::Token::Punctuator),
            jac::lex::Token(1, 4, "i32", jac::lex::Token::IdentifierName)
        };

        jac::ast::ParserState state(tokens);

        auto result = jac::ast::parseTypeAnnotation(state);
        CAPTURE(state.getErrorMessage());
        CAPTURE(state.getErrorToken());
        REQUIRE(state.isEnd());
        REQUIRE(result);

        REQUIRE(result->name == "i32");
    }
}
