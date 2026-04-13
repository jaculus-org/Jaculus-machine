#include "ast.h"
#include <cmath>
#include <iomanip>
#include <iostream>
#include <sstream>
#include <stdexcept>


namespace jac::ast {


namespace detail {


inline void writeIndent(std::ostream& out, int indent) {
    out << std::string(indent, ' ');
}


inline void writeJsonEscaped(std::ostream& out, std::string_view value) {
    out << '"';
    for (char ch : value) {
        switch (ch) {
            case '"': out << "\\\""; break;
            case '\\': out << "\\\\"; break;
            case '\b': out << "\\b"; break;
            case '\f': out << "\\f"; break;
            case '\n': out << "\\n"; break;
            case '\r': out << "\\r"; break;
            case '\t': out << "\\t"; break;
            default:
                if (static_cast<unsigned char>(ch) < 0x20u) {
                    static constexpr auto hex = "0123456789abcdef";
                    out << "\\u00";
                    out << hex[(static_cast<unsigned char>(ch) >> 4) & 0x0f];
                    out << hex[static_cast<unsigned char>(ch) & 0x0f];
                } else {
                    out << ch;
                }
                break;
        }
    }
    out << '"';
}


inline const char* nodeTypeName(const ASTNode& node) {
    return visitNode(node, overloaded{
        [](const TypeAnnotation&) -> const char* { return "TypeAnnotation"; },
        [](const BindingElement&) -> const char* { return "BindingElement"; },
        [](const FormalParameters&) -> const char* { return "FormalParameters"; },
        [](const Arguments&) -> const char* { return "Arguments"; },
        [](const CoverParenthesizedExpressionAndArrowParameterList&) -> const char* { return "CoverParenthesizedExpressionAndArrowParameterList"; },
        [](const CoverCallExpressionAndAsyncArrowHead&) -> const char* { return "CoverCallExpressionAndAsyncArrowHead"; },
        [](const Script&) -> const char* { return "Script"; },
        [](const Identifier&) -> const char* { return "Identifier"; },
        [](const Literal&) -> const char* { return "Literal"; },
        [](const BinaryExpression&) -> const char* { return "BinaryExpression"; },
        [](const ConditionalExpression&) -> const char* { return "ConditionalExpression"; },
        [](const UnaryExpression&) -> const char* { return "UnaryExpression"; },
        [](const UpdateExpression&) -> const char* { return "UpdateExpression"; },
        [](const Function&) -> const char* { return "Function"; },
        [](const NewCallExpression&) -> const char* { return "NewCallExpression"; },
        [](const CommaExpression&) -> const char* { return "CommaExpression"; },
        [](const Assignment&) -> const char* { return "Assignment"; },
        [](const MemberAccessExpression&) -> const char* { return "MemberAccessExpression"; },
        [](const TaggedTemplateExpression&) -> const char* { return "TaggedTemplateExpression"; },
        [](const CallExpression&) -> const char* { return "CallExpression"; },
        [](const ThisExpression&) -> const char* { return "ThisExpression"; },
        [](const ExpressionStatement&) -> const char* { return "ExpressionStatement"; },
        [](const StatementList&) -> const char* { return "StatementList"; },
        [](const LexicalDeclaration&) -> const char* { return "LexicalDeclaration"; },
        [](const IterationStatement&) -> const char* { return "IterationStatement"; },
        [](const ContinueStatement&) -> const char* { return "ContinueStatement"; },
        [](const BreakStatement&) -> const char* { return "BreakStatement"; },
        [](const ReturnStatement&) -> const char* { return "ReturnStatement"; },
        [](const ThrowStatement&) -> const char* { return "ThrowStatement"; },
        [](const DebuggerStatement&) -> const char* { return "DebuggerStatement"; },
        [](const HoistableDeclaration&) -> const char* { return "HoistableDeclaration"; },
        [](const IfStatement&) -> const char* { return "IfStatement"; },
        [](const EmptyStatement&) -> const char* { return "EmptyStatement"; },
        [](const Expression&) -> const char* { return "Expression"; },
        [](const Statement&) -> const char* { return "Statement"; },
        [](const ASTNode&) -> const char* { return "ASTNode"; }
    });
}


inline const char* binaryOpName(BinaryExpression::Op op) {
    switch (op) {
        case BinaryExpression::Coalesce: return "??";
        case BinaryExpression::LogOr: return "||";
        case BinaryExpression::LogAnd: return "&&";
        case BinaryExpression::BitOr: return "|";
        case BinaryExpression::BitXor: return "^";
        case BinaryExpression::BitAnd: return "&";
        case BinaryExpression::Eq: return "==";
        case BinaryExpression::Neq: return "!=";
        case BinaryExpression::StrictEq: return "===";
        case BinaryExpression::StrictNeq: return "!==";
        case BinaryExpression::InstanceOf: return "instanceof";
        case BinaryExpression::Lt: return "<";
        case BinaryExpression::Lte: return "<=";
        case BinaryExpression::Gt: return ">";
        case BinaryExpression::Gte: return ">=";
        case BinaryExpression::In: return "in";
        case BinaryExpression::LShift: return "<<";
        case BinaryExpression::RShift: return ">>";
        case BinaryExpression::URShift: return ">>>";
        case BinaryExpression::Add: return "+";
        case BinaryExpression::Sub: return "-";
        case BinaryExpression::Mul: return "*";
        case BinaryExpression::Div: return "/";
        case BinaryExpression::Rem: return "%";
        case BinaryExpression::Exp: return "**";
    }
    throw std::runtime_error("Unknown BinaryExpression::Op in serializer");
}


inline const char* unaryOpName(UnaryExpression::Op op) {
    switch (op) {
        case UnaryExpression::LogNot: return "!";
        case UnaryExpression::BitNot: return "~";
        case UnaryExpression::Plus: return "+";
        case UnaryExpression::Minus: return "-";
        case UnaryExpression::Typeof: return "typeof";
        case UnaryExpression::Void: return "void";
        case UnaryExpression::Delete: return "delete";
        case UnaryExpression::Await: return "await";
    }
    throw std::runtime_error("Unknown UnaryExpression::Op in serializer");
}


inline const char* updateOpName(UpdateExpression::Op op) {
    switch (op) {
        case UpdateExpression::Op::PreInc: return "++x";
        case UpdateExpression::Op::PreDec: return "--x";
        case UpdateExpression::Op::PostInc: return "x++";
        case UpdateExpression::Op::PostDec: return "x--";
    }
    throw std::runtime_error("Unknown UpdateExpression::Op in serializer");
}


inline const char* statementListKindName(StatementList::Kind kind) {
    switch (kind) {
        case StatementList::Normal: return "Normal";
        case StatementList::Block: return "Block";
    }
    throw std::runtime_error("Unknown StatementList::Kind in serializer");
}


inline const char* iterationKindName(IterationStatement::Kind kind) {
    switch (kind) {
        case IterationStatement::While: return "While";
        case IterationStatement::For: return "For";
        case IterationStatement::DoWhile: return "DoWhile";
    }
    throw std::runtime_error("Unknown IterationStatement::Kind in serializer");
}


inline const char* assignmentOpName(Assignment::Op op) {
    switch (op) {
        case Assignment::Assign: return "=";
        case Assignment::AddAssign: return "+=";
        case Assignment::SubAssign: return "-=";
        case Assignment::ExpAssign: return "**=";
        case Assignment::MulAssign: return "*=";
        case Assignment::DivAssign: return "/=";
        case Assignment::RemAssign: return "%=";
        case Assignment::LShiftAssign: return "<<=";
        case Assignment::RShiftAssign: return ">>=";
        case Assignment::URShiftAssign: return ">>>=";
        case Assignment::BitAndAssign: return "&=";
        case Assignment::BitXorAssign: return "^=";
        case Assignment::BitOrAssign: return "|=";
        case Assignment::LogAndAssign: return "&&=";
        case Assignment::LogOrAssign: return "||=";
        case Assignment::CoalesceAssign: return "?" "?=";
    }
    throw std::runtime_error("Unknown Assignment::Op in serializer");
}


inline void writeNodeJson(const ASTNode* node, std::ostream& out, int indent, int indentStep);


inline void writeLiteralValue(const Literal& lit, std::ostream& out) {
    std::visit(overloaded{
        [&](Literal::Null) {
            out << "null";
        },
        [&](bool value) {
            out << (value ? "true" : "false");
        },
        [&](double value) {
            if (std::isfinite(value)) {
                std::ostringstream ss;
                ss << std::setprecision(17) << value;
                out << ss.str();
            }
            else if (std::isnan(value)) {
                writeJsonEscaped(out, "NaN");
            }
            else if (value > 0.0) {
                writeJsonEscaped(out, "Infinity");
            }
            else {
                writeJsonEscaped(out, "-Infinity");
            }
        },
        [&](int32_t value) {
            out << value;
        },
        [&](const std::string& value) {
            writeJsonEscaped(out, value);
        }
    }, lit.value);
}


inline void writeNodeJson(const ASTNode* node, std::ostream& out, int indent, int indentStep) {
    if (node == nullptr) {
        out << "null";
        return;
    }

    out << "{\n";
    bool firstField = true;

    auto writeField = [&](std::string_view key, auto&& writer) {
        if (!firstField) {
            out << ",\n";
        }
        firstField = false;
        writeIndent(out, indent + indentStep);
        writeJsonEscaped(out, key);
        out << ": ";
        writer();
    };

    writeField("type", [&] {
        writeJsonEscaped(out, nodeTypeName(*node));
    });

    visitNode(*node, overloaded{
        [&](const Identifier& specificNode) {
            writeField("name", [&] {
                writeJsonEscaped(out, specificNode.name);
            });
            writeField("private", [&] {
                out << (specificNode.isPrivate() ? "true" : "false");
            });
        },
        [&](const TypeAnnotation& specificNode) {
            writeField("name", [&] {
                writeJsonEscaped(out, specificNode.name);
            });
        },
        [&](const Literal& specificNode) {
            writeField("value", [&] {
                writeLiteralValue(specificNode, out);
            });
        },
        [&](const BinaryExpression& specificNode) {
            writeField("op", [&] {
                writeJsonEscaped(out, binaryOpName(specificNode.op));
            });
        },
        [&](const UnaryExpression& specificNode) {
            writeField("op", [&] {
                writeJsonEscaped(out, unaryOpName(specificNode.op));
            });
        },
        [&](const UpdateExpression& specificNode) {
            writeField("op", [&] {
                writeJsonEscaped(out, updateOpName(specificNode.kind));
            });
        },
        [&](const StatementList& specificNode) {
            writeField("kind", [&] {
                writeJsonEscaped(out, statementListKindName(specificNode.kind));
            });
            writeField("hoistedDeclarations", [&] {
                out << "[";
                bool first = true;
                for (const auto& [id, isConst] : specificNode.hoistedDeclarations) {
                    if (!first) {
                        out << ", ";
                    }
                    first = false;
                    writeJsonEscaped(out, id + (isConst ? "[const]" : ""));
                }
                out << "]";
            });
        },
        [&](const LexicalDeclaration& specificNode) {
            writeField("const", [&] {
                out << (specificNode.isConst ? "true" : "false");
            });
        },
        [&](const Function& specificNode) {
            writeField("generator", [&] {
                out << (specificNode.isGenerator ? "true" : "false");
            });
            writeField("async", [&] {
                out << (specificNode.isAsync ? "true" : "false");
            });
            writeField("code", [&] {
                writeJsonEscaped(out, specificNode.code);
            });
            writeField("closureVars", [&] {
                out << "[";
                for (size_t i = 0; i < specificNode.closureVars.size(); ++i) {
                    if (i > 0) {
                        out << ", ";
                    }
                    writeJsonEscaped(out, specificNode.closureVars[i]);
                }
                out << "]";
            });
            writeField("globalVars", [&] {
                out << "[";
                for (size_t i = 0; i < specificNode.globalVars.size(); ++i) {
                    if (i > 0) {
                        out << ", ";
                    }
                    writeJsonEscaped(out, specificNode.globalVars[i]);
                }
                out << "]";
            });
        },
        [&](const Script& specificNode) {
            writeField("globalVars", [&] {
                out << "[";
                for (size_t i = 0; i < specificNode.globalVars.size(); ++i) {
                    if (i > 0) {
                        out << ", ";
                    }
                    writeJsonEscaped(out, specificNode.globalVars[i]);
                }
                out << "]";
            });
        },
        [&](const IterationStatement& specificNode) {
            writeField("kind", [&] {
                writeJsonEscaped(out, iterationKindName(specificNode.kind));
            });
        },
        [&](const Assignment& specificNode) {
            writeField("op", [&] {
                writeJsonEscaped(out, assignmentOpName(specificNode.op));
            });
        },
        [&](const auto&) {
            // No extra metadata for this node type.
        }
    });

    writeField("children", [&] {
        out << "[";
        if (!node->children.empty()) {
            out << "\n";
            for (size_t i = 0; i < node->children.size(); ++i) {
                writeIndent(out, indent + indentStep * 2);
                writeNodeJson(node->children[i].get(), out, indent + indentStep * 2, indentStep);
                if (i + 1 != node->children.size()) {
                    out << ",";
                }
                out << "\n";
            }
            writeIndent(out, indent + indentStep);
        }
        out << "]";
    });

    out << "\n";
    writeIndent(out, indent);
    out << "}";
}


}  // namespace detail


inline void printASTJson(const ASTNode& node, std::ostream& out, int indent = 0, int indentStep = 2) {
    detail::writeNodeJson(&node, out, indent, indentStep);
}


inline std::string serializeASTJson(const ASTNode& node, int indentStep = 2) {
    std::ostringstream out;
    printASTJson(node, out, 0, indentStep);
    return out.str();
}


inline void printAST(const ASTNode& node, std::ostream& out, int indent = 0) {
    printASTJson(node, out, indent, 2);
}


}  // namespace jac::ast
