#include "ast.h"
#include <iostream>


namespace jac::ast {


void printAST(auto& node, std::ostream& out, int indent) {
    visitNode(node, [&](auto& specificNode) {
        using NodeType = std::decay_t<decltype(specificNode)>;
        out << std::string(indent, ' ') << typeid(NodeType).name() << "\n";
        for (auto& child : specificNode.children) {
            if (child == nullptr) {
                out << std::string(indent + 2, ' ') << "null\n";
                continue;
            }
            printAST(*child, out, indent + 2);
        }
    });
}


}  // namespace jac::ast
