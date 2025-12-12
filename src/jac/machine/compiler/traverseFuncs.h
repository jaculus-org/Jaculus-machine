#pragma once

#include <variant>
#include <vector>

#include "ast.h"


namespace jac::ast::traverse {


struct Functions {
    std::vector<const Function*> functions;

    void add(const Function& func) {
        functions.push_back(&func);
    }
};

inline void funcs(const StatementList& statementList, Functions& out) {
    for (size_t i = 0; i < statementList.statementCount(); i++) {
        auto stmt = statementList.statementGet(i);
        if (auto func = dynamic_cast<const HoistableDeclaration*>(stmt)) {
            out.add(*func->function());
        }
    }
}


inline void funcs(const Script& script, Functions& out) {
    if (!script.body()) {
        return;
    }

    funcs(*script.body(), out);
}


} // namespace jac::ast::traverse
