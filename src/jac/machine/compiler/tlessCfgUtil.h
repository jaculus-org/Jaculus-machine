#pragma once


#include <iostream>
#include <list>
#include <map>
#include <stack>

#include "tlessCfg.h"


namespace jac::cfg::tless {


namespace detail {

    inline std::set<BasicBlockPtr> findReachable(BasicBlockPtr entry) {
        std::set<BasicBlockPtr> seen;
        std::stack<BasicBlockPtr> stack;
        stack.push(entry);

        auto push = [&](BasicBlockPtr block) {
            if (!seen.contains(block)) {
                stack.push(block);
            }
        };

        while (!stack.empty()) {
            auto block = stack.top();
            stack.pop();
            seen.insert(block);
            if (block->terminator.type == Terminator::Branch) {
                push(block->terminator.target);
                push(block->terminator.other);
            }
            else if (block->terminator.type == Terminator::Jump) {
                push(block->terminator.target);
            }
        }
        return seen;
    }

}  // namespace detail


inline void removeEmptyBlocks(Function& fn) {
    std::set<BasicBlockPtr> toRemove;

    auto replace = [&] (BasicBlockPtr a, BasicBlockPtr b) {
        if (a == fn.entry) {
            fn.entry = b;
        }
        auto preds = a->predecessors;
        for (auto pred : preds) {
            std::cout << "  replacing pred " << pred << " target " << a << " with " << b << std::endl;
            if (pred->terminator.target == a) {
                pred->terminator.target = b;
            }
            if (pred->terminator.other == a) {
                pred->terminator.other = b;
            }
            b->predecessors.insert(pred);
        }
    };

    std::vector<BasicBlockPtr> stack;
    std::set<BasicBlockPtr> visited;
    stack.push_back(fn.entry);

    while (!stack.empty()) {
        auto block = stack.back();
        stack.pop_back();
        if (block == nullptr || visited.contains(block)) {
            continue;
        }
        visited.insert(block);

        if (!block->instructions.empty() || !block->args.empty() || !block->terminator.args.empty()) {
            stack.push_back(block->terminator.target);
            stack.push_back(block->terminator.other);
            continue;
        }
        if (block->terminator.type == Terminator::Jump) {
            std::cout << "Removing empty block " << block << " jumping to " << block->terminator.target << std::endl;
            block->terminator.target->predecessors.erase(block);
            replace(block, block->terminator.target);
            toRemove.insert(block);
        }
        stack.push_back(block->terminator.target);
        stack.push_back(block->terminator.other);
    }

    for (auto it = fn.blocks.begin(); it != fn.blocks.end();) {
        if (!visited.contains(it->get()) || toRemove.contains(it->get())) {
            std::cout << "Erasing block " << it->get() << " (" << (!visited.contains(it->get()) ? "unreachable" : "empty") << ")" << std::endl;
            it = fn.blocks.erase(it);
        }
        else {
            ++it;
        }
    }
}


}  // namespace jac::cfg
