#pragma once


#include <list>
#include <map>
#include <stack>

#include "cfg.h"


namespace jac::cfg {


using Replacements = std::map<BasicBlockPtr, std::list<BasicBlockPtr>>;
using Mapping = std::map<BasicBlockPtr, BasicBlockPtr>;


namespace detail {

    inline void removeTransitive(Replacements& replacements) {
        for (auto group = replacements.begin(); group != replacements.end(); ++group) {
            auto& [target, sources] = *group;
            for (const auto& source : sources) {
                auto it = replacements.find(source);
                if (it != replacements.end()) {
                    sources.splice(sources.end(), std::move(it->second));
                    replacements.erase(it);
                }
            }
        }
    }

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
            if (block->jump.type == Terminator::Branch) {
                push(block->jump.target);
                push(block->jump.other);
            }
            else if (block->jump.type == Terminator::Jump) {
                push(block->jump.target);
            }
        }
        return seen;
    }

}  // namespace detail


inline void removeEmptyBlocks(Function& fn) {
    Replacements replacements;

    for (auto& block : fn.blocks) {
        if (!block->instructions.empty()) {
            continue;
        }
        if (block->jump.type == Terminator::Jump) {
            replacements[block->jump.target].push_back(block.get());
        }
        else if (block->jump.type == Terminator::Branch) {
            if (block->jump.target == block->jump.other) {
                replacements[block->jump.target].push_back(block.get());
            }
        }
    }

    detail::removeTransitive(replacements);

    std::map<BasicBlockPtr, BasicBlockPtr> remap;
    for (const auto& [target, sources] : replacements) {
        for (const auto& source : sources) {
            remap[source] = target;
        }
    }

    auto replace = [&] (BasicBlockPtr& target) {
        if (target && remap.contains(target)) {
            target = remap.at(target);
        }
    };
    for (auto& block : fn.blocks) {
        replace(block->jump.target);
        replace(block->jump.other);
    }

    if (remap.contains(fn.entry)) {
        fn.entry = remap.at(fn.entry);
    }

    for (auto it = fn.blocks.begin(); it != fn.blocks.end(); ++it) {
        if (remap.contains(it->get())) {
            it = fn.blocks.erase(it);
        }
    }

    auto reachable = detail::findReachable(fn.entry);
    for (auto it = fn.blocks.begin(); it != fn.blocks.end();) {
        if (!reachable.contains(it->get())) {
            it = fn.blocks.erase(it);
        }
        else {
            ++it;
        }
    }
}


}  // namespace jac::cfg
