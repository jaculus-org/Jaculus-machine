#include "opBodies.h"

#include <cstddef>
#include <map>
#include <memory>
#include <stdexcept>
#include <string>
#include <utility>


namespace jac::cfg {


namespace {


BasicBlockBuilderPtr addBlock(Function& fn, size_t blockArgs) {
    auto block = std::make_unique<BasicBlock>();
    BasicBlockPtr blockPtr = block.get();
    fn.blocks.push_back(std::move(block));

    blockPtr->args.reserve(blockArgs);
    for (size_t i = 0; i < blockArgs; i++) {
        blockPtr->args.push_back(Reg::createTmp());
    }
    return std::make_shared<BasicBlockBuilder>(blockPtr);
}


BasicBlockBuilderPtr initBody(Function& fn, Opcode opcode) {
    const auto& info = opInfo(opcode);
    if (info.argCount == VARIADIC) {
        throw std::invalid_argument("Variadic opcodes cannot have bodies");
    }

    fn._name = std::string(info.name) + "Body";
    fn.argCount = info.argCount;

    auto entry = addBlock(fn, info.argCount);
    fn.entry = entry->block;
    return entry;
}



std::unique_ptr<Function> buildArithmeticBody(Opcode bodyOpcode, Opcode slowOpcode, Opcode i32Opcode, Opcode f64Opcode) {
    auto fn = std::make_unique<Function>();
    auto entry = initBody(*fn, bodyOpcode);

    auto lhsIntRhsIntDispatch = addBlock(*fn, 2);
    auto lhsIntRhsFloatDispatch = addBlock(*fn, 2);
    auto lhsFloatDispatch = addBlock(*fn, 2);
    auto lhsFloatRhsIntDispatch = addBlock(*fn, 2);
    auto lhsFloatRhsFloatDispatch = addBlock(*fn, 2);

    auto intCase = addBlock(*fn, 2);
    auto intExactCase = addBlock(*fn, 3);
    auto intInexactCase = addBlock(*fn, 3);
    auto floatCase = addBlock(*fn, 2);
    auto intFloatCase = addBlock(*fn, 2);
    auto floatIntCase = addBlock(*fn, 2);
    auto slowCase = addBlock(*fn, 2);

    BasicBlockBuilderPtr lhsPrimitiveException;
    BasicBlockBuilderPtr lhsPrimitiveSuccess;
    BasicBlockBuilderPtr rhsPrimitiveException;
    BasicBlockBuilderPtr rhsPrimitiveSuccess;
    BasicBlockBuilderPtr rhsStringDispatch;
    BasicBlockBuilderPtr stringConcatCase;
    BasicBlockBuilderPtr addSlowCase;
    if (bodyOpcode == Opcode::Add) {
        lhsPrimitiveException = addBlock(*fn, 4);
        lhsPrimitiveSuccess = addBlock(*fn, 4);
        rhsPrimitiveException = addBlock(*fn, 4);
        rhsPrimitiveSuccess = addBlock(*fn, 4);
        rhsStringDispatch = addBlock(*fn, 2);
        stringConcatCase = addBlock(*fn, 2);
        addSlowCase = addBlock(*fn, 2);
    }

    auto emitTagTest = [&](const BasicBlockBuilderPtr& block, size_t operandIndex, Tag tag, const BasicBlockBuilderPtr& match, const BasicBlockBuilderPtr& next) {
        auto tagged = block->emitOperation(2, Opcode::GetTag, { block->args[operandIndex] });
        Reg expected = block->emitConst(RawTagConst{ tag });
        Reg matches = block->emitOperation(1, Opcode::CmpEqTag, { tagged[1], expected })[0];
        std::vector<Reg> values = { block->args[0], block->args[1] };
        values[operandIndex] = tagged[0];
        block->setBranch(matches, *match, *next, std::move(values));
    };

    emitTagTest(entry, 0, Tag::Int, lhsIntRhsIntDispatch, lhsFloatDispatch);
    emitTagTest(lhsIntRhsIntDispatch, 1, Tag::Int, intCase, lhsIntRhsFloatDispatch);
    emitTagTest(lhsIntRhsFloatDispatch, 1, Tag::Float64, intFloatCase, slowCase);
    emitTagTest(lhsFloatDispatch, 0, Tag::Float64, lhsFloatRhsIntDispatch, slowCase);
    emitTagTest(lhsFloatRhsIntDispatch, 1, Tag::Int, floatIntCase, lhsFloatRhsFloatDispatch);
    emitTagTest(lhsFloatRhsFloatDispatch, 1, Tag::Float64, floatCase, slowCase);

    auto emitSuccess = [&](const BasicBlockBuilderPtr& block, Reg result) {
        Reg exception = block->emitOperation(1, Opcode::CreateUndefined, {})[0];
        Reg hadException = block->emitConst(RawBoolConst{ false });
        block->setExit({ result, exception, hadException });
    };

    {
        Reg lhs = intCase->emitOperation(1, Opcode::UnboxI32, { intCase->args[0] })[0];
        Reg rhs = intCase->emitOperation(1, Opcode::UnboxI32, { intCase->args[1] })[0];
        auto lhsCopies = intCase->emitOperation(2, Opcode::Dup, { lhs });
        auto rhsCopies = intCase->emitOperation(2, Opcode::Dup, { rhs });
        auto result = intCase->emitOperation(2, i32Opcode, { lhsCopies[0], rhsCopies[0] });
        intCase->setBranch(result[1], *intInexactCase, *intExactCase, { lhsCopies[1], rhsCopies[1], result[0] });
    }

    {
        intExactCase->emitOperation(0, Opcode::Kill, { intExactCase->args[0] });
        intExactCase->emitOperation(0, Opcode::Kill, { intExactCase->args[1] });
        emitSuccess(intExactCase, intExactCase->emitOperation(1, Opcode::BoxI32, { intExactCase->args[2] })[0]);
    }

    {
        Reg lhs = intInexactCase->emitOperation(1, Opcode::I32ToF64, { intInexactCase->args[0] })[0];
        Reg rhs = intInexactCase->emitOperation(1, Opcode::I32ToF64, { intInexactCase->args[1] })[0];
        intInexactCase->emitOperation(0, Opcode::Kill, { intInexactCase->args[2] });
        Reg result = intInexactCase->emitOperation(1, f64Opcode, { lhs, rhs })[0];
        emitSuccess(intInexactCase, intInexactCase->emitOperation(1, Opcode::BoxF64, { result })[0]);
    }

    {
        Reg lhs = floatCase->emitOperation(1, Opcode::UnboxF64, { floatCase->args[0] })[0];
        Reg rhs = floatCase->emitOperation(1, Opcode::UnboxF64, { floatCase->args[1] })[0];
        Reg result = floatCase->emitOperation(1, f64Opcode, { lhs, rhs })[0];
        emitSuccess(floatCase, floatCase->emitOperation(1, Opcode::BoxF64, { result })[0]);
    }

    {
        Reg lhs = intFloatCase->emitOperation(1, Opcode::UnboxI32, { intFloatCase->args[0] })[0];
        Reg rhs = intFloatCase->emitOperation(1, Opcode::UnboxF64, { intFloatCase->args[1] })[0];
        Reg lhsFloat = intFloatCase->emitOperation(1, Opcode::I32ToF64, { lhs })[0];
        Reg result = intFloatCase->emitOperation(1, f64Opcode, { lhsFloat, rhs })[0];
        emitSuccess(intFloatCase, intFloatCase->emitOperation(1, Opcode::BoxF64, { result })[0]);
    }

    {
        Reg lhs = floatIntCase->emitOperation(1, Opcode::UnboxF64, { floatIntCase->args[0] })[0];
        Reg rhs = floatIntCase->emitOperation(1, Opcode::UnboxI32, { floatIntCase->args[1] })[0];
        Reg rhsFloat = floatIntCase->emitOperation(1, Opcode::I32ToF64, { rhs })[0];
        Reg result = floatIntCase->emitOperation(1, f64Opcode, { lhs, rhsFloat })[0];
        emitSuccess(floatIntCase, floatIntCase->emitOperation(1, Opcode::BoxF64, { result })[0]);
    }

    if (bodyOpcode != Opcode::Add) {
        auto results = slowCase->emitOperation(3, slowOpcode, { slowCase->args[0], slowCase->args[1] });
        slowCase->setExit(std::move(results));
    }
    else {
        auto lhsPrimitive = slowCase->emitOperation(3, Opcode::ToPrimitive, { slowCase->args[0] });
        auto hadException = slowCase->emitOperation(2, Opcode::Dup, { lhsPrimitive[2] });
        slowCase->setBranch(hadException[0], *lhsPrimitiveException, *lhsPrimitiveSuccess,
            { lhsPrimitive[0], slowCase->args[1], lhsPrimitive[1], hadException[1] }
        );

        lhsPrimitiveException->emitOperation(0, Opcode::Kill, { lhsPrimitiveException->args[1] });
        lhsPrimitiveException->setExit({ lhsPrimitiveException->args[0], lhsPrimitiveException->args[2], lhsPrimitiveException->args[3] });

        lhsPrimitiveSuccess->emitOperation(0, Opcode::Kill, { lhsPrimitiveSuccess->args[2] });
        lhsPrimitiveSuccess->emitOperation(0, Opcode::Kill, { lhsPrimitiveSuccess->args[3] });
        auto rhsPrimitive = lhsPrimitiveSuccess->emitOperation(3, Opcode::ToPrimitive, { lhsPrimitiveSuccess->args[1] });
        hadException = lhsPrimitiveSuccess->emitOperation(2, Opcode::Dup, { rhsPrimitive[2] });
        lhsPrimitiveSuccess->setBranch(hadException[0], *rhsPrimitiveException, *rhsPrimitiveSuccess,
            { lhsPrimitiveSuccess->args[0], rhsPrimitive[0], rhsPrimitive[1], hadException[1] }
        );

        rhsPrimitiveException->emitOperation(0, Opcode::Kill, { rhsPrimitiveException->args[0] });
        rhsPrimitiveException->setExit({ rhsPrimitiveException->args[1], rhsPrimitiveException->args[2], rhsPrimitiveException->args[3] });

        rhsPrimitiveSuccess->emitOperation(0, Opcode::Kill, { rhsPrimitiveSuccess->args[2] });
        rhsPrimitiveSuccess->emitOperation(0, Opcode::Kill, { rhsPrimitiveSuccess->args[3] });
        emitTagTest(rhsPrimitiveSuccess, 0, Tag::String, stringConcatCase, rhsStringDispatch);
        emitTagTest(rhsStringDispatch, 1, Tag::String, stringConcatCase, addSlowCase);

        stringConcatCase->setExit(stringConcatCase->emitOperation(3, Opcode::StringConcat, { stringConcatCase->args[0], stringConcatCase->args[1] }));

        addSlowCase->setExit(addSlowCase->emitOperation(3, Opcode::AddSlow, { addSlowCase->args[0], addSlowCase->args[1] }));
    }

    return fn;
}


}  // namespace


const Function* opcodeBody(Opcode opcode) {
    static const std::map<Opcode, std::unique_ptr<Function>> bodies = [] {
        std::map<Opcode, std::unique_ptr<Function>> result;
        result.emplace(Opcode::Add, buildArithmeticBody(Opcode::Add, Opcode::AddSlow, Opcode::AddI32, Opcode::AddF64));
        result.emplace(Opcode::Sub, buildArithmeticBody(Opcode::Sub, Opcode::SubSlow, Opcode::SubI32, Opcode::SubF64));
        result.emplace(Opcode::Mul, buildArithmeticBody(Opcode::Mul, Opcode::MulSlow, Opcode::MulI32, Opcode::MulF64));
        return result;
    }();

    auto it = bodies.find(opcode);
    return it == bodies.end() ? nullptr : it->second.get();
}


}  // namespace jac::cfg
