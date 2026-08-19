/*---
desc: Intermediate values carried across checked operations and basic blocks are preserved
---*/

function exerciseCarriedValues() {
    let value = 4;
    let previous = value++;
    if (previous !== 4 || value !== 5) {
        throw new Error("post-increment carry was not preserved");
    }

    let absolute = Math.abs(-7);
    if (absolute !== 7) {
        throw new Error("method receiver carry was not preserved");
    }

    let assigned = (Math.__tlessCarry = 4);
    if (assigned !== 4 || Math.__tlessCarry !== 4) {
        throw new Error("member assignment result was not preserved");
    }

    let oldIncrement = Math.__tlessCarry++;
    let oldDecrement = Math.__tlessCarry--;
    if (oldIncrement !== 4 || oldDecrement !== 5
        || Math.__tlessCarry !== 4) {
        throw new Error("member update result was not preserved");
    }

    let compound = (Math.__tlessCarry += 3);
    if (compound !== 7 || Math.__tlessCarry !== 7) {
        throw new Error("member compound result was not preserved");
    }

    let andAssigned = (Math.__tlessCarry &&= 9);
    Math.__tlessCarry = 0;
    let andSkipped = (Math.__tlessCarry &&= 10);
    let orAssigned = (Math.__tlessCarry ||= 11);
    let orSkipped = (Math.__tlessCarry ||= 12);
    if (andAssigned !== 9 || andSkipped !== 0
        || orAssigned !== 11 || orSkipped !== 11) {
        throw new Error("member short-circuit result was not preserved");
    }
}

exerciseCarriedValues();
$DONE();
