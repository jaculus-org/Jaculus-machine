/*---
desc: A function return selects the successful Exit operand
---*/

function returned() {
    return 42;
}

if (returned() !== 42) {
    throw new Error("value return selected the wrong Exit operand");
}

$DONE();
