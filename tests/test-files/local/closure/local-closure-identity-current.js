/*---
desc: StrictEq body preserves the current local-slot closure identity behavior
---*/

function outer() {
    function local() {}

    if (local === local) {
        throw new Error("local closure identity changed unexpectedly");
    }
}

outer();
$DONE();
