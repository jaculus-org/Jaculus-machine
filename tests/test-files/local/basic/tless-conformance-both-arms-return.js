/*---
desc: Control flow remains valid when both branches return
---*/

function choose(c) {
    if (c) {
        return 1;
    } else {
        return 2;
    }
}

choose(true);
$DONE();
