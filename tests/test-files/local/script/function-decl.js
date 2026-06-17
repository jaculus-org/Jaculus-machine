/*---
desc: Script function declaration uses global slot
---*/
function f() { return 42; }
if (f() != 42) throw new Error("function global call failed");
