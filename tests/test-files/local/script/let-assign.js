/*---
desc: Script let declaration uses global slot
---*/
let x = 1;
if (x != 1) throw new Error("let global read failed");
x = 2;
if (x != 2) throw new Error("let global reassignment failed");
