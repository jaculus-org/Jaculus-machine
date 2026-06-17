/*---
desc: Script var declaration uses global slot
---*/
var x = 1;
if (x != 1) throw new Error("var global read failed");
x = 2;
if (x != 2) throw new Error("var global write failed");
