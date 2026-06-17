/*---
desc: Script const declaration uses global slot
---*/
const x = 1;
if (x != 1) throw new Error("const global read failed");
