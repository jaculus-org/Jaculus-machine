/*---
desc: property access on null/undefined throws TypeError instead of aborting
negative:
  phase: runtime
  type: TypeError
---*/

var obj;
obj.x;
