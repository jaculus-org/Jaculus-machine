/*---
desc: implicit Symbol-to-primitive coercion in arithmetic throws TypeError instead of aborting
negative:
  phase: runtime
  type: TypeError
---*/

var s = Symbol();
1 + s;
