/*---
desc: Script const cannot be reassigned
negative:
  phase: runtime
  type: TypeError
---*/
const x = 1;
x = 2;
