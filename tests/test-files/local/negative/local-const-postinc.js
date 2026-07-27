/*---
desc: local const cannot be incremented
negative:
  phase: runtime
  type: TypeError
---*/

function f() {
  const x = 1;
  x++;
}

f();
