/*---
desc: local const cannot be reassigned
negative:
  phase: runtime
  type: TypeError
---*/

function f() {
  const x = 1;
  x = 2;
}

f();
