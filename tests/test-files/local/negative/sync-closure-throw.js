/*---
desc: uncaught exception thrown by a synchronous cfg closure surfaces with its real type
flags: [module]
negative:
  phase: runtime
  type: TypeError
---*/

function f() {
    throw new TypeError("boom");
}
f();
