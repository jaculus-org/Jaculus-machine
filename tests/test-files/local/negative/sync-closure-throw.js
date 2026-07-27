/*---
desc: uncaught exception thrown by a synchronous tless closure surfaces with its real type
flags: [module]
negative:
  phase: runtime
  type: TypeError
---*/

function f() {
    throw new TypeError("boom");
}
f();
