/*---
desc: Simple closure - capture and read a local variable
---*/
function makeCounter() {
  let n = 42;
  function inner() { return n; }
  return inner;
}
let c = makeCounter();
let r = c();
if (r != 42) throw new Error("expected 42, got " + r);
