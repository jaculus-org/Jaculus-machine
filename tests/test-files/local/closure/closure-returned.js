/*---
desc: Closure returned and called from module scope
---*/
function makeCounter() {
  let n = 0;
  function counter() { return n; }
  return counter;
}
let c = makeCounter();
let r = c();
if (r != 0) throw new Error("expected 0, got " + r);
