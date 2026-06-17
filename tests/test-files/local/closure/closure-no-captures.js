/*---
desc: Nested function with no captured variables
---*/
function outer() {
  function inner() { return 42; }
  return inner;
}
function test() {
  let f = outer();
  let r = f();
  if (r != 42) throw new Error("expected 42, got " + r);
}
test();
