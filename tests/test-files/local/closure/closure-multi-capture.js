/*---
desc: Closure capturing multiple variables from enclosing scope, all used
---*/
function make(a, b, c) {
  function sum() { return a + b + c; }
  return sum;
}
function test() {
  let f = make(1, 2, 3);
  let r = f();
  if (r != 6) throw new Error("expected 6, got " + r);
}
test();
