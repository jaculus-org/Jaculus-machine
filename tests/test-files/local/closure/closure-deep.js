/*---
desc: Deeply nested function closing over grandparent variable
---*/
function outer() {
  let a = 1;
  function inner() {
    function innermost() { return a; }
    return innermost;
  }
  let f = inner();
  let r = f();
  if (r != 1) throw new Error("expected 1, got " + r);
}
outer();
