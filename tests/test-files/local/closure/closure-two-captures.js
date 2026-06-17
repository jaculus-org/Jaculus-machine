/*---
desc: Closure capturing two variables (read-only)
---*/
function makeAdder(base) {
  let inc = 5;
  function add() { return base; }
  return add;
}
function test() {
  let f = makeAdder(10);
  let r = f();
  if (r != 10) throw new Error("expected 10, got " + r);
}
test();
