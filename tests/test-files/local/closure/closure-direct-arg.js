/*---
desc: Closure called directly inside parent without returning
---*/
function makeAdder(base) {
  let inc = 1;
  function add() { return base; }
  let r = add();
  if (r != 10) throw new Error("expected 10, got " + r);
}
makeAdder(10);
