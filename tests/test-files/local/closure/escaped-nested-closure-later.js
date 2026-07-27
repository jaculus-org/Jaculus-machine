/*---
desc: escaped nested closure survives invocation after creator frame completes
flags: [async]
---*/

let escaped;

function makeEscaped(base) {
  function outer(offset) {
    function inner(extra) {
      return base + offset + extra;
    }

    return inner;
  }

  return outer(2);
}

escaped = makeEscaped(5);

function invokeEscaped() {
  let result = escaped(3);
  if (result !== 10) {
    throw new Error("expected 10, got " + result);
  }
  $DONE();
}

setTimeout(invokeEscaped, 0);
