/*---
desc: escaped nested async closure survives invocation after creator frame completes
flags: [async]
---*/

let escaped;
let result = 0;

function makeEscaped(base) {
  function outer(offset) {
    async function inner(extra) {
      return await Promise.resolve(base + offset + extra);
    }

    return inner;
  }

  return outer(2);
}

function store(value) {
  result = value;
  if (result !== 10) {
    throw new Error("expected 10, got " + result);
  }
  $DONE();
}

function invokeEscaped() {
  escaped(3).then(store);
}

escaped = makeEscaped(5);
setTimeout(invokeEscaped, 0);
