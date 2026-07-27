/*---
desc: escaped async closure can be invoked later from timer callback
flags: [module, async]
---*/

function makeAsyncAdder(base) {
  async function addLater(value) {
    return await Promise.resolve(base + value);
  }

  return addLater;
}

let escaped = makeAsyncAdder(5);
let result = 0;

function store(value) {
  result = value;
}

function invokeLater() {
  escaped(2).then(store);
}

setTimeout(invokeLater, 0);

await sleep(0);
await Promise.resolve();

if (result !== 7) throw new Error("expected 7, got " + result);
$DONE();
