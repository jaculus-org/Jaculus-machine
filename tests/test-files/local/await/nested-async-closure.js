/*---
desc: nested async closure resolves through await
flags: [module, async]
---*/

async function inner() {
  return await Promise.resolve(7);
}

let result = await inner();
if (result !== 7) throw new Error("expected 7, got " + result);
$DONE();
