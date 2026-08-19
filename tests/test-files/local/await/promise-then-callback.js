/*---
desc: async cfg closure can be used as Promise.then callback
flags: [module, async]
---*/

async function plusOne(value) {
  return await Promise.resolve(value + 1);
}

let result = await Promise.resolve(7).then(plusOne);
if (result !== 8) throw new Error("expected 8, got " + result);
$DONE();
