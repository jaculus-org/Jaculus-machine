/*---
desc: top-level await resolves through tless cfg interp
flags: [module, async]
---*/

let value = await Promise.resolve(123);
if (value !== 123) throw new Error("expected 123, got " + value);
$DONE();
