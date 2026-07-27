/*---
desc: await rejection propagates as runtime failure
flags: [module]
negative:
  phase: runtime
  type: Error
---*/

await Promise.reject(new Error("boom"));
