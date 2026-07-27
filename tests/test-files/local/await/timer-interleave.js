/*---
desc: timer callback runs before awaited sleep continuation
flags: [module, async]
---*/

let events = "";

function timer() {
  events = events + "timer,";
}

setTimeout(timer, 0);

await sleep(0);
events = events + "after";

if (events !== "timer,after") {
  throw new Error("unexpected order: " + events);
}

$DONE();
