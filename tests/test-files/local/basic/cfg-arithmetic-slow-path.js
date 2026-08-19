/*---
desc: cfg arithmetic slow paths use the requested operation
---*/

if (2 * 0.5 !== 1) throw new Error("mixed-mul");
if (5 / 0.5 !== 10) throw new Error("mixed-div");
if (-5 % 2 !== -1) throw new Error("slow-rem");
if (-1.25 !== 0 - 1.25) throw new Error("fractional-unary-minus");
$DONE();
