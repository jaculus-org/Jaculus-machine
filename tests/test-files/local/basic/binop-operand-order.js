/*---
desc: non-commutative binary ops keep correct operand order after exception-check wrapping
---*/

if (5 - 2 !== 3) throw new Error("sub");
if (10 / 2 !== 5) throw new Error("div");
if (10 % 3 !== 1) throw new Error("rem");
if (2 ** 3 !== 8) throw new Error("pow");
if ((1 < 2) !== true) throw new Error("lt-true");
if ((2 < 1) !== false) throw new Error("lt-false");
if ((8 >> 1) !== 4) throw new Error("rshift");
if ((1 << 2) !== 4) throw new Error("lshift");
