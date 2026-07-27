/*---
desc: call arguments buried deep enough on the interm stack that consumeArgs
  must reorder past rot5l's reach
---*/
function id(x) { return x; }
function encode6(a, b, c, d, e, f) {
  return a * 100000 + b * 10000 + c * 1000 + d * 100 + e * 10 + f;
}

var r = encode6(id(1), id(2), id(3), id(4), id(5), id(6));
if (r !== 123456) throw new Error("encode6 argument order wrong: " + r);

var s = encode6(id(6), id(5), id(4), id(3), id(2), id(1));
if (s !== 654321) throw new Error("encode6 second argument order wrong: " + s);
