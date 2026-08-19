/*---
desc: a carried assignment result is consumed on the SetMember exception path
flags: [onlyStrict]
negative:
  phase: runtime
  type: TypeError
---*/

let primitive = "abc";
let assigned = (primitive.length = 1);
