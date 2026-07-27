/*---
desc: Script const cannot be redeclared with let
negative:
  phase: parse
  type: SyntaxError
---*/
const x = 1;
let x = 2;
