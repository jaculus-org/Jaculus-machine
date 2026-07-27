/*---
desc: reading a local lexical binding before initialization throws
negative:
  phase: runtime
  type: ReferenceError
---*/

function readBeforeInitialization() {
  let value = value;
}

readBeforeInitialization();
