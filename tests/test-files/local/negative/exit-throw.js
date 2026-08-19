/*---
desc: Explicit throw selects the exceptional Exit operand
negative:
  phase: runtime
  type: Error
---*/

function thrown() {
    throw new Error("explicit throw");
}

thrown();
