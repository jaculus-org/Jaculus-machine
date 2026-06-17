/*---
desc: Test that fails at runtime
negative:
  phase: runtime
  type: Error
---*/
throw new Error("Intentionally thrown");
