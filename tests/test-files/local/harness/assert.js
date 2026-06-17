function assert(condition, message) {
  if (!condition) throw new Error("Assertion failed" + (message ? ": " + message : ""));
}
var assert = assert;
