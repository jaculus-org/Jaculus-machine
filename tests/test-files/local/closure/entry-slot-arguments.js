/*---
desc: Parameters and captures are entry slots, including first-use Dup paths
---*/

function readParameter(value, unused) {
  return value;
}
if (readParameter(11) !== 11) {
  throw new Error("declared trailing parameter changed argument handling");
}

function captureParameter(value, unused) {
  function readCapturedParameter() {
    return value;
  }
  return readCapturedParameter;
}
let capturedParameter = captureParameter(22);
if (capturedParameter() !== 22) {
  throw new Error("parameter-first-Dup capture failed");
}

function recaptureParameter(value, unused) {
  function middle() {
    function readRecapturedParameter() {
      return value;
    }
    return readRecapturedParameter;
  }
  return middle;
}
let middle = recaptureParameter(33);
let recapturedParameter = middle();
if (recapturedParameter() !== 33) {
  throw new Error("capture-first-Dup capture failed");
}

$DONE();
