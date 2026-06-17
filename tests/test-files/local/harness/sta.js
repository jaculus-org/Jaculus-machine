function $ERROR(message) {
  throw new Error(message ? 'Test262: ' + message : 'Test262');
}
var Test262Error = Error;
