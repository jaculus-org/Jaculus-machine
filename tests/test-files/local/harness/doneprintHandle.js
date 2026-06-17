function $DONE(error) {
  if (error) {
    print('Test262:AsyncTestFailure:' + error);
  } else {
    print('Test262:AsyncTestComplete');
  }
}
