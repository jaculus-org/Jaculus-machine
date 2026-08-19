/*---
desc: a carried method receiver is consumed on the GetMember exception path
negative:
  phase: runtime
  type: TypeError
---*/

function failBeforeMethodCall() {
    let receiver;
    receiver.missing();
}

failBeforeMethodCall();
