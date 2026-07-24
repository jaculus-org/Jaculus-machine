

# File eventLoopTerminal.h

[**File List**](files.md) **>** [**features**](dir_6f95e06b732314161804ab1ef73c9681.md) **>** [**eventLoopTerminal.h**](eventLoopTerminal_8h.md)

[Go to the documentation of this file](eventLoopTerminal_8h.md)


```C++
#pragma once


namespace jac {


template<class Next>
class EventLoopTerminal : public Next {
public:
    virtual void runOnEventLoop() override {  // NOLINT
        Next::onEventLoop();
    };
};


} // namespace jac
```


