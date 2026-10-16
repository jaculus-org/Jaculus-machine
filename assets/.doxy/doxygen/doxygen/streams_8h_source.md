

# File streams.h

[**File List**](files.md) **>** [**features**](dir_6f95e06b732314161804ab1ef73c9681.md) **>** [**types**](dir_7e10f281dae724a55a0e1ba0acd02229.md) **>** [**streams.h**](streams_8h.md)

[Go to the documentation of this file](streams_8h.md)


```C++
#pragma once

#include <functional>
#include <string>


namespace jac {


class Writable {
public:
    virtual void write(std::string data) = 0;

    virtual ~Writable() = default;
};


class Readable {
public:
    virtual bool get(std::function<void(char)> callback) = 0;
    virtual bool read(std::function<void(std::string)> callback) = 0;

    virtual ~Readable() = default;
};

class WritableRef : public Writable {
private:
    Writable* _ptr;
public:
    WritableRef(Writable* ptr): _ptr(ptr) {}

    void write(std::string data) override {
        _ptr->write(std::move(data));
    }
};

class ReadableRef : public Readable {
private:
    Readable* _ptr;
public:
    ReadableRef(Readable* ptr): _ptr(ptr) {}

    bool read(std::function<void(std::string)> callback) override {
        return _ptr->read(std::move(callback));
    }

    bool get(std::function<void(char)> callback) override {
        return _ptr->get(std::move(callback));
    }
};


} // namespace jac
```


