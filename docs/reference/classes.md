# Classes

Classes, in the context of Jaculus-machine are, similarly to JavaScript classes, templates for creating objects. Unlike raw JavaScript classes, however,
classes defined in C++ can contain opaque C++ data, which can then be used through an interface defined in the class definition.

## Defining a class

To create a class, you must first define a `ProtoBuilder` structure. A `ProtoBuilder` describes the class's behavior and prototype through a **static**
interface. Features of the class are specified by inheriting from the structures in the `jac::ProtoBuilder` namespace and overriding their **static** interfaces.
These structs contain a default implementation of their interface and some convenience functions for describing the class.

The list of base structs and their interface/convenience functions can be found in the [API documentation](/doxygen/namespacejac_1_1ProtoBuilder/).

An example of the `ProtoBuilder` structure is shown below:

```cpp
/* Describes a simple JavaScript class with opaque object of type MyClass */
struct MyBuilder : public ProtoBuilder::Opaque<MyClass>, public ProtoBuilder::Properties {

    /* describes how to construct the opaque object */
    static MyClass* constructOpaque(ContextRef ctx, ValueVectorWeak args) {
        if (args.size() < 1) {
            throw std::runtime_error("MyClass constructor expects 1 argument");
        }
        /* get the first argument as an integer */
        int a = args[0].to<int>();
        return new MyClass(a);
    }

    /* describes how to destruct the opaque object, copy-pasted from the
       default implementation provided by the base class (can be omited) */
    static void destructOpaque(JSRuntime* rt, MyClass* ptr) noexcept {
        delete ptr;
    }

    /* describes which properties should be added to the prototype */
    static void addProperties(ContextRef ctx, Object proto) {
        /* a convenience function from ProtoBuilder::Opaque,
           which allows simplified access to the opaque object */
        addPropMember<int, &MyClass::foo>(ctx, proto, "foo");

        /* a convenience function from ProtoBuilder::Opaque */
        addMethodMember<decltype(&MyClass::bar), &MyClass::bar>(ctx, proto, "bar");

        /* a more roundabout way of adding a method to the prototype */
        FunctionFactory ff(ctx);
        proto.defineProperty("baz", ff.newFunctionThis([](ContextRef ctx, ValueWeak thisValue) {
            /* another convenience function from ProtoBuilder::Opaque */
            MyClass& self = *getOpaque(ctx, thisValue);
            self.baz();
        }));
    }
};
```

The `jac::Class` template can be then instantiated with the `ProtoBuilder` structure to create a class definition, and a name can be assigned to it
using the `init` method. This method can be called repeatedly without any effect if called with the same name; otherwise, an exception will be thrown.

```cpp
using MyClassJs = Class<MyBuilder>;

MyClassJs::init("MyClass");
```

To use the class in a given Context, the class must be initialized in the Context by calling the idempotent `initContext` method.
It returns the existing or newly created prototype:


```cpp
ContextRef ctx = ...;

Object prototype = MyClassJs::initContext(ctx);
```

After the class and context are initialized, you can get the class constructor and prototype and instantiate the class:

```cpp
Value constructor = MyClassJs::getConstructor(ctx);
Value prototype = MyClassJs::getPrototype(ctx);

Value obj = constructor.to<Function>().callConstructor();
```
