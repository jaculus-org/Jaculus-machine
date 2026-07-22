# Functions

Jaculus-machine allows the wrapping of callable objects into JavaScript functions and the calling of JavaScript functions from C++.

Exceptions thrown in the context of a wrapped function and exceptions thrown by the JavaScript functions are propagated as described
in the [Exceptions](./exceptions.md) section.

## Wrapping callable objects

Callable objects can be wrapped into JavaScript functions using the `jac::FunctionFactory` class. The call operator of the object
must be unambiguous, and the arguments and return type must be convertible to and from JavaScript values.

To wrap a function, the user can use the `newFunction` and `newFunctionThis`. All arguments that are passed to the function call
will be converted to the types of the function parameters. If the number of arguments does not match or if the values cannot be
converted to the required types, a `TypeError` will be thrown.

The methods `newFunctionVariadic` and `newFunctionThisVariadic` can be used to create variadic functions --- all arguments that
are passed to the function call are exposed through a borrowed `ValueVectorWeak`. The view is valid only for the duration of
the native callback. Call `toOwned()` to retain the arguments in an owning `ValueVector`.

The methods `newFunctionThis` and `newFunctionThisVariadic` additionally give access to the `this` value of the function
(for example, when the function is called as a method of an object).


Here are some examples:

```cpp
jac::ContextRef ctx = ...;
jac::FunctionFactory ff(ctx);

jac::Function f1 = ff.newFunction([](int a, int b) { return a + b; });

jac::Function f2 = ff.newFunctionVariadic([](jac::ValueVectorWeak args) {
    int sum = 0;
    for (jac::ValueWeak arg : args) {
        sum += arg.to<int>();
    }
    return sum;
});

jac::Function f3 = ff.newFunctionThis([](jac::ContextRef, jac::ValueWeak thisValue, int a, int b) {
    auto obj = thisValue.to<jac::ObjectWeak>();

    return obj.get("x").to<int>() + a + b;
});

jac::Function f4 = ff.newFunctionThisVariadic([](jac::ContextRef, jac::ValueWeak thisValue, jac::ValueVectorWeak args) {
    auto obj = thisValue.to<jac::ObjectWeak>();

    int sum = obj.get("x").to<int>();
    for (jac::ValueWeak arg : args) {
        sum += arg.to<int>();
    }
    return sum;
});
```

## Calling JavaScript functions

JavaScript functions can be called either as free functions or as methods of an object.

A free function is represented by the `jac::Function` type, which has two methods for calling the function: `call<Res, Args...>(Args... args)` and `callThis<Res, Args...>(jac::ValueWeak thisValue, Args... args)`. The first method performs a standard function call, while the second method calls the function with the `this` value set to the value passed as the first argument. The template arguments `Res` and `Args...` specify the return type and the argument types of the function.

An object is represented by the `jac::Object` type, which has a method `invoke<Res, Args...>(const std::string& name, Args... args)` for calling a method of the object. The first argument specifies the name of the method, while the rest of the arguments are passed to the method. It has two other overloads, which allow specifying the identifier of the method as an `uint32_t` or a `jac::Atom`.

They can be used as follows:

```cpp
MachineBase machine;
machine.initialize();

jac::Function f1 = machine.eval("function(a, b) { return a + b; }", "<eval>", jac::EvalFlags::Global).to<jac::Function>();
jac::Function f2 = machine.eval("function() { return this.x; }", "<eval>", jac::EvalFlags::Global).to<jac::Function>();

int res1 = f1.call<int>(1, 2);  // calls f1(1, 2) and returns the result as an int

jac::Value obj = machine.eval("({ x: 5 })", "<eval>", jac::EvalFlags::Global);
int res2 = f2.callThis<int>(obj);  // calls f2.call(obj) and returns the result as an int

jac::Object obj2 = jac::Object::create(machine.context());
obj2.set("x", 10);
obj2.set("f", f2);

int res3 = obj2.invoke<int>("f");  // calls obj2.f() and returns the result as an int
```

## Calling constructors

JavaScript distinguishes whether a function is called as a constructor or as a normal function - inside JavaScript, this is signified by the `new` keyword.
To call a `jac::Function` as a constructor, use the `callConstructor<Res, Args...>(Args... args)` method.

```cpp
MachineBase machine;
machine.initialize();

jac::Function a = machine.eval("class A { constructor(x) { this.x = x } }", "<eval>", jac::EvalFlags::Global).to<jac::Function>();

jac::Value obj = a.callConstructor(5);  // calls new f(5) and returns the result as a Value
```
