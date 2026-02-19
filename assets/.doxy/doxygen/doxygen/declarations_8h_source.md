

# File declarations.h

[**File List**](files.md) **>** [**internal**](dir_f815192ecbd68c2ad40e839ff65387cf.md) **>** [**declarations.h**](declarations_8h.md)

[Go to the documentation of this file](declarations_8h.md)


```C++
#pragma once


namespace jac {


template<bool managed>
class ValueWrapper;
template<bool managed>
class ObjectWrapper;
template<bool managed>
class ArrayWrapper;
template<bool managed>
class FunctionWrapper;
template<bool managed>
class PromiseWrapper;
template<bool managed>
class ExceptionWrapper;
template<bool managed>
class ArrayBufferWrapper;


using Value = ValueWrapper<true>;        // value/strong reference
using ValueWeak = ValueWrapper<false>;  // weak reference

using Object = ObjectWrapper<true>;
using ObjectWeak = ObjectWrapper<false>;

using Function = FunctionWrapper<true>;
using FunctionWeak = FunctionWrapper<false>;

using Array = ArrayWrapper<true>;
using ArrayWeak = ArrayWrapper<false>;

using Promise = PromiseWrapper<true>;
using PromiseWeak = PromiseWrapper<false>;

using Exception = ExceptionWrapper<true>;
using ExceptionWeak = ExceptionWrapper<false>;

using ArrayBuffer = ArrayBufferWrapper<true>;
using ArrayBufferWeak = ArrayBufferWrapper<false>;


}  // namespace jac
```


