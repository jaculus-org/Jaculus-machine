

# Class jac::ExceptionWrapper

**template &lt;bool managed&gt;**



[**ClassList**](annotated.md) **>** [**jac**](namespacejac.md) **>** [**ExceptionWrapper**](classjac_1_1ExceptionWrapper.md)



_An exception wrapper which can either wrap a JSValue or contain an exception description and can be thrown into JS as a specific Error type._ [More...](#detailed-description)

* `#include <values.h>`



Inherits the following classes: [jac::ValueWrapper](classjac_1_1ValueWrapper.md),  std::exception














## Public Types

| Type | Name |
| ---: | :--- |
| enum  | [**Type**](#enum-type)  <br> |








































## Public Functions

| Type | Name |
| ---: | :--- |
|   | [**ExceptionWrapper**](#function-exceptionwrapper-23) ([**ValueWrapper**](classjac_1_1ValueWrapper.md)&lt; managed &gt; value) <br>_Wrap an existing JSValue. If managed is true, JSValue will be freed when the Exception is destroyed._  |
|   | [**ExceptionWrapper**](#function-exceptionwrapper-33) ([**ContextRef**](classjac_1_1ContextRef.md) ctx, JSValue val) <br> |
|  std::string | [**stackTrace**](#function-stacktrace) () noexcept<br>_Get the exception stack trace._  |
|  JSValue | [**throwJS**](#function-throwjs) ([**ContextRef**](classjac_1_1ContextRef.md) ctx) <br>_Throw the exception into JS._  |
|  const char \* | [**what**](#function-what) () noexcept override const<br>_Get the exception message._  |


## Public Functions inherited from jac::ValueWrapper

See [jac::ValueWrapper](classjac_1_1ValueWrapper.md)

| Type | Name |
| ---: | :--- |
|   | [**ValueWrapper**](classjac_1_1ValueWrapper.md#function-valuewrapper-13) ([**ContextRef**](classjac_1_1ContextRef.md) ctx, JSValue val) <br>_Wrap an existing JSValue. If managed is true, JSValue will be freed when the Value is destroyed._  |
|   | [**ValueWrapper**](classjac_1_1ValueWrapper.md#function-valuewrapper-23) (const [**ValueWrapper**](classjac_1_1ValueWrapper.md) & other) <br> |
|   | [**ValueWrapper**](classjac_1_1ValueWrapper.md#function-valuewrapper-33) ([**ValueWrapper**](classjac_1_1ValueWrapper.md) && other) <br> |
|  JSValue & | [**getVal**](classjac_1_1ValueWrapper.md#function-getval) () <br>_Get reference to the underlying JSValue._  |
|  bool | [**isArray**](classjac_1_1ValueWrapper.md#function-isarray) () <br>_Check if the Value is an array._  |
|  bool | [**isBigInt**](classjac_1_1ValueWrapper.md#function-isbigint) () <br>_Check if the Value is BigInt._  |
|  bool | [**isBoolean**](classjac_1_1ValueWrapper.md#function-isboolean) () <br>_Check if the Value is boolean._  |
|  bool | [**isFunction**](classjac_1_1ValueWrapper.md#function-isfunction) () <br>_Check if the Value is a function._  |
|  bool | [**isInstanceOf**](classjac_1_1ValueWrapper.md#function-isinstanceof) ([**ObjectWeak**](classjac_1_1ObjectWrapper.md) obj) <br>_Check if the Value is an instance of the given object._  |
|  bool | [**isNull**](classjac_1_1ValueWrapper.md#function-isnull) () <br>_Check if the Value is null._  |
|  bool | [**isNumber**](classjac_1_1ValueWrapper.md#function-isnumber) () <br>_Check if the Value is number._  |
|  bool | [**isObject**](classjac_1_1ValueWrapper.md#function-isobject) () <br>_Check if the Value is an object._  |
|  bool | [**isString**](classjac_1_1ValueWrapper.md#function-isstring) () <br>_Check if the Value is string._  |
|  bool | [**isSymbol**](classjac_1_1ValueWrapper.md#function-issymbol) () <br>_Check if the Value is symbol._  |
|  bool | [**isUndefined**](classjac_1_1ValueWrapper.md#function-isundefined) () <br>_Check if the Value is undefined._  |
|  std::pair&lt; [**ContextRef**](classjac_1_1ContextRef.md), JSValue &gt; | [**loot**](classjac_1_1ValueWrapper.md#function-loot) () <br>_Release ownership of the JSValue. The JSValue will have to be freed manually._  |
|   | [**operator ValueWeak**](classjac_1_1ValueWrapper.md#function-operator-valueweak) () <br> |
|  [**ValueWrapper**](classjac_1_1ValueWrapper.md) & | [**operator=**](classjac_1_1ValueWrapper.md#function-operator) (const [**ValueWrapper**](classjac_1_1ValueWrapper.md) & other) <br> |
|  [**ValueWrapper**](classjac_1_1ValueWrapper.md) & | [**operator=**](classjac_1_1ValueWrapper.md#function-operator_1) ([**ValueWrapper**](classjac_1_1ValueWrapper.md) && other) <br> |
|  [**Value**](classjac_1_1ValueWrapper.md) | [**stringify**](classjac_1_1ValueWrapper.md#function-stringify) (int indent=0) <br>_Convert the Value to a JSON representation._  |
|  T | [**to**](classjac_1_1ValueWrapper.md#function-to) () <br>_Convert the Value to a specified type._  |
|  [**StringView**](classjac_1_1StringView.md) | [**toString**](classjac_1_1ValueWrapper.md#function-tostring) () <br>_Convert the Value to a_ [_**StringView**_](classjac_1_1StringView.md) _._ |
|   | [**~ValueWrapper**](classjac_1_1ValueWrapper.md#function-valuewrapper) () <br> |


## Public Static Functions

| Type | Name |
| ---: | :--- |
|  [**Exception**](classjac_1_1ExceptionWrapper.md) | [**create**](#function-create) (Type type, std::string message) <br>_Create a new Exception._  |


## Public Static Functions inherited from jac::ValueWrapper

See [jac::ValueWrapper](classjac_1_1ValueWrapper.md)

| Type | Name |
| ---: | :--- |
|  [**Value**](classjac_1_1ValueWrapper.md) | [**from**](classjac_1_1ValueWrapper.md#function-from) ([**ContextRef**](classjac_1_1ContextRef.md) ctx, T val) <br>_Create a new Value by converting a given value._  |
|  [**Value**](classjac_1_1ValueWrapper.md) | [**fromJSON**](classjac_1_1ValueWrapper.md#function-fromjson) ([**ContextRef**](classjac_1_1ContextRef.md) ctx, std::string json, std::string filename="&lt;json&gt;", bool extended=false) <br>_Create a new Value from a given JSON string._  |
|  [**Value**](classjac_1_1ValueWrapper.md) | [**null**](classjac_1_1ValueWrapper.md#function-null) ([**ContextRef**](classjac_1_1ContextRef.md) ctx) <br>_Create a new Value containing null._  |
|  [**Value**](classjac_1_1ValueWrapper.md) | [**undefined**](classjac_1_1ValueWrapper.md#function-undefined) ([**ContextRef**](classjac_1_1ContextRef.md) ctx) <br>_Create a new Value containing undefined._  |












## Protected Attributes inherited from jac::ValueWrapper

See [jac::ValueWrapper](classjac_1_1ValueWrapper.md)

| Type | Name |
| ---: | :--- |
|  [**ContextRef**](classjac_1_1ContextRef.md) | [**\_ctx**](classjac_1_1ValueWrapper.md#variable-_ctx)  <br> |
|  JSValue | [**\_val**](classjac_1_1ValueWrapper.md#variable-_val)  <br> |






































## Detailed Description




**Template parameters:**


* `managed` whether the JSValue should be freed when the wrapper is destroyed. 




    
## Public Types Documentation




### enum Type 

```C++
enum jac::ExceptionWrapper::Type {
    Any,
    Error,
    SyntaxError,
    TypeError,
    ReferenceError,
    RangeError,
    InternalError
};
```




<hr>
## Public Functions Documentation




### function ExceptionWrapper [2/3]

_Wrap an existing JSValue. If managed is true, JSValue will be freed when the Exception is destroyed._ 
```C++
inline jac::ExceptionWrapper::ExceptionWrapper (
    ValueWrapper < managed > value
) 
```





**Note:**

Used internally when directly working with QuickJS API. New Exception should be created using [**Exception::create()**](classjac_1_1ExceptionWrapper.md#function-create) or by converting an existing Value to an Exception using [**Value::to&lt;Exception&gt;()**](classjac_1_1ValueWrapper.md#function-to).




**Parameters:**


* `ctx` context to work in 
* `val` JSValue to wrap 




        

<hr>



### function ExceptionWrapper [3/3]

```C++
inline jac::ExceptionWrapper::ExceptionWrapper (
    ContextRef ctx,
    JSValue val
) 
```




<hr>



### function stackTrace 

_Get the exception stack trace._ 
```C++
std::string jac::ExceptionWrapper::stackTrace () noexcept
```





**Returns:**

std::string containing the stack trace 





        

<hr>



### function throwJS 

_Throw the exception into JS._ 
```C++
JSValue jac::ExceptionWrapper::throwJS (
    ContextRef ctx
) 
```





**Note:**

Used internally when directly working with QuickJS API. In most cases, exceptions should be thrown using a throw statement and wrapper functions will propagate the exception to JS. 




**Parameters:**


* `ctx` context to throw the exception in



**Returns:**

JSValue containing the exception 





        

<hr>



### function what 

_Get the exception message._ 
```C++
inline const char * jac::ExceptionWrapper::what () noexcept override const
```





**Returns:**

std::string containing the exception message 





        

<hr>
## Public Static Functions Documentation




### function create 

_Create a new Exception._ 
```C++
static inline Exception jac::ExceptionWrapper::create (
    Type type,
    std::string message
) 
```





**Parameters:**


* `type` type of the exception 
* `message` exception message 



**Returns:**

The resulting Exception 





        

<hr>

------------------------------
The documentation for this class was generated from the following file `src/jac/machine/values.h`

