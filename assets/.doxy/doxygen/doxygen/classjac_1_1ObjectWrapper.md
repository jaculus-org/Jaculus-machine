

# Class jac::ObjectWrapper

**template &lt;bool managed&gt;**



[**ClassList**](annotated.md) **>** [**jac**](namespacejac.md) **>** [**ObjectWrapper**](classjac_1_1ObjectWrapper.md)



_A wrapper for JSValue with Object type with RAII._ [More...](#detailed-description)

* `#include <values.h>`



Inherits the following classes: [jac::ValueWrapper](classjac_1_1ValueWrapper.md)


Inherited by the following classes: [jac::ArrayBufferWrapper](classjac_1_1ArrayBufferWrapper.md),  [jac::ArrayWrapper](classjac_1_1ArrayWrapper.md),  [jac::FunctionWrapper](classjac_1_1FunctionWrapper.md),  [jac::PromiseWrapper](classjac_1_1PromiseWrapper.md)




















































## Public Functions

| Type | Name |
| ---: | :--- |
|   | [**ObjectWrapper**](#function-objectwrapper-12) ([**ValueWrapper**](classjac_1_1ValueWrapper.md)&lt; managed &gt; value) <br>_Wrap an existing JSValue. If managed is true, JSValue will be freed when the Object is destroyed._  |
|   | [**ObjectWrapper**](#function-objectwrapper-22) ([**ContextRef**](classjac_1_1ContextRef.md) ctx, JSValue val) <br> |
|  void | [**defineProperty**](#function-defineproperty) (Id id, [**Value**](classjac_1_1ValueWrapper.md) value, PropFlags flags=PropFlags::Default) <br>_Define a property of the object._  |
|  void | [**deleteProperty**](#function-deleteproperty) (Id id) <br>_Delete a property of the object._  |
|  T | [**get**](#function-get-13) ([**Atom**](classjac_1_1Atom.md) prop) <br>_Get a property of the object._  |
|  T | [**get**](#function-get-23) (const std::string & name) <br> |
|  T | [**get**](#function-get-33) (uint32\_t idx) <br> |
|  std::vector&lt; [**Atom**](classjac_1_1Atom.md) &gt; | [**getOwnPropertyNames**](#function-getownpropertynames) (int flags=JS\_GPN\_STRING\_MASK\|JS\_GPN\_ENUM\_ONLY) <br>_Get own property names of the object._  |
|  [**Object**](classjac_1_1ObjectWrapper.md) | [**getPrototype**](#function-getprototype) () <br>_Get the prototype of the object._  |
|  bool | [**hasProperty**](#function-hasproperty) (Id id) <br>_Check if the object has a property._  |
|  Res | [**invoke**](#function-invoke-13) ([**Atom**](classjac_1_1Atom.md) key, Args... args) <br>_Invoke a method of the object._  |
|  Res | [**invoke**](#function-invoke-23) (const std::string & key, Args... args) <br> |
|  Res | [**invoke**](#function-invoke-33) (uint32\_t idx, Args... args) <br> |
|  void | [**set**](#function-set-13) ([**Atom**](classjac_1_1Atom.md) prop, T val) <br>_Set a property of the object._  |
|  void | [**set**](#function-set-23) (const std::string & name, T val) <br> |
|  void | [**set**](#function-set-33) (uint32\_t idx, T val) <br> |
|  void | [**setPrototype**](#function-setprototype) ([**Object**](classjac_1_1ObjectWrapper.md) proto) <br>_Set the prototype of the object._  |


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
|  [**Object**](classjac_1_1ObjectWrapper.md) | [**create**](#function-create) ([**ContextRef**](classjac_1_1ContextRef.md) ctx) <br>_Create a new empty object._  |


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




    
## Public Functions Documentation




### function ObjectWrapper [1/2]

_Wrap an existing JSValue. If managed is true, JSValue will be freed when the Object is destroyed._ 
```C++
inline jac::ObjectWrapper::ObjectWrapper (
    ValueWrapper < managed > value
) 
```





**Note:**

Used internally when directly working with QuickJS API. New Object should be created using [**Object::create()**](classjac_1_1ObjectWrapper.md#function-create) or by converting an existing Value to an Object using [**Value::to&lt;Object&gt;()**](classjac_1_1ValueWrapper.md#function-to).




**Parameters:**


* `ctx` context to work in 
* `val` JSValue to wrap 




        

<hr>



### function ObjectWrapper [2/2]

```C++
inline jac::ObjectWrapper::ObjectWrapper (
    ContextRef ctx,
    JSValue val
) 
```




<hr>



### function defineProperty 

_Define a property of the object._ 
```C++
template<typename Id>
inline void jac::ObjectWrapper::defineProperty (
    Id id,
    Value value,
    PropFlags flags=PropFlags::Default
) 
```





**Template parameters:**


* `Id` the type of the property identifier ([**Atom**](classjac_1_1Atom.md), std::string, uint32\_t) 



**Parameters:**


* `id` the property identifier 
* `value` the value to set 
* `flags` the property flags 




        

<hr>



### function deleteProperty 

_Delete a property of the object._ 
```C++
template<typename Id>
inline void jac::ObjectWrapper::deleteProperty (
    Id id
) 
```





**Template parameters:**


* `Id` the type of the property identifier ([**Atom**](classjac_1_1Atom.md), std::string, uint32\_t) 



**Parameters:**


* `id` the property identifier 




        

<hr>



### function get [1/3]

_Get a property of the object._ 
```C++
template<typename T>
inline T jac::ObjectWrapper::get (
    Atom prop
) 
```





**Template parameters:**


* `T` type to convert the property to 



**Parameters:**


* `prop` the property identifier 



**Returns:**

The resulting value 





        

<hr>



### function get [2/3]

```C++
template<typename T>
inline T jac::ObjectWrapper::get (
    const std::string & name
) 
```




<hr>



### function get [3/3]

```C++
template<typename T>
inline T jac::ObjectWrapper::get (
    uint32_t idx
) 
```




<hr>



### function getOwnPropertyNames 

_Get own property names of the object._ 
```C++
inline std::vector< Atom > jac::ObjectWrapper::getOwnPropertyNames (
    int flags=JS_GPN_STRING_MASK|JS_GPN_ENUM_ONLY
) 
```





**Parameters:**


* `flags` flags to filter the properties 



**Returns:**

Vector of property names (atoms) 





        

<hr>



### function getPrototype 

_Get the prototype of the object._ 
```C++
inline Object jac::ObjectWrapper::getPrototype () 
```





**Returns:**

The prototype 





        

<hr>



### function hasProperty 

_Check if the object has a property._ 
```C++
template<typename Id>
inline bool jac::ObjectWrapper::hasProperty (
    Id id
) 
```





**Template parameters:**


* `Id` the type of the property identifier ([**Atom**](classjac_1_1Atom.md), std::string, uint32\_t) 



**Parameters:**


* `id` the property identifier 



**Returns:**

true if the object has the property, false otherwise 





        

<hr>



### function invoke [1/3]

_Invoke a method of the object._ 
```C++
template<typename Res, typename... Args>
Res jac::ObjectWrapper::invoke (
    Atom key,
    Args... args
) 
```





**Note:**

The call will automatically convert the arguments to their JavaScript counterparts, the result will be converted to the specified type and Exceptions thrown in JS will be propagated to C++ as jac::Exception.




**Template parameters:**


* `Res` type to convert the result to 
* `Args` types of the arguments 



**Parameters:**


* `prop` the property identifier 
* `args` the arguments 



**Returns:**

The resulting value 





        

<hr>



### function invoke [2/3]

```C++
template<typename Res, typename... Args>
inline Res jac::ObjectWrapper::invoke (
    const std::string & key,
    Args... args
) 
```




<hr>



### function invoke [3/3]

```C++
template<typename Res, typename... Args>
inline Res jac::ObjectWrapper::invoke (
    uint32_t idx,
    Args... args
) 
```




<hr>



### function set [1/3]

_Set a property of the object._ 
```C++
template<typename T>
inline void jac::ObjectWrapper::set (
    Atom prop,
    T val
) 
```





**Template parameters:**


* `T` type of the value to set 



**Parameters:**


* `prop` the property identifier 
* `val` the value to set 




        

<hr>



### function set [2/3]

```C++
template<typename T>
inline void jac::ObjectWrapper::set (
    const std::string & name,
    T val
) 
```




<hr>



### function set [3/3]

```C++
template<typename T>
inline void jac::ObjectWrapper::set (
    uint32_t idx,
    T val
) 
```




<hr>



### function setPrototype 

_Set the prototype of the object._ 
```C++
inline void jac::ObjectWrapper::setPrototype (
    Object proto
) 
```





**Parameters:**


* `proto` the prototype 




        

<hr>
## Public Static Functions Documentation




### function create 

_Create a new empty object._ 
```C++
static inline Object jac::ObjectWrapper::create (
    ContextRef ctx
) 
```





**Parameters:**


* `ctx` the context to create the object in 



**Returns:**

The new object 





        

<hr>

------------------------------
The documentation for this class was generated from the following file `src/jac/machine/values.h`

