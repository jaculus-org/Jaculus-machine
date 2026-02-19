

# Class jac::PromiseWrapper

**template &lt;bool managed&gt;**



[**ClassList**](annotated.md) **>** [**jac**](namespacejac.md) **>** [**PromiseWrapper**](classjac_1_1PromiseWrapper.md)



_A wrapper for JSValue with Promise type with RAII._ [More...](#detailed-description)

* `#include <values.h>`



Inherits the following classes: [jac::ObjectWrapper](classjac_1_1ObjectWrapper.md)










































































## Public Functions

| Type | Name |
| ---: | :--- |
|   | [**PromiseWrapper**](#function-promisewrapper-12) ([**ObjectWrapper**](classjac_1_1ObjectWrapper.md)&lt; managed &gt; value) <br>_Wrap an existing JSValue. If managed is true, JSValue will be freed when the Promise is destroyed._  |
|   | [**PromiseWrapper**](#function-promisewrapper-22) ([**ContextRef**](classjac_1_1ContextRef.md) ctx, JSValue val) <br> |


## Public Functions inherited from jac::ObjectWrapper

See [jac::ObjectWrapper](classjac_1_1ObjectWrapper.md)

| Type | Name |
| ---: | :--- |
|   | [**ObjectWrapper**](classjac_1_1ObjectWrapper.md#function-objectwrapper-12) ([**ValueWrapper**](classjac_1_1ValueWrapper.md)&lt; managed &gt; value) <br>_Wrap an existing JSValue. If managed is true, JSValue will be freed when the Object is destroyed._  |
|   | [**ObjectWrapper**](classjac_1_1ObjectWrapper.md#function-objectwrapper-22) ([**ContextRef**](classjac_1_1ContextRef.md) ctx, JSValue val) <br> |
|  void | [**defineProperty**](classjac_1_1ObjectWrapper.md#function-defineproperty) (Id id, [**Value**](classjac_1_1ValueWrapper.md) value, PropFlags flags=PropFlags::Default) <br>_Define a property of the object._  |
|  void | [**deleteProperty**](classjac_1_1ObjectWrapper.md#function-deleteproperty) (Id id) <br>_Delete a property of the object._  |
|  T | [**get**](classjac_1_1ObjectWrapper.md#function-get-13) ([**Atom**](classjac_1_1Atom.md) prop) <br>_Get a property of the object._  |
|  T | [**get**](classjac_1_1ObjectWrapper.md#function-get-23) (const std::string & name) <br> |
|  T | [**get**](classjac_1_1ObjectWrapper.md#function-get-33) (uint32\_t idx) <br> |
|  std::vector&lt; [**Atom**](classjac_1_1Atom.md) &gt; | [**getOwnPropertyNames**](classjac_1_1ObjectWrapper.md#function-getownpropertynames) (int flags=JS\_GPN\_STRING\_MASK\|JS\_GPN\_ENUM\_ONLY) <br>_Get own property names of the object._  |
|  [**Object**](classjac_1_1ObjectWrapper.md) | [**getPrototype**](classjac_1_1ObjectWrapper.md#function-getprototype) () <br>_Get the prototype of the object._  |
|  bool | [**hasProperty**](classjac_1_1ObjectWrapper.md#function-hasproperty) (Id id) <br>_Check if the object has a property._  |
|  Res | [**invoke**](classjac_1_1ObjectWrapper.md#function-invoke-13) ([**Atom**](classjac_1_1Atom.md) key, Args... args) <br>_Invoke a method of the object._  |
|  Res | [**invoke**](classjac_1_1ObjectWrapper.md#function-invoke-23) (const std::string & key, Args... args) <br> |
|  Res | [**invoke**](classjac_1_1ObjectWrapper.md#function-invoke-33) (uint32\_t idx, Args... args) <br> |
|  void | [**set**](classjac_1_1ObjectWrapper.md#function-set-13) ([**Atom**](classjac_1_1Atom.md) prop, T val) <br>_Set a property of the object._  |
|  void | [**set**](classjac_1_1ObjectWrapper.md#function-set-23) (const std::string & name, T val) <br> |
|  void | [**set**](classjac_1_1ObjectWrapper.md#function-set-33) (uint32\_t idx, T val) <br> |
|  void | [**setPrototype**](classjac_1_1ObjectWrapper.md#function-setprototype) ([**Object**](classjac_1_1ObjectWrapper.md) proto) <br>_Set the prototype of the object._  |


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
|  std::tuple&lt; [**Promise**](classjac_1_1PromiseWrapper.md), [**Function**](classjac_1_1FunctionWrapper.md), [**Function**](classjac_1_1FunctionWrapper.md) &gt; | [**create**](#function-create) ([**ContextRef**](classjac_1_1ContextRef.md) ctx) <br>_Create a new Promise object._  |


## Public Static Functions inherited from jac::ObjectWrapper

See [jac::ObjectWrapper](classjac_1_1ObjectWrapper.md)

| Type | Name |
| ---: | :--- |
|  [**Object**](classjac_1_1ObjectWrapper.md) | [**create**](classjac_1_1ObjectWrapper.md#function-create) ([**ContextRef**](classjac_1_1ContextRef.md) ctx) <br>_Create a new empty object._  |


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




### function PromiseWrapper [1/2]

_Wrap an existing JSValue. If managed is true, JSValue will be freed when the Promise is destroyed._ 
```C++
inline jac::PromiseWrapper::PromiseWrapper (
    ObjectWrapper < managed > value
) 
```





**Note:**

Used internally when directly working with QuickJS API. New Promise should be created using [**Promise::create()**](classjac_1_1PromiseWrapper.md#function-create).




**Parameters:**


* `ctx` context to work in 
* `val` JSValue to wrap 




        

<hr>



### function PromiseWrapper [2/2]

```C++
inline jac::PromiseWrapper::PromiseWrapper (
    ContextRef ctx,
    JSValue val
) 
```




<hr>
## Public Static Functions Documentation




### function create 

_Create a new Promise object._ 
```C++
static inline std::tuple< Promise , Function , Function > jac::PromiseWrapper::create (
    ContextRef ctx
) 
```





**Returns:**

Tuple of the Promise object, resolve function, reject function 





        

<hr>

------------------------------
The documentation for this class was generated from the following file `src/jac/machine/values.h`

