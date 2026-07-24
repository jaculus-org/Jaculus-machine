

# Class jac::ValueWrapper

**template &lt;bool managed&gt;**



[**ClassList**](annotated.md) **>** [**jac**](namespacejac.md) **>** [**ValueWrapper**](classjac_1_1ValueWrapper.md)



_A wrapper around JSValue with RAII._ [More...](#detailed-description)

* `#include <values.h>`





Inherited by the following classes: [jac::ExceptionWrapper](classjac_1_1ExceptionWrapper.md),  [jac::ObjectWrapper](classjac_1_1ObjectWrapper.md)
































## Public Functions

| Type | Name |
| ---: | :--- |
|   | [**ValueWrapper**](#function-valuewrapper-13) ([**ContextRef**](classjac_1_1ContextRef.md) ctx, JSValue val) <br>_Wrap an existing JSValue. If managed is true, JSValue will be freed when the Value is destroyed._  |
|   | [**ValueWrapper**](#function-valuewrapper-23) (const [**ValueWrapper**](classjac_1_1ValueWrapper.md) & other) <br> |
|   | [**ValueWrapper**](#function-valuewrapper-33) ([**ValueWrapper**](classjac_1_1ValueWrapper.md) && other) <br> |
|  JSValue & | [**getVal**](#function-getval) () <br>_Get reference to the underlying JSValue._  |
|  bool | [**isArray**](#function-isarray) () <br>_Check if the Value is an array._  |
|  bool | [**isBigInt**](#function-isbigint) () <br>_Check if the Value is BigInt._  |
|  bool | [**isBoolean**](#function-isboolean) () <br>_Check if the Value is boolean._  |
|  bool | [**isFunction**](#function-isfunction) () <br>_Check if the Value is a function._  |
|  bool | [**isInstanceOf**](#function-isinstanceof) ([**ObjectWeak**](classjac_1_1ObjectWrapper.md) obj) <br>_Check if the Value is an instance of the given object._  |
|  bool | [**isNull**](#function-isnull) () <br>_Check if the Value is null._  |
|  bool | [**isNumber**](#function-isnumber) () <br>_Check if the Value is number._  |
|  bool | [**isObject**](#function-isobject) () <br>_Check if the Value is an object._  |
|  bool | [**isString**](#function-isstring) () <br>_Check if the Value is string._  |
|  bool | [**isSymbol**](#function-issymbol) () <br>_Check if the Value is symbol._  |
|  bool | [**isUndefined**](#function-isundefined) () <br>_Check if the Value is undefined._  |
|  std::pair&lt; [**ContextRef**](classjac_1_1ContextRef.md), JSValue &gt; | [**loot**](#function-loot) () <br>_Release ownership of the JSValue. The JSValue will have to be freed manually._  |
|   | [**operator ValueWeak**](#function-operator-valueweak) () <br> |
|  [**ValueWrapper**](classjac_1_1ValueWrapper.md) & | [**operator=**](#function-operator) (const [**ValueWrapper**](classjac_1_1ValueWrapper.md) & other) <br> |
|  [**ValueWrapper**](classjac_1_1ValueWrapper.md) & | [**operator=**](#function-operator_1) ([**ValueWrapper**](classjac_1_1ValueWrapper.md) && other) <br> |
|  [**Value**](classjac_1_1ValueWrapper.md) | [**stringify**](#function-stringify) (int indent=0) <br>_Convert the Value to a JSON representation._  |
|  T | [**to**](#function-to) () <br>_Convert the Value to a specified type._  |
|  [**StringView**](classjac_1_1StringView.md) | [**toString**](#function-tostring) () <br>_Convert the Value to a_ [_**StringView**_](classjac_1_1StringView.md) _._ |
|   | [**~ValueWrapper**](#function-valuewrapper) () <br> |


## Public Static Functions

| Type | Name |
| ---: | :--- |
|  [**Value**](classjac_1_1ValueWrapper.md) | [**from**](#function-from) ([**ContextRef**](classjac_1_1ContextRef.md) ctx, T val) <br>_Create a new Value by converting a given value._  |
|  [**Value**](classjac_1_1ValueWrapper.md) | [**fromJSON**](#function-fromjson) ([**ContextRef**](classjac_1_1ContextRef.md) ctx, std::string json, std::string filename="&lt;json&gt;", bool extended=false) <br>_Create a new Value from a given JSON string._  |
|  [**Value**](classjac_1_1ValueWrapper.md) | [**null**](#function-null) ([**ContextRef**](classjac_1_1ContextRef.md) ctx) <br>_Create a new Value containing null._  |
|  [**Value**](classjac_1_1ValueWrapper.md) | [**undefined**](#function-undefined) ([**ContextRef**](classjac_1_1ContextRef.md) ctx) <br>_Create a new Value containing undefined._  |






## Protected Attributes

| Type | Name |
| ---: | :--- |
|  [**ContextRef**](classjac_1_1ContextRef.md) | [**\_ctx**](#variable-_ctx)  <br> |
|  JSValue | [**\_val**](#variable-_val)  <br> |




















## Detailed Description




**Template parameters:**


* `managed` whether the JSValue should be freed when the wrapper is destroyed. 




    
## Public Functions Documentation




### function ValueWrapper [1/3]

_Wrap an existing JSValue. If managed is true, JSValue will be freed when the Value is destroyed._ 
```C++
jac::ValueWrapper::ValueWrapper (
    ContextRef ctx,
    JSValue val
) 
```





**Note:**

Used internally when directly working with QuickJS API. New Value should be created using [**Value::from&lt;T&gt;()**](classjac_1_1ValueWrapper.md#function-from), [**Value::undefined()**](classjac_1_1ValueWrapper.md#function-undefined), etc.




**Parameters:**


* `ctx` context to work in 
* `val` JSValue to wrap 




        

<hr>



### function ValueWrapper [2/3]

```C++
inline jac::ValueWrapper::ValueWrapper (
    const ValueWrapper & other
) 
```




<hr>



### function ValueWrapper [3/3]

```C++
inline jac::ValueWrapper::ValueWrapper (
    ValueWrapper && other
) 
```




<hr>



### function getVal 

_Get reference to the underlying JSValue._ 
```C++
inline JSValue & jac::ValueWrapper::getVal () 
```





**Returns:**

JSValue reference 





        

<hr>



### function isArray 

_Check if the Value is an array._ 
```C++
inline bool jac::ValueWrapper::isArray () 
```





**Returns:**

true if the Value is an array, false otherwise 





        

<hr>



### function isBigInt 

_Check if the Value is BigInt._ 
```C++
inline bool jac::ValueWrapper::isBigInt () 
```





**Returns:**

true if the Value is BigInt, false otherwise 





        

<hr>



### function isBoolean 

_Check if the Value is boolean._ 
```C++
inline bool jac::ValueWrapper::isBoolean () 
```





**Returns:**

true if the Value is boolean, false otherwise 





        

<hr>



### function isFunction 

_Check if the Value is a function._ 
```C++
inline bool jac::ValueWrapper::isFunction () 
```





**Returns:**

true if the Value is a function, false otherwise 





        

<hr>



### function isInstanceOf 

_Check if the Value is an instance of the given object._ 
```C++
bool jac::ValueWrapper::isInstanceOf (
    ObjectWeak obj
) 
```





**Parameters:**


* `obj` object to check against



**Returns:**

true if the Value is an instance of the given object, false otherwise 





        

<hr>



### function isNull 

_Check if the Value is null._ 
```C++
inline bool jac::ValueWrapper::isNull () 
```





**Returns:**

true if the Value is null, false otherwise 





        

<hr>



### function isNumber 

_Check if the Value is number._ 
```C++
inline bool jac::ValueWrapper::isNumber () 
```





**Returns:**

true if the Value is number, false otherwise 





        

<hr>



### function isObject 

_Check if the Value is an object._ 
```C++
inline bool jac::ValueWrapper::isObject () 
```





**Returns:**

true if the Value is an object, false otherwise 





        

<hr>



### function isString 

_Check if the Value is string._ 
```C++
inline bool jac::ValueWrapper::isString () 
```





**Returns:**

true if the Value is string, false otherwise 





        

<hr>



### function isSymbol 

_Check if the Value is symbol._ 
```C++
inline bool jac::ValueWrapper::isSymbol () 
```





**Returns:**

true if the Value is symbol, false otherwise 





        

<hr>



### function isUndefined 

_Check if the Value is undefined._ 
```C++
inline bool jac::ValueWrapper::isUndefined () 
```





**Returns:**

true if the Value is undefined, false otherwise 





        

<hr>



### function loot 

_Release ownership of the JSValue. The JSValue will have to be freed manually._ 
```C++
inline std::pair< ContextRef , JSValue > jac::ValueWrapper::loot () 
```





**Note:**

After this call, the Value is in an undefined state.




**Returns:**

Pair of [**ContextRef**](classjac_1_1ContextRef.md) and JSValue 





        

<hr>



### function operator ValueWeak 

```C++
inline jac::ValueWrapper::operator ValueWeak () 
```




<hr>



### function operator= 

```C++
inline ValueWrapper & jac::ValueWrapper::operator= (
    const ValueWrapper & other
) 
```




<hr>



### function operator= 

```C++
inline ValueWrapper & jac::ValueWrapper::operator= (
    ValueWrapper && other
) 
```




<hr>



### function stringify 

_Convert the Value to a JSON representation._ 
```C++
inline Value jac::ValueWrapper::stringify (
    int indent=0
) 
```





**Parameters:**


* `indent` indentation level 



**Returns:**

Value containing the JSON representation 





        

<hr>



### function to 

_Convert the Value to a specified type._ 
```C++
template<typename T>
inline T jac::ValueWrapper::to () 
```





**Template parameters:**


* `T` Type to convert to 



**Returns:**

The converted value 





        

<hr>



### function toString 

_Convert the Value to a_ [_**StringView**_](classjac_1_1StringView.md) _._
```C++
inline StringView jac::ValueWrapper::toString () 
```





**Returns:**

The [**StringView**](classjac_1_1StringView.md) 





        

<hr>



### function ~ValueWrapper 

```C++
inline jac::ValueWrapper::~ValueWrapper () 
```




<hr>
## Public Static Functions Documentation




### function from 

_Create a new Value by converting a given value._ 
```C++
template<typename T>
static inline Value jac::ValueWrapper::from (
    ContextRef ctx,
    T val
) 
```





**Template parameters:**


* `T` Type of the value 



**Parameters:**


* `ctx` context to work in 
* `val` value to convert 



**Returns:**

The resulting Value 





        

<hr>



### function fromJSON 

_Create a new Value from a given JSON string._ 
```C++
static inline Value jac::ValueWrapper::fromJSON (
    ContextRef ctx,
    std::string json,
    std::string filename="<json>",
    bool extended=false
) 
```





**Parameters:**


* `ctx` context to work in 
* `json` JSON string 
* `filename` filename to use as the source of the JSON 
* `extended` whether to use extended JSON 



**Returns:**

The resulting Value 





        

<hr>



### function null 

_Create a new Value containing null._ 
```C++
static inline Value jac::ValueWrapper::null (
    ContextRef ctx
) 
```





**Parameters:**


* `ctx` context to work in 



**Returns:**

The resulting Value 





        

<hr>



### function undefined 

_Create a new Value containing undefined._ 
```C++
static inline Value jac::ValueWrapper::undefined (
    ContextRef ctx
) 
```





**Parameters:**


* `ctx` context to work in 



**Returns:**

The resulting Value 





        

<hr>
## Protected Attributes Documentation




### variable \_ctx 

```C++
ContextRef jac::ValueWrapper< managed >::_ctx;
```




<hr>



### variable \_val 

```C++
JSValue jac::ValueWrapper< managed >::_val;
```




<hr>## Friends Documentation





### friend operator&lt;&lt; 

```C++
inline std::ostream & jac::ValueWrapper::operator<< (
    std::ostream & os,
    ValueWrapper & val
) 
```




<hr>

------------------------------
The documentation for this class was generated from the following file `src/jac/machine/values.h`

