

# Struct jac::ProtoBuilder::Opaque

**template &lt;[**typename**](structjac_1_1ProtoBuilder_1_1Opaque.md#function-addmethodmember) [**T**](structjac_1_1ProtoBuilder_1_1Opaque.md#function-addmethodmember)&gt;**



[**ClassList**](annotated.md) **>** [**jac**](namespacejac.md) **>** [**ProtoBuilder**](namespacejac_1_1ProtoBuilder.md) **>** [**Opaque**](structjac_1_1ProtoBuilder_1_1Opaque.md)



_A base class for javascript classes with opaque data._ [More...](#detailed-description)

* `#include <class.h>`

















## Public Types

| Type | Name |
| ---: | :--- |
| typedef [**T**](structjac_1_1ProtoBuilder_1_1Opaque.md#function-addmethodmember) | [**OpaqueType**](#typedef-opaquetype)  <br> |






## Public Static Attributes

| Type | Name |
| ---: | :--- |
|  [**JSClassID**](structjac_1_1ProtoBuilder_1_1Opaque.md#function-addmethodmember) | [**classId**](#variable-classid)  <br> |
















## Public Static Functions

| Type | Name |
| ---: | :--- |
|  void | [**addMethodMember**](#function-addmethodmember) ([**ContextRef**](classjac_1_1ContextRef.md) ctx, [**Object**](classjac_1_1ObjectWrapper.md) proto, std::string name, PropFlags flags=PropFlags::Default) <br>_Add a property to the object prototype from a member function of the wrapped class._  |
|  [**void**](structjac_1_1ProtoBuilder_1_1Opaque.md#function-addmethodmember) | [**addPropMember**](#function-addpropmember) ([**ContextRef**](classjac_1_1ContextRef.md) ctx, [**Object**](classjac_1_1ObjectWrapper.md) proto, std::string name, PropFlags flags=PropFlags::Default) <br>_Add a property to the object prototype from a member variable of the wrapped class._  |
|  [**Value**](classjac_1_1ValueWrapper.md) | [**callMember**](#function-callmember) ([**ContextRef**](classjac_1_1ContextRef.md) ctx, [**ValueWeak**](classjac_1_1ValueWrapper.md) funcObj, [**ValueWeak**](classjac_1_1ValueWrapper.md) thisVal, std::vector&lt; [**ValueWeak**](classjac_1_1ValueWrapper.md) &gt; argv) <br>_Process a call to a member function of the wrapped class._  |
|  [**T**](structjac_1_1ProtoBuilder_1_1Opaque.md#function-addmethodmember) \* | [**constructOpaque**](#function-constructopaque) ([**ContextRef**](classjac_1_1ContextRef.md), std::vector&lt; [**ValueWeak**](classjac_1_1ValueWrapper.md) &gt;) <br>_Construct a new_ [_**Opaque**_](structjac_1_1ProtoBuilder_1_1Opaque.md) _object from javascript arguments._ |
|  [**void**](structjac_1_1ProtoBuilder_1_1Opaque.md#function-addmethodmember) | [**destroyOpaque**](#function-destroyopaque) ([**JSRuntime**](structjac_1_1ProtoBuilder_1_1Opaque.md#function-addmethodmember) \*, [**T**](structjac_1_1ProtoBuilder_1_1Opaque.md#function-addmethodmember) \* ptr) noexcept<br>_Destroy the_ [_**Opaque**_](structjac_1_1ProtoBuilder_1_1Opaque.md) _object._ |
|  [**T**](structjac_1_1ProtoBuilder_1_1Opaque.md#function-addmethodmember) \* | [**getOpaque**](#function-getopaque) ([**ContextRef**](classjac_1_1ContextRef.md), [**ValueWeak**](classjac_1_1ValueWrapper.md) thisVal) <br>_Get the_ [_**Opaque**_](structjac_1_1ProtoBuilder_1_1Opaque.md) _object from an instance of the class._ |


























## Detailed Description


The functions `constructOpaque` and `destroyOpaque` can be overriden to provide custom construction and destruction of the opaque data.




**Template parameters:**


* `T` The type of the opaque data 




    
## Public Types Documentation




### typedef OpaqueType 

```C++
using jac::ProtoBuilder::Opaque< T >::OpaqueType =  T;
```




<hr>
## Public Static Attributes Documentation




### variable classId 

```C++
JSClassID jac::ProtoBuilder::Opaque< T >::classId;
```




<hr>
## Public Static Functions Documentation




### function addMethodMember 

_Add a property to the object prototype from a member function of the wrapped class._ 
```C++
template<typename  Sgn, Sgn member>
static inline void jac::ProtoBuilder::Opaque::addMethodMember (
    ContextRef ctx,
    Object proto,
    std::string name,
    PropFlags flags=PropFlags::Default
) 
```





**Template parameters:**


* `Sgn` signature of the member function 
* `member` pointer to the member function 



**Parameters:**


* `ctx` context to work in 
* `proto` the prototype of the class 
* `name` name of the property 
* `flags` flags of the property 




        

<hr>



### function addPropMember 

_Add a property to the object prototype from a member variable of the wrapped class._ 
```C++
template<typename  U, UT::* member>
static inline void jac::ProtoBuilder::Opaque::addPropMember (
    ContextRef ctx,
    Object proto,
    std::string name,
    PropFlags flags=PropFlags::Default
) 
```





**Template parameters:**


* `U` the type of the member variable 
* `U(T::*member)` pointer to the member variable 



**Parameters:**


* `ctx` context to work in 
* `proto` the prototype of the class 
* `name` name of the property 
* `flags` flags of the property 




        

<hr>



### function callMember 

_Process a call to a member function of the wrapped class._ 
```C++
template<typename  Sgn, Sgn member>
static inline Value jac::ProtoBuilder::Opaque::callMember (
    ContextRef ctx,
    ValueWeak funcObj,
    ValueWeak thisVal,
    std::vector< ValueWeak > argv
) 
```





**Note:**

The arguments and return value are automatically converted to and from javascript values




**Template parameters:**


* `Sgn` the signature of the member function 
* `member` pointer to the member function 



**Parameters:**


* `ctx` context to work in 
* `funcObj` instance of the class 
* `thisVal` value of `this` in the function 
* `argv` arguments passed to the function 



**Returns:**

Result of the call 





        

<hr>



### function constructOpaque 

_Construct a new_ [_**Opaque**_](structjac_1_1ProtoBuilder_1_1Opaque.md) _object from javascript arguments._
```C++
static inline T * jac::ProtoBuilder::Opaque::constructOpaque (
    ContextRef,
    std::vector< ValueWeak >
) 
```





**Note:**

This function is only called upon javascript class instantiation




**Parameters:**


* `ctx` context to work in 
* `args` arguments passed to the constructor 



**Returns:**

A pointer to the opaque data 





        

<hr>



### function destroyOpaque 

_Destroy the_ [_**Opaque**_](structjac_1_1ProtoBuilder_1_1Opaque.md) _object._
```C++
static inline void jac::ProtoBuilder::Opaque::destroyOpaque (
    JSRuntime *,
    T * ptr
) noexcept
```





**Note:**

This function is only called when the javascript object is garbage collected




**Parameters:**


* `rt` runtime to work in 
* `ptr` pointer to the opaque data 




        

<hr>



### function getOpaque 

_Get the_ [_**Opaque**_](structjac_1_1ProtoBuilder_1_1Opaque.md) _object from an instance of the class._
```C++
static inline T * jac::ProtoBuilder::Opaque::getOpaque (
    ContextRef,
    ValueWeak thisVal
) 
```





**Parameters:**


* `ctx` context to work in 
* `thisVal` the instance of the class 



**Returns:**

A pointer to the opaque data 





        

<hr>

------------------------------
The documentation for this class was generated from the following file `src/jac/machine/class.h`

