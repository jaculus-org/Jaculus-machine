

# Class jac::ContextRef



[**ClassList**](annotated.md) **>** [**jac**](namespacejac.md) **>** [**ContextRef**](classjac_1_1ContextRef.md)



_A wrapper around JSContext\* providing some related functionality._ 

* `#include <context.h>`





































## Public Functions

| Type | Name |
| ---: | :--- |
|   | [**ContextRef**](#function-contextref) (JSContext \* ctx) <br> |
|  JSContext \* | [**get**](#function-get) () <br>_Get the underlying JSContext\*._  |
|  [**Exception**](classjac_1_1ExceptionWrapper.md) | [**getException**](#function-getexception) () <br>_Get pending exception thrown in this context._  |
|  [**Object**](classjac_1_1ObjectWrapper.md) | [**getGlobalObject**](#function-getglobalobject) () <br>_Get the global object of this context._  |
|   | [**operator JSContext \***](#function-operator-jscontext-*-12) () <br> |
|   | [**operator JSContext \***](#function-operator-jscontext-*-22) () const<br> |
|   | [**operator bool**](#function-operator-bool) () <br> |
|  [**ContextRef**](classjac_1_1ContextRef.md) & | [**operator=**](#function-operator) (JSContext \* ctx) <br> |




























## Public Functions Documentation




### function ContextRef 

```C++
inline jac::ContextRef::ContextRef (
    JSContext * ctx
) 
```




<hr>



### function get 

_Get the underlying JSContext\*._ 
```C++
inline JSContext * jac::ContextRef::get () 
```





**Returns:**

The JSContext\* 





        

<hr>



### function getException 

_Get pending exception thrown in this context._ 
```C++
Exception jac::ContextRef::getException () 
```





**Note:**

If there is no pending exception, new Exception will be thrown




**Returns:**

The Exception 





        

<hr>



### function getGlobalObject 

_Get the global object of this context._ 
```C++
Object jac::ContextRef::getGlobalObject () 
```





**Returns:**

The global object 





        

<hr>



### function operator JSContext \* [1/2]

```C++
inline jac::ContextRef::operator JSContext * () 
```




<hr>



### function operator JSContext \* [2/2]

```C++
inline jac::ContextRef::operator JSContext * () const
```




<hr>



### function operator bool 

```C++
inline jac::ContextRef::operator bool () 
```




<hr>



### function operator= 

```C++
inline ContextRef & jac::ContextRef::operator= (
    JSContext * ctx
) 
```




<hr>

------------------------------
The documentation for this class was generated from the following file `src/jac/machine/context.h`

