

# Class jac::FunctionFactory



[**ClassList**](annotated.md) **>** [**jac**](namespacejac.md) **>** [**FunctionFactory**](classjac_1_1FunctionFactory.md)



_Various methods for wrapping C++ functions into javascript functions._ [More...](#detailed-description)

* `#include <functionFactory.h>`





































## Public Functions

| Type | Name |
| ---: | :--- |
|   | [**FunctionFactory**](#function-functionfactory) ([**ContextRef**](classjac_1_1ContextRef.md) context) <br> |
|  [**Function**](classjac_1_1FunctionWrapper.md) | [**newFunction**](#function-newfunction) (Func func) <br>_Wraps a C++ function into a javascript function object._  |
|  [**Function**](classjac_1_1FunctionWrapper.md) | [**newFunctionThis**](#function-newfunctionthis) (Func func) <br>_Wraps a C++ function into a javascript function object._  |
|  [**Function**](classjac_1_1FunctionWrapper.md) | [**newFunctionThisVariadic**](#function-newfunctionthisvariadic) (Func func) <br>_Wraps a C++ function into a javascript function object._  |
|  [**Function**](classjac_1_1FunctionWrapper.md) | [**newFunctionVariadic**](#function-newfunctionvariadic) (Func func) <br>_Wraps a C++ function into a javascript function object._  |




























## Detailed Description


About exceptions propagation:


When jac::Exception is thrown, the wrapped value or given error type is thrown. When std::exception is thrown, an InternalError is thrown. When any other exception is thrown, an InternalError is thrown with the message "unknown error". 


    
## Public Functions Documentation




### function FunctionFactory 

```C++
inline jac::FunctionFactory::FunctionFactory (
    ContextRef context
) 
```




<hr>



### function newFunction 

_Wraps a C++ function into a javascript function object._ 
```C++
template<class Func>
inline Function jac::FunctionFactory::newFunction (
    Func func
) 
```



The expected signature of the function object is Res(Args...). Arguments and the result of the function call are automatically converted to and from javascript values. Exceptions thrown within the function are automatically propagated to the javascript side.




**Template parameters:**


* `Func` type of the function to be wrapped 



**Parameters:**


* `func` the function object to be wrapped 



**Returns:**

The created function object 





        

<hr>



### function newFunctionThis 

_Wraps a C++ function into a javascript function object._ 
```C++
template<class Func>
inline Function jac::FunctionFactory::newFunctionThis (
    Func func
) 
```



The expected signature of the function object is Res([**ContextRef**](classjac_1_1ContextRef.md), ValueWeak, Args...). Arguments and the result of the function call are automatically converted to and from javascript values. Exceptions thrown within the function are automatically propagated to the javascript side.




**Template parameters:**


* `Func` type of the function to be wrapped 



**Parameters:**


* `func` the function object to be wrapped 



**Returns:**

The created function object 





        

<hr>



### function newFunctionThisVariadic 

_Wraps a C++ function into a javascript function object._ 
```C++
template<class Func>
inline Function jac::FunctionFactory::newFunctionThisVariadic (
    Func func
) 
```



The expected signature of the function object is Res([**ContextRef**](classjac_1_1ContextRef.md), ValueWeak, std::vector&lt;ValueWeak&gt;). The vector will contain all arguments passed to the function. The the result of the function call is automatically converted from a javascript value. Exceptions thrown within the function are automatically propagated to the javascript side.




**Template parameters:**


* `Func` type of the function to be wrapped 



**Parameters:**


* `func` the function object to be wrapped 



**Returns:**

The created function object 





        

<hr>



### function newFunctionVariadic 

_Wraps a C++ function into a javascript function object._ 
```C++
template<class Func>
inline Function jac::FunctionFactory::newFunctionVariadic (
    Func func
) 
```



The expected signature of the function object is Res(std::vector&lt;ValueWeak&gt;). The vector will contain all arguments passed to the function. The result of the function call is automatically converted from a javascript value. Exceptions thrown within the function are automatically propagated to the javascript side.




**Template parameters:**


* `Func` type of the function to be wrapped 



**Parameters:**


* `func` the function object to be wrapped 



**Returns:**

The created function object 





        

<hr>

------------------------------
The documentation for this class was generated from the following file `src/jac/machine/functionFactory.h`

