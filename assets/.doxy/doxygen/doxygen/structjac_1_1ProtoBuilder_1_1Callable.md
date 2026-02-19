

# Struct jac::ProtoBuilder::Callable



[**ClassList**](annotated.md) **>** [**jac**](namespacejac.md) **>** [**ProtoBuilder**](namespacejac_1_1ProtoBuilder.md) **>** [**Callable**](structjac_1_1ProtoBuilder_1_1Callable.md)



_A base class for javascript classes with callable instances._ [More...](#detailed-description)

* `#include <class.h>`







































## Public Static Functions

| Type | Name |
| ---: | :--- |
|  [**Value**](classjac_1_1ValueWrapper.md) | [**callConstructor**](#function-callconstructor) ([**ContextRef**](classjac_1_1ContextRef.md), [**ValueWeak**](classjac_1_1ValueWrapper.md), [**ValueWeak**](classjac_1_1ValueWrapper.md), std::vector&lt; [**ValueWeak**](classjac_1_1ValueWrapper.md) &gt;) <br>_Process a call to the wrapped class as a constructor._  |
|  [**Value**](classjac_1_1ValueWrapper.md) | [**callFunction**](#function-callfunction) ([**ContextRef**](classjac_1_1ContextRef.md), [**ValueWeak**](classjac_1_1ValueWrapper.md), [**ValueWeak**](classjac_1_1ValueWrapper.md), std::vector&lt; [**ValueWeak**](classjac_1_1ValueWrapper.md) &gt;) <br>_Process a call to the wrapped class._  |


























## Detailed Description


The functions `callFunction` and `callConstructor` can be overriden to provide custom handling of the class instance when it's called as a function or as a constructor (with `new`). 


    
## Public Static Functions Documentation




### function callConstructor 

_Process a call to the wrapped class as a constructor._ 
```C++
static inline Value jac::ProtoBuilder::Callable::callConstructor (
    ContextRef,
    ValueWeak,
    ValueWeak,
    std::vector< ValueWeak >
) 
```





**Parameters:**


* `ctx` context to work in 
* `funcObj` instance of the class 
* `target` value of `new.target` in the function 
* `args` arguments passed to the function 



**Returns:**

Result of the call 





        

<hr>



### function callFunction 

_Process a call to the wrapped class._ 
```C++
static inline Value jac::ProtoBuilder::Callable::callFunction (
    ContextRef,
    ValueWeak,
    ValueWeak,
    std::vector< ValueWeak >
) 
```





**Parameters:**


* `ctx` context to work in 
* `funcObj` instance of the class 
* `thisVal` value of `this` in the function 
* `args` arguments passed to the function 



**Returns:**

result of the call 





        

<hr>

------------------------------
The documentation for this class was generated from the following file `src/jac/machine/class.h`

