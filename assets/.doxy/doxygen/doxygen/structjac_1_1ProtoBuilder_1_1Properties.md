

# Struct jac::ProtoBuilder::Properties



[**ClassList**](annotated.md) **>** [**jac**](namespacejac.md) **>** [**ProtoBuilder**](namespacejac_1_1ProtoBuilder.md) **>** [**Properties**](structjac_1_1ProtoBuilder_1_1Properties.md)



_A base class for javascript classes with added properties._ [More...](#detailed-description)

* `#include <class.h>`





Inherited by the following classes: [jac::FileProtoBuilder](structjac_1_1FileProtoBuilder.md),  [jac::ReadableProtoBuilder](structjac_1_1ReadableProtoBuilder.md),  [jac::WritableProtoBuilder](structjac_1_1WritableProtoBuilder.md)


































## Public Static Functions

| Type | Name |
| ---: | :--- |
|  void | [**addProperties**](#function-addproperties) ([**ContextRef**](classjac_1_1ContextRef.md) ctx, [**Object**](classjac_1_1ObjectWrapper.md) proto) <br>_Add properties to the object prototype._  |


























## Detailed Description


The function `addProperties` can be overriden to specify custom properties for the class prototype. 


    
## Public Static Functions Documentation




### function addProperties 

_Add properties to the object prototype._ 
```C++
static inline void jac::ProtoBuilder::Properties::addProperties (
    ContextRef ctx,
    Object proto
) 
```





**Note:**

This function is only called when the class prototype is created




**Parameters:**


* `ctx` context to work in 
* `proto` the prototype of the class 




        

<hr>

------------------------------
The documentation for this class was generated from the following file `src/jac/machine/class.h`

