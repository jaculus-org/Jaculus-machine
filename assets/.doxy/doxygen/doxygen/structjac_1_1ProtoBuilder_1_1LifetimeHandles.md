

# Struct jac::ProtoBuilder::LifetimeHandles



[**ClassList**](annotated.md) **>** [**jac**](namespacejac.md) **>** [**ProtoBuilder**](namespacejac_1_1ProtoBuilder.md) **>** [**LifetimeHandles**](structjac_1_1ProtoBuilder_1_1LifetimeHandles.md)



_A base class used to add handles for lifetime events of an instance._ [More...](#detailed-description)

* `#include <class.h>`







































## Public Static Functions

| Type | Name |
| ---: | :--- |
|  void | [**postConstruction**](#function-postconstruction) ([**ContextRef**](classjac_1_1ContextRef.md) ctx, [**Object**](classjac_1_1ObjectWrapper.md) thisVal, [**ValueVectorWeak**](classjac_1_1ValueVectorWeak.md) args) <br> |


























## Detailed Description


The functions `postConstruction` can be overriden to provide custom handling of the class instance after it's constructed. 


    
## Public Static Functions Documentation




### function postConstruction 

```C++
static inline void jac::ProtoBuilder::LifetimeHandles::postConstruction (
    ContextRef ctx,
    Object thisVal,
    ValueVectorWeak args
) 
```




<hr>

------------------------------
The documentation for this class was generated from the following file `src/jac/machine/class.h`

