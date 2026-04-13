

# Class jac::Class

**template &lt;class Builder&gt;**



[**ClassList**](annotated.md) **>** [**jac**](namespacejac.md) **>** [**Class**](classjac_1_1Class.md)












































## Public Static Functions

| Type | Name |
| ---: | :--- |
|  std::enable\_if\_t&lt; is\_base\_of\_template\_v&lt; [**ProtoBuilder::Opaque**](structjac_1_1ProtoBuilder_1_1Opaque.md), Bdr &gt; &&std::is\_base\_of\_v&lt; typename Bdr::OpaqueType, T &gt; &&std::is\_same\_v&lt; Bdr, Builder &gt;, [**Value**](classjac_1_1ValueWrapper.md) &gt; | [**createInstance**](#function-createinstance) ([**ContextRef**](classjac_1_1ContextRef.md) ctx, T \* instance) <br>_Create a new instance of this class in given context._  |
|  JSClassID | [**getClassId**](#function-getclassid) () <br>_Get the class id of this class._  |
|  [**Function**](classjac_1_1FunctionWrapper.md) | [**getConstructor**](#function-getconstructor) ([**ContextRef**](classjac_1_1ContextRef.md) ctx) <br>_Get the constructor object of this class in given context._  |
|  [**Object**](classjac_1_1ObjectWrapper.md) | [**getProto**](#function-getproto) ([**ContextRef**](classjac_1_1ContextRef.md) ctx) <br>_Get the prototype object of this class in given context._  |
|  void | [**init**](#function-init) (std::string name, bool isCtor=false) <br>_Initialize the class._  |
|  void | [**initContext**](#function-initcontext) ([**ContextRef**](classjac_1_1ContextRef.md) ctx) <br>_Initialize the class prototype._  |


























## Public Static Functions Documentation




### function createInstance 

_Create a new instance of this class in given context._ 
```C++
template<typename T, typename Bdr>
static inline std::enable_if_t< is_base_of_template_v< ProtoBuilder::Opaque , Bdr > &&std::is_base_of_v< typename Bdr::OpaqueType, T > &&std::is_same_v< Bdr, Builder >, Value > jac::Class::createInstance (
    ContextRef ctx,
    T * instance
) 
```





**Note:**

if the class wasn't initialized in the context, it will be initialized




**Parameters:**


* `ctx` the context 
* `instance` a new-allocated instance to be saved as opaque data 



**Returns:**

The new instance 





        

<hr>



### function getClassId 

_Get the class id of this class._ 
```C++
static inline JSClassID jac::Class::getClassId () 
```





**Returns:**

JSClassID 





        

<hr>



### function getConstructor 

_Get the constructor object of this class in given context._ 
```C++
static inline Function jac::Class::getConstructor (
    ContextRef ctx
) 
```





**Note:**

if the class wasn't initialized in the context, it will be initialized




**Parameters:**


* `ctx` context to work in 



**Returns:**

The constructor object 





        

<hr>



### function getProto 

_Get the prototype object of this class in given context._ 
```C++
static inline Object jac::Class::getProto (
    ContextRef ctx
) 
```





**Note:**

if the class wasn't initialized in the context, it will be initialized




**Parameters:**


* `ctx` context to work in 



**Returns:**

The prototype object 





        

<hr>



### function init 

_Initialize the class._ 
```C++
static inline void jac::Class::init (
    std::string name,
    bool isCtor=false
) 
```





**Note:**

This function should be called only once. Any subsequent calls with different parameters will throw an exception




**Parameters:**


* `name` name of the class 
* `isCtor` whether or not the class is callable a constructor (if it's callable at all) 




        

<hr>



### function initContext 

_Initialize the class prototype._ 
```C++
static inline void jac::Class::initContext (
    ContextRef ctx
) 
```





**Note:**

If the class is already initialized, this function does nothing




**Parameters:**


* `ctx` context to work in 




        

<hr>

------------------------------
The documentation for this class was generated from the following file `src/jac/machine/class.h`

