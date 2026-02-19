

# Class jac::Module



[**ClassList**](annotated.md) **>** [**jac**](namespacejac.md) **>** [**Module**](classjac_1_1Module.md)



_A wrapper around JSModuleDef that allows for easy exporting of values._ 

* `#include <machine.h>`





































## Public Functions

| Type | Name |
| ---: | :--- |
|   | [**Module**](#function-module-13) ([**ContextRef**](classjac_1_1ContextRef.md) ctx, std::string name) <br>_Create a new module in the given context. Should not be called directly, use_ [_**MachineBase::newModule**_](classjac_1_1MachineBase.md#function-newmodule) _instead._ |
|   | [**Module**](#function-module-23) (const [**Module**](classjac_1_1Module.md) &) = delete<br> |
|   | [**Module**](#function-module-33) ([**Module**](classjac_1_1Module.md) && other) <br> |
|  void | [**addExport**](#function-addexport) (std::string name, [**Value**](classjac_1_1ValueWrapper.md) val) <br>_Add a value to the module's exports._  |
|  JSModuleDef \* | [**get**](#function-get) () <br>_Get the underlying JSModuleDef\* for this module._  |
|  [**Module**](classjac_1_1Module.md) & | [**operator=**](#function-operator) (const [**Module**](classjac_1_1Module.md) &) = delete<br> |
|  [**Module**](classjac_1_1Module.md) & | [**operator=**](#function-operator_1) ([**Module**](classjac_1_1Module.md) && other) <br> |




























## Public Functions Documentation




### function Module [1/3]

_Create a new module in the given context. Should not be called directly, use_ [_**MachineBase::newModule**_](classjac_1_1MachineBase.md#function-newmodule) _instead._
```C++
jac::Module::Module (
    ContextRef ctx,
    std::string name
) 
```





**Parameters:**


* `ctx` context to work in 
* `name` name of the module 




        

<hr>



### function Module [2/3]

```C++
jac::Module::Module (
    const Module &
) = delete
```




<hr>



### function Module [3/3]

```C++
inline jac::Module::Module (
    Module && other
) 
```




<hr>



### function addExport 

_Add a value to the module's exports._ 
```C++
void jac::Module::addExport (
    std::string name,
    Value val
) 
```





**Parameters:**


* `name` name of the export 
* `val` the value to export 




        

<hr>



### function get 

_Get the underlying JSModuleDef\* for this module._ 
```C++
inline JSModuleDef * jac::Module::get () 
```





**Returns:**

JSModuleDef\* 





        

<hr>



### function operator= 

```C++
Module & jac::Module::operator= (
    const Module &
) = delete
```




<hr>



### function operator= 

```C++
inline Module & jac::Module::operator= (
    Module && other
) 
```




<hr>

------------------------------
The documentation for this class was generated from the following file `src/jac/machine/machine.h`

