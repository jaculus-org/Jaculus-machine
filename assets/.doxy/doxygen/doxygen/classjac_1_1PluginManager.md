

# Class jac::PluginManager



[**ClassList**](annotated.md) **>** [**jac**](namespacejac.md) **>** [**PluginManager**](classjac_1_1PluginManager.md)



_A class for managing groups of plugins and initializing them all at once._ 

* `#include <plugins.h>`





































## Public Functions

| Type | Name |
| ---: | :--- |
|   | [**PluginManager**](#function-pluginmanager) () = default<br> |
|  size\_t | [**addPlugin**](#function-addplugin) () <br>_Adds a plugin to the manager._  |
|  [**PluginHandle**](classjac_1_1PluginHandle.md) | [**initialize**](#function-initialize) (Machine & machine) <br>_Initializes all plugins in the manager._  |




























## Public Functions Documentation




### function PluginManager 

```C++
jac::PluginManager::PluginManager () = default
```




<hr>



### function addPlugin 

_Adds a plugin to the manager._ 
```C++
template<typename Plugin_t>
inline size_t jac::PluginManager::addPlugin () 
```





**Note:**

The initialization of the plugin is to be performed by the constructor. 




**Template parameters:**


* `Plugin_t` The type of the plugin to add 



**Returns:**

The offset of the plugin in the group. Can be added to the [**PluginHandle**](classjac_1_1PluginHandle.md) returned by [**initialize()**](classjac_1_1PluginManager.md#function-initialize) to get a handle to the plugin instance inside the Machine. 





        

<hr>



### function initialize 

_Initializes all plugins in the manager._ 
```C++
template<typename Machine>
inline PluginHandle jac::PluginManager::initialize (
    Machine & machine
) 
```





**Template parameters:**


* `Machine` The type of the machine to initialize the plugins for 



**Parameters:**


* `machine` The machine to initialize the plugins for 



**Returns:**

A Handle to the first plugin initialized. 





        

<hr>

------------------------------
The documentation for this class was generated from the following file `src/jac/machine/plugins.h`

