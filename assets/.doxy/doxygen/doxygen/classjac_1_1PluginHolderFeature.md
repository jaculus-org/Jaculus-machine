

# Class jac::PluginHolderFeature

**template &lt;typename Next&gt;**



[**ClassList**](annotated.md) **>** [**jac**](namespacejac.md) **>** [**PluginHolderFeature**](classjac_1_1PluginHolderFeature.md)



_An MFeature that allows for inserting plugins into the machine and retrieving them using PluginHandeles._ 

* `#include <plugins.h>`



Inherits the following classes: Next


































## Public Functions

| Type | Name |
| ---: | :--- |
|   | [**PluginHolderFeature**](#function-pluginholderfeature) () = default<br> |
|  Plugin\_t & | [**getPlugin**](#function-getplugin) ([**PluginHandle**](classjac_1_1PluginHandle.md) handle) <br> |
|  [**PluginHandle**](classjac_1_1PluginHandle.md) | [**holdPlugin**](#function-holdplugin) (std::unique\_ptr&lt; [**Plugin**](classjac_1_1Plugin.md) &gt; plugin) <br> |
|  [**PluginHandle**](classjac_1_1PluginHandle.md) | [**pluginBegin**](#function-pluginbegin) () <br> |
|  [**PluginHandle**](classjac_1_1PluginHandle.md) | [**pluginEnd**](#function-pluginend) () <br> |




























## Public Functions Documentation




### function PluginHolderFeature 

```C++
jac::PluginHolderFeature::PluginHolderFeature () = default
```




<hr>



### function getPlugin 

```C++
template<typename Plugin_t>
inline Plugin_t & jac::PluginHolderFeature::getPlugin (
    PluginHandle handle
) 
```




<hr>



### function holdPlugin 

```C++
inline PluginHandle jac::PluginHolderFeature::holdPlugin (
    std::unique_ptr< Plugin > plugin
) 
```




<hr>



### function pluginBegin 

```C++
inline PluginHandle jac::PluginHolderFeature::pluginBegin () 
```




<hr>



### function pluginEnd 

```C++
inline PluginHandle jac::PluginHolderFeature::pluginEnd () 
```




<hr>

------------------------------
The documentation for this class was generated from the following file `src/jac/machine/plugins.h`

