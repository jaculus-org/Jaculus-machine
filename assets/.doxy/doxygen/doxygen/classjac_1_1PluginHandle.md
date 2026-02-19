

# Class jac::PluginHandle



[**ClassList**](annotated.md) **>** [**jac**](namespacejac.md) **>** [**PluginHandle**](classjac_1_1PluginHandle.md)



_A handle which can be used to retrieve a plugin from a machine._ 

* `#include <plugins.h>`





































## Public Functions

| Type | Name |
| ---: | :--- |
|   | [**PluginHandle**](#function-pluginhandle) (size\_t index) <br> |
|  size\_t | [**index**](#function-index) () const<br> |
|  [**PluginHandle**](classjac_1_1PluginHandle.md) | [**operator+**](#function-operator) (size\_t offset) <br> |
|  [**PluginHandle**](classjac_1_1PluginHandle.md) & | [**operator++**](#function-operator_1) () <br> |
|  [**PluginHandle**](classjac_1_1PluginHandle.md) | [**operator++**](#function-operator_2) (int) <br> |
|  bool | [**operator==**](#function-operator_3) (const [**PluginHandle**](classjac_1_1PluginHandle.md) & other) const<br> |




























## Public Functions Documentation




### function PluginHandle 

```C++
inline jac::PluginHandle::PluginHandle (
    size_t index
) 
```




<hr>



### function index 

```C++
inline size_t jac::PluginHandle::index () const
```




<hr>



### function operator+ 

```C++
inline PluginHandle jac::PluginHandle::operator+ (
    size_t offset
) 
```




<hr>



### function operator++ 

```C++
inline PluginHandle & jac::PluginHandle::operator++ () 
```




<hr>



### function operator++ 

```C++
inline PluginHandle jac::PluginHandle::operator++ (
    int
) 
```




<hr>



### function operator== 

```C++
bool jac::PluginHandle::operator== (
    const PluginHandle & other
) const
```




<hr>

------------------------------
The documentation for this class was generated from the following file `src/jac/machine/plugins.h`

