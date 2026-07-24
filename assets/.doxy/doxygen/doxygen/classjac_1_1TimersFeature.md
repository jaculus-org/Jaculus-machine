

# Class jac::TimersFeature

**template &lt;class Next&gt;**



[**ClassList**](annotated.md) **>** [**jac**](namespacejac.md) **>** [**TimersFeature**](classjac_1_1TimersFeature.md)








Inherits the following classes: Next


































## Public Functions

| Type | Name |
| ---: | :--- |
|  void | [**clearInterval**](#function-clearinterval) (int id) <br> |
|  void | [**clearTimeout**](#function-cleartimeout) (int id) <br> |
|  void | [**initialize**](#function-initialize) () <br> |
|  int | [**setInterval**](#function-setinterval) (std::function&lt; void()&gt; func, std::chrono::milliseconds millis) <br> |
|  int | [**setTimeout**](#function-settimeout) (std::function&lt; void()&gt; func, std::chrono::milliseconds millis) <br> |
|   | [**~TimersFeature**](#function-timersfeature) () <br> |




























## Public Functions Documentation




### function clearInterval 

```C++
inline void jac::TimersFeature::clearInterval (
    int id
) 
```




<hr>



### function clearTimeout 

```C++
inline void jac::TimersFeature::clearTimeout (
    int id
) 
```




<hr>



### function initialize 

```C++
inline void jac::TimersFeature::initialize () 
```




<hr>



### function setInterval 

```C++
inline int jac::TimersFeature::setInterval (
    std::function< void()> func,
    std::chrono::milliseconds millis
) 
```




<hr>



### function setTimeout 

```C++
inline int jac::TimersFeature::setTimeout (
    std::function< void()> func,
    std::chrono::milliseconds millis
) 
```




<hr>



### function ~TimersFeature 

```C++
inline jac::TimersFeature::~TimersFeature () 
```




<hr>

------------------------------
The documentation for this class was generated from the following file `src/jac/features/timersFeature.h`

