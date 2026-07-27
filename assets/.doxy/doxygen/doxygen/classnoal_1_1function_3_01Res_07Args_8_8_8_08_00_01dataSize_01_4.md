

# Class noal::function&lt; Res(Args...), dataSize &gt;

**template &lt;typename Res, typename... Args, size\_t dataSize&gt;**



[**ClassList**](annotated.md) **>** [**noal**](namespacenoal.md) **>** [**function&lt; Res(Args...), dataSize &gt;**](classnoal_1_1function_3_01Res_07Args_8_8_8_08_00_01dataSize_01_4.md)










































## Public Functions

| Type | Name |
| ---: | :--- |
|   | [**function**](#function-function-17) () = default<br> |
|   | [**function**](#function-function-27) (const [**function**](classnoal_1_1function.md)&lt; Sign, otherSize &gt; & other) <br> |
|   | [**function**](#function-function-37) ([**function**](classnoal_1_1function.md)&lt; Sign, otherSize &gt; && other) <br> |
|   | [**function**](#function-function-47) (Res(\*)(Args...) func) <br> |
|   | [**function**](#function-function-57) (Res(Class::\*)(Args...) func, Class \* self) <br> |
|   | [**function**](#function-function-67) (Res(Class::\*)(Args...) const func, Class \* self) <br> |
|   | [**function**](#function-function-77) (Func func) <br> |
|   | [**operator bool**](#function-operator-bool) () const<br> |
|  Res | [**operator()**](#function-operator) (Args... args) <br> |
|  [**function**](classnoal_1_1function.md) & | [**operator=**](#function-operator_1) (const [**function**](classnoal_1_1function.md)&lt; Sign, otherSize &gt; & other) <br> |
|  [**function**](classnoal_1_1function.md) & | [**operator=**](#function-operator_2) ([**function**](classnoal_1_1function.md)&lt; Sign, otherSize &gt; && other) <br> |
|  [**function**](classnoal_1_1function.md) & | [**operator=**](#function-operator_3) (Res(\*)(Args...) func) <br> |
|  [**function**](classnoal_1_1function.md) & | [**operator=**](#function-operator_4) (Func func) <br> |




























## Public Functions Documentation




### function function [1/7]

```C++
noal::function< Res(Args...), dataSize >::function () = default
```




<hr>



### function function [2/7]

```C++
template<size_t otherSize>
inline noal::function< Res(Args...), dataSize >::function (
    const function < Sign, otherSize > & other
) 
```




<hr>



### function function [3/7]

```C++
template<size_t otherSize>
inline noal::function< Res(Args...), dataSize >::function (
    function < Sign, otherSize > && other
) 
```




<hr>



### function function [4/7]

```C++
inline explicit noal::function< Res(Args...), dataSize >::function (
    Res(*)(Args...) func
) 
```




<hr>



### function function [5/7]

```C++
template<class Class>
inline noal::function< Res(Args...), dataSize >::function (
    Res(Class::*)(Args...) func,
    Class * self
) 
```




<hr>



### function function [6/7]

```C++
template<class Class>
inline noal::function< Res(Args...), dataSize >::function (
    Res(Class::*)(Args...) const func,
    Class * self
) 
```




<hr>



### function function [7/7]

```C++
template<typename Func, typename Sign>
inline explicit noal::function< Res(Args...), dataSize >::function (
    Func func
) 
```




<hr>



### function operator bool 

```C++
inline explicit noal::function< Res(Args...), dataSize >::operator bool () const
```




<hr>



### function operator() 

```C++
inline Res noal::function< Res(Args...), dataSize >::operator() (
    Args... args
) 
```




<hr>



### function operator= 

```C++
template<size_t otherSize>
inline function & noal::function< Res(Args...), dataSize >::operator= (
    const function < Sign, otherSize > & other
) 
```




<hr>



### function operator= 

```C++
template<size_t otherSize>
inline function & noal::function< Res(Args...), dataSize >::operator= (
    function < Sign, otherSize > && other
) 
```




<hr>



### function operator= 

```C++
inline function & noal::function< Res(Args...), dataSize >::operator= (
    Res(*)(Args...) func
) 
```




<hr>



### function operator= 

```C++
template<typename Func, typename Sign>
inline function & noal::function< Res(Args...), dataSize >::operator= (
    Func func
) 
```




<hr>

------------------------------
The documentation for this class was generated from the following file `src/noal_func.h`

