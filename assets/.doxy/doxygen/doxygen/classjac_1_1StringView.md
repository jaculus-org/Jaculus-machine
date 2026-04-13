

# Class jac::StringView



[**ClassList**](annotated.md) **>** [**jac**](namespacejac.md) **>** [**StringView**](classjac_1_1StringView.md)



_A wrapper around QuickJS C-string with automatic memory management._ 

* `#include <stringView.h>`



Inherits the following classes: std::basic_string_view< char >


































## Public Functions

| Type | Name |
| ---: | :--- |
|   | [**StringView**](#function-stringview-15) ([**StringView**](classjac_1_1StringView.md) && other) <br> |
|   | [**StringView**](#function-stringview-25) (const basic\_string\_view & other) = delete<br> |
|   | [**StringView**](#function-stringview-35) (const [**StringView**](classjac_1_1StringView.md) & other) = delete<br> |
|   | [**StringView**](#function-stringview-45) () = default<br> |
|   | [**StringView**](#function-stringview-55) ([**ContextRef**](classjac_1_1ContextRef.md) ctx, const char \* str) <br>_Wrap a QuickJS allocated string. The string will be freed when the_ [_**StringView**_](classjac_1_1StringView.md) _is freed._ |
|  const char \* | [**c\_str**](#function-c_str) () const<br>_Get the C string._  |
|   | [**string**](#function-string) () const<br> |
|  [**StringView**](classjac_1_1StringView.md) & | [**operator=**](#function-operator) ([**StringView**](classjac_1_1StringView.md) && other) <br> |
|  [**StringView**](classjac_1_1StringView.md) & | [**operator=**](#function-operator_1) (const [**StringView**](classjac_1_1StringView.md) & other) = delete<br> |
|   | [**~StringView**](#function-stringview) () <br> |




























## Public Functions Documentation




### function StringView [1/5]

```C++
inline jac::StringView::StringView (
    StringView && other
) 
```




<hr>



### function StringView [2/5]

```C++
jac::StringView::StringView (
    const basic_string_view & other
) = delete
```




<hr>



### function StringView [3/5]

```C++
jac::StringView::StringView (
    const StringView & other
) = delete
```




<hr>



### function StringView [4/5]

```C++
jac::StringView::StringView () = default
```




<hr>



### function StringView [5/5]

_Wrap a QuickJS allocated string. The string will be freed when the_ [_**StringView**_](classjac_1_1StringView.md) _is freed._
```C++
inline jac::StringView::StringView (
    ContextRef ctx,
    const char * str
) 
```





**Note:**

The string must be allocated using QuickJS functions - JS\_NewString, JS\_ToCString, etc.




**Parameters:**


* `ctx` context to work in 
* `str` string to wrap 




        

<hr>



### function c\_str 

_Get the C string._ 
```C++
inline const char * jac::StringView::c_str () const
```





**Returns:**

const char\* 





        

<hr>



### function string 

```C++
inline jac::StringView::string () const
```




<hr>



### function operator= 

```C++
inline StringView & jac::StringView::operator= (
    StringView && other
) 
```




<hr>



### function operator= 

```C++
StringView & jac::StringView::operator= (
    const StringView & other
) = delete
```




<hr>



### function ~StringView 

```C++
inline jac::StringView::~StringView () 
```




<hr>

------------------------------
The documentation for this class was generated from the following file `src/jac/machine/stringView.h`

