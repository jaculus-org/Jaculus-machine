

# Class jac::OwnedString



[**ClassList**](annotated.md) **>** [**jac**](namespacejac.md) **>** [**OwnedString**](classjac_1_1OwnedString.md)



_An owning RAII handle for a QuickJS-allocated C string._ [More...](#detailed-description)

* `#include <ownedString.h>`





































## Public Functions

| Type | Name |
| ---: | :--- |
|   | [**OwnedString**](#function-ownedstring-13) () = default<br> |
|   | [**OwnedString**](#function-ownedstring-23) ([**ContextRef**](classjac_1_1ContextRef.md) ctx, const char \* str) <br>_Wrap a QuickJS allocated, null-terminated string._  |
|   | [**OwnedString**](#function-ownedstring-33) ([**ContextRef**](classjac_1_1ContextRef.md) ctx, const char \* str, std::size\_t len) <br>_Wrap a QuickJS allocated string of a known length. The string may contain embedded null bytes._  |
|  const char \* | [**c\_str**](#function-c_str) () noexcept const<br>_Get the underlying null-terminated C string._  |
|  const char \* | [**data**](#function-data) () noexcept const<br> |
|  bool | [**empty**](#function-empty) () noexcept const<br> |
|   | [**string**](#function-string) () const<br> |
|   | [**string\_view**](#function-string_view) () noexcept const<br> |
|  std::size\_t | [**size**](#function-size) () noexcept const<br> |
|  std::string\_view | [**view**](#function-view) () noexcept const<br>_Get a non-owning view of the string._  |




























## Detailed Description


The string must be allocated using QuickJS functions (JS\_ToCString, JS\_AtomToCString, etc.); it is released with JS\_FreeCString when the handle is destroyed. The handle is move-only. Use [**view**](classjac_1_1OwnedString.md#function-view) to obtain a (non-owning) std::string\_view for inspection. 


    
## Public Functions Documentation




### function OwnedString [1/3]

```C++
jac::OwnedString::OwnedString () = default
```




<hr>



### function OwnedString [2/3]

_Wrap a QuickJS allocated, null-terminated string._ 
```C++
inline jac::OwnedString::OwnedString (
    ContextRef ctx,
    const char * str
) 
```





**Parameters:**


* `ctx` context to work in 
* `str` string to take ownership of 




        

<hr>



### function OwnedString [3/3]

_Wrap a QuickJS allocated string of a known length. The string may contain embedded null bytes._ 
```C++
inline jac::OwnedString::OwnedString (
    ContextRef ctx,
    const char * str,
    std::size_t len
) 
```





**Parameters:**


* `ctx` context to work in 
* `str` string to take ownership of 
* `len` length of the string in bytes 




        

<hr>



### function c\_str 

_Get the underlying null-terminated C string._ 
```C++
inline const char * jac::OwnedString::c_str () noexcept const
```





**Returns:**

const char\* 





        

<hr>



### function data 

```C++
inline const char * jac::OwnedString::data () noexcept const
```




<hr>



### function empty 

```C++
inline bool jac::OwnedString::empty () noexcept const
```




<hr>



### function string 

```C++
inline jac::OwnedString::string () const
```




<hr>



### function string\_view 

```C++
inline jac::OwnedString::string_view () noexcept const
```




<hr>



### function size 

```C++
inline std::size_t jac::OwnedString::size () noexcept const
```




<hr>



### function view 

_Get a non-owning view of the string._ 
```C++
inline std::string_view jac::OwnedString::view () noexcept const
```





**Returns:**

std::string\_view valid for the lifetime of this handle 





        

<hr>## Friends Documentation





### friend operator&lt;&lt; 

```C++
inline std::ostream & jac::OwnedString::operator<< (
    std::ostream & os,
    const OwnedString & str
) 
```




<hr>

------------------------------
The documentation for this class was generated from the following file `src/jac/machine/ownedString.h`

