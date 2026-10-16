

# Class jac::ValueVectorWeak



[**ClassList**](annotated.md) **>** [**jac**](namespacejac.md) **>** [**ValueVectorWeak**](classjac_1_1ValueVectorWeak.md)



_A non-owning view over a contiguous sequence of JavaScript values._ [More...](#detailed-description)

* `#include <values.h>`















## Classes

| Type | Name |
| ---: | :--- |
| class | [**const\_iterator**](classjac_1_1ValueVectorWeak_1_1const__iterator.md) <br> |






















## Public Functions

| Type | Name |
| ---: | :--- |
|   | [**ValueVectorWeak**](#function-valuevectorweak-12) ([**ContextRef**](classjac_1_1ContextRef.md) ctx, const JSValue \* data, std::size\_t size) <br> |
|   | [**ValueVectorWeak**](#function-valuevectorweak-22) ([**ContextRef**](classjac_1_1ContextRef.md) ctx, const JSValue \* data, int size) <br> |
|  [**ValueWeak**](classjac_1_1ValueWrapper.md) | [**at**](#function-at) (std::size\_t index) const<br> |
|  [**const\_iterator**](classjac_1_1ValueVectorWeak_1_1const__iterator.md) | [**begin**](#function-begin) () noexcept const<br> |
|  const JSValue \* | [**data**](#function-data) () noexcept const<br> |
|  bool | [**empty**](#function-empty) () noexcept const<br> |
|  [**const\_iterator**](classjac_1_1ValueVectorWeak_1_1const__iterator.md) | [**end**](#function-end) () noexcept const<br> |
|  [**ValueWeak**](classjac_1_1ValueWrapper.md) | [**operator[]**](#function-operator) (std::size\_t index) const<br> |
|  std::size\_t | [**size**](#function-size) () noexcept const<br> |
|  [**ValueVector**](classjac_1_1ValueVector.md) | [**toOwned**](#function-toowned) () const<br> |




























## Detailed Description


The view and all ValueWeak objects obtained from it are valid only while the underlying sequence remains alive and unmoved. All values must belong to the stored context; violating this precondition is undefined behavior. 


    
## Public Functions Documentation




### function ValueVectorWeak [1/2]

```C++
inline jac::ValueVectorWeak::ValueVectorWeak (
    ContextRef ctx,
    const JSValue * data,
    std::size_t size
) 
```




<hr>



### function ValueVectorWeak [2/2]

```C++
inline jac::ValueVectorWeak::ValueVectorWeak (
    ContextRef ctx,
    const JSValue * data,
    int size
) 
```




<hr>



### function at 

```C++
inline ValueWeak jac::ValueVectorWeak::at (
    std::size_t index
) const
```




<hr>



### function begin 

```C++
inline const_iterator jac::ValueVectorWeak::begin () noexcept const
```




<hr>



### function data 

```C++
inline const JSValue * jac::ValueVectorWeak::data () noexcept const
```




<hr>



### function empty 

```C++
inline bool jac::ValueVectorWeak::empty () noexcept const
```




<hr>



### function end 

```C++
inline const_iterator jac::ValueVectorWeak::end () noexcept const
```




<hr>



### function operator[] 

```C++
inline ValueWeak jac::ValueVectorWeak::operator[] (
    std::size_t index
) const
```




<hr>



### function size 

```C++
inline std::size_t jac::ValueVectorWeak::size () noexcept const
```




<hr>



### function toOwned 

```C++
inline ValueVector jac::ValueVectorWeak::toOwned () const
```




<hr>

------------------------------
The documentation for this class was generated from the following file `src/jac/machine/values.h`

