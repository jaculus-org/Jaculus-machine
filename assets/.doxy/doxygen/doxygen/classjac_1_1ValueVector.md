

# Class jac::ValueVector



[**ClassList**](annotated.md) **>** [**jac**](namespacejac.md) **>** [**ValueVector**](classjac_1_1ValueVector.md)



_An owning contiguous sequence of JavaScript values._ [More...](#detailed-description)

* `#include <values.h>`

















## Public Types

| Type | Name |
| ---: | :--- |
| typedef [**ValueVectorWeak::const\_iterator**](classjac_1_1ValueVectorWeak_1_1const__iterator.md) | [**const\_iterator**](#typedef-const_iterator)  <br> |




















## Public Functions

| Type | Name |
| ---: | :--- |
|   | [**ValueVector**](#function-valuevector-13) ([**ContextRef**](classjac_1_1ContextRef.md) ctx) <br> |
|   | [**ValueVector**](#function-valuevector-23) (const [**ValueVector**](classjac_1_1ValueVector.md) & other) <br> |
|   | [**ValueVector**](#function-valuevector-33) ([**ValueVector**](classjac_1_1ValueVector.md) && other) noexcept<br> |
|  [**ValueWeak**](classjac_1_1ValueWrapper.md) | [**at**](#function-at) (std::size\_t index) const<br> |
|  [**const\_iterator**](classjac_1_1ValueVectorWeak_1_1const__iterator.md) | [**begin**](#function-begin) () noexcept const<br> |
|  std::size\_t | [**capacity**](#function-capacity) () noexcept const<br> |
|  void | [**clear**](#function-clear) () noexcept<br> |
|  JSValue \* | [**data**](#function-data-12) () noexcept<br> |
|  const JSValue \* | [**data**](#function-data-22) () noexcept const<br> |
|  bool | [**empty**](#function-empty) () noexcept const<br> |
|  [**const\_iterator**](classjac_1_1ValueVectorWeak_1_1const__iterator.md) | [**end**](#function-end) () noexcept const<br> |
|   | [**operator ValueVectorWeak**](#function-operator-valuevectorweak) () noexcept const<br> |
|  [**ValueVector**](classjac_1_1ValueVector.md) & | [**operator=**](#function-operator) (const [**ValueVector**](classjac_1_1ValueVector.md) & other) <br> |
|  [**ValueVector**](classjac_1_1ValueVector.md) & | [**operator=**](#function-operator_1) ([**ValueVector**](classjac_1_1ValueVector.md) && other) noexcept<br> |
|  [**ValueWeak**](classjac_1_1ValueWrapper.md) | [**operator[]**](#function-operator_2) (std::size\_t index) const<br> |
|  void | [**pop\_back**](#function-pop_back) () noexcept<br> |
|  void | [**push\_back**](#function-push_back-12) ([**ValueWeak**](classjac_1_1ValueWrapper.md) value) <br> |
|  void | [**push\_back**](#function-push_back-22) ([**Value**](classjac_1_1ValueWrapper.md) && value) <br> |
|  void | [**reserve**](#function-reserve) (std::size\_t capacity) <br> |
|  std::size\_t | [**size**](#function-size) () noexcept const<br> |
|  void | [**swap**](#function-swap) ([**ValueVector**](classjac_1_1ValueVector.md) & other) noexcept<br> |
|   | [**~ValueVector**](#function-valuevector) () <br> |




























## Detailed Description


Copies retain every contained JavaScript value. Moves transfer ownership. Values inserted into the vector must belong to its context; violating this precondition is undefined behavior. Mutating raw data must preserve the same retain/free ownership invariant. 


    
## Public Types Documentation




### typedef const\_iterator 

```C++
using jac::ValueVector::const_iterator =  ValueVectorWeak::const_iterator;
```




<hr>
## Public Functions Documentation




### function ValueVector [1/3]

```C++
inline explicit jac::ValueVector::ValueVector (
    ContextRef ctx
) 
```




<hr>



### function ValueVector [2/3]

```C++
inline jac::ValueVector::ValueVector (
    const ValueVector & other
) 
```




<hr>



### function ValueVector [3/3]

```C++
inline jac::ValueVector::ValueVector (
    ValueVector && other
) noexcept
```




<hr>



### function at 

```C++
inline ValueWeak jac::ValueVector::at (
    std::size_t index
) const
```




<hr>



### function begin 

```C++
inline const_iterator jac::ValueVector::begin () noexcept const
```




<hr>



### function capacity 

```C++
inline std::size_t jac::ValueVector::capacity () noexcept const
```




<hr>



### function clear 

```C++
inline void jac::ValueVector::clear () noexcept
```




<hr>



### function data [1/2]

```C++
inline JSValue * jac::ValueVector::data () noexcept
```




<hr>



### function data [2/2]

```C++
inline const JSValue * jac::ValueVector::data () noexcept const
```




<hr>



### function empty 

```C++
inline bool jac::ValueVector::empty () noexcept const
```




<hr>



### function end 

```C++
inline const_iterator jac::ValueVector::end () noexcept const
```




<hr>



### function operator ValueVectorWeak 

```C++
inline jac::ValueVector::operator ValueVectorWeak () noexcept const
```




<hr>



### function operator= 

```C++
inline ValueVector & jac::ValueVector::operator= (
    const ValueVector & other
) 
```




<hr>



### function operator= 

```C++
inline ValueVector & jac::ValueVector::operator= (
    ValueVector && other
) noexcept
```




<hr>



### function operator[] 

```C++
inline ValueWeak jac::ValueVector::operator[] (
    std::size_t index
) const
```




<hr>



### function pop\_back 

```C++
inline void jac::ValueVector::pop_back () noexcept
```




<hr>



### function push\_back [1/2]

```C++
inline void jac::ValueVector::push_back (
    ValueWeak value
) 
```




<hr>



### function push\_back [2/2]

```C++
inline void jac::ValueVector::push_back (
    Value && value
) 
```




<hr>



### function reserve 

```C++
inline void jac::ValueVector::reserve (
    std::size_t capacity
) 
```




<hr>



### function size 

```C++
inline std::size_t jac::ValueVector::size () noexcept const
```




<hr>



### function swap 

```C++
inline void jac::ValueVector::swap (
    ValueVector & other
) noexcept
```




<hr>



### function ~ValueVector 

```C++
inline jac::ValueVector::~ValueVector () 
```




<hr>

------------------------------
The documentation for this class was generated from the following file `src/jac/machine/values.h`

