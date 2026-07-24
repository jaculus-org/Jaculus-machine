

# Class jac::ValueVectorWeak::const\_iterator



[**ClassList**](annotated.md) **>** [**jac**](namespacejac.md) **>** [**ValueVectorWeak**](classjac_1_1ValueVectorWeak.md) **>** [**const\_iterator**](classjac_1_1ValueVectorWeak_1_1const__iterator.md)






















## Public Types

| Type | Name |
| ---: | :--- |
| typedef std::ptrdiff\_t | [**difference\_type**](#typedef-difference_type)  <br> |
| typedef std::random\_access\_iterator\_tag | [**iterator\_category**](#typedef-iterator_category)  <br> |
| typedef std::random\_access\_iterator\_tag | [**iterator\_concept**](#typedef-iterator_concept)  <br> |
| typedef [**ValueWeak**](classjac_1_1ValueWrapper.md) | [**reference**](#typedef-reference)  <br> |
| typedef [**ValueWeak**](classjac_1_1ValueWrapper.md) | [**value\_type**](#typedef-value_type)  <br> |




















## Public Functions

| Type | Name |
| ---: | :--- |
|   | [**const\_iterator**](#function-const_iterator-12) () <br> |
|   | [**const\_iterator**](#function-const_iterator-22) ([**ContextRef**](classjac_1_1ContextRef.md) ctx, const JSValue \* ptr) <br> |
|  [**ValueWeak**](classjac_1_1ValueWrapper.md) | [**operator\***](#function-operator) () const<br> |
|  [**const\_iterator**](classjac_1_1ValueVectorWeak_1_1const__iterator.md) & | [**operator++**](#function-operator_1) () <br> |
|  [**const\_iterator**](classjac_1_1ValueVectorWeak_1_1const__iterator.md) | [**operator++**](#function-operator_2) (int) <br> |
|  [**const\_iterator**](classjac_1_1ValueVectorWeak_1_1const__iterator.md) & | [**operator+=**](#function-operator_3) (difference\_type offset) <br> |
|  [**const\_iterator**](classjac_1_1ValueVectorWeak_1_1const__iterator.md) & | [**operator--**](#function-operator-) () <br> |
|  [**const\_iterator**](classjac_1_1ValueVectorWeak_1_1const__iterator.md) | [**operator--**](#function-operator-_1) (int) <br> |
|  [**const\_iterator**](classjac_1_1ValueVectorWeak_1_1const__iterator.md) & | [**operator-=**](#function-operator-) (difference\_type offset) <br> |
|  [**ValueWeak**](classjac_1_1ValueWrapper.md) | [**operator[]**](#function-operator_4) (difference\_type offset) const<br> |




























## Public Types Documentation




### typedef difference\_type 

```C++
using jac::ValueVectorWeak::const_iterator::difference_type =  std::ptrdiff_t;
```




<hr>



### typedef iterator\_category 

```C++
using jac::ValueVectorWeak::const_iterator::iterator_category =  std::random_access_iterator_tag;
```




<hr>



### typedef iterator\_concept 

```C++
using jac::ValueVectorWeak::const_iterator::iterator_concept =  std::random_access_iterator_tag;
```




<hr>



### typedef reference 

```C++
using jac::ValueVectorWeak::const_iterator::reference =  ValueWeak;
```




<hr>



### typedef value\_type 

```C++
using jac::ValueVectorWeak::const_iterator::value_type =  ValueWeak;
```




<hr>
## Public Functions Documentation




### function const\_iterator [1/2]

```C++
inline jac::ValueVectorWeak::const_iterator::const_iterator () 
```




<hr>



### function const\_iterator [2/2]

```C++
inline jac::ValueVectorWeak::const_iterator::const_iterator (
    ContextRef ctx,
    const JSValue * ptr
) 
```




<hr>



### function operator\* 

```C++
inline ValueWeak jac::ValueVectorWeak::const_iterator::operator* () const
```




<hr>



### function operator++ 

```C++
inline const_iterator & jac::ValueVectorWeak::const_iterator::operator++ () 
```




<hr>



### function operator++ 

```C++
inline const_iterator jac::ValueVectorWeak::const_iterator::operator++ (
    int
) 
```




<hr>



### function operator+= 

```C++
inline const_iterator & jac::ValueVectorWeak::const_iterator::operator+= (
    difference_type offset
) 
```




<hr>



### function operator-- 

```C++
inline const_iterator & jac::ValueVectorWeak::const_iterator::operator-- () 
```




<hr>



### function operator-- 

```C++
inline const_iterator jac::ValueVectorWeak::const_iterator::operator-- (
    int
) 
```




<hr>



### function operator-= 

```C++
inline const_iterator & jac::ValueVectorWeak::const_iterator::operator-= (
    difference_type offset
) 
```




<hr>



### function operator[] 

```C++
inline ValueWeak jac::ValueVectorWeak::const_iterator::operator[] (
    difference_type offset
) const
```




<hr>## Friends Documentation





### friend operator+ 

```C++
inline const_iterator jac::ValueVectorWeak::const_iterator::operator+ (
    const_iterator it,
    difference_type offset
) 
```




<hr>



### friend operator+ 

```C++
inline const_iterator jac::ValueVectorWeak::const_iterator::operator+ (
    difference_type offset,
    const_iterator it
) 
```




<hr>



### friend operator- 

```C++
inline const_iterator jac::ValueVectorWeak::const_iterator::operator- (
    const_iterator it,
    difference_type offset
) 
```




<hr>



### friend operator- 

```C++
inline difference_type jac::ValueVectorWeak::const_iterator::operator- (
    const_iterator lhs,
    const_iterator rhs
) 
```




<hr>



### friend operator&lt;=&gt; 

```C++
inline auto jac::ValueVectorWeak::const_iterator::operator<=> (
    const_iterator lhs,
    const_iterator rhs
) 
```




<hr>



### friend operator== 

```C++
inline bool jac::ValueVectorWeak::const_iterator::operator== (
    const_iterator lhs,
    const_iterator rhs
) 
```




<hr>

------------------------------
The documentation for this class was generated from the following file `src/jac/machine/values.h`

