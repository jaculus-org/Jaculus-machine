

# Class jac::Atom



[**ClassList**](annotated.md) **>** [**jac**](namespacejac.md) **>** [**Atom**](classjac_1_1Atom.md)



_A wrapper around JSAtom with RAII. In the context of QuickJS,_ [_**Atom**_](classjac_1_1Atom.md) _is used to represent identifiers of properties, variables, functions, etc._

* `#include <atom.h>`





































## Public Functions

| Type | Name |
| ---: | :--- |
|   | [**Atom**](#function-atom-13) ([**ContextRef**](classjac_1_1ContextRef.md) ctx, JSAtom atom) <br>_Wrap an existing JSAtom. The JSAtom will be freed when the_ [_**Atom**_](classjac_1_1Atom.md) _is destroyed._ |
|   | [**Atom**](#function-atom-23) (const [**Atom**](classjac_1_1Atom.md) & other) <br> |
|   | [**Atom**](#function-atom-33) ([**Atom**](classjac_1_1Atom.md) && other) <br> |
|  JSAtom & | [**get**](#function-get) () <br>_Get reference to the underlying JSAtom._  |
|  std::pair&lt; [**ContextRef**](classjac_1_1ContextRef.md), JSAtom &gt; | [**loot**](#function-loot) () <br>_Release ownership of the JSAtom. The JSAtom will have to be freed manually._  |
|  [**Atom**](classjac_1_1Atom.md) & | [**operator=**](#function-operator) (const [**Atom**](classjac_1_1Atom.md) & other) <br> |
|  [**Atom**](classjac_1_1Atom.md) & | [**operator=**](#function-operator_1) ([**Atom**](classjac_1_1Atom.md) && other) <br> |
|  [**StringView**](classjac_1_1StringView.md) | [**toString**](#function-tostring) () const<br>_Get string representation of the atom._  |
|   | [**~Atom**](#function-atom) () <br> |


## Public Static Functions

| Type | Name |
| ---: | :--- |
|  [**Atom**](classjac_1_1Atom.md) | [**create**](#function-create-13) ([**ContextRef**](classjac_1_1ContextRef.md) ctx, uint32\_t value) <br>_Create a new atom from an uint32\_t value._  |
|  [**Atom**](classjac_1_1Atom.md) | [**create**](#function-create-23) ([**ContextRef**](classjac_1_1ContextRef.md) ctx, const char \* value) <br>_Create a new atom from a string._  |
|  [**Atom**](classjac_1_1Atom.md) | [**create**](#function-create-33) ([**ContextRef**](classjac_1_1ContextRef.md) ctx, std::string value) <br>_Create a new atom from a string._  |






## Protected Attributes

| Type | Name |
| ---: | :--- |
|  JSAtom | [**\_atom**](#variable-_atom)  <br> |
|  [**ContextRef**](classjac_1_1ContextRef.md) | [**\_ctx**](#variable-_ctx)  <br> |




















## Public Functions Documentation




### function Atom [1/3]

_Wrap an existing JSAtom. The JSAtom will be freed when the_ [_**Atom**_](classjac_1_1Atom.md) _is destroyed._
```C++
inline jac::Atom::Atom (
    ContextRef ctx,
    JSAtom atom
) 
```





**Note:**

Used internally when directly working with QuickJS API. New [**Atom**](classjac_1_1Atom.md) should be created using [**Atom::create()**](classjac_1_1Atom.md#function-create-13).




**Parameters:**


* `ctx` context to work in 
* `atom` JSAtom to wrap 




        

<hr>



### function Atom [2/3]

```C++
inline jac::Atom::Atom (
    const Atom & other
) 
```




<hr>



### function Atom [3/3]

```C++
inline jac::Atom::Atom (
    Atom && other
) 
```




<hr>



### function get 

_Get reference to the underlying JSAtom._ 
```C++
inline JSAtom & jac::Atom::get () 
```





**Returns:**

JSAtom reference 





        

<hr>



### function loot 

_Release ownership of the JSAtom. The JSAtom will have to be freed manually._ 
```C++
inline std::pair< ContextRef , JSAtom > jac::Atom::loot () 
```





**Note:**

After this call, the [**Atom**](classjac_1_1Atom.md) will be in an invalid state.




**Returns:**

Pair of [**ContextRef**](classjac_1_1ContextRef.md) and JSAtom 





        

<hr>



### function operator= 

```C++
inline Atom & jac::Atom::operator= (
    const Atom & other
) 
```




<hr>



### function operator= 

```C++
inline Atom & jac::Atom::operator= (
    Atom && other
) 
```




<hr>



### function toString 

_Get string representation of the atom._ 
```C++
inline StringView jac::Atom::toString () const
```





**Returns:**

[**StringView**](classjac_1_1StringView.md) 





        

<hr>



### function ~Atom 

```C++
inline jac::Atom::~Atom () 
```




<hr>
## Public Static Functions Documentation




### function create [1/3]

_Create a new atom from an uint32\_t value._ 
```C++
static inline Atom jac::Atom::create (
    ContextRef ctx,
    uint32_t value
) 
```





**Parameters:**


* `ctx` context to create the atom in 
* `value` the value 



**Returns:**

The newly constructed atom 





        

<hr>



### function create [2/3]

_Create a new atom from a string._ 
```C++
static inline Atom jac::Atom::create (
    ContextRef ctx,
    const char * value
) 
```





**Parameters:**


* `ctx` context to create the atom in 
* `value` the value 



**Returns:**

The newly constructed atom 





        

<hr>



### function create [3/3]

_Create a new atom from a string._ 
```C++
static inline Atom jac::Atom::create (
    ContextRef ctx,
    std::string value
) 
```





**Parameters:**


* `ctx` context to create the atom in 
* `value` the value 



**Returns:**

The newly constructed atom 





        

<hr>
## Protected Attributes Documentation




### variable \_atom 

```C++
JSAtom jac::Atom::_atom;
```




<hr>



### variable \_ctx 

```C++
ContextRef jac::Atom::_ctx;
```




<hr>## Friends Documentation





### friend operator&lt;&lt; 

```C++
inline std::ostream & jac::Atom::operator<< (
    std::ostream & os,
    Atom & val
) 
```




<hr>

------------------------------
The documentation for this class was generated from the following file `src/jac/machine/atom.h`

