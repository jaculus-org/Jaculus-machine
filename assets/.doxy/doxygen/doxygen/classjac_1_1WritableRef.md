

# Class jac::WritableRef



[**ClassList**](annotated.md) **>** [**jac**](namespacejac.md) **>** [**WritableRef**](classjac_1_1WritableRef.md)








Inherits the following classes: [jac::Writable](classjac_1_1Writable.md)






















































## Public Functions

| Type | Name |
| ---: | :--- |
|   | [**WritableRef**](#function-writableref) ([**Writable**](classjac_1_1Writable.md) \* ptr) <br> |
| virtual void | [**write**](#function-write) (std::string data) override<br> |


## Public Functions inherited from jac::Writable

See [jac::Writable](classjac_1_1Writable.md)

| Type | Name |
| ---: | :--- |
| virtual void | [**write**](classjac_1_1Writable.md#function-write) (std::string data) = 0<br> |
| virtual  | [**~Writable**](classjac_1_1Writable.md#function-writable) () = default<br> |






















































## Public Functions Documentation




### function WritableRef 

```C++
inline jac::WritableRef::WritableRef (
    Writable * ptr
) 
```




<hr>



### function write 

```C++
inline virtual void jac::WritableRef::write (
    std::string data
) override
```



Implements [*jac::Writable::write*](classjac_1_1Writable.md#function-write)


<hr>

------------------------------
The documentation for this class was generated from the following file `src/jac/features/types/streams.h`

