

# Class jac::OsWritable



[**ClassList**](annotated.md) **>** [**jac**](namespacejac.md) **>** [**OsWritable**](classjac_1_1OsWritable.md)








Inherits the following classes: [jac::Writable](classjac_1_1Writable.md)






















































## Public Functions

| Type | Name |
| ---: | :--- |
|   | [**OsWritable**](#function-oswritable) (std::ostream & stream) <br> |
| virtual void | [**write**](#function-write) (std::string data) override<br> |


## Public Functions inherited from jac::Writable

See [jac::Writable](classjac_1_1Writable.md)

| Type | Name |
| ---: | :--- |
| virtual void | [**write**](classjac_1_1Writable.md#function-write) (std::string data) = 0<br> |
| virtual  | [**~Writable**](classjac_1_1Writable.md#function-writable) () = default<br> |






















































## Public Functions Documentation




### function OsWritable 

```C++
inline jac::OsWritable::OsWritable (
    std::ostream & stream
) 
```




<hr>



### function write 

```C++
inline virtual void jac::OsWritable::write (
    std::string data
) override
```



Implements [*jac::Writable::write*](classjac_1_1Writable.md#function-write)


<hr>

------------------------------
The documentation for this class was generated from the following file `src/jac/features/util/ostreamjs.h`

