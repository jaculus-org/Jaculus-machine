

# Class jac::ReadableRef



[**ClassList**](annotated.md) **>** [**jac**](namespacejac.md) **>** [**ReadableRef**](classjac_1_1ReadableRef.md)








Inherits the following classes: [jac::Readable](classjac_1_1Readable.md)






















































## Public Functions

| Type | Name |
| ---: | :--- |
|   | [**ReadableRef**](#function-readableref) ([**Readable**](classjac_1_1Readable.md) \* ptr) <br> |
| virtual bool | [**get**](#function-get) (std::function&lt; void(char)&gt; callback) override<br> |
| virtual bool | [**read**](#function-read) (std::function&lt; void(std::string)&gt; callback) override<br> |


## Public Functions inherited from jac::Readable

See [jac::Readable](classjac_1_1Readable.md)

| Type | Name |
| ---: | :--- |
| virtual bool | [**get**](classjac_1_1Readable.md#function-get) (std::function&lt; void(char)&gt; callback) = 0<br> |
| virtual bool | [**read**](classjac_1_1Readable.md#function-read) (std::function&lt; void(std::string)&gt; callback) = 0<br> |
| virtual  | [**~Readable**](classjac_1_1Readable.md#function-readable) () = default<br> |






















































## Public Functions Documentation




### function ReadableRef 

```C++
inline jac::ReadableRef::ReadableRef (
    Readable * ptr
) 
```




<hr>



### function get 

```C++
inline virtual bool jac::ReadableRef::get (
    std::function< void(char)> callback
) override
```



Implements [*jac::Readable::get*](classjac_1_1Readable.md#function-get)


<hr>



### function read 

```C++
inline virtual bool jac::ReadableRef::read (
    std::function< void(std::string)> callback
) override
```



Implements [*jac::Readable::read*](classjac_1_1Readable.md#function-read)


<hr>

------------------------------
The documentation for this class was generated from the following file `src/jac/features/types/streams.h`

