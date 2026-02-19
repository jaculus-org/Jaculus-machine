

# Class jac::Readable



[**ClassList**](annotated.md) **>** [**jac**](namespacejac.md) **>** [**Readable**](classjac_1_1Readable.md)










Inherited by the following classes: [jac::ReadableRef](classjac_1_1ReadableRef.md)
































## Public Functions

| Type | Name |
| ---: | :--- |
| virtual bool | [**get**](#function-get) (std::function&lt; void(char)&gt; callback) = 0<br> |
| virtual bool | [**read**](#function-read) (std::function&lt; void(std::string)&gt; callback) = 0<br> |
| virtual  | [**~Readable**](#function-readable) () = default<br> |




























## Public Functions Documentation




### function get 

```C++
virtual bool jac::Readable::get (
    std::function< void(char)> callback
) = 0
```




<hr>



### function read 

```C++
virtual bool jac::Readable::read (
    std::function< void(std::string)> callback
) = 0
```




<hr>



### function ~Readable 

```C++
virtual jac::Readable::~Readable () = default
```




<hr>

------------------------------
The documentation for this class was generated from the following file `src/jac/features/types/streams.h`

