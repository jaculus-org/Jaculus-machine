

# Class noal::memberconstfuncptr

**template &lt;class Class, typename Res, typename... Args&gt;**



[**ClassList**](annotated.md) **>** [**noal**](namespacenoal.md) **>** [**memberconstfuncptr**](classnoal_1_1memberconstfuncptr.md)










































## Public Functions

| Type | Name |
| ---: | :--- |
|   | [**memberconstfuncptr**](#function-memberconstfuncptr) (Res(Class::\*)(Args...) const func, Class \* self) <br> |
|  Res | [**operator()**](#function-operator) (Args... args) <br> |


## Public Static Functions

| Type | Name |
| ---: | :--- |
|  Res | [**call**](#function-call) (void \* self, Args... args) <br> |


























## Public Functions Documentation




### function memberconstfuncptr 

```C++
inline noal::memberconstfuncptr::memberconstfuncptr (
    Res(Class::*)(Args...) const func,
    Class * self
) 
```




<hr>



### function operator() 

```C++
inline Res noal::memberconstfuncptr::operator() (
    Args... args
) 
```




<hr>
## Public Static Functions Documentation




### function call 

```C++
static inline Res noal::memberconstfuncptr::call (
    void * self,
    Args... args
) 
```




<hr>

------------------------------
The documentation for this class was generated from the following file `src/noal_func.h`

