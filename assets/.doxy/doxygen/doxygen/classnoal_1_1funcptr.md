

# Class noal::funcptr

**template &lt;typename Res, typename... Args&gt;**



[**ClassList**](annotated.md) **>** [**noal**](namespacenoal.md) **>** [**funcptr**](classnoal_1_1funcptr.md)










































## Public Functions

| Type | Name |
| ---: | :--- |
|   | [**funcptr**](#function-funcptr) (Res(\*)(Args...) func) <br> |
|  Res | [**operator()**](#function-operator) (Args... args) <br> |


## Public Static Functions

| Type | Name |
| ---: | :--- |
|  Res | [**call**](#function-call) (void \* self, Args... args) <br> |


























## Public Functions Documentation




### function funcptr 

```C++
inline noal::funcptr::funcptr (
    Res(*)(Args...) func
) 
```




<hr>



### function operator() 

```C++
inline Res noal::funcptr::operator() (
    Args... args
) 
```




<hr>
## Public Static Functions Documentation




### function call 

```C++
static inline Res noal::funcptr::call (
    void * self,
    Args... args
) 
```




<hr>

------------------------------
The documentation for this class was generated from the following file `src/noal_func.h`

