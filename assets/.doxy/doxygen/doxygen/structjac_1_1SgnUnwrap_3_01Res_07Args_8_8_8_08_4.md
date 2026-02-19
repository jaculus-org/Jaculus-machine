

# Struct jac::SgnUnwrap&lt; Res(Args...)&gt;

**template &lt;typename Res, typename... Args&gt;**



[**ClassList**](annotated.md) **>** [**jac**](namespacejac.md) **>** [**SgnUnwrap&lt; Res(Args...)&gt;**](structjac_1_1SgnUnwrap_3_01Res_07Args_8_8_8_08_4.md)






















## Public Types

| Type | Name |
| ---: | :--- |
| typedef std::tuple&lt; Args... &gt; | [**ArgTypes**](#typedef-argtypes)  <br> |
| typedef Res | [**ResType**](#typedef-restype)  <br> |




















## Public Functions

| Type | Name |
| ---: | :--- |
|   | [**SgnUnwrap**](#function-sgnunwrap-12) (Res(Class::\*)(Args...)) <br> |
|   | [**SgnUnwrap**](#function-sgnunwrap-22) (Res(Class::\*)(Args...) const) <br> |




























## Public Types Documentation




### typedef ArgTypes 

```C++
using jac::SgnUnwrap< Res(Args...)>::ArgTypes =  std::tuple<Args...>;
```




<hr>



### typedef ResType 

```C++
using jac::SgnUnwrap< Res(Args...)>::ResType =  Res;
```




<hr>
## Public Functions Documentation




### function SgnUnwrap [1/2]

```C++
template<class Class>
inline jac::SgnUnwrap< Res(Args...)>::SgnUnwrap (
    Res(Class::*)(Args...)
) 
```




<hr>



### function SgnUnwrap [2/2]

```C++
template<class Class>
inline jac::SgnUnwrap< Res(Args...)>::SgnUnwrap (
    Res(Class::*)(Args...) const
) 
```




<hr>

------------------------------
The documentation for this class was generated from the following file `src/jac/machine/class.h`

