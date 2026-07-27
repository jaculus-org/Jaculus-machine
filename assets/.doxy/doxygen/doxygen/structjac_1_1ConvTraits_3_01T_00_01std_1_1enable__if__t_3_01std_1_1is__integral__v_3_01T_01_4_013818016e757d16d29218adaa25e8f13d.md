

# Struct jac::ConvTraits&lt; T, std::enable\_if\_t&lt; std::is\_integral\_v&lt; T &gt; &&detail::is\_leq\_i32&lt; T &gt;, T &gt; &gt;

**template &lt;typename T&gt;**



[**ClassList**](annotated.md) **>** [**jac**](namespacejac.md) **>** [**ConvTraits&lt; T, std::enable\_if\_t&lt; std::is\_integral\_v&lt; T &gt; &&detail::is\_leq\_i32&lt; T &gt;, T &gt; &gt;**](structjac_1_1ConvTraits_3_01T_00_01std_1_1enable__if__t_3_01std_1_1is__integral__v_3_01T_01_4_013818016e757d16d29218adaa25e8f13d.md)












































## Public Static Functions

| Type | Name |
| ---: | :--- |
|  T | [**from**](#function-from) ([**ContextRef**](classjac_1_1ContextRef.md) ctx, [**ValueWeak**](classjac_1_1ValueWrapper.md) val) <br> |
|  [**Value**](classjac_1_1ValueWrapper.md) | [**to**](#function-to) ([**ContextRef**](classjac_1_1ContextRef.md) ctx, T val) <br> |


























## Public Static Functions Documentation




### function from 

```C++
static inline T jac::ConvTraits< T, std::enable_if_t< std::is_integral_v< T > &&detail::is_leq_i32< T >, T > >::from (
    ContextRef ctx,
    ValueWeak val
) 
```




<hr>



### function to 

```C++
static inline Value jac::ConvTraits< T, std::enable_if_t< std::is_integral_v< T > &&detail::is_leq_i32< T >, T > >::to (
    ContextRef ctx,
    T val
) 
```




<hr>

------------------------------
The documentation for this class was generated from the following file `src/jac/machine/traits.h`

