

# Struct jac::detail::is\_base\_of\_template\_impl

**template &lt;template&lt; typename... &gt; class Base, typename Derived&gt;**



[**ClassList**](annotated.md) **>** [**jac**](namespacejac.md) **>** [**detail**](namespacejac_1_1detail.md) **>** [**is\_base\_of\_template\_impl**](structjac_1_1detail_1_1is__base__of__template__impl.md)




















## Classes

| Type | Name |
| ---: | :--- |
| struct | [**check**](structjac_1_1detail_1_1is__base__of__template__impl_1_1check.md) &lt;A, class Void&gt;<br> |
| struct | [**check&lt; A, std::void\_t&lt; A&lt; Derived &gt; &gt; &gt;**](structjac_1_1detail_1_1is__base__of__template__impl_1_1check_3_01A_00_01std_1_1void__t_3_01A_3_01Derived_01_4_01_4_01_4.md) &lt;A&gt;<br> |


## Public Types

| Type | Name |
| ---: | :--- |
| typedef decltype(is\_callable(std::declval&lt; T \* &gt;())) | [**is\_callable\_t**](#typedef-is_callable_t)  <br> |
| typedef [**check**](structjac_1_1detail_1_1is__base__of__template__impl_1_1check.md)&lt; is\_callable\_t &gt; | [**value\_t**](#typedef-value_t)  <br> |






















## Public Static Functions

| Type | Name |
| ---: | :--- |
|  constexpr void | [**is\_callable**](#function-is_callable) (Base&lt; Ts... &gt; \*) <br> |


























## Public Types Documentation




### typedef is\_callable\_t 

```C++
using jac::detail::is_base_of_template_impl< Base, Derived >::is_callable_t =  decltype(is_callable(std::declval<T*>()));
```




<hr>



### typedef value\_t 

```C++
using jac::detail::is_base_of_template_impl< Base, Derived >::value_t =  check<is_callable_t>;
```




<hr>
## Public Static Functions Documentation




### function is\_callable 

```C++
template<typename... Ts>
static constexpr void jac::detail::is_base_of_template_impl::is_callable (
    Base< Ts... > *
) 
```




<hr>

------------------------------
The documentation for this class was generated from the following file `src/jac/machine/class.h`

