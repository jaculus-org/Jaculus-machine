

# Namespace jac::detail



[**Namespace List**](namespaces.md) **>** [**jac**](namespacejac.md) **>** [**detail**](namespacejac_1_1detail.md)




















## Classes

| Type | Name |
| ---: | :--- |
| class | [**CallableBase**](classjac_1_1detail_1_1CallableBase.md) <br> |
| class | [**CallableHolder**](classjac_1_1detail_1_1CallableHolder.md) &lt;typename Func, typename Signature, withThis, variadic&gt;<br> |
| class | [**CallableHolder&lt; Func, std::function&lt; Res(Args...)&gt;, false, false &gt;**](classjac_1_1detail_1_1CallableHolder_3_01Func_00_01std_1_1function_3_01Res_07Args_8_8_8_08_4_00_01false_00_01false_01_4.md) &lt;typename Func, typename Res, Args&gt;<br> |
| class | [**CallableHolder&lt; Func, std::function&lt; Res(ContextRef, ValueWeak, Args...)&gt;, true, false &gt;**](classjac_1_1detail_1_1CallableHolder_3_01Func_00_01std_1_1function_3_01Res_07ContextRef_00_01Valdffb9d51e028f8e1cdf9f888a2d43a1c.md) &lt;typename Func, typename Res, Args&gt;<br> |
| class | [**CallableHolder&lt; Func, std::function&lt; Res(ContextRef, ValueWeak, ValueVectorWeak)&gt;, true, true &gt;**](classjac_1_1detail_1_1CallableHolder_3_01Func_00_01std_1_1function_3_01Res_07ContextRef_00_01Valba2213dbfb9eb8737d81218b674334ab.md) &lt;typename Func, typename Res&gt;<br> |
| class | [**CallableHolder&lt; Func, std::function&lt; Res(ValueVectorWeak)&gt;, false, true &gt;**](classjac_1_1detail_1_1CallableHolder_3_01Func_00_01std_1_1function_3_01Res_07ValueVectorWeak_08_4_00_01false_00_01true_01_4.md) &lt;typename Func, typename Res&gt;<br> |
| struct | [**CallableProtoBuilder**](structjac_1_1detail_1_1CallableProtoBuilder.md) <br> |
| struct | [**is\_base\_of\_template\_impl**](structjac_1_1detail_1_1is__base__of__template__impl.md) &lt;Base, typename Derived&gt;<br> |


## Public Types

| Type | Name |
| ---: | :--- |
| typedef [**Class**](classjac_1_1Class.md)&lt; [**CallableProtoBuilder**](structjac_1_1detail_1_1CallableProtoBuilder.md) &gt; | [**CallableClass**](#typedef-callableclass)  <br> |




## Public Attributes

| Type | Name |
| ---: | :--- |
|  constexpr bool | [**is\_leq\_i32**](#variable-is_leq_i32)   = `/* multi line expression */`<br> |












































## Public Types Documentation




### typedef CallableClass 

```C++
using jac::detail::CallableClass = typedef Class<CallableProtoBuilder>;
```




<hr>
## Public Attributes Documentation




### variable is\_leq\_i32 

```C++
constexpr bool jac::detail::is_leq_i32;
```




<hr>

------------------------------
The documentation for this class was generated from the following file `src/jac/machine/class.h`

