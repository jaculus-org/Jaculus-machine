

# Class jac::detail::CallableBase



[**ClassList**](annotated.md) **>** [**jac**](namespacejac.md) **>** [**detail**](namespacejac_1_1detail.md) **>** [**CallableBase**](classjac_1_1detail_1_1CallableBase.md)










Inherited by the following classes: [jac::detail::CallableHolder&lt; Func, std::function&lt; Res(Args...)&gt;, false, false &gt;](classjac_1_1detail_1_1CallableHolder_3_01Func_00_01std_1_1function_3_01Res_07Args_8_8_8_08_4_00_01false_00_01false_01_4.md),  [jac::detail::CallableHolder&lt; Func, std::function&lt; Res(ContextRef, ValueWeak, Args...)&gt;, true, false &gt;](classjac_1_1detail_1_1CallableHolder_3_01Func_00_01std_1_1function_3_01Res_07ContextRef_00_01Valdffb9d51e028f8e1cdf9f888a2d43a1c.md),  [jac::detail::CallableHolder&lt; Func, std::function&lt; Res(ContextRef, ValueWeak, ValueVectorWeak)&gt;, true, true &gt;](classjac_1_1detail_1_1CallableHolder_3_01Func_00_01std_1_1function_3_01Res_07ContextRef_00_01Valba2213dbfb9eb8737d81218b674334ab.md),  [jac::detail::CallableHolder&lt; Func, std::function&lt; Res(ValueVectorWeak)&gt;, false, true &gt;](classjac_1_1detail_1_1CallableHolder_3_01Func_00_01std_1_1function_3_01Res_07ValueVectorWeak_08_4_00_01false_00_01true_01_4.md)
































## Public Functions

| Type | Name |
| ---: | :--- |
| virtual [**Value**](classjac_1_1ValueWrapper.md) | [**invoke**](#function-invoke) ([**ContextRef**](classjac_1_1ContextRef.md) ctx, [**ValueWeak**](classjac_1_1ValueWrapper.md) thisVal, [**ValueVectorWeak**](classjac_1_1ValueVectorWeak.md) args) = 0<br> |
| virtual  | [**~CallableBase**](#function-callablebase) () = default<br> |




























## Public Functions Documentation




### function invoke 

```C++
virtual Value jac::detail::CallableBase::invoke (
    ContextRef ctx,
    ValueWeak thisVal,
    ValueVectorWeak args
) = 0
```




<hr>



### function ~CallableBase 

```C++
virtual jac::detail::CallableBase::~CallableBase () = default
```




<hr>

------------------------------
The documentation for this class was generated from the following file `src/jac/machine/functionFactory.h`

