

# Class jac::detail::CallableHolder&lt; Func, std::function&lt; Res(ValueVectorWeak)&gt;, false, true &gt;

**template &lt;typename Func, typename Res&gt;**



[**ClassList**](annotated.md) **>** [**jac**](namespacejac.md) **>** [**detail**](namespacejac_1_1detail.md) **>** [**CallableHolder&lt; Func, std::function&lt; Res(ValueVectorWeak)&gt;, false, true &gt;**](classjac_1_1detail_1_1CallableHolder_3_01Func_00_01std_1_1function_3_01Res_07ValueVectorWeak_08_4_00_01false_00_01true_01_4.md)








Inherits the following classes: [jac::detail::CallableBase](classjac_1_1detail_1_1CallableBase.md)






















































## Public Functions

| Type | Name |
| ---: | :--- |
|   | [**CallableHolder**](#function-callableholder) (Func func) <br> |
| virtual [**Value**](classjac_1_1ValueWrapper.md) | [**invoke**](#function-invoke) ([**ContextRef**](classjac_1_1ContextRef.md) ctx, [**ValueWeak**](classjac_1_1ValueWrapper.md) thisVal, [**ValueVectorWeak**](classjac_1_1ValueVectorWeak.md) args) override<br> |


## Public Functions inherited from jac::detail::CallableBase

See [jac::detail::CallableBase](classjac_1_1detail_1_1CallableBase.md)

| Type | Name |
| ---: | :--- |
| virtual [**Value**](classjac_1_1ValueWrapper.md) | [**invoke**](classjac_1_1detail_1_1CallableBase.md#function-invoke) ([**ContextRef**](classjac_1_1ContextRef.md) ctx, [**ValueWeak**](classjac_1_1ValueWrapper.md) thisVal, [**ValueVectorWeak**](classjac_1_1ValueVectorWeak.md) args) = 0<br> |
| virtual  | [**~CallableBase**](classjac_1_1detail_1_1CallableBase.md#function-callablebase) () = default<br> |






















































## Public Functions Documentation




### function CallableHolder 

```C++
inline explicit jac::detail::CallableHolder< Func, std::function< Res(ValueVectorWeak)>, false, true >::CallableHolder (
    Func func
) 
```




<hr>



### function invoke 

```C++
inline virtual Value jac::detail::CallableHolder< Func, std::function< Res(ValueVectorWeak)>, false, true >::invoke (
    ContextRef ctx,
    ValueWeak thisVal,
    ValueVectorWeak args
) override
```



Implements [*jac::detail::CallableBase::invoke*](classjac_1_1detail_1_1CallableBase.md#function-invoke)


<hr>

------------------------------
The documentation for this class was generated from the following file `src/jac/machine/functionFactory.h`

