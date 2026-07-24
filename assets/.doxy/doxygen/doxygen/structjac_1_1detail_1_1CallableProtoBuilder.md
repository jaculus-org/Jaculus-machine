

# Struct jac::detail::CallableProtoBuilder



[**ClassList**](annotated.md) **>** [**jac**](namespacejac.md) **>** [**detail**](namespacejac_1_1detail.md) **>** [**CallableProtoBuilder**](structjac_1_1detail_1_1CallableProtoBuilder.md)








Inherits the following classes: [jac::ProtoBuilder::Opaque](structjac_1_1ProtoBuilder_1_1Opaque.md),  [jac::ProtoBuilder::Callable](structjac_1_1ProtoBuilder_1_1Callable.md)
















## Public Types inherited from jac::ProtoBuilder::Opaque

See [jac::ProtoBuilder::Opaque](structjac_1_1ProtoBuilder_1_1Opaque.md)

| Type | Name |
| ---: | :--- |
| typedef [**T**](structjac_1_1ProtoBuilder_1_1Opaque.md#function-addmethodmember) | [**OpaqueType**](structjac_1_1ProtoBuilder_1_1Opaque.md#typedef-opaquetype)  <br> |


















## Public Static Attributes inherited from jac::ProtoBuilder::Opaque

See [jac::ProtoBuilder::Opaque](structjac_1_1ProtoBuilder_1_1Opaque.md)

| Type | Name |
| ---: | :--- |
|  [**JSClassID**](structjac_1_1ProtoBuilder_1_1Opaque.md#function-addmethodmember) | [**classId**](structjac_1_1ProtoBuilder_1_1Opaque.md#variable-classid)  <br> |














































## Public Static Functions

| Type | Name |
| ---: | :--- |
|  [**Value**](classjac_1_1ValueWrapper.md) | [**callFunction**](#function-callfunction) ([**ContextRef**](classjac_1_1ContextRef.md) ctx, [**ValueWeak**](classjac_1_1ValueWrapper.md) funcObj, [**ValueWeak**](classjac_1_1ValueWrapper.md) thisVal, [**ValueVectorWeak**](classjac_1_1ValueVectorWeak.md) args) <br> |


## Public Static Functions inherited from jac::ProtoBuilder::Opaque

See [jac::ProtoBuilder::Opaque](structjac_1_1ProtoBuilder_1_1Opaque.md)

| Type | Name |
| ---: | :--- |
|  void | [**addMethodMember**](structjac_1_1ProtoBuilder_1_1Opaque.md#function-addmethodmember) ([**ContextRef**](classjac_1_1ContextRef.md) ctx, [**Object**](classjac_1_1ObjectWrapper.md) proto, std::string name, PropFlags flags=PropFlags::Default) <br>_Add a property to the object prototype from a member function of the wrapped class._  |
|  [**void**](structjac_1_1ProtoBuilder_1_1Opaque.md#function-addmethodmember) | [**addPropMember**](structjac_1_1ProtoBuilder_1_1Opaque.md#function-addpropmember) ([**ContextRef**](classjac_1_1ContextRef.md) ctx, [**Object**](classjac_1_1ObjectWrapper.md) proto, std::string name, PropFlags flags=PropFlags::Default) <br>_Add a property to the object prototype from a member variable of the wrapped class._  |
|  [**Value**](classjac_1_1ValueWrapper.md) | [**callMember**](structjac_1_1ProtoBuilder_1_1Opaque.md#function-callmember) ([**ContextRef**](classjac_1_1ContextRef.md) ctx, [**ValueWeak**](classjac_1_1ValueWrapper.md) funcObj, [**ValueWeak**](classjac_1_1ValueWrapper.md) thisVal, [**ValueVectorWeak**](classjac_1_1ValueVectorWeak.md) argv) <br>_Process a call to a member function of the wrapped class._  |
|  [**T**](structjac_1_1ProtoBuilder_1_1Opaque.md#function-addmethodmember) \* | [**constructOpaque**](structjac_1_1ProtoBuilder_1_1Opaque.md#function-constructopaque) ([**ContextRef**](classjac_1_1ContextRef.md), [**ValueVectorWeak**](classjac_1_1ValueVectorWeak.md)) <br>_Construct a new_ [_**Opaque**_](structjac_1_1ProtoBuilder_1_1Opaque.md) _object from javascript arguments._ |
|  [**void**](structjac_1_1ProtoBuilder_1_1Opaque.md#function-addmethodmember) | [**destroyOpaque**](structjac_1_1ProtoBuilder_1_1Opaque.md#function-destroyopaque) ([**JSRuntime**](structjac_1_1ProtoBuilder_1_1Opaque.md#function-addmethodmember) \*, [**T**](structjac_1_1ProtoBuilder_1_1Opaque.md#function-addmethodmember) \* ptr) noexcept<br>_Destroy the_ [_**Opaque**_](structjac_1_1ProtoBuilder_1_1Opaque.md) _object._ |
|  [**T**](structjac_1_1ProtoBuilder_1_1Opaque.md#function-addmethodmember) \* | [**getOpaque**](structjac_1_1ProtoBuilder_1_1Opaque.md#function-getopaque) ([**ContextRef**](classjac_1_1ContextRef.md) ctx, [**ValueWeak**](classjac_1_1ValueWrapper.md) thisVal) <br>_Get the_ [_**Opaque**_](structjac_1_1ProtoBuilder_1_1Opaque.md) _object from an instance of the class._ |


## Public Static Functions inherited from jac::ProtoBuilder::Callable

See [jac::ProtoBuilder::Callable](structjac_1_1ProtoBuilder_1_1Callable.md)

| Type | Name |
| ---: | :--- |
|  [**Value**](classjac_1_1ValueWrapper.md) | [**callConstructor**](structjac_1_1ProtoBuilder_1_1Callable.md#function-callconstructor) ([**ContextRef**](classjac_1_1ContextRef.md), [**ValueWeak**](classjac_1_1ValueWrapper.md), [**ValueWeak**](classjac_1_1ValueWrapper.md), [**ValueVectorWeak**](classjac_1_1ValueVectorWeak.md)) <br>_Process a call to the wrapped class as a constructor._  |
|  [**Value**](classjac_1_1ValueWrapper.md) | [**callFunction**](structjac_1_1ProtoBuilder_1_1Callable.md#function-callfunction) ([**ContextRef**](classjac_1_1ContextRef.md), [**ValueWeak**](classjac_1_1ValueWrapper.md), [**ValueWeak**](classjac_1_1ValueWrapper.md), [**ValueVectorWeak**](classjac_1_1ValueVectorWeak.md)) <br>_Process a call to the wrapped class._  |










































































## Public Static Functions Documentation




### function callFunction 

```C++
static inline Value jac::detail::CallableProtoBuilder::callFunction (
    ContextRef ctx,
    ValueWeak funcObj,
    ValueWeak thisVal,
    ValueVectorWeak args
) 
```




<hr>

------------------------------
The documentation for this class was generated from the following file `src/jac/machine/functionFactory.h`

