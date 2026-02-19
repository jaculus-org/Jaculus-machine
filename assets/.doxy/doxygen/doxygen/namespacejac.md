

# Namespace jac



[**Namespace List**](namespaces.md) **>** [**jac**](namespacejac.md)


















## Namespaces

| Type | Name |
| ---: | :--- |
| namespace | [**ProtoBuilder**](namespacejac_1_1ProtoBuilder.md) <br> |
| namespace | [**detail**](namespacejac_1_1detail.md) <br> |


## Classes

| Type | Name |
| ---: | :--- |
| class | [**ArrayBufferWrapper**](classjac_1_1ArrayBufferWrapper.md) &lt;managed&gt;<br>_A wrapper for JSValue with ArrayBuffer type with RAII._  |
| class | [**ArrayWrapper**](classjac_1_1ArrayWrapper.md) &lt;managed&gt;<br>_A wrapper for JSValue with Array type with RAII._  |
| class | [**Atom**](classjac_1_1Atom.md) <br>_A wrapper around JSAtom with RAII. In the context of QuickJS,_ [_**Atom**_](classjac_1_1Atom.md) _is used to represent identifiers of properties, variables, functions, etc._ |
| class | [**BasicStreamFeature**](classjac_1_1BasicStreamFeature.md) &lt;class Next&gt;<br> |
| class | [**Class**](classjac_1_1Class.md) &lt;class Builder&gt;<br> |
| struct | [**ComposeMachine**](structjac_1_1ComposeMachine.md) &lt;class Base, MFeatures&gt;<br> |
| struct | [**ComposeMachine&lt; Base &gt;**](structjac_1_1ComposeMachine_3_01Base_01_4.md) &lt;class Base&gt;<br> |
| struct | [**ComposeMachine&lt; Base, FirstFeature, MFeatures... &gt;**](structjac_1_1ComposeMachine_3_01Base_00_01FirstFeature_00_01MFeatures_8_8_8_01_4.md) &lt;class Base, FirstFeature, MFeatures&gt;<br> |
| class | [**ContextRef**](classjac_1_1ContextRef.md) <br>_A wrapper around JSContext\* providing some related functionality._  |
| struct | [**ConvTraits**](structjac_1_1ConvTraits.md) &lt;typename T, typename En&gt;<br> |
| struct | [**ConvTraits&lt; T, std::enable\_if\_t&lt; std::is\_integral\_v&lt; T &gt; &&detail::is\_leq\_i32&lt; T &gt;, T &gt; &gt;**](structjac_1_1ConvTraits_3_01T_00_01std_1_1enable__if__t_3_01std_1_1is__integral__v_3_01T_01_4_013818016e757d16d29218adaa25e8f13d.md) &lt;typename T&gt;<br> |
| struct | [**ConvTraits&lt; bool &gt;**](structjac_1_1ConvTraits_3_01bool_01_4.md) &lt;&gt;<br> |
| struct | [**ConvTraits&lt; std::chrono::milliseconds &gt;**](structjac_1_1ConvTraits_3_01std_1_1chrono_1_1milliseconds_01_4.md) &lt;&gt;<br> |
| class | [**EventLoopFeature**](classjac_1_1EventLoopFeature.md) &lt;class Next&gt;<br> |
| class | [**EventLoopTerminal**](classjac_1_1EventLoopTerminal.md) &lt;class Next&gt;<br> |
| class | [**EventQueueFeature**](classjac_1_1EventQueueFeature.md) &lt;class Next&gt;<br> |
| class | [**ExceptionWrapper**](classjac_1_1ExceptionWrapper.md) &lt;managed&gt;<br>_An exception wrapper which can either wrap a JSValue or contain an exception description and can be thrown into JS as a specific Error type._  |
| class | [**File**](classjac_1_1File.md) <br> |
| struct | [**FileProtoBuilder**](structjac_1_1FileProtoBuilder.md) <br> |
| class | [**FilesystemFeature**](classjac_1_1FilesystemFeature.md) &lt;class Next&gt;<br> |
| class | [**FunctionFactory**](classjac_1_1FunctionFactory.md) <br>_Various methods for wrapping C++ functions into javascript functions._  |
| class | [**FunctionWrapper**](classjac_1_1FunctionWrapper.md) &lt;managed&gt;<br>_A wrapper for JSValue with Function type with RAII._  |
| class | [**MachineBase**](classjac_1_1MachineBase.md) <br> |
| class | [**Module**](classjac_1_1Module.md) <br>_A wrapper around JSModuleDef that allows for easy exporting of values._  |
| class | [**NodeModuleLoaderFeature**](classjac_1_1NodeModuleLoaderFeature.md) &lt;class Next&gt;<br> |
| class | [**ObjectWrapper**](classjac_1_1ObjectWrapper.md) &lt;managed&gt;<br>_A wrapper for JSValue with Object type with RAII._  |
| class | [**OsWritable**](classjac_1_1OsWritable.md) <br> |
| class | [**Plugin**](classjac_1_1Plugin.md) <br>_A base class for all plugins._  |
| class | [**PluginHandle**](classjac_1_1PluginHandle.md) <br>_A handle which can be used to retrieve a plugin from a machine._  |
| class | [**PluginHolderFeature**](classjac_1_1PluginHolderFeature.md) &lt;typename Next&gt;<br>_An MFeature that allows for inserting plugins into the machine and retrieving them using PluginHandeles._  |
| class | [**PluginManager**](classjac_1_1PluginManager.md) <br>_A class for managing groups of plugins and initializing them all at once._  |
| class | [**PromiseWrapper**](classjac_1_1PromiseWrapper.md) &lt;managed&gt;<br>_A wrapper for JSValue with Promise type with RAII._  |
| class | [**Readable**](classjac_1_1Readable.md) <br> |
| struct | [**ReadableProtoBuilder**](structjac_1_1ReadableProtoBuilder.md) <br> |
| class | [**ReadableRef**](classjac_1_1ReadableRef.md) <br> |
| struct | [**SgnUnwrap**](structjac_1_1SgnUnwrap.md) &lt;typename Sgn&gt;<br> |
| struct | [**SgnUnwrap&lt; Res(Args...)&gt;**](structjac_1_1SgnUnwrap_3_01Res_07Args_8_8_8_08_4.md) &lt;typename Res, Args&gt;<br> |
| class | [**SimpleModuleLoaderFeature**](classjac_1_1SimpleModuleLoaderFeature.md) &lt;class Next&gt;<br> |
| class | [**StdioFeature**](classjac_1_1StdioFeature.md) &lt;class Next&gt;<br> |
| class | [**StringView**](classjac_1_1StringView.md) <br>_A wrapper around QuickJS C-string with automatic memory management._  |
| class | [**TimersFeature**](classjac_1_1TimersFeature.md) &lt;class Next&gt;<br> |
| class | [**ValueWrapper**](classjac_1_1ValueWrapper.md) &lt;managed&gt;<br>_A wrapper around JSValue with RAII._  |
| class | [**Writable**](classjac_1_1Writable.md) <br> |
| struct | [**WritableProtoBuilder**](structjac_1_1WritableProtoBuilder.md) <br> |
| class | [**WritableRef**](classjac_1_1WritableRef.md) <br> |
| struct | [**is\_base\_of\_template**](structjac_1_1is__base__of__template.md) &lt;Base, typename Derived&gt;<br>_Checks if a type is derived from a template class._  |


## Public Types

| Type | Name |
| ---: | :--- |
| typedef [**ArrayWrapper**](classjac_1_1ArrayWrapper.md)&lt; true &gt; | [**Array**](#typedef-array)  <br> |
| typedef [**ArrayBufferWrapper**](classjac_1_1ArrayBufferWrapper.md)&lt; true &gt; | [**ArrayBuffer**](#typedef-arraybuffer)  <br> |
| typedef [**ArrayBufferWrapper**](classjac_1_1ArrayBufferWrapper.md)&lt; false &gt; | [**ArrayBufferWeak**](#typedef-arraybufferweak)  <br> |
| typedef [**ArrayWrapper**](classjac_1_1ArrayWrapper.md)&lt; false &gt; | [**ArrayWeak**](#typedef-arrayweak)  <br> |
| enum int | [**EvalFlags**](#enum-evalflags)  <br> |
| typedef [**ExceptionWrapper**](classjac_1_1ExceptionWrapper.md)&lt; true &gt; | [**Exception**](#typedef-exception)  <br> |
| typedef [**ExceptionWrapper**](classjac_1_1ExceptionWrapper.md)&lt; false &gt; | [**ExceptionWeak**](#typedef-exceptionweak)  <br> |
| typedef [**FunctionWrapper**](classjac_1_1FunctionWrapper.md)&lt; true &gt; | [**Function**](#typedef-function)  <br> |
| typedef [**FunctionWrapper**](classjac_1_1FunctionWrapper.md)&lt; false &gt; | [**FunctionWeak**](#typedef-functionweak)  <br> |
| typedef [**ObjectWrapper**](classjac_1_1ObjectWrapper.md)&lt; true &gt; | [**Object**](#typedef-object)  <br> |
| typedef [**ObjectWrapper**](classjac_1_1ObjectWrapper.md)&lt; false &gt; | [**ObjectWeak**](#typedef-objectweak)  <br> |
| typedef [**PromiseWrapper**](classjac_1_1PromiseWrapper.md)&lt; true &gt; | [**Promise**](#typedef-promise)  <br> |
| typedef [**PromiseWrapper**](classjac_1_1PromiseWrapper.md)&lt; false &gt; | [**PromiseWeak**](#typedef-promiseweak)  <br> |
| enum int | [**PropFlags**](#enum-propflags)  <br>_Flags to specify property access attributes._  |
| typedef [**ValueWrapper**](classjac_1_1ValueWrapper.md)&lt; true &gt; | [**Value**](#typedef-value)  <br> |
| typedef [**ValueWrapper**](classjac_1_1ValueWrapper.md)&lt; false &gt; | [**ValueWeak**](#typedef-valueweak)  <br> |
| typedef typename [**is\_base\_of\_template**](structjac_1_1is__base__of__template.md)&lt; Base, Derived &gt;::type | [**is\_base\_of\_template\_t**](#typedef-is_base_of_template_t)  <br> |




## Public Attributes

| Type | Name |
| ---: | :--- |
|  constexpr bool | [**is\_base\_of\_template\_v**](#variable-is_base_of_template_v)   = `[**is\_base\_of\_template**](structjac_1_1is__base__of__template.md)&lt;Base, Derived&gt;::value`<br> |
















## Public Functions

| Type | Name |
| ---: | :--- |
|   | [**SgnUnwrap**](#function-sgnunwrap) (Res(Class::\*)(Args...)) <br> |
|  std::tuple&lt; Args... &gt; | [**convertArgs**](#function-convertargs) ([**ContextRef**](classjac_1_1ContextRef.md) ctx, std::vector&lt; [**ValueWeak**](classjac_1_1ValueWrapper.md) &gt; argv, std::index\_sequence&lt; Is... &gt;) <br> |
|  std::tuple&lt; Args... &gt; | [**convertArgs**](#function-convertargs) ([**ContextRef**](classjac_1_1ContextRef.md) ctx, JSValueConst \* argv, int argc, std::index\_sequence&lt; Is... &gt;) <br> |
|  T | [**fromValue**](#function-fromvalue) ([**ContextRef**](classjac_1_1ContextRef.md) ctx, [**ValueWeak**](classjac_1_1ValueWrapper.md) val) <br> |
|  constexpr EvalFlags | [**operator&**](#function-operator) (EvalFlags a, EvalFlags b) <br> |
|  constexpr PropFlags | [**operator&**](#function-operator_1) (PropFlags a, PropFlags b) <br> |
|  constexpr EvalFlags | [**operator\|**](#function-operator_2) (EvalFlags a, EvalFlags b) <br> |
|  constexpr PropFlags | [**operator\|**](#function-operator_3) (PropFlags a, PropFlags b) <br> |
|  [**Value**](classjac_1_1ValueWrapper.md) | [**processCall**](#function-processcall) ([**ContextRef**](classjac_1_1ContextRef.md) ctx, [**ValueWeak**](classjac_1_1ValueWrapper.md), std::vector&lt; [**ValueWeak**](classjac_1_1ValueWrapper.md) &gt; argv, Func & f) <br> |
|  JSValue | [**processCallRaw**](#function-processcallraw) ([**ContextRef**](classjac_1_1ContextRef.md) ctx, JSValueConst, int argc, JSValueConst \* argv, Func & f) <br> |
|  [**Value**](classjac_1_1ValueWrapper.md) | [**processCallThis**](#function-processcallthis) ([**ContextRef**](classjac_1_1ContextRef.md) ctx, [**ValueWeak**](classjac_1_1ValueWrapper.md) thisVal, std::vector&lt; [**ValueWeak**](classjac_1_1ValueWrapper.md) &gt; argv, Func & f) <br> |
|  [**Value**](classjac_1_1ValueWrapper.md) | [**processCallThisVariadic**](#function-processcallthisvariadic) ([**ContextRef**](classjac_1_1ContextRef.md) ctx, [**ValueWeak**](classjac_1_1ValueWrapper.md) thisVal, std::vector&lt; [**ValueWeak**](classjac_1_1ValueWrapper.md) &gt; argv, Func & f) <br> |
|  [**Value**](classjac_1_1ValueWrapper.md) | [**processCallVariadic**](#function-processcallvariadic) ([**ContextRef**](classjac_1_1ContextRef.md) ctx, [**ValueWeak**](classjac_1_1ValueWrapper.md), std::vector&lt; [**ValueWeak**](classjac_1_1ValueWrapper.md) &gt; argv, Func & f) <br> |
|  JSValue | [**processCallVariadicRaw**](#function-processcallvariadicraw) ([**ContextRef**](classjac_1_1ContextRef.md) ctx, JSValueConst, int argc, JSValueConst \* argv, Func & f) <br> |
|  JSValue | [**propagateExceptions**](#function-propagateexceptions) ([**ContextRef**](classjac_1_1ContextRef.md) ctx, Func & f) noexcept<br> |
|  JSValue | [**propagateExceptions**](#function-propagateexceptions) ([**ContextRef**](classjac_1_1ContextRef.md) ctx, Func && f) noexcept<br> |
|  constexpr bool | [**static\_false**](#function-static_false) () <br> |
|  bool | [**strContains**](#function-strcontains) (const auto & str, const auto & substr) <br> |
|  std::string\_view | [**substring**](#function-substring) (const std::string & str, size\_t pos, size\_t count=std::string::npos) <br> |
|  [**Value**](classjac_1_1ValueWrapper.md) | [**toValue**](#function-tovalue) ([**ContextRef**](classjac_1_1ContextRef.md) ctx, T val) <br> |


## Public Static Functions

| Type | Name |
| ---: | :--- |
|  void | [**initializeIo**](#function-initializeio) (Machine & machine) <br> |


























## Public Types Documentation




### typedef Array 

```C++
using jac::Array = typedef ArrayWrapper<true>;
```




<hr>



### typedef ArrayBuffer 

```C++
using jac::ArrayBuffer = typedef ArrayBufferWrapper<true>;
```




<hr>



### typedef ArrayBufferWeak 

```C++
using jac::ArrayBufferWeak = typedef ArrayBufferWrapper<false>;
```




<hr>



### typedef ArrayWeak 

```C++
using jac::ArrayWeak = typedef ArrayWrapper<false>;
```




<hr>



### enum EvalFlags 

```C++
enum jac::EvalFlags {
    Global = JS_EVAL_TYPE_GLOBAL,
    Module = JS_EVAL_TYPE_MODULE,
    Strict = JS_EVAL_FLAG_STRICT,
    CompileOnly = JS_EVAL_FLAG_COMPILE_ONLY,
    BacktraceBarrier = JS_EVAL_FLAG_BACKTRACE_BARRIER
};
```




<hr>



### typedef Exception 

```C++
using jac::Exception = typedef ExceptionWrapper<true>;
```




<hr>



### typedef ExceptionWeak 

```C++
using jac::ExceptionWeak = typedef ExceptionWrapper<false>;
```




<hr>



### typedef Function 

```C++
using jac::Function = typedef FunctionWrapper<true>;
```




<hr>



### typedef FunctionWeak 

```C++
using jac::FunctionWeak = typedef FunctionWrapper<false>;
```




<hr>



### typedef Object 

```C++
using jac::Object = typedef ObjectWrapper<true>;
```




<hr>



### typedef ObjectWeak 

```C++
using jac::ObjectWeak = typedef ObjectWrapper<false>;
```




<hr>



### typedef Promise 

```C++
using jac::Promise = typedef PromiseWrapper<true>;
```




<hr>



### typedef PromiseWeak 

```C++
using jac::PromiseWeak = typedef PromiseWrapper<false>;
```




<hr>



### enum PropFlags 

_Flags to specify property access attributes._ 
```C++
enum jac::PropFlags {
    Default = 0,
    Configurable = JS_PROP_CONFIGURABLE,
    Writable = JS_PROP_WRITABLE,
    Enumerable = JS_PROP_ENUMERABLE,
    C_W_E = JS_PROP_C_W_E
};
```




<hr>



### typedef Value 

```C++
using jac::Value = typedef ValueWrapper<true>;
```




<hr>



### typedef ValueWeak 

```C++
using jac::ValueWeak = typedef ValueWrapper<false>;
```




<hr>



### typedef is\_base\_of\_template\_t 

```C++
using jac::is_base_of_template_t = typedef typename is_base_of_template<Base, Derived>::type;
```




<hr>
## Public Attributes Documentation




### variable is\_base\_of\_template\_v 

```C++
constexpr bool jac::is_base_of_template_v;
```




<hr>
## Public Functions Documentation




### function SgnUnwrap 

```C++
template<class Class, typename Res, typename... Args>
jac::SgnUnwrap (
    Res(Class::*)(Args...)
) 
```




<hr>



### function convertArgs 

```C++
template<typename... Args, std::size_t... Is>
inline std::tuple< Args... > jac::convertArgs (
    ContextRef ctx,
    std::vector< ValueWeak > argv,
    std::index_sequence< Is... >
) 
```




<hr>



### function convertArgs 

```C++
template<typename... Args, std::size_t... Is>
inline std::tuple< Args... > jac::convertArgs (
    ContextRef ctx,
    JSValueConst * argv,
    int argc,
    std::index_sequence< Is... >
) 
```




<hr>



### function fromValue 

```C++
template<typename T>
T jac::fromValue (
    ContextRef ctx,
    ValueWeak val
) 
```




<hr>



### function operator& 

```C++
inline constexpr EvalFlags jac::operator& (
    EvalFlags a,
    EvalFlags b
) 
```




<hr>



### function operator& 

```C++
inline constexpr PropFlags jac::operator& (
    PropFlags a,
    PropFlags b
) 
```




<hr>



### function operator\| 

```C++
inline constexpr EvalFlags jac::operator| (
    EvalFlags a,
    EvalFlags b
) 
```




<hr>



### function operator\| 

```C++
inline constexpr PropFlags jac::operator| (
    PropFlags a,
    PropFlags b
) 
```




<hr>



### function processCall 

```C++
template<typename Func, typename Res, typename... Args>
inline Value jac::processCall (
    ContextRef ctx,
    ValueWeak,
    std::vector< ValueWeak > argv,
    Func & f
) 
```




<hr>



### function processCallRaw 

```C++
template<typename Func, typename Res, typename... Args>
inline JSValue jac::processCallRaw (
    ContextRef ctx,
    JSValueConst,
    int argc,
    JSValueConst * argv,
    Func & f
) 
```




<hr>



### function processCallThis 

```C++
template<typename Func, typename Res, typename... Args>
inline Value jac::processCallThis (
    ContextRef ctx,
    ValueWeak thisVal,
    std::vector< ValueWeak > argv,
    Func & f
) 
```




<hr>



### function processCallThisVariadic 

```C++
template<typename Func, typename Res>
inline Value jac::processCallThisVariadic (
    ContextRef ctx,
    ValueWeak thisVal,
    std::vector< ValueWeak > argv,
    Func & f
) 
```




<hr>



### function processCallVariadic 

```C++
template<typename Func, typename Res>
inline Value jac::processCallVariadic (
    ContextRef ctx,
    ValueWeak,
    std::vector< ValueWeak > argv,
    Func & f
) 
```




<hr>



### function processCallVariadicRaw 

```C++
template<typename Func, typename Res>
inline JSValue jac::processCallVariadicRaw (
    ContextRef ctx,
    JSValueConst,
    int argc,
    JSValueConst * argv,
    Func & f
) 
```




<hr>



### function propagateExceptions 

```C++
template<typename Func>
inline JSValue jac::propagateExceptions (
    ContextRef ctx,
    Func & f
) noexcept
```



Various functions to process function calls with unprocessed javascript arguments. Arguments and return value of the functions are automatically converted to and from javascript values. Exceptions thrown within the functions are caught and propagated to the javascript side. 


        

<hr>



### function propagateExceptions 

```C++
template<typename Func>
inline JSValue jac::propagateExceptions (
    ContextRef ctx,
    Func && f
) noexcept
```




<hr>



### function static\_false 

```C++
template<typename T>
constexpr bool jac::static_false () 
```




<hr>



### function strContains 

```C++
bool jac::strContains (
    const auto & str,
    const auto & substr
) 
```




<hr>



### function substring 

```C++
std::string_view jac::substring (
    const std::string & str,
    size_t pos,
    size_t count=std::string::npos
) 
```




<hr>



### function toValue 

```C++
template<typename T>
Value jac::toValue (
    ContextRef ctx,
    T val
) 
```




<hr>
## Public Static Functions Documentation




### function initializeIo 

```C++
template<class Machine>
static inline void jac::initializeIo (
    Machine & machine
) 
```




<hr>

------------------------------
The documentation for this class was generated from the following file `src/jac/features/basicStreamFeature.h`

