

# Class jac::MachineBase



[**ClassList**](annotated.md) **>** [**jac**](namespacejac.md) **>** [**MachineBase**](classjac_1_1MachineBase.md)






















## Public Types

| Type | Name |
| ---: | :--- |
| typedef std::function&lt; JSModuleDef \*(JSContext \*ctx, const char \*name, JSValueConst attributes)&gt; | [**FileModuleLoader**](#typedef-filemoduleloader)  <br> |
| typedef std::function&lt; void([**Module**](classjac_1_1Module.md) &)&gt; | [**ModuleBuilder**](#typedef-modulebuilder)  <br> |




















## Public Functions

| Type | Name |
| ---: | :--- |
|   | [**MachineBase**](#function-machinebase-13) () = default<br> |
|   | [**MachineBase**](#function-machinebase-23) (const [**MachineBase**](classjac_1_1MachineBase.md) &) = delete<br> |
|   | [**MachineBase**](#function-machinebase-33) ([**MachineBase**](classjac_1_1MachineBase.md) &&) = delete<br> |
|  [**ContextRef**](classjac_1_1ContextRef.md) | [**context**](#function-context) () <br>_Get the_ [_**ContextRef**_](classjac_1_1ContextRef.md) _for this machine._ |
|  [**Value**](classjac_1_1ValueWrapper.md) | [**eval**](#function-eval) (std::string code, std::string filename, EvalFlags flags=EvalFlags::Global) <br>_Evaluate a string containing javascript code._  |
|  void | [**initialize**](#function-initialize) () <br>_Initialize the machine. Should be called after machine configuration is done and before any interaction with the javascript engine. Other MFeatures in the Machine stack can implement this method to initialize themselves but the first call should be Next::initialize()._  |
|  void | [**interruptRuntime**](#function-interruptruntime) () <br>_Interrupt running javascript code. Execution will be thrown in the javascript as an InterruptError._  |
|  void | [**newModule**](#function-newmodule) (std::string name, ModuleBuilder builder) <br>_Register a new native module. The content of the module is provided by the builder callback, which is run on the first import of the module._  |
|  [**MachineBase**](classjac_1_1MachineBase.md) & | [**operator=**](#function-operator) (const [**MachineBase**](classjac_1_1MachineBase.md) &) = delete<br> |
|  [**MachineBase**](classjac_1_1MachineBase.md) & | [**operator=**](#function-operator_1) ([**MachineBase**](classjac_1_1MachineBase.md) &&) = delete<br> |
|  void | [**resetWatchdog**](#function-resetwatchdog) () <br>_Reset the watchdog timer. This should be called periodically to prevent the watchdog from triggering._  |
|  JSRuntime \* | [**runtime**](#function-runtime) () <br>_Get the JSRuntime\* for this machine._  |
|  void | [**setMallocFunctions**](#function-setmallocfunctions) (const JSMallocFunctions \* fns) <br>_Set custom malloc functions for the QuickJS engine. If the value is nullptr, the default malloc functions will be used._  |
|  void | [**setWatchdogHandler**](#function-setwatchdoghandler) (std::function&lt; bool()&gt; callback) <br>_Set the watchdog callback. The callback will be called when the watchdog timeout has passed since the last reset. If the callback returns true, the runtime will be interrupted._  |
|  void | [**setWatchdogTimeout**](#function-setwatchdogtimeout) (std::chrono::milliseconds timeout) <br>_Set the watchdog timeout. If the timeout is zero, the watchdog is disabled. Otherwise, the watchdog will be called when the timeout has passed since the last reset._  |
| virtual  | [**~MachineBase**](#function-machinebase) () <br> |
























## Protected Functions

| Type | Name |
| ---: | :--- |
|  void | [**setFileModuleLoader**](#function-setfilemoduleloader) (FileModuleLoader loader) <br>_Set the loader used for source-file (non-native) modules._  |




## Public Types Documentation




### typedef FileModuleLoader 

```C++
using jac::MachineBase::FileModuleLoader =  std::function<JSModuleDef*(JSContext* ctx, const char* name, JSValueConst attributes)>;
```




<hr>



### typedef ModuleBuilder 

```C++
using jac::MachineBase::ModuleBuilder =  std::function<void(Module&)>;
```




<hr>
## Public Functions Documentation




### function MachineBase [1/3]

```C++
jac::MachineBase::MachineBase () = default
```




<hr>



### function MachineBase [2/3]

```C++
jac::MachineBase::MachineBase (
    const MachineBase &
) = delete
```




<hr>



### function MachineBase [3/3]

```C++
jac::MachineBase::MachineBase (
    MachineBase &&
) = delete
```




<hr>



### function context 

_Get the_ [_**ContextRef**_](classjac_1_1ContextRef.md) _for this machine._
```C++
inline ContextRef jac::MachineBase::context () 
```





**Returns:**

The [**ContextRef**](classjac_1_1ContextRef.md) 





        

<hr>



### function eval 

_Evaluate a string containing javascript code._ 
```C++
Value jac::MachineBase::eval (
    std::string code,
    std::string filename,
    EvalFlags flags=EvalFlags::Global
) 
```





**Note:**

If the evaluation mode is EvalFlags::Module, the result will be a Promise




**Parameters:**


* `code` the code to evaluate 
* `filename` filename to use for the code. Used for error reporting 
* `flags` flags to evaluate the code with 



**Returns:**

Result of the evaluation 





        

<hr>



### function initialize 

_Initialize the machine. Should be called after machine configuration is done and before any interaction with the javascript engine. Other MFeatures in the Machine stack can implement this method to initialize themselves but the first call should be Next::initialize()._ 
```C++
void jac::MachineBase::initialize () 
```




<hr>



### function interruptRuntime 

_Interrupt running javascript code. Execution will be thrown in the javascript as an InterruptError._ 
```C++
inline void jac::MachineBase::interruptRuntime () 
```




<hr>



### function newModule 

_Register a new native module. The content of the module is provided by the builder callback, which is run on the first import of the module._ 
```C++
void jac::MachineBase::newModule (
    std::string name,
    ModuleBuilder builder
) 
```





**Parameters:**


* `name` name of the module 
* `builder` callback that populates the module's exports on first import 




        

<hr>



### function operator= 

```C++
MachineBase & jac::MachineBase::operator= (
    const MachineBase &
) = delete
```




<hr>



### function operator= 

```C++
MachineBase & jac::MachineBase::operator= (
    MachineBase &&
) = delete
```




<hr>



### function resetWatchdog 

_Reset the watchdog timer. This should be called periodically to prevent the watchdog from triggering._ 
```C++
inline void jac::MachineBase::resetWatchdog () 
```




<hr>



### function runtime 

_Get the JSRuntime\* for this machine._ 
```C++
inline JSRuntime * jac::MachineBase::runtime () 
```





**Returns:**

The JSRuntime\* 





        

<hr>



### function setMallocFunctions 

_Set custom malloc functions for the QuickJS engine. If the value is nullptr, the default malloc functions will be used._ 
```C++
inline void jac::MachineBase::setMallocFunctions (
    const JSMallocFunctions * fns
) 
```





**Parameters:**


* `fns` malloc functions to use 




        

<hr>



### function setWatchdogHandler 

_Set the watchdog callback. The callback will be called when the watchdog timeout has passed since the last reset. If the callback returns true, the runtime will be interrupted._ 
```C++
inline void jac::MachineBase::setWatchdogHandler (
    std::function< bool()> callback
) 
```





**Parameters:**


* `callback` callback to call 




        

<hr>



### function setWatchdogTimeout 

_Set the watchdog timeout. If the timeout is zero, the watchdog is disabled. Otherwise, the watchdog will be called when the timeout has passed since the last reset._ 
```C++
inline void jac::MachineBase::setWatchdogTimeout (
    std::chrono::milliseconds timeout
) 
```





**Parameters:**


* `timeout` watchdog timeout 




        

<hr>



### function ~MachineBase 

```C++
inline virtual jac::MachineBase::~MachineBase () 
```




<hr>
## Protected Functions Documentation




### function setFileModuleLoader 

_Set the loader used for source-file (non-native) modules._ 
```C++
inline void jac::MachineBase::setFileModuleLoader (
    FileModuleLoader loader
) 
```





**Parameters:**


* `loader` the file module loader 




        

<hr>

------------------------------
The documentation for this class was generated from the following file `src/jac/machine/machine.h`

