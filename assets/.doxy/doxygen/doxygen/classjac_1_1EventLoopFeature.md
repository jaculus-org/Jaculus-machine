

# Class jac::EventLoopFeature

**template &lt;class Next&gt;**



[**ClassList**](annotated.md) **>** [**jac**](namespacejac.md) **>** [**EventLoopFeature**](classjac_1_1EventLoopFeature.md)



[More...](#detailed-description)

* `#include <eventLoopFeature.h>`



Inherits the following classes: Next


































## Public Functions

| Type | Name |
| ---: | :--- |
|  void | [**evalModuleWithEventLoop**](#function-evalmodulewitheventloop) (std::string code, std::string filename) <br> |
|  void | [**exit**](#function-exit) (int code) <br> |
|  int | [**getExitCode**](#function-getexitcode) () <br> |
|  void | [**initialize**](#function-initialize) () <br> |
|  void | [**kill**](#function-kill) () <br> |
|  void | [**onEventLoop**](#function-oneventloop) () <br> |
|  void | [**runEventLoop**](#function-runeventloop) () <br> |
| virtual void | [**runOnEventLoop**](#function-runoneventloop) () = 0<br> |








## Protected Attributes

| Type | Name |
| ---: | :--- |
|  std::optional&lt; [**Exception**](classjac_1_1ExceptionWrapper.md) &gt; | [**\_error**](#variable-_error)   = `std::nullopt`<br> |
















## Protected Functions

| Type | Name |
| ---: | :--- |
|  void | [**evalWithEventLoopCommon**](#function-evalwitheventloopcommon) ([**Value**](classjac_1_1ValueWrapper.md) & promise) <br> |




## Detailed Description




**Note:**

The [**EventLoopFeature**](classjac_1_1EventLoopFeature.md) must be companied by [**EventLoopTerminal**](classjac_1_1EventLoopTerminal.md) at the top of the Machine stack. 





    
## Public Functions Documentation




### function evalModuleWithEventLoop 

```C++
inline void jac::EventLoopFeature::evalModuleWithEventLoop (
    std::string code,
    std::string filename
) 
```




<hr>



### function exit 

```C++
inline void jac::EventLoopFeature::exit (
    int code
) 
```




<hr>



### function getExitCode 

```C++
inline int jac::EventLoopFeature::getExitCode () 
```




<hr>



### function initialize 

```C++
inline void jac::EventLoopFeature::initialize () 
```




<hr>



### function kill 

```C++
inline void jac::EventLoopFeature::kill () 
```




<hr>



### function onEventLoop 

```C++
inline void jac::EventLoopFeature::onEventLoop () 
```




<hr>



### function runEventLoop 

```C++
inline void jac::EventLoopFeature::runEventLoop () 
```




<hr>



### function runOnEventLoop 

```C++
virtual void jac::EventLoopFeature::runOnEventLoop () = 0
```




<hr>
## Protected Attributes Documentation




### variable \_error 

```C++
std::optional<Exception> jac::EventLoopFeature< Next >::_error;
```




<hr>
## Protected Functions Documentation




### function evalWithEventLoopCommon 

```C++
inline void jac::EventLoopFeature::evalWithEventLoopCommon (
    Value & promise
) 
```




<hr>

------------------------------
The documentation for this class was generated from the following file `src/jac/features/eventLoopFeature.h`

