

# Class jac::NodeModuleLoaderFeature

**template &lt;class Next&gt;**



[**ClassList**](annotated.md) **>** [**jac**](namespacejac.md) **>** [**NodeModuleLoaderFeature**](classjac_1_1NodeModuleLoaderFeature.md)








Inherits the following classes: Next




















## Public Static Attributes

| Type | Name |
| ---: | :--- |
|  const std::set&lt; std::string\_view &gt; | [**defaultConditions**](#variable-defaultconditions)   = `{ "jaculus", "node", "import" }`<br> |














## Public Functions

| Type | Name |
| ---: | :--- |
|  [**Value**](classjac_1_1ValueWrapper.md) | [**evalFile**](#function-evalfile) (std::string path\_) <br>_Evaluate a file._  |
|  void | [**evalFileWithEventLoop**](#function-evalfilewitheventloop) (std::string path\_) <br>_Evaluate a file and run the event loop until the program exits or throws an exception._  |
|  void | [**initialize**](#function-initialize) () <br> |


## Public Static Functions

| Type | Name |
| ---: | :--- |
|  std::string | [**ESM\_RESOLVE**](#function-esm_resolve) (const std::string & specifier, [**NodeModuleLoaderFeature**](classjac_1_1NodeModuleLoaderFeature.md)&lt; Next &gt; & self) <br> |
|  std::string | [**PACKAGE\_EXPORTS\_RESOLVE**](#function-package_exports_resolve) (const std::string & packageURL, const std::string & subpath, [**jac::Value**](classjac_1_1ValueWrapper.md) exports, [**NodeModuleLoaderFeature**](classjac_1_1NodeModuleLoaderFeature.md)&lt; Next &gt; & self) <br> |
|  std::optional&lt; std::string &gt; | [**PACKAGE\_IMPORTS\_EXPORTS\_RESOLVE**](#function-package_imports_exports_resolve) (const std::string & matchKey, [**jac::Object**](classjac_1_1ObjectWrapper.md) matchObj, const std::string & packageURL, [**NodeModuleLoaderFeature**](classjac_1_1NodeModuleLoaderFeature.md)&lt; Next &gt; & self) <br> |
|  std::string | [**PACKAGE\_RESOLVE**](#function-package_resolve) (const std::string & packageSpecifier, [**NodeModuleLoaderFeature**](classjac_1_1NodeModuleLoaderFeature.md)&lt; Next &gt; & self) <br> |
|  std::optional&lt; std::string &gt; | [**PACKAGE\_SELF\_RESOLVE**](#function-package_self_resolve) (const std::string & packageName, const std::string & packageSubpath, [**NodeModuleLoaderFeature**](classjac_1_1NodeModuleLoaderFeature.md)&lt; Next &gt; & self) <br> |
|  std::optional&lt; std::string &gt; | [**PACKAGE\_TARGET\_RESOLVE**](#function-package_target_resolve) (const std::string & packageURL, [**jac::Value**](classjac_1_1ValueWrapper.md) target, std::optional&lt; std::string &gt; patternMatch, [**NodeModuleLoaderFeature**](classjac_1_1NodeModuleLoaderFeature.md)&lt; Next &gt; & self) <br> |
|  [**jac::Object**](classjac_1_1ObjectWrapper.md) | [**READ\_PACKAGE\_JSON**](#function-read_package_json) (const std::string & packageURL, [**NodeModuleLoaderFeature**](classjac_1_1NodeModuleLoaderFeature.md)&lt; Next &gt; & self) <br> |


























## Public Static Attributes Documentation




### variable defaultConditions 

```C++
const std::set<std::string_view> jac::NodeModuleLoaderFeature< Next >::defaultConditions;
```




<hr>
## Public Functions Documentation




### function evalFile 

_Evaluate a file._ 
```C++
inline Value jac::NodeModuleLoaderFeature::evalFile (
    std::string path_
) 
```





**Parameters:**


* `path_` Path to the file 



**Returns:**

A promise that will be resolved when the module ends, and rejected if the module throws an exception. 





        

<hr>



### function evalFileWithEventLoop 

_Evaluate a file and run the event loop until the program exits or throws an exception._ 
```C++
inline void jac::NodeModuleLoaderFeature::evalFileWithEventLoop (
    std::string path_
) 
```





**Parameters:**


* `path_` Path to the file 




        

<hr>



### function initialize 

```C++
inline void jac::NodeModuleLoaderFeature::initialize () 
```




<hr>
## Public Static Functions Documentation




### function ESM\_RESOLVE 

```C++
static inline std::string jac::NodeModuleLoaderFeature::ESM_RESOLVE (
    const std::string & specifier,
    NodeModuleLoaderFeature < Next > & self
) 
```




<hr>



### function PACKAGE\_EXPORTS\_RESOLVE 

```C++
static inline std::string jac::NodeModuleLoaderFeature::PACKAGE_EXPORTS_RESOLVE (
    const std::string & packageURL,
    const std::string & subpath,
    jac::Value exports,
    NodeModuleLoaderFeature < Next > & self
) 
```




<hr>



### function PACKAGE\_IMPORTS\_EXPORTS\_RESOLVE 

```C++
static inline std::optional< std::string > jac::NodeModuleLoaderFeature::PACKAGE_IMPORTS_EXPORTS_RESOLVE (
    const std::string & matchKey,
    jac::Object matchObj,
    const std::string & packageURL,
    NodeModuleLoaderFeature < Next > & self
) 
```




<hr>



### function PACKAGE\_RESOLVE 

```C++
static inline std::string jac::NodeModuleLoaderFeature::PACKAGE_RESOLVE (
    const std::string & packageSpecifier,
    NodeModuleLoaderFeature < Next > & self
) 
```




<hr>



### function PACKAGE\_SELF\_RESOLVE 

```C++
static inline std::optional< std::string > jac::NodeModuleLoaderFeature::PACKAGE_SELF_RESOLVE (
    const std::string & packageName,
    const std::string & packageSubpath,
    NodeModuleLoaderFeature < Next > & self
) 
```




<hr>



### function PACKAGE\_TARGET\_RESOLVE 

```C++
static inline std::optional< std::string > jac::NodeModuleLoaderFeature::PACKAGE_TARGET_RESOLVE (
    const std::string & packageURL,
    jac::Value target,
    std::optional< std::string > patternMatch,
    NodeModuleLoaderFeature < Next > & self
) 
```




<hr>



### function READ\_PACKAGE\_JSON 

```C++
static inline jac::Object jac::NodeModuleLoaderFeature::READ_PACKAGE_JSON (
    const std::string & packageURL,
    NodeModuleLoaderFeature < Next > & self
) 
```




<hr>

------------------------------
The documentation for this class was generated from the following file `src/jac/features/nodeModuleLoaderFeature.h`

