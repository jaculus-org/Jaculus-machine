

# Class jac::SimpleModuleLoaderFeature

**template &lt;class Next&gt;**



[**ClassList**](annotated.md) **>** [**jac**](namespacejac.md) **>** [**SimpleModuleLoaderFeature**](classjac_1_1SimpleModuleLoaderFeature.md)








Inherits the following classes: Next


































## Public Functions

| Type | Name |
| ---: | :--- |
|  [**Value**](classjac_1_1ValueWrapper.md) | [**evalFile**](#function-evalfile) (std::string path\_) <br>_Evaluate a file._  |
|  void | [**evalFileWithEventLoop**](#function-evalfilewitheventloop) (std::string path\_) <br>_Evaluate a file and run the event loop until the program exits or throws an exception._  |
|  void | [**initialize**](#function-initialize) () <br> |




























## Public Functions Documentation




### function evalFile 

_Evaluate a file._ 
```C++
inline Value jac::SimpleModuleLoaderFeature::evalFile (
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
inline void jac::SimpleModuleLoaderFeature::evalFileWithEventLoop (
    std::string path_
) 
```





**Parameters:**


* `path_` Path to the file 




        

<hr>



### function initialize 

```C++
inline void jac::SimpleModuleLoaderFeature::initialize () 
```




<hr>

------------------------------
The documentation for this class was generated from the following file `src/jac/features/simpleModuleLoaderFeature.h`

