

# Class jac::FilesystemFeature

**template &lt;class Next&gt;**



[**ClassList**](annotated.md) **>** [**jac**](namespacejac.md) **>** [**FilesystemFeature**](classjac_1_1FilesystemFeature.md)








Inherits the following classes: Next












## Classes

| Type | Name |
| ---: | :--- |
| class | [**Path**](classjac_1_1FilesystemFeature_1_1Path.md) <br> |


## Public Types

| Type | Name |
| ---: | :--- |
| typedef [**Class**](classjac_1_1Class.md)&lt; [**FileProtoBuilder**](structjac_1_1FileProtoBuilder.md) &gt; | [**FileClass**](#typedef-fileclass)  <br> |




## Public Attributes

| Type | Name |
| ---: | :--- |
|  Fs | [**fs**](#variable-fs)  <br> |
|  [**Path**](classjac_1_1FilesystemFeature_1_1Path.md) | [**path**](#variable-path)  <br> |
















## Public Functions

| Type | Name |
| ---: | :--- |
|   | [**FilesystemFeature**](#function-filesystemfeature) () <br> |
|  void | [**initialize**](#function-initialize) () <br> |
|  void | [**setCodeDir**](#function-setcodedir) (std::string path\_) <br> |
|  void | [**setWorkingDir**](#function-setworkingdir) (std::string path\_) <br> |




























## Public Types Documentation




### typedef FileClass 

```C++
using jac::FilesystemFeature< Next >::FileClass =  Class<FileProtoBuilder>;
```




<hr>
## Public Attributes Documentation




### variable fs 

```C++
Fs jac::FilesystemFeature< Next >::fs;
```




<hr>



### variable path 

```C++
Path jac::FilesystemFeature< Next >::path;
```




<hr>
## Public Functions Documentation




### function FilesystemFeature 

```C++
inline jac::FilesystemFeature::FilesystemFeature () 
```




<hr>



### function initialize 

```C++
inline void jac::FilesystemFeature::initialize () 
```




<hr>



### function setCodeDir 

```C++
inline void jac::FilesystemFeature::setCodeDir (
    std::string path_
) 
```




<hr>



### function setWorkingDir 

```C++
inline void jac::FilesystemFeature::setWorkingDir (
    std::string path_
) 
```




<hr>

------------------------------
The documentation for this class was generated from the following file `src/jac/features/filesystemFeature.h`

