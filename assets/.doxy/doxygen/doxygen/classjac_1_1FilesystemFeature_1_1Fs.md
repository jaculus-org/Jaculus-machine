

# Class jac::FilesystemFeature::Fs



[**ClassList**](annotated.md) **>** [**Fs**](classjac_1_1FilesystemFeature_1_1Fs.md)










































## Public Functions

| Type | Name |
| ---: | :--- |
|   | [**Fs**](#function-fs) ([**FilesystemFeature**](classjac_1_1FilesystemFeature.md) & feature) <br> |
|  bool | [**exists**](#function-exists) (std::string path\_) <br> |
|  bool | [**existsCode**](#function-existscode) (std::string path\_) <br> |
|  bool | [**isDirectory**](#function-isdirectory) (std::string path\_) <br> |
|  bool | [**isDirectoryCode**](#function-isdirectorycode) (std::string path\_) <br> |
|  bool | [**isFile**](#function-isfile) (std::string path\_) <br> |
|  bool | [**isFileCode**](#function-isfilecode) (std::string path\_) <br> |
|  std::string | [**loadCode**](#function-loadcode) (std::string filename) <br> |
|  void | [**mkdir**](#function-mkdir) (std::string path\_) <br> |
|  [**File**](classjac_1_1File.md) | [**open**](#function-open) (std::string path\_, std::string flags) <br> |
|  std::vector&lt; std::string &gt; | [**readdir**](#function-readdir) (std::string path\_) <br> |
|  void | [**rm**](#function-rm) (std::string path\_) <br> |
|  void | [**rmdir**](#function-rmdir) (std::string path\_) <br> |




























## Public Functions Documentation




### function Fs 

```C++
inline Fs::Fs (
    FilesystemFeature & feature
) 
```




<hr>



### function exists 

```C++
inline bool Fs::exists (
    std::string path_
) 
```




<hr>



### function existsCode 

```C++
inline bool Fs::existsCode (
    std::string path_
) 
```




<hr>



### function isDirectory 

```C++
inline bool Fs::isDirectory (
    std::string path_
) 
```




<hr>



### function isDirectoryCode 

```C++
inline bool Fs::isDirectoryCode (
    std::string path_
) 
```




<hr>



### function isFile 

```C++
inline bool Fs::isFile (
    std::string path_
) 
```




<hr>



### function isFileCode 

```C++
inline bool Fs::isFileCode (
    std::string path_
) 
```




<hr>



### function loadCode 

```C++
inline std::string Fs::loadCode (
    std::string filename
) 
```




<hr>



### function mkdir 

```C++
inline void Fs::mkdir (
    std::string path_
) 
```




<hr>



### function open 

```C++
inline File Fs::open (
    std::string path_,
    std::string flags
) 
```




<hr>



### function readdir 

```C++
inline std::vector< std::string > Fs::readdir (
    std::string path_
) 
```




<hr>



### function rm 

```C++
inline void Fs::rm (
    std::string path_
) 
```




<hr>



### function rmdir 

```C++
inline void Fs::rmdir (
    std::string path_
) 
```




<hr>

------------------------------
The documentation for this class was generated from the following file `src/jac/features/filesystemFeature.h`

