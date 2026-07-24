

# Class jac::File



[**ClassList**](annotated.md) **>** [**jac**](namespacejac.md) **>** [**File**](classjac_1_1File.md)


























## Public Attributes

| Type | Name |
| ---: | :--- |
|  std::string | [**path\_**](#variable-path_)  <br> |
















## Public Functions

| Type | Name |
| ---: | :--- |
|   | [**File**](#function-file-12) (std::string path, std::string flags) <br> |
|   | [**File**](#function-file-22) (std::filesystem::path path, std::string flags) <br> |
|  void | [**close**](#function-close) () <br> |
|  bool | [**isOpen**](#function-isopen) () <br> |
|  std::string | [**read**](#function-read) (int length=1024) <br> |
|  void | [**write**](#function-write) (std::string data) <br> |
|   | [**~File**](#function-file) () <br> |




























## Public Attributes Documentation




### variable path\_ 

```C++
std::string jac::File::path_;
```




<hr>
## Public Functions Documentation




### function File [1/2]

```C++
inline jac::File::File (
    std::string path,
    std::string flags
) 
```




<hr>



### function File [2/2]

```C++
inline jac::File::File (
    std::filesystem::path path,
    std::string flags
) 
```




<hr>



### function close 

```C++
inline void jac::File::close () 
```




<hr>



### function isOpen 

```C++
inline bool jac::File::isOpen () 
```




<hr>



### function read 

```C++
inline std::string jac::File::read (
    int length=1024
) 
```




<hr>



### function write 

```C++
inline void jac::File::write (
    std::string data
) 
```




<hr>



### function ~File 

```C++
inline jac::File::~File () 
```




<hr>

------------------------------
The documentation for this class was generated from the following file `src/jac/features/types/file.h`

