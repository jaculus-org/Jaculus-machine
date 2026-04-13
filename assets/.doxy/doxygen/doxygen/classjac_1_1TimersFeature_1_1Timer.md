

# Class jac::TimersFeature::Timer



[**ClassList**](annotated.md) **>** [**Timer**](classjac_1_1TimersFeature_1_1Timer.md)










































## Public Functions

| Type | Name |
| ---: | :--- |
|   | [**Timer**](#function-timer) (std::function&lt; void()&gt; callback, std::chrono::milliseconds duration, int id, bool isRepeating=false) <br> |
|  void | [**cancel**](#function-cancel) () <br> |
|  std::function&lt; void()&gt; | [**getCallback**](#function-getcallback) () <br> |
|  std::chrono::time\_point&lt; std::chrono::steady\_clock &gt; | [**getEndTime**](#function-getendtime) () const<br> |
|  int | [**getId**](#function-getid) () const<br> |
|  bool | [**isCancelled**](#function-iscancelled) () const<br> |
|  bool | [**isRepeating**](#function-isrepeating) () const<br> |
|  bool | [**operator&lt;**](#function-operator) (const Timer & other) const<br> |
|  void | [**update**](#function-update) () <br> |




























## Public Functions Documentation




### function Timer 

```C++
inline Timer::Timer (
    std::function< void()> callback,
    std::chrono::milliseconds duration,
    int id,
    bool isRepeating=false
) 
```




<hr>



### function cancel 

```C++
inline void Timer::cancel () 
```




<hr>



### function getCallback 

```C++
inline std::function< void()> Timer::getCallback () 
```




<hr>



### function getEndTime 

```C++
inline std::chrono::time_point< std::chrono::steady_clock > Timer::getEndTime () const
```




<hr>



### function getId 

```C++
inline int Timer::getId () const
```




<hr>



### function isCancelled 

```C++
inline bool Timer::isCancelled () const
```




<hr>



### function isRepeating 

```C++
inline bool Timer::isRepeating () const
```




<hr>



### function operator&lt; 

```C++
inline bool Timer::operator< (
    const Timer & other
) const
```




<hr>



### function update 

```C++
inline void Timer::update () 
```




<hr>

------------------------------
The documentation for this class was generated from the following file `src/jac/features/timersFeature.h`

