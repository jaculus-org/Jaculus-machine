

# Class jac::EventQueueFeature

**template &lt;class Next&gt;**



[**ClassList**](annotated.md) **>** [**jac**](namespacejac.md) **>** [**EventQueueFeature**](classjac_1_1EventQueueFeature.md)








Inherits the following classes: Next


































## Public Functions

| Type | Name |
| ---: | :--- |
|  std::optional&lt; std::function&lt; void()&gt; &gt; | [**getEvent**](#function-getevent) (bool wait) <br>_Check the event queue and return the first event._  |
|  void | [**notifyEventLoop**](#function-notifyeventloop) () <br>_Wake up event loop if it is waiting for events._  |
|  void | [**scheduleEvent**](#function-scheduleevent) (std::function&lt; void()&gt; func) <br>_Schedule an event to be run._  |
|   | [**~EventQueueFeature**](#function-eventqueuefeature) () <br> |




























## Public Functions Documentation




### function getEvent 

_Check the event queue and return the first event._ 
```C++
inline std::optional< std::function< void()> > jac::EventQueueFeature::getEvent (
    bool wait
) 
```





**Parameters:**


* `wait` Wait for event if no event is available 



**Returns:**

Event or std::nullopt if no event is available 





        

<hr>



### function notifyEventLoop 

_Wake up event loop if it is waiting for events._ 
```C++
inline void jac::EventQueueFeature::notifyEventLoop () 
```




<hr>



### function scheduleEvent 

_Schedule an event to be run._ 
```C++
inline void jac::EventQueueFeature::scheduleEvent (
    std::function< void()> func
) 
```





**Parameters:**


* `func` Function to be run 




        

<hr>



### function ~EventQueueFeature 

```C++
inline jac::EventQueueFeature::~EventQueueFeature () 
```




<hr>

------------------------------
The documentation for this class was generated from the following file `src/jac/features/eventQueueFeature.h`

