

# File eventQueueFeature.h

[**File List**](files.md) **>** [**features**](dir_6f95e06b732314161804ab1ef73c9681.md) **>** [**eventQueueFeature.h**](eventQueueFeature_8h.md)

[Go to the documentation of this file](eventQueueFeature_8h.md)


```C++
#pragma once

#include <condition_variable>
#include <deque>
#include <functional>
#include <mutex>
#include <optional>


namespace jac {


template<class Next>
class EventQueueFeature : public Next {
private:
    std::deque<std::function<void()>> _scheduledFunctions;
    std::mutex _scheduledFunctionsMutex;
    std::condition_variable _scheduledFunctionsCondition;
public:
    std::optional<std::function<void()>> getEvent(bool wait) {
        std::unique_lock lock(_scheduledFunctionsMutex);
        if (wait && _scheduledFunctions.empty()) {
            _scheduledFunctionsCondition.wait(lock);
        }
        if (_scheduledFunctions.empty()) {
            return std::nullopt;
        }
        auto func = std::move(_scheduledFunctions.front());
        _scheduledFunctions.pop_front();
        lock.unlock();

        return func;
    }

    void scheduleEvent(std::function<void()> func) {
        {
            std::scoped_lock lock(_scheduledFunctionsMutex);
            _scheduledFunctions.push_back(std::move(func));
        }
        _scheduledFunctionsCondition.notify_one();
    }

    void notifyEventLoop() {
        _scheduledFunctionsCondition.notify_one();
    }

    ~EventQueueFeature() {
        notifyEventLoop();
        std::scoped_lock lock(_scheduledFunctionsMutex);
        _scheduledFunctions.clear();
    }
};


} // namespace jac
```


