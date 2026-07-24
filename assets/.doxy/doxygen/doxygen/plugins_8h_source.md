

# File plugins.h

[**File List**](files.md) **>** [**jac**](dir_256037ad7d0c306238e2bc4f945d341d.md) **>** [**machine**](dir_10e7d6e7bc593e38e57ffe1bab5ed259.md) **>** [**plugins.h**](plugins_8h.md)

[Go to the documentation of this file](plugins_8h.md)


```C++
#pragma once

#include <jac/machine/machine.h>

#include <memory>
#include <vector>


namespace jac {


class Plugin {
public:
    virtual ~Plugin() = default;
};


class PluginHandle {
    size_t _index;
public:
    PluginHandle(size_t index) : _index(index) {}

    size_t index() const {
        return _index;
    }

    PluginHandle& operator++() {
        ++_index;
        return *this;
    }

    PluginHandle operator++(int) {
        PluginHandle copy = *this;
        ++_index;
        return copy;
    }

    PluginHandle operator+(size_t offset) {
        return PluginHandle(_index + offset);
    }

    bool operator==(const PluginHandle& other) const = default;
};


template<typename Next>
class PluginHolderFeature : public Next {
private:
    std::vector<std::unique_ptr<Plugin>> _plugins;
public:
    PluginHolderFeature() = default;

    PluginHandle holdPlugin(std::unique_ptr<Plugin> plugin) {
        _plugins.push_back(std::move(plugin));
        return _plugins.size() - 1;
    }

    template<typename Plugin_t = Plugin>
    Plugin_t& getPlugin(PluginHandle handle) {
        return dynamic_cast<Plugin_t&>(*_plugins[handle.index()]);
    }

    PluginHandle pluginBegin() {
        return 0;
    }

    PluginHandle pluginEnd() {
        return _plugins.size();
    }
};


class PluginManager {
private:
    std::vector<std::unique_ptr<Plugin>(*)(MachineBase&)> _plugins;
public:
    PluginManager() = default;


    template<typename Plugin_t>
    size_t addPlugin() {
        _plugins.push_back(+[](MachineBase& machine) -> std::unique_ptr<Plugin> {
            return std::make_unique<Plugin_t>(machine);
        });

        return _plugins.size() - 1;
    }

    template<typename Machine>
    PluginHandle initialize(Machine& machine) {
        PluginHandle first = machine.pluginBegin();

        for (auto& plugin : _plugins) {
            machine.holdPlugin(plugin(machine));
        }

        return first;
    }
};


} // namespace jac
```


