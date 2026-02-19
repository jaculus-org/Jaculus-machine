
# Class List


Here are the classes, structs, unions and interfaces with brief descriptions:

* **namespace** [**jac**](namespacejac.md)     
    * **class** [**ArrayBufferWrapper**](classjac_1_1ArrayBufferWrapper.md) _A wrapper for JSValue with ArrayBuffer type with RAII._     
    * **class** [**ArrayWrapper**](classjac_1_1ArrayWrapper.md) _A wrapper for JSValue with Array type with RAII._     
    * **class** [**Atom**](classjac_1_1Atom.md) _A wrapper around JSAtom with RAII. In the context of QuickJS,_ [_**Atom**_](classjac_1_1Atom.md) _is used to represent identifiers of properties, variables, functions, etc._    
    * **class** [**BasicStreamFeature**](classjac_1_1BasicStreamFeature.md)     
    * **class** [**Class**](classjac_1_1Class.md)     
    * **struct** [**ComposeMachine**](structjac_1_1ComposeMachine.md) 
    * **struct** [**ComposeMachine&lt; Base &gt;**](structjac_1_1ComposeMachine_3_01Base_01_4.md) 
    * **struct** [**ComposeMachine&lt; Base, FirstFeature, MFeatures... &gt;**](structjac_1_1ComposeMachine_3_01Base_00_01FirstFeature_00_01MFeatures_8_8_8_01_4.md) 
    * **class** [**ContextRef**](classjac_1_1ContextRef.md) _A wrapper around JSContext\* providing some related functionality._     
    * **struct** [**ConvTraits**](structjac_1_1ConvTraits.md)     
    * **struct** [**ConvTraits&lt; T, std::enable\_if\_t&lt; std::is\_integral\_v&lt; T &gt; &&detail::is\_leq\_i32&lt; T &gt;, T &gt; &gt;**](structjac_1_1ConvTraits_3_01T_00_01std_1_1enable__if__t_3_01std_1_1is__integral__v_3_01T_01_4_013818016e757d16d29218adaa25e8f13d.md)     
    * **struct** [**ConvTraits&lt; bool &gt;**](structjac_1_1ConvTraits_3_01bool_01_4.md)     
    * **struct** [**ConvTraits&lt; std::chrono::milliseconds &gt;**](structjac_1_1ConvTraits_3_01std_1_1chrono_1_1milliseconds_01_4.md)     
    * **class** [**EventLoopFeature**](classjac_1_1EventLoopFeature.md)     
    * **class** [**EventLoopTerminal**](classjac_1_1EventLoopTerminal.md)     
    * **class** [**EventQueueFeature**](classjac_1_1EventQueueFeature.md)     
    * **class** [**ExceptionWrapper**](classjac_1_1ExceptionWrapper.md) _An exception wrapper which can either wrap a JSValue or contain an exception description and can be thrown into JS as a specific Error type._     
    * **class** [**File**](classjac_1_1File.md)     
    * **struct** [**FileProtoBuilder**](structjac_1_1FileProtoBuilder.md)     
    * **class** [**FilesystemFeature**](classjac_1_1FilesystemFeature.md)     
        * **class** [**Path**](classjac_1_1FilesystemFeature_1_1Path.md)     
    * **class** [**FunctionFactory**](classjac_1_1FunctionFactory.md) _Various methods for wrapping C++ functions into javascript functions._     
    * **class** [**FunctionWrapper**](classjac_1_1FunctionWrapper.md) _A wrapper for JSValue with Function type with RAII._     
    * **class** [**MachineBase**](classjac_1_1MachineBase.md)     
    * **class** [**Module**](classjac_1_1Module.md) _A wrapper around JSModuleDef that allows for easy exporting of values._     
    * **class** [**NodeModuleLoaderFeature**](classjac_1_1NodeModuleLoaderFeature.md)     
    * **class** [**ObjectWrapper**](classjac_1_1ObjectWrapper.md) _A wrapper for JSValue with Object type with RAII._     
    * **class** [**OsWritable**](classjac_1_1OsWritable.md)     
    * **class** [**Plugin**](classjac_1_1Plugin.md) _A base class for all plugins._     
    * **class** [**PluginHandle**](classjac_1_1PluginHandle.md) _A handle which can be used to retrieve a plugin from a machine._     
    * **class** [**PluginHolderFeature**](classjac_1_1PluginHolderFeature.md) _An MFeature that allows for inserting plugins into the machine and retrieving them using PluginHandeles._     
    * **class** [**PluginManager**](classjac_1_1PluginManager.md) _A class for managing groups of plugins and initializing them all at once._     
    * **class** [**PromiseWrapper**](classjac_1_1PromiseWrapper.md) _A wrapper for JSValue with Promise type with RAII._     
    * **namespace** [**ProtoBuilder**](namespacejac_1_1ProtoBuilder.md)     
        * **struct** [**Callable**](structjac_1_1ProtoBuilder_1_1Callable.md) _A base class for javascript classes with callable instances._     
        * **struct** [**LifetimeHandles**](structjac_1_1ProtoBuilder_1_1LifetimeHandles.md) _A base class used to add handles for lifetime events of an instance._     
        * **struct** [**Opaque**](structjac_1_1ProtoBuilder_1_1Opaque.md) _A base class for javascript classes with opaque data._     
        * **struct** [**Properties**](structjac_1_1ProtoBuilder_1_1Properties.md) _A base class for javascript classes with added properties._     
    * **class** [**Readable**](classjac_1_1Readable.md)     
    * **struct** [**ReadableProtoBuilder**](structjac_1_1ReadableProtoBuilder.md)     
    * **class** [**ReadableRef**](classjac_1_1ReadableRef.md)     
    * **struct** [**SgnUnwrap**](structjac_1_1SgnUnwrap.md) 
    * **struct** [**SgnUnwrap&lt; Res(Args...)&gt;**](structjac_1_1SgnUnwrap_3_01Res_07Args_8_8_8_08_4.md)     
    * **class** [**SimpleModuleLoaderFeature**](classjac_1_1SimpleModuleLoaderFeature.md)     
    * **class** [**StdioFeature**](classjac_1_1StdioFeature.md)     
    * **class** [**StringView**](classjac_1_1StringView.md) _A wrapper around QuickJS C-string with automatic memory management._     
    * **class** [**TimersFeature**](classjac_1_1TimersFeature.md)     
    * **class** [**ValueWrapper**](classjac_1_1ValueWrapper.md) _A wrapper around JSValue with RAII._     
    * **class** [**Writable**](classjac_1_1Writable.md)     
    * **struct** [**WritableProtoBuilder**](structjac_1_1WritableProtoBuilder.md)     
    * **class** [**WritableRef**](classjac_1_1WritableRef.md)     
    * **namespace** [**detail**](namespacejac_1_1detail.md)     
        * **struct** [**is\_base\_of\_template\_impl**](structjac_1_1detail_1_1is__base__of__template__impl.md)     
            * **struct** [**check**](structjac_1_1detail_1_1is__base__of__template__impl_1_1check.md) 
            * **struct** [**check&lt; A, std::void\_t&lt; A&lt; Derived &gt; &gt; &gt;**](structjac_1_1detail_1_1is__base__of__template__impl_1_1check_3_01A_00_01std_1_1void__t_3_01A_3_01Derived_01_4_01_4_01_4.md) 
    * **struct** [**is\_base\_of\_template**](structjac_1_1is__base__of__template.md) _Checks if a type is derived from a template class._ 
* **class** [**Fs**](classjac_1_1FilesystemFeature_1_1Fs.md)     
* **class** [**Stdio**](classjac_1_1StdioFeature_1_1Stdio.md)     
* **class** [**CompareTimer**](classjac_1_1TimersFeature_1_1CompareTimer.md)     
* **class** [**Timer**](classjac_1_1TimersFeature_1_1Timer.md)     
* **namespace** [**noal**](namespacenoal.md)     
    * **class** [**callableany**](classnoal_1_1callableany.md) 
    * **class** [**callableany&lt; Func, Res(Args...)&gt;**](classnoal_1_1callableany_3_01Func_00_01Res_07Args_8_8_8_08_4.md)     
    * **class** [**funcptr**](classnoal_1_1funcptr.md)     
    * **class** [**function**](classnoal_1_1function.md) 
    * **class** [**function&lt; Res(Args...), dataSize &gt;**](classnoal_1_1function_3_01Res_07Args_8_8_8_08_00_01dataSize_01_4.md)     
    * **class** [**memberconstfuncptr**](classnoal_1_1memberconstfuncptr.md)     
    * **class** [**memberfuncptr**](classnoal_1_1memberfuncptr.md)     
    * **struct** [**signatureHelper**](structnoal_1_1signatureHelper.md) 
    * **struct** [**signatureHelper&lt; Res(Func::\*)(Args...) & &gt;**](structnoal_1_1signatureHelper_3_01Res_07Func_1_1_5_08_07Args_8_8_8_08_01_6_01_4.md)     
    * **struct** [**signatureHelper&lt; Res(Func::\*)(Args...) const & &gt;**](structnoal_1_1signatureHelper_3_01Res_07Func_1_1_5_08_07Args_8_8_8_08_01const_01_6_01_4.md)     
    * **struct** [**signatureHelper&lt; Res(Func::\*)(Args...) const &gt;**](structnoal_1_1signatureHelper_3_01Res_07Func_1_1_5_08_07Args_8_8_8_08_01const_01_4.md)     
    * **struct** [**signatureHelper&lt; Res(Func::\*)(Args...)&gt;**](structnoal_1_1signatureHelper_3_01Res_07Func_1_1_5_08_07Args_8_8_8_08_4.md)     
* **namespace** [**std**](namespacestd.md) 

