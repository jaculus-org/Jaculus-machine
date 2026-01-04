cmake_minimum_required(VERSION 3.0)

Include(FetchContent)
include(CTest)

FetchContent_Declare(
    Catch2
    URL https://github.com/catchorg/Catch2/archive/refs/tags/v3.8.1.tar.gz
)

FetchContent_MakeAvailable(Catch2)


add_executable(main main.cpp)
target_link_libraries(main PUBLIC jac-machine)


function(add_test_executable name)
    add_executable(${name} ${name}.cpp)
    target_link_libraries(${name} PUBLIC jac-machine Catch2::Catch2WithMain)
    add_test(NAME ${name} COMMAND ${EXECUTABLE_OUTPUT_PATH}/${name})
endfunction()

add_test_executable(filesystem)
add_test_executable(eval)

add_executable(simpleModuleLoader moduleLoader.cpp)
target_link_libraries(simpleModuleLoader PUBLIC jac-machine Catch2::Catch2WithMain)
add_test(NAME simpleModuleLoader COMMAND ${EXECUTABLE_OUTPUT_PATH}/simpleModuleLoader)

add_executable(nodeModuleLoader moduleLoader.cpp)
target_compile_definitions(nodeModuleLoader PRIVATE USE_NODE_MODULE_LOADER)
target_link_libraries(nodeModuleLoader PUBLIC jac-machine Catch2::Catch2WithMain)
add_test(NAME nodeModuleLoader COMMAND ${EXECUTABLE_OUTPUT_PATH}/nodeModuleLoader)

add_test_executable(nodeModuleLoaderExtended)

add_test_executable(machineBase)
add_test_executable(values)
add_test_executable(functionFactory)
add_test_executable(class)
add_test_executable(plugins)
add_test_executable(regression)

file(COPY test_files DESTINATION "./")
