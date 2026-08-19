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
add_test_executable(moduleLoader)
add_test_executable(machineBase)
add_test_executable(values)
add_test_executable(functionFactory)
add_test_executable(class)
add_test_executable(plugins)

add_test_executable(scanner)
add_test_executable(parser)

add_test_executable(regression)

add_executable(bench bench.cpp)
target_link_libraries(bench PUBLIC jac-machine Catch2::Catch2WithMain)

add_executable(bytecodeGen bytecodeGen.cpp)
target_link_libraries(bytecodeGen PUBLIC jac-machine Catch2::Catch2WithMain)

add_executable(bytecodeDecompile bytecodeDecompile.cpp)
target_link_libraries(bytecodeDecompile PUBLIC jac-machine Catch2::Catch2WithMain)

add_executable(bcRun bcRun.cpp)
target_link_libraries(bcRun PUBLIC jac-machine Catch2::Catch2WithMain)

add_executable(run-quickjs run-quickjs.cpp)
target_link_libraries(run-quickjs PUBLIC jac-machine)

add_executable(run-cfg2bc run-cfg2bc.cpp)
target_link_libraries(run-cfg2bc PUBLIC jac-machine)

add_executable(run-cfg-interp run-cfg-interp.cpp)
target_link_libraries(run-cfg-interp PUBLIC jac-machine)

find_package(Python3 COMPONENTS Interpreter)

function(add_runner_target name)
    add_custom_target(run-${name}-tests
        COMMAND ${Python3_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/test262/runner.py
            --executable $<TARGET_FILE:run-${name}>
            --test-list ${CMAKE_CURRENT_SOURCE_DIR}/test-lists/${name}.yaml
        WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
        DEPENDS run-${name}
        VERBATIM
    )
    if(NOT "${ARGV1}" STREQUAL "NO_TEST")
        add_test(NAME ${name}-tests
            COMMAND ${Python3_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/test262/runner.py
                --executable $<TARGET_FILE:run-${name}>
                --test-list ${CMAKE_CURRENT_SOURCE_DIR}/test-lists/${name}.yaml
            WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
        )
    endif()
endfunction()

add_runner_target(quickjs)
add_runner_target(cfg2bc)
add_runner_target(cfg-interp)

file(COPY fixtures DESTINATION "./")
