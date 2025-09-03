cmake_minimum_required(VERSION 3.0)

set(JAC_MACHINE_SRC
    "jac/machine/machine.cpp"
    "jac/machine/context.cpp"
)


if(ESP_PLATFORM)
    idf_component_register(
        SRCS ${JAC_MACHINE_SRC}
        INCLUDE_DIRS .
    )
endif()


Include(FetchContent)
FetchContent_Declare(
    QuickJS
    URL               https://github.com/bellard/quickjs/archive/19abf1888db5884a5758036ff6e7fa2b340acedc.tar.gz
    PATCH_COMMAND     ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_SOURCE_DIR}/quickjs.CMakeLists.txt <SOURCE_DIR>/CMakeLists.txt
    CMAKE_ARGS        -DESP_PLATFORM=${ESP_PLATFORM}
)
FetchContent_MakeAvailable(QuickJS)


if(ESP_PLATFORM)
    target_link_libraries(${COMPONENT_LIB} PUBLIC quickjs)
else()

    add_library(jac-machine STATIC ${JAC_MACHINE_SRC})
    target_include_directories(jac-machine PUBLIC .)
    set_target_properties(jac-machine PROPERTIES LINKER_LANGUAGE CXX)
    target_link_libraries(jac-machine PUBLIC quickjs)
    target_compile_options(jac-machine PUBLIC -Wall -Wextra -Wold-style-cast -Wshadow -Wimplicit-fallthrough -Wno-unused-parameter -Wno-cast-function-type -Wno-missing-field-initializers -Wno-old-style-cast)

endif()
