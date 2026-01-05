cmake_minimum_required(VERSION 3.0)

set(JAC_MACHINE_SRC
    "jac/machine/machine.cpp"
    "jac/machine/context.cpp"
    "jac/machine/compiler/ast.cpp"
    "jac/machine/compiler/cfg.cpp"
    "jac/machine/compiler/ast2cfg.cpp"
    "jac/machine/compiler/cfg2mir.cpp"
    "jac/machine/compiler/mirUtil.cpp"
)


if(ESP_PLATFORM)
    idf_component_register(
        SRCS ${JAC_MACHINE_SRC}
        INCLUDE_DIRS .
    )

    set(QUICKJS_ENABLE_BIGNUM_EXT ${CONFIG_QUICKJS_ENABLE_BIGNUM_EXT})

else()
    option(QUICKJS_ENABLE_BIGNUM_EXT "Enable QuickJS Bignum extensions" OFF)
    option(QUICKJS_DUMP_LEAKS "Dump memory leaks" OFF)

endif()


Include(FetchContent)
FetchContent_Declare(
    QuickJS
    URL               https://github.com/bellard/quickjs/archive/19abf1888db5884a5758036ff6e7fa2b340acedc.tar.gz
    PATCH_COMMAND     ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_SOURCE_DIR}/quickjs.CMakeLists.txt <SOURCE_DIR>/CMakeLists.txt
    CMAKE_ARGS        -DESP_PLATFORM=${ESP_PLATFORM} -DQUICKJS_ENABLE_BIGNUM_EXT=${QUICKJS_ENABLE_BIGNUM_EXT} -DQUICKJS_DUMP_LEAKS=${QUICKJS_DUMP_LEAKS}
)
FetchContent_MakeAvailable(QuickJS)

FetchContent_Declare(
    MIR
    URL               https://github.com/vnmakarov/mir/archive/refs/tags/v0.1.2.tar.gz
    CMAKE_ARGS        -D
)
FetchContent_MakeAvailable(MIR)
set(MIR_INCLUDE_DIR ${mir_SOURCE_DIR}/include)


if(ESP_PLATFORM)
    target_link_libraries(${COMPONENT_LIB} PUBLIC quickjs)
else()

    add_library(jac-machine STATIC ${JAC_MACHINE_SRC})
    target_include_directories(jac-machine PUBLIC . ${mir_SOURCE_DIR})
    set_target_properties(jac-machine PROPERTIES LINKER_LANGUAGE CXX)
    target_link_libraries(jac-machine PUBLIC quickjs mir)
    target_compile_options(jac-machine PUBLIC -Wall -Wextra -Wold-style-cast -Wshadow -Wimplicit-fallthrough -Wno-unused-parameter -Wno-cast-function-type -Wno-missing-field-initializers -Wno-old-style-cast)

endif()
