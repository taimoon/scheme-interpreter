cmake_minimum_required(VERSION 3.13)
include(pico_sdk_import.cmake)
project(scheme_pico)
pico_sdk_init()

add_executable(main
    main.c
)

target_sources(main PRIVATE
    ports/rp2040/sys_port.c
)

target_compile_definitions(main PRIVATE SCM_HOSTED=0 SCM_UTF32=0)

target_include_directories(main PRIVATE
    ${CMAKE_CURRENT_LIST_DIR}/ports
)

pico_enable_stdio_usb(main 1)
pico_add_extra_outputs(main)
target_link_libraries(main pico_stdlib)