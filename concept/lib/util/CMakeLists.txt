project(util)

add_library(${PROJECT_NAME} src/hash_finder.cpp)

target_include_directories( ${PROJECT_NAME}
    PRIVATE ${PROJECT_SOURCE_DIR}/kernels
)

SET(GCC_COVERAGE_COMPILE_FLAGS "-Wall -O2")

add_definitions(${GCC_COVERAGE_COMPILE_FLAGS})

target_include_directories( ${PROJECT_NAME}
    PUBLIC ${PROJECT_SOURCE_DIR}/src
)