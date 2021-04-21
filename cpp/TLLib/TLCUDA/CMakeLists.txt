project(TLCUDA LANGUAGES CXX)

find_package(CUDAToolkit)
if (CUDAToolkit_FOUND)
    target_compile_definitions(${PROJECT_NAME} PUBLIC CUDA_COMPILED)
    add_custom_target(PTX COMMAND export SOURCE_DIR=${CMAKE_BINARY_DIR} && make 
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    )
    target_include_directories(prog PUBLIC ${CUDAToolkit_INCLUDE_DIRS})
    target_link_libraries (prog CUDA::cuda_driver cuew)
endif()

#add_executable(prog ${TLCUDA_SOURCE_DIR}/prog/main.cpp)
add_library(${PROJECT_NAME} src/cuda_sha.cpp)
target_include_directories(${PROJECT_NAME} PUBLIC src)

add_executable(prog2 ${TLCUDA_SOURCE_DIR}/prog/main2.cpp)

target_link_libraries(prog2 ${PROJECT_NAME})

