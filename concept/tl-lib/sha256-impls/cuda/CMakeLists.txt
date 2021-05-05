project(cuda LANGUAGES CXX)

add_library(${PROJECT_NAME} src/cuda_sha.cpp)

find_package(CUDAToolkit)
if (CUDAToolkit_FOUND)
    target_compile_definitions(${PROJECT_NAME} PRIVATE CUDA_COMPILED)

    add_custom_command(OUTPUT sha256_iter.fatbin
        DEPENDS kernels/sha256_iter.cu
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR} 
        COMMAND export SOURCE_DIR=${CMAKE_CURRENT_BINARY_DIR} && make 
    )
    
    add_custom_target(fatbin DEPENDS sha256_iter.fatbin)

    target_link_libraries(${PROJECT_NAME} cuew)
    add_dependencies(${PROJECT_NAME} fatbin)
endif()

target_include_directories(${PROJECT_NAME} PUBLIC src)