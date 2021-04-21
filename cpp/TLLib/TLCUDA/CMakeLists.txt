project(TLCUDA LANGUAGES CXX)

add_library(${PROJECT_NAME} src/cuda_sha.cpp)

find_package(CUDAToolkit)
if (CUDAToolkit_FOUND)
    target_compile_definitions(${PROJECT_NAME} PRIVATE CUDA_COMPILED)
    #add_custom_target(PTX COMMAND export SOURCE_DIR=${CMAKE_BINARY_DIR} && make 
        #WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR} DEPENDS kernels/sha256_iter.cu OUTPUT sha256_iter.fatbin
    #)
    add_custom_command(OUTPUT sha256_iter.fatbin
        DEPENDS kernels/sha256_iter.cu
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR} 
        COMMAND export SOURCE_DIR=${CMAKE_CURRENT_BINARY_DIR} && make 
    )
    
    add_custom_target(fatbin DEPENDS sha256_iter.fatbin)

    target_link_libraries(${PROJECT_NAME} cuew)
    add_dependencies(${PROJECT_NAME} fatbin)

    add_executable(prog ${TLCUDA_SOURCE_DIR}/prog/main.cpp)
    target_include_directories(prog PUBLIC ${CUDAToolkit_INCLUDE_DIRS})
    target_link_libraries (prog CUDA::cuda_driver cuew)
endif()

target_include_directories(${PROJECT_NAME} PUBLIC src)

add_executable(prog2 ${TLCUDA_SOURCE_DIR}/prog/main2.cpp)

target_link_libraries(prog2 ${PROJECT_NAME})

