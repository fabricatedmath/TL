project(cuda-sha LANGUAGES CXX)

add_library(${PROJECT_NAME} src/cuda_sha.cpp)

list(APPEND SMS 35 37 50 52 60 61 70 75 80 86)
foreach(sm ${SMS})
    list(APPEND GENCODE_FLAGS -gencode arch=compute_${sm},code=sm_${sm})
endforeach()

message(${GENCODE_FLAGS})

find_package(CUDAToolkit)
if (CUDAToolkit_FOUND)
    target_compile_definitions(${PROJECT_NAME} PRIVATE CUDA_COMPILED)

    # TODO document putting cl.exe on path 
    # Something like C:\Program Files (x86)\Microsoft Visual Studio\2019\Community\VC\Tools\MSVC\14.28.29910\bin\Hostx64\x64
    add_custom_command(OUTPUT sha256_iter.fatbin
        DEPENDS kernels/sha256_iter.cu Makefile
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
        COMMAND nvcc -O2 -Xptxas=-v --fatbin ${GENCODE_FLAGS} kernels/sha256_iter.cu -o ${CMAKE_CURRENT_BINARY_DIR}/sha256_iter.fatbin;
    )
    
    add_custom_target(fatbin DEPENDS sha256_iter.fatbin)

    target_link_libraries(${PROJECT_NAME} cuew)
    add_dependencies(${PROJECT_NAME} fatbin)
endif()

target_include_directories(${PROJECT_NAME} PUBLIC src)