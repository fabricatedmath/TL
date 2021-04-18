project(TLCUDA LANGUAGES CUDA)

find_package(CUDAToolkit REQUIRED)

#set(CMAKE_CXX_STANDARD 17)
#set(CMAKE_CUDA_STANDARD 17)

set(cuda_source_files ${cuda_source_files} 
  src/sha256_iter.cu
  )

add_library(${PROJECT_NAME} ${cuda_source_files})

set_target_properties( ${PROJECT_NAME}
                       PROPERTIES 
                       CUDA_SEPARABLE_COMPILATION ON
                       POSITION_INDEPENDENT_CODE ON
                       CUDA_RESOLVE_DEVICE_SYMBOLS ON
                       )

target_compile_options(${PROJECT_NAME} PRIVATE
  "$<$<COMPILE_LANGUAGE:CUDA>:SHELL:-gencode arch=compute_50,code=sm_50>"
  "$<$<COMPILE_LANGUAGE:CUDA>:SHELL:-gencode arch=compute_52,code=sm_52>"
  "$<$<COMPILE_LANGUAGE:CUDA>:SHELL:-gencode arch=compute_60,code=sm_60>"
  "$<$<COMPILE_LANGUAGE:CUDA>:SHELL:-gencode arch=compute_61,code=sm_61>"
  "$<$<COMPILE_LANGUAGE:CUDA>:SHELL:-gencode arch=compute_70,code=sm_70>"
  "$<$<COMPILE_LANGUAGE:CUDA>:SHELL:-gencode arch=compute_75,code=sm_75>"
  "$<$<COMPILE_LANGUAGE:CUDA>:SHELL:-gencode arch=compute_86,code=sm_86>"
)

target_include_directories( ${PROJECT_NAME}
    PUBLIC ${PROJECT_SOURCE_DIR}/include
)

target_link_libraries( ${PROJECT_NAME}
  PRIVATE CUDA::cudart
)

add_executable(prog ${TLCUDA_SOURCE_DIR}/prog/main.cu)
target_link_libraries (prog TLCUDA cuew)