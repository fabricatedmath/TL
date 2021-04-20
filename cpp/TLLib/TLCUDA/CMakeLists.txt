project(TLCUDA LANGUAGES CXX)

find_package(CUDAToolkit REQUIRED)

add_executable(prog ${TLCUDA_SOURCE_DIR}/prog/main.cpp)

target_include_directories(prog PUBLIC ${CUDAToolkit_INCLUDE_DIRS})
target_link_libraries (prog CUDA::cuda_driver cuew)