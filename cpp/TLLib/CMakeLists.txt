project(TLLib)

add_subdirectory(TLCPP)

find_package(CUDAToolkit)
if (CUDAToolkit_FOUND)
  add_subdirectory(TLCUDA)
endif()
