project(TLLib)

add_subdirectory(TLCPP)

find_package(CUDAToolkit)
if (CUDAToolkit_FOUND)
  add_subdirectory(TLCUDA)
else()
  target_compile_definitions(${PROJECT_NAME} PUBLIC FOO)
endif()
