project(TLLib)

add_subdirectory(TLCPP)

find_package(CUDAToolkit)
if (CUDAToolkit_FOUND)
  add_custom_target(PTX COMMAND export SOURCE_DIR=${CMAKE_BINARY_DIR} && make 
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/TLCUDA
  )
  add_subdirectory(TLCUDA)
endif()
