project(TL)

add_executable(${PROJECT_NAME} main.cpp)

target_link_libraries(${PROJECT_NAME}
    TLX86EXTS
    TLCUDA
)

find_package(CUDAToolkit)
#if (CUDAToolkit_FOUND)
    #target_compile_definitions(${PROJECT_NAME} PUBLIC CUDACOMPILED)
#endif()

find_package(Boost COMPONENTS serialization program_options REQUIRED)

target_link_libraries(${PROJECT_NAME} ${Boost_LIBRARIES})
target_link_libraries(${PROJECT_NAME} sodium)
target_link_libraries(${PROJECT_NAME} cuew)

#include_directories(../extern/cuew/include)

SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -pthread")




#SET(GCC_COVERAGE_COMPILE_FLAGS "-msse4.1 -msha -Wall -O2 -pthread")

#add_definitions(${GCC_COVERAGE_COMPILE_FLAGS})