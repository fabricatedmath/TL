project(sandbox)

add_executable(${PROJECT_NAME} cmd.cpp)

target_link_libraries(${PROJECT_NAME}
    #TLX86EXTS
    #TLARM
    #TLCUDA
    #TLGENERIC
    #TLUTIL
)

find_package(Boost COMPONENTS program_options REQUIRED)

target_link_libraries(${PROJECT_NAME} ${Boost_LIBRARIES})
#target_link_libraries(${PROJECT_NAME} sodium)

SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -pthread")
