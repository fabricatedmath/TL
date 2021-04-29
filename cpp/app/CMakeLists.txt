project(TL)

add_executable(${PROJECT_NAME} main.cpp)

target_link_libraries(${PROJECT_NAME}
    TLX86EXTS
    TLCUDA
    TLGENERIC
    TLUTIL
)

find_package(Boost COMPONENTS serialization program_options REQUIRED)

target_link_libraries(${PROJECT_NAME} ${Boost_LIBRARIES})
target_link_libraries(${PROJECT_NAME} sodium)

SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -pthread")
