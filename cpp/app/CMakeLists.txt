project(TL)

add_executable(${PROJECT_NAME} main.cpp)

target_link_libraries(${PROJECT_NAME}
    TLLib
)

find_package(Boost COMPONENTS serialization program_options REQUIRED)


target_link_libraries(${PROJECT_NAME} ${Boost_LIBRARIES})