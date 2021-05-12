cmake_minimum_required(VERSION 3.20)

project (lib)

add_subdirectory(extern/cuew)
add_subdirectory(extern/cpu_features)
add_subdirectory(sha-impls)
add_subdirectory(app)

add_library(${PROJECT_NAME} export.cpp)

target_include_directories(${PROJECT_NAME}
  PRIVATE ${PROJECT_SOURCE_DIR}
)

target_link_libraries(${PROJECT_NAME}
  cpu_features
  cuew
  arm-sha
  cuda-sha
)