cmake_minimum_required(VERSION 3.20)

project(TL)
add_subdirectory(extern/cuew)
add_subdirectory(extern/cpu_features)
add_subdirectory(app)
add_subdirectory(TLLib)

add_subdirectory(sandbox)
