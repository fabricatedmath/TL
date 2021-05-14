cmake_minimum_required(VERSION 3.20)

project(sha)

add_subdirectory(extern/cuew)
add_subdirectory(extern/cpu_features)
add_subdirectory(sha-impls)
add_subdirectory(app)