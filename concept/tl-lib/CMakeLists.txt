cmake_minimum_required(VERSION 3.20)

project (tl-lib)

add_subdirectory(extern/cuew)
add_subdirectory(extern/cpu_features)

add_subdirectory(sha256-impls)

add_subdirectory(app)