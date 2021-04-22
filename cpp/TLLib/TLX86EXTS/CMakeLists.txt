project (TLX86EXTS)

set(c_source_files ${c_source_files} kernels/sha256-sse41-sha-x86.c)

add_library(${PROJECT_NAME} ${c_source_files} src/x86exts_sha.cpp)

target_include_directories( ${PROJECT_NAME}
    PRIVATE ${PROJECT_SOURCE_DIR}/kernels
)

target_include_directories( ${PROJECT_NAME}
    PUBLIC ${PROJECT_SOURCE_DIR}/src
)

#TODO: do this right
#set(gcc_like_cxx "$<COMPILE_LANG_AND_ID:CXX,ARMClang,AppleClang,Clang,GNU>")
#set(msvc_cxx "$<COMPILE_LANG_AND_ID:CXX,MSVC>")
#target_compile_options(tutorial_compiler_flags INTERFACE
  #"$<${gcc_like_cxx}:$<BUILD_INTERFACE:-Wall;-Wextra;-Wshadow;-Wformat=2;-Wunused>>"
#  "$<${gcc_like_cxx}:$<BUILD_INTERFACE:-msse4.1;-msha>>"
  #"$<${msvc_cxx}:$<BUILD_INTERFACE:-W3>>"
#)

SET(GCC_COVERAGE_COMPILE_FLAGS "-msse4.1 -msha -Wall -O2")

add_definitions(${GCC_COVERAGE_COMPILE_FLAGS})

target_link_libraries (${PROJECT_NAME} cpu_features)



