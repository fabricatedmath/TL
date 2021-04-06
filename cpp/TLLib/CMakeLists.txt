project (TLLib)

add_library(${PROJECT_NAME} src/sha256-sse41-sha-x86.c)

#add_library(tutorial_compiler_flags INTERFACE)
#target_compile_features(tutorial_compiler_flags INTERFACE cxx_std_11)

target_include_directories( ${PROJECT_NAME}
    PUBLIC ${PROJECT_SOURCE_DIR}/include
)

#TODO: do this right
SET(GCC_COVERAGE_COMPILE_FLAGS "-msse4.1 -msha -Wall -O2")

add_definitions(${GCC_COVERAGE_COMPILE_FLAGS})

#set(gcc_like_cxx "$<COMPILE_LANG_AND_ID:CXX,ARMClang,AppleClang,Clang,GNU>")
#set(msvc_cxx "$<COMPILE_LANG_AND_ID:CXX,MSVC>")
#target_compile_options(tutorial_compiler_flags INTERFACE
  #"$<${gcc_like_cxx}:$<BUILD_INTERFACE:-Wall;-Wextra;-Wshadow;-Wformat=2;-Wunused>>"
#  "$<${gcc_like_cxx}:$<BUILD_INTERFACE:-msse4.1;-msha>>"
  #"$<${msvc_cxx}:$<BUILD_INTERFACE:-W3>>"
#)

