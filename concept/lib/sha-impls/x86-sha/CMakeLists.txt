project (x86-sha)

#message( WARNING "OS: ${CMAKE_HOST_SYSTEM_NAME}" )
#message( WARNING "CPU: ${CMAKE_HOST_SYSTEM_PROCESSOR}" )

message( ${CMAKE_HOST_SYSTEM_PROCESSOR} )

set(X86 (${CMAKE_HOST_SYSTEM_PROCESSOR} MATCHES "x86_64") OR (${CMAKE_HOST_SYSTEM_PROCESSOR} MATCHES "AMD64"))

if (${X86})
  set(c_source_files ${c_source_files} kernels/sha256_sse41_sha_x86.c)
endif()

add_library(${PROJECT_NAME} src/x86_sha.cpp ${c_source_files})

if (${X86})
  target_compile_definitions(${PROJECT_NAME} PRIVATE X86_COMPILED)
  target_include_directories( ${PROJECT_NAME}
      PRIVATE ${PROJECT_SOURCE_DIR}/kernels
  )

  SET(GCC_COVERAGE_COMPILE_FLAGS "-msse4.1 -msha -Wall -O2")

  add_definitions(${GCC_COVERAGE_COMPILE_FLAGS})

  target_link_libraries (${PROJECT_NAME} cpu_features)

endif()

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