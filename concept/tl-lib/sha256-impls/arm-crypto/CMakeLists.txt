project (arm-crypto)

#message( WARNING "OS: ${CMAKE_HOST_SYSTEM_NAME}" )
#message( WARNING "CPU: ${CMAKE_HOST_SYSTEM_PROCESSOR}" )

set(ARM (${CMAKE_HOST_SYSTEM_PROCESSOR} MATCHES "aarch64"))

if (${ARM})
  set(c_source_files ${c_source_files} kernels/sha256-sha-arm.c)
endif()

add_library(${PROJECT_NAME} src/arm_sha.cpp ${c_source_files})

if (${ARM})
  target_compile_definitions(${PROJECT_NAME} PRIVATE ARM_COMPILED)
  target_include_directories( ${PROJECT_NAME}
      PRIVATE ${PROJECT_SOURCE_DIR}/kernels
  )

  SET(GCC_COVERAGE_COMPILE_FLAGS "-march=armv8-a+crypto -Wall -O2")

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