// #include <cuda.h>

#include <unistd.h>
#include <stdio.h>
#include <limits.h>

#include <cuew.h>

int main(int argc, char** argv) {
  char cwd[PATH_MAX];
  if (getcwd(cwd, sizeof(cwd)) != NULL) {
      printf("Current working dir: %s\n", cwd);
  } else {
      perror("getcwd() error");
      return 1;
  }
  
  if (cuewInit(CUEW_INIT_CUDA) == CUEW_SUCCESS) {
    printf("CUDA found\n");
    printf("NVCC path: %s\n", cuewCompilerPath());
    printf("NVCC version: %d\n", cuewCompilerVersion());

    const int cuDevId = 0;

    CUresult result = cuInit(0);
    if (result != CUDA_SUCCESS) {
      printf("Failed to initialize CUDA runtime (%s)\n", cuewErrorString(result));
      return -1;
    } 

    CUdevice cuDevice = 0;
    result = cuDeviceGet(&cuDevice, cuDevId);
    if (result != CUDA_SUCCESS) {
      printf("Failed to get CUDA device handle from ordinal (%s)", cuewErrorString(result));
      return -1;
    }

    const unsigned int ctx_flags = 0;
    CUcontext cuContext = 0;
    result = cuCtxCreate(&cuContext, ctx_flags, cuDevice);
    if (result != CUDA_SUCCESS) {
      printf("Failed to create CUDA context (%s)", cuewErrorString(result));
      return -1;
    }

    CUmodule cuModule = 0;
    result = cuModuleLoad(&cuModule, "/home/cdurham/TL/cpp/TLLib/TLCUDA/sha256_iter.fatbin");
    if (result != CUDA_SUCCESS) {
      printf("Failed to load CUDA module (%s)", cuewErrorString(result));
      return -1;
    }

    CUfunction sha256_iter_kernel = 0;
    result = cuModuleGetFunction(&sha256_iter_kernel, cuModule, "sha256_iter_kernel");
    if (result != CUDA_SUCCESS) {
      printf("Failed to find CUDA kernel (%s)", cuewErrorString(result));
      return -1;
    }

  }
  else {
    printf("CUDA not found\n");
  }
/*
  CUresult result = cuInit(0);
  if (result != CUDA_SUCCESS) {
    printf("done\n");
    //printf("Failed to initialize CUDA runtime (%s)\n", cuewErrorString(result));
    return 0;
  } else {
    printf("doner\n");
    //printf("Failed to initialize CUDA runtime (%s)\n", cuewErrorString(result));
  }
*/
  return 0;
}