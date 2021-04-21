// #include <cuda.h>

#include <unistd.h>
#include <stdio.h>
#include <limits.h>

#include <iostream>
using namespace std;

#include <chrono>
using namespace std::chrono;


#include <cuew.h>

// Allocates an array with random float entries.
void RandomInit(uint32_t *data, int n)
{
    for (int i = 0; i < n; ++i)
    {
        data[i] = rand();
    }
}

void print256(uint32_t* data) {
    printf("Hex: ");
    for (int i = 0; i < 8; i++) {
        const uint32_t v = data[i];
        printf("%08x ", v);
    }
    printf("\n");
}

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

    int threadsPerBlock = 256;
    int blocksPerGrid   = 68*2;

    int numIters = 10000000;
    int numTowers = blocksPerGrid * threadsPerBlock;
    const size_t size = 32 * numTowers; // 32 bytes = 32*8 = 256 bits per tower

    uint32_t* h_A = (uint32_t*)malloc(size);
    uint32_t* h_B = (uint32_t*)malloc(size);

    RandomInit(h_A, 8 * numTowers);

    cout << "random done" << endl;

    CUdeviceptr d_A;
    CUdeviceptr d_B;

    result = cuMemAlloc(&d_A, size);
    if (result != CUDA_SUCCESS) {
      printf("Failed to allocate CUDA memory (%s)", cuewErrorString(result));
      return -1;
    }

    result = cuMemcpyHtoD(d_A, h_A, size);
    if (result != CUDA_SUCCESS) {
      printf("Failed to copy host memory to CUDA memory (%s)", cuewErrorString(result));
      return -1;
    }

    result = cuMemAlloc(&d_B, size);
    if (result != CUDA_SUCCESS) {
      printf("Failed to allocate CUDA memory (%s)", cuewErrorString(result));
      return -1;
    }

    void *args[] = { &numTowers, &numIters, &d_A, &d_B };
    result = cuCtxSynchronize();
    if (result != CUDA_SUCCESS) {
      printf("Kernel launch failed (%s)", cuewErrorString(result));
      return -1;
    }

    auto t1 = high_resolution_clock::now();
    // Launch the CUDA kernel
    result = cuLaunchKernel(sha256_iter_kernel,  blocksPerGrid, 1, 1, threadsPerBlock, 1, 1, 0, NULL, args, NULL);
    if (result != CUDA_SUCCESS) {
      printf("Failed to launch CUDA kernel (%s)", cuewErrorString(result));
      return -1;
    }

    result = cuCtxSynchronize();
    if (result != CUDA_SUCCESS) {
      printf("Kernel launch failed (%s)", cuewErrorString(result));
      return -1;
    }
    auto t2 = high_resolution_clock::now();

    /* Getting number of milliseconds as an integer. */
    auto ms_int = duration_cast<milliseconds>(t2 - t1);

    /* Getting number of milliseconds as a double. */
    duration<double, std::milli> ms_double = t2 - t1;

    std::cout << ms_int.count() << "ms" << endl;
    std::cout << ms_double.count() << "ms" << endl;

    result = cuMemcpyDtoH(h_B, d_B, size);
    if (result != CUDA_SUCCESS) {
      printf("Failed to copy CUDA memory to host memory (%s)", cuewErrorString(result));
      return -1;
    }

    print256(h_A);
    print256(h_B);

    free(h_A);
    free(h_B);

    result = cuMemFree(d_A);
    if (result != CUDA_SUCCESS) {
      printf("Failed to free CUDA memory (%s)", cuewErrorString(result));
      return -1;
    }

    result = cuMemFree(d_B);
    if (result != CUDA_SUCCESS) {
      printf("Failed to free CUDA memory (%s)", cuewErrorString(result));
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

