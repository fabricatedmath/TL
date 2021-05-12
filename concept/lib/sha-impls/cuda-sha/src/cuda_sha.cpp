
#ifdef CUDA_COMPILED
  #include <cuew.h>
#endif

#include <cuda_sha.hpp>
#include <algorithm>
#include <stdio.h>

const char * CudaSha::getAvailabilityString(const CudaSha::Availability availability) {
  switch(availability) {
      case CudaSha::Available:
        return "available";
      case CudaSha::NotCompiled:
        return "not compiled";
      case CudaSha::NoNvidiaDriver:
        return "no NVIDIA driver found";
      default:
        return "error fall through";
  }
}

#ifdef CUDA_COMPILED
CudaSha::Availability CudaSha::check_availablity() {
  if (cuewInit(CUEW_INIT_CUDA) == CUEW_SUCCESS) {
    return Available;
  } else {
    return NoNvidiaDriver;
  }
}
#else
CudaSha::Availability CudaSha::check_availablity() {
    return NotCompiled;
}
#endif


#ifdef CUDA_COMPILED
CudaSha::CudaSha() {}
#else
CudaSha::CudaSha() {}
#endif

#ifdef CUDA_COMPILED
int CudaSha::init(const void* fatbin) {
  CUresult result = cuInit(0);
  if (result != CUDA_SUCCESS) {
    printf("Failed to initialize CUDA runtime (%s)\n", cuewErrorString(result));
    return -1;
  } 

  const int cuDevId = 0;
  cuDevice = 0;
  result = cuDeviceGet(&cuDevice, cuDevId);
  if (result != CUDA_SUCCESS) {
    printf("Failed to get CUDA device handle from ordinal (%s)", cuewErrorString(result));
    return -1;
  }

  const unsigned int ctx_flags = 0;
  cuContext = 0;
  result = cuCtxCreate(&cuContext, ctx_flags, cuDevice);
  if (result != CUDA_SUCCESS) {
    printf("Failed to create CUDA context (%s)", cuewErrorString(result));
    return -1;
  }

  CUmodule cuModule = 0;
  //result = cuModuleLoad(&cuModule, "tl-lib/build/sha256-impls/sha-cuda/sha256_iter.fatbin");
  result = cuModuleLoadFatBinary(&cuModule, fatbin);
  if (result != CUDA_SUCCESS) {
    printf("Failed to load CUDA module (%s)", cuewErrorString(result));
    return -1;
  }

  sha256_iter_kernel = 0;
  result = cuModuleGetFunction(&sha256_iter_kernel, cuModule, "sha256_iter_kernel");
  if (result != CUDA_SUCCESS) {
    printf("Failed to find CUDA kernel (%s)", cuewErrorString(result));
    return -1;
  }

  return 0;
}
#else
int CudaSha::init(const void* fatbin) {
  return -1;
}
#endif

#ifdef CUDA_COMPILED
int CudaSha::createChains(const int numTowers, const int numIters, uint32_t* const hashes) {
  const int maxThreadsPerBlock = 256;
  const int roundedThreadsPerBlock = ((numTowers + 32 - 1) / 32) * 32; // divisible by 32
  const int threadsPerBlock = std::min(roundedThreadsPerBlock, maxThreadsPerBlock);
  const int blocksPerGrid = ((numTowers + threadsPerBlock - 1) / threadsPerBlock);

  printf("Running %d threads and %d blocks\n", threadsPerBlock, blocksPerGrid);
  const size_t size = 32 * numTowers; // 32 bytes = 32*8 = 256 bits per tower

  CUdeviceptr d_mem;

  CUresult result = cuMemAlloc(&d_mem, size);
  if (result != CUDA_SUCCESS) {
    printf("Failed to allocate CUDA memory (%s)", cuewErrorString(result));
    return -1;
  }

  result = cuMemcpyHtoD(d_mem, hashes, size);
  if (result != CUDA_SUCCESS) {
    printf("Failed to copy host memory to CUDA memory (%s)", cuewErrorString(result));
    return -1;
  }
  int numTowersArg = numTowers;
  int numItersArg = numIters;

  void *args[] = { &numTowersArg, &numItersArg, &d_mem, &d_mem };

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

  result = cuMemcpyDtoH(hashes, d_mem, size);
  if (result != CUDA_SUCCESS) {
    printf("Failed to copy CUDA memory to host memory (%s)", cuewErrorString(result));
    return -1;
  }

  result = cuMemFree(d_mem);
  if (result != CUDA_SUCCESS) {
    printf("Failed to free CUDA memory (%s)", cuewErrorString(result));
    return -1;
  }

  return 0;
}
#else
int CudaSha::createChains(const int numTowers, const int numIters, uint32_t* const hashes) { return -1; }
#endif

#ifdef CUDA_COMPILED
CudaSha::~CudaSha() {
  CUresult result = cuCtxDestroy(cuContext);
  if (result != CUDA_SUCCESS) {
    printf("Failed to destroy CUDA context (%s)", cuewErrorString(result));
  }
}
#else
CudaSha::~CudaSha() {}
#endif