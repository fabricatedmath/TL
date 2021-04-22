
#ifdef CUDA_COMPILED
  #include <cuew.h>
#endif

#include <cuda_sha.hpp>

#include<stdio.h>

const char * CudaSHA::getAvailabilityString(const CudaSHA::Availability availability) {
  switch(availability) {
      case CudaSHA::Available:
        return "available";
      case CudaSHA::NotCompiled:
        return "not compiled";
      case CudaSHA::NoNvidiaDriver:
        return "no NVIDIA driver found";
      default:
        return "error fall through";
  }
}

#ifdef CUDA_COMPILED
CudaSHA::Availability CudaSHA::check_availablity() {
  if (cuewInit(CUEW_INIT_CUDA) == CUEW_SUCCESS) {
    return Available;
  } else {
    return NoNvidiaDriver;
  }
    
}
#else
CudaSHA::Availability CudaSHA::check_availablity() {
    return NotCompiled;
}
#endif


#ifdef CUDA_COMPILED
CudaSHA::CudaSHA() {}
#else
CudaSHA::CudaSHA() {}
#endif

#ifdef CUDA_COMPILED
int CudaSHA::init() {
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
  result = cuModuleLoad(&cuModule, "sha256_iter.fatbin");
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
void CudaSHA::init() {}
#endif

#ifdef CUDA_COMPILED
int CudaSHA::createChains(const int numTowers, const int numIters, uint32_t* startingHashes, uint32_t* endingHashes) {
  int threadsPerBlock = 256;
  int blocksPerGrid   = 68*2;

  int numIters2 = 1000000;
  int numTowers2 = blocksPerGrid * threadsPerBlock;
  const size_t size = 32 * numTowers; // 32 bytes = 32*8 = 256 bits per tower

  CUdeviceptr d_mem;

  uint32_t* h_A = (uint32_t*)malloc(size);
  uint32_t* h_B = (uint32_t*)malloc(size);

  CUresult result = cuMemAlloc(&d_mem, size);
  if (result != CUDA_SUCCESS) {
    printf("Failed to allocate CUDA memory (%s)", cuewErrorString(result));
    return -1;
  }

  result = cuMemcpyHtoD(d_mem, h_A, size);
  if (result != CUDA_SUCCESS) {
    printf("Failed to copy host memory to CUDA memory (%s)", cuewErrorString(result));
    return -1;
  }

  void *args[] = { &numTowers2, &numIters2, &d_mem, &d_mem };

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

  result = cuMemcpyDtoH(h_B, d_mem, size);
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
void CudaSHA::createChains(const int numTowers, const int numIters, uint32_t* startingHashes, uint32_t* endingHashes) {}
#endif

#ifdef CUDA_COMPILED
CudaSHA::~CudaSHA() {
  CUresult result = cuCtxDestroy(cuContext);
  if (result != CUDA_SUCCESS) {
    printf("Failed to destroy CUDA context (%s)", cuewErrorString(result));
  }
}
#else
CudaSHA::~CudaSHA() {}
#endif