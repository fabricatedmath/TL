
#ifdef CUDA_COMPILED
  #include <cuew.h>
#endif

#include <cuda_sha.hpp>
#include <algorithm>
#include <stdio.h>

const char * CudaSha::getAvailabilityString(const Availability availability) {
  switch(availability) {
      case Available:
        return "available";
      case NotCompiled:
        return "not compiled";
      case NoNvidiaDriver:
        return "no NVIDIA driver found";
      default:
        return "error fall through";
  }
}


Availability CudaSha::check_availablity() {
#ifdef CUDA_COMPILED
  if (cuewInit(CUEW_INIT_CUDA) == CUEW_SUCCESS) {
    return Available;
  } else {
    return NoNvidiaDriver;
  }
#else
  return NotCompiled;    
#endif
}


CudaSha::CudaSha() {
#ifdef CUDA_COMPILED
#else
#endif
}

#ifdef CUDA_COMPILED
// Beginning of GPU Architecture definitions
inline int _ConvertSMVer2CoresDRV(int major, int minor) {
  // Defines for GPU Architecture types (using the SM version to determine the #
  // of cores per SM
  typedef struct {
    int SM;  // 0xMm (hexidecimal notation), M = SM Major version, and m = SM
             // minor version
    int Cores;
  } sSMtoCores;

  sSMtoCores nGpuArchCoresPerSM[] = {
      {0x30, 192},
      {0x32, 192},
      {0x35, 192},
      {0x37, 192},
      {0x50, 128},
      {0x52, 128},
      {0x53, 128},
      {0x60,  64},
      {0x61, 128},
      {0x62, 128},
      {0x70,  64},
      {0x72,  64},
      {0x75,  64},
      {0x80,  64},
      {0x86, 128},
      {-1, -1}};

  int index = 0;

  while (nGpuArchCoresPerSM[index].SM != -1) {
    if (nGpuArchCoresPerSM[index].SM == ((major << 4) + minor)) {
      return nGpuArchCoresPerSM[index].Cores;
    }

    index++;
  }

  // If we don't find the values, we default use the previous one to run
  // properly
  printf(
      "MapSMtoCores for SM %d.%d is undefined.  Default to use %d Cores/SM\n",
      major, minor, nGpuArchCoresPerSM[index - 1].Cores);
  return nGpuArchCoresPerSM[index - 1].Cores;
}
  // end of GPU Architecture definitions
#endif

// TODO: choose devices
// TODO: multi gpu
int CudaSha::init(const void* fatbin) {
#ifdef CUDA_COMPILED
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

  result = cuDeviceGetAttribute(&majorComputeCapability, CU_DEVICE_ATTRIBUTE_COMPUTE_CAPABILITY_MAJOR, cuDevId);
  if (result != CUDA_SUCCESS) {
    printf("Failed to get CUDA Major Compute Capability (%s)", cuewErrorString(result));
    return -1;
  }

  result = cuDeviceGetAttribute(&minorComputeCapability, CU_DEVICE_ATTRIBUTE_COMPUTE_CAPABILITY_MINOR, cuDevId);
  if (result != CUDA_SUCCESS) {
    printf("Failed to get CUDA Minor Compute Capability (%s)", cuewErrorString(result));
    return -1;
  }

  result = cuDeviceGetAttribute(&multiProcessorCount, CU_DEVICE_ATTRIBUTE_MULTIPROCESSOR_COUNT, cuDevId);
  if (result != CUDA_SUCCESS) {
    printf("Failed to get CUDA Multi Processor Count (%s)", cuewErrorString(result));
    return -1;
  }

  coresPerMultiProcessor = _ConvertSMVer2CoresDRV(majorComputeCapability, minorComputeCapability);

  numCudaCores = multiProcessorCount * coresPerMultiProcessor;

  const unsigned int ctx_flags = 0;
  cuContext = 0;
  result = cuCtxCreate(&cuContext, ctx_flags, cuDevice);
  if (result != CUDA_SUCCESS) {
    printf("Failed to create CUDA context (%s)", cuewErrorString(result));
    return -1;
  }

  CUmodule cuModule = 0;
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
#else
  return -1;
#endif
}

int CudaSha::getNumCudaCores() {
#ifdef CUDA_COMPILED
  return numCudaCores;
#else
  return 0;
#endif
}

int CudaSha::createChains(const int numTowers, const int numIters, uint32_t* const hashes) {
#ifdef CUDA_COMPILED
  const int maxThreadsPerBlock = 256;
  const int roundedThreadsPerBlock = ((numTowers + 32 - 1) / 32) * 32; // divisible by 32
  const int threadsPerBlock = std::min(roundedThreadsPerBlock, maxThreadsPerBlock);
  const int blocksPerGrid = ((numTowers + threadsPerBlock - 1) / threadsPerBlock);

  //printf("Running %d threads and %d blocks\n", threadsPerBlock, blocksPerGrid);
  const size_t size = 32 * numTowers; // 32 bytes = 32*8 = 256 bits per tower

  CUdeviceptr d_mem;

  CUresult result;

  result = cuCtxPushCurrent(cuContext);
  if (result != CUDA_SUCCESS) {
    printf("Failed to make context current (%s)\n", cuewErrorString(result));
    return -1;
  }

  result = cuMemAlloc(&d_mem, size);
  if (result != CUDA_SUCCESS) {
    printf("Failed to allocate CUDA memory (%s)\n", cuewErrorString(result));
    return -1;
  }

  result = cuMemcpyHtoD(d_mem, hashes, size);
  if (result != CUDA_SUCCESS) {
    printf("Failed to copy host memory to CUDA memory (%s)\n", cuewErrorString(result));
    return -1;
  }
  int numTowersArg = numTowers;
  int numItersArg = numIters;

  void *args[] = { &numTowersArg, &numItersArg, &d_mem, &d_mem };

  // Launch the CUDA kernel
  result = cuLaunchKernel(sha256_iter_kernel,  blocksPerGrid, 1, 1, threadsPerBlock, 1, 1, 0, NULL, args, NULL);
  if (result != CUDA_SUCCESS) {
    printf("Failed to launch CUDA kernel (%s)\n", cuewErrorString(result));
    return -1;
  }

  result = cuCtxSynchronize();
  if (result != CUDA_SUCCESS) {
    printf("Kernel launch failed (%s)\n", cuewErrorString(result));
    return -1;
  }

  result = cuMemcpyDtoH(hashes, d_mem, size);
  if (result != CUDA_SUCCESS) {
    printf("Failed to copy CUDA memory to host memory (%s)\n", cuewErrorString(result));
    return -1;
  }

  result = cuMemFree(d_mem);
  if (result != CUDA_SUCCESS) {
    printf("Failed to free CUDA memory (%s)\n", cuewErrorString(result));
    return -1;
  }

  return 0;
#else
  return -1;
#endif
}

CudaSha::~CudaSha() {
#ifdef CUDA_COMPILED
  CUresult result = cuCtxDestroy(cuContext);
  if (result != CUDA_SUCCESS) {
    printf("Failed to destroy CUDA context (%s)", cuewErrorString(result));
  }
#else
#endif
}

int cudaIsAvailable() {
  return CudaSha::check_availablity();
}

CudaSha* cudaNew() {
  return new CudaSha();
}

int cudaInit(CudaSha* cudaSha, const void* fatbin) {
  return cudaSha->init(fatbin);
}

int cudaGetNumCores(CudaSha* cudaSha) {
  return cudaSha->getNumCudaCores();
}

int cudaCreateChains(CudaSha* cudaSha, const int numTowers, const int numIters, uint32_t* const hashes) {
  return cudaSha->createChains(numTowers, numIters, hashes);
}

void cudaDelete(CudaSha* cudaSha) {
  delete cudaSha;
}