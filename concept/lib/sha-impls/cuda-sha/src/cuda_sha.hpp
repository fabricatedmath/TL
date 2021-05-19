#pragma once

#ifdef CUDA_COMPILED
    #include <cuew.h>
#endif

#include <stdint.h>

enum Availability { Available, NotCompiled, NoNvidiaDriver };

class CudaSha {
private:
#ifdef CUDA_COMPILED
    CUdevice cuDevice;
    CUcontext cuContext;
    CUfunction sha256_iter_kernel;
#endif

public:
    static const char * getAvailabilityString(const Availability availability);
    static Availability check_availablity();

    CudaSha();

    int init(const void* fatbin);

    int createChains(const int numTowers, const int numIters, uint32_t* const hashes);

    ~CudaSha();
};

extern "C" {
  int cudaIsAvailable();
  CudaSha* cudaNew();
  int cudaInit(CudaSha* cudaSha, const void* fatbin);
  int cudaCreateChains(CudaSha* cudaSha, const int numTowers, const int numIters, uint32_t* const hashes);
  void cudaDelete(CudaSha* cudaSha);
}