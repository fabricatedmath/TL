#pragma once

#ifdef CUDA_COMPILED
    #include <cuew.h>
#endif

#include <stdint.h>

class CudaSha {
private:
#ifdef CUDA_COMPILED
    CUdevice cuDevice;
    CUcontext cuContext;
    // CUmodule cuModule; // Can't store this in class? free() errors..
    CUfunction sha256_iter_kernel;

#endif
public:
    enum Availability {
        Available, NotCompiled, NoNvidiaDriver
    };

    static const char * getAvailabilityString(const Availability availability);
    static Availability check_availablity();

    CudaSha();

    int init(const void* fatbin);

    int createChains(const int numTowers, const int numIters, uint32_t* const hashes);

    ~CudaSha();
};