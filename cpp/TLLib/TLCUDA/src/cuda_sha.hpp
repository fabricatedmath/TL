#pragma once

#ifdef CUDA_COMPILED
    #include <cuew.h>
#endif

class CudaSHA {
private:
#ifdef CUDA_COMPILED
    CUdevice cuDevice;
    CUcontext cuContext;
    CUmodule cuModule;
    CUfunction sha256_iter_kernel;

    CUdeviceptr d_mem;
#endif
public:
    enum Availability {
        Available, NotCompiled, NoNvidiaDriver
    };

    static const char * getAvailabilityString(const Availability availability);
    static Availability check_availablity();

    CudaSHA();

    int init();
};