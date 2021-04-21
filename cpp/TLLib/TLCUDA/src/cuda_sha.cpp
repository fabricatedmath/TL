#include <cuda_sha.hpp>

#ifdef CUDA_COMPILED
bool CudaSHA::is_available() {
    return true;
}
#else
bool CudaSHA::is_available() {
    return false;
}
#endif