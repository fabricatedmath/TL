#include <cuda_sha.hpp>

#ifdef CUDA_COMPILED
  #include <cuew.h>
#endif

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
CudaSHA::CudaSHA() : cuDevice(0), cuContext(0), cuModule(0), sha256_iter_kernel(0) {}
#else
CudaSHA::CudaSHA() {}
#endif

#ifdef CUDA_COMPILED
int CudaSHA::init() {
  //if (cuewInit(CUEW_INIT_CUDA) == CUEW_SUCCESS) {
  //}
  return 0;
}
#else
void CudaSHA::init() {}
#endif