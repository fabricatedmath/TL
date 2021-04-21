#include <cuda_sha.hpp>

static const char * availabilityStrings[] = { "bananas & monkeys", "Round and orange", "APPLE" };

const char * CudaSHA::availabilityString(const CudaSHA::Availability availability) {
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
Availability CudaSHA::check_availablity() {
    return Available;
}
#else
CudaSHA::Availability CudaSHA::check_availablity() {
    return NotCompiled;
}
#endif