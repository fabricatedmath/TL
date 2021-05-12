#pragma once

#include <cuda_sha.hpp>

extern "C" {
  bool armIsAvailable();

  int cudaIsAvailable();

  CudaSha* cudaNew();
  int cudaInit(CudaSha* cudaSha, const void* fatbin);
  void cudaDelete(CudaSha* cudaSha);
}