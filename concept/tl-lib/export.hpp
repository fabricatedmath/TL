#pragma once

#include <cuda_sha.hpp>

extern "C" {
  bool x86IsAvailable();
  bool armIsAvailable();

  int cudaIsAvailable();

  CudaSHA* cudaNew();
  int cudaInit(CudaSHA* cudaSha, const void* fatbin);
  void cudaDelete(CudaSHA* cudaSha);
}