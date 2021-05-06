#include <export.hpp>

#include <x86exts_sha.hpp>
#include <arm_sha.hpp>
#include <cuda_sha.hpp>

#include <iostream>
using namespace std;

bool x86IsAvailable() {
  return X86ExtsSHA::is_available();
}

bool armIsAvailable() {
  return ARMSHA::is_available();
}

int cudaIsAvailable() {
  return CudaSHA::check_availablity();
}

CudaSHA* cudaNew() {
  return new CudaSHA();
}

int cudaInit(CudaSHA* cudaSha, const void* fatbin) {
  return cudaSha->init(fatbin);
}

void cudaDelete(CudaSHA* cudaSha) {
  printf("here\n");
  delete cudaSha;
}