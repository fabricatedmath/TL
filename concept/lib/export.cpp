#include <export.hpp>

#include <arm_sha.hpp>
//#include <cuda-sha.hpp>

#include <iostream>
using namespace std;

bool armIsAvailable() {
  return ArmSha::is_available();
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

void cudaDelete(CudaSha* cudaSha) {
  printf("here\n");
  delete cudaSha;
}