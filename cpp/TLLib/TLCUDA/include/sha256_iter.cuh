#pragma once

#include <stdint.h>

extern "C" {
  int sha256_iter_cuda(const int numTowers, const int numIters, uint32_t* startingHashes);
  const char * getErrorString(const int errorCode);
}
