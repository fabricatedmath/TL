#include <sha_generic.hpp>

extern "C" {
  #include <sha256-generic.h>
}

int isAvailable_generic() {
  return Available;
}

void iterateHash_generic(const int numIter, uint32_t* startingHash) {
  sha256_iter_generic(numIter, startingHash);
}