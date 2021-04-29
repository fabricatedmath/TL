#include <sha256.hpp>

extern "C" {
#include <sha256.h>
}

void SHA::iterateHash(const int numIter, uint32_t* startingHash) {
    sha256_iter(numIter, startingHash);
}