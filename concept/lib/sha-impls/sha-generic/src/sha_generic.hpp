#pragma once
#include <stdint.h>

enum Availability { Available };

extern "C" {
    int isAvailable_generic();
    void iterateHash_generic(const int numIter, uint32_t* startingHash);
};