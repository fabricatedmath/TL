#pragma once

#include <stdint.h>

class SHA {
public:
    static void iterateHash(const int numIter, uint32_t* startingHash);
};