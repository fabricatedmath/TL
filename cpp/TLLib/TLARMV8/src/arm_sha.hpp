#pragma once

#include <stdint.h>

class ARMSHA {
public:
    static bool is_available();
    static void iterateHash(const int numIter, uint32_t* startingHash);
};