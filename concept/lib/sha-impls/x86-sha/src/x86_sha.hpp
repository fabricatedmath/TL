#pragma once

#include <stdint.h>

class X86Sha {
public:
    static bool is_available();
    static void iterateHash(const int numIter, uint32_t* const startingHash);
};