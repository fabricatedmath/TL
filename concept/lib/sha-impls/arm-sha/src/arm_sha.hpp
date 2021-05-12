#pragma once

#include <stdint.h>

class ArmSha {
public:
    static bool is_available();
    static void iterateHash(const int numIter, uint32_t* startingHash);
};