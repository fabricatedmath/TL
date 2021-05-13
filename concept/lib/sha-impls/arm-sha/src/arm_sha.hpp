#pragma once

#include <stdint.h>

enum Availability { Available, NotCompiled, NoSha };

extern "C" {
    int isAvailable_arm();
    void iterateHash_arm(const int numIter, uint32_t* startingHash);
};