#pragma once

#include <stdint.h>

enum Availability { Available, NotCompiled, NoSSE41, NoSha };

extern "C" {
    int isAvailable_x86();
    void iterateHash_x86(const int numIter, uint32_t* const startingHash);
}