#pragma once

#include <stdint.h>

enum Availability {
        Available, NotCompiled, NoSSE41, NoSha
    };

extern "C" {
    int x86ShaIsAvailable();
    void x86ShaIterateHash(const int numIter, uint32_t* const startingHash);
}