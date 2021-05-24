#include <arm_sha.hpp>

#ifdef ARM_COMPILED
    extern "C" {
        #include <sha256-sha-arm.h>
        #include "cpuinfo_aarch64.h"
    }

    using namespace cpu_features;

    static const Aarch64Features features = GetAarch64Info().features;
#endif

int isAvailable_arm() {
    #ifdef ARM_COMPILED
        return features.sha2;
    #else
        return NotCompiled;
    #endif
}


void iterateHash_arm(const int numIter, uint32_t* startingHash) {
    #ifdef ARM_COMPILED
        sha256_iter_arm(numIter, startingHash);
    #else
        return;
    #endif
}