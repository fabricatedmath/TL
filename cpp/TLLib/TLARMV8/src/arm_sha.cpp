#include <arm_sha.hpp>

#ifdef ARM_COMPILED
    extern "C" {
    #include <sha256-sha-arm.h>
    #include "cpuinfo_aarch64.h"

    using namespace cpu_features;
    }

    static const Aarch64Features features = GetAarch64Info().features;
#endif

#ifdef ARM_COMPILED
bool ARMSHA::is_available() {
    return features.sha2;
}
#else
bool ARMSHA::is_available() {
    return false;
}
#endif

#ifdef X86_COMPILED
void ARMSHA::iterateHash(const int numIter, uint32_t* startingHash) {
    sha256_iter(numIter, startingHash);
}
#else
void ARMSHA::iterateHash(const int numIter, uint32_t* startingHash) {}
#endif