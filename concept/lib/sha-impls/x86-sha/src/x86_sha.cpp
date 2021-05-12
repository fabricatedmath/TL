#include <x86_sha.hpp>

#ifdef X86_COMPILED
    extern "C" {
    #include <sha256_sse41_sha_x86.h>
    #include "cpuinfo_x86.h"
    }

    using namespace cpu_features;

    static const X86Features features = GetX86Info().features;
#endif

#ifdef X86_COMPILED
bool X86Sha::is_available() {
    return features.sse4_1 && features.sha;
}
#else
bool X86Sha::is_available() {
    return false;
}
#endif

#ifdef X86_COMPILED
void X86Sha::iterateHash(const int numIter, uint32_t* const startingHash) {
    sha256_iter(numIter, startingHash);
}
#else
void X86Sha::iterateHash(const int numIter, uint32_t* const startingHash) {}
#endif