#include <x86exts_sha.hpp>

#ifdef X86_COMPILED
    extern "C" {
    #include <sha256-sse41-sha-x86.h>
    #include "cpuinfo_x86.h"

    using namespace cpu_features;
    }


    static const X86Features features = GetX86Info().features;
#endif

#ifdef X86_COMPILED
bool X86ExtsSHA::is_available() {
    return features.sse4_1 && features.sha;
}
#else
bool X86ExtsSHA::is_available() {
    return false;
}
#endif

#ifdef X86_COMPILED
void X86ExtsSHA::iterateHash(const int numIter, uint32_t* startingHash) {
    sha256_iter(numIter, startingHash);
}
#else
void X86ExtsSHA::iterateHash(const int numIter, uint32_t* startingHash) {}
#endif