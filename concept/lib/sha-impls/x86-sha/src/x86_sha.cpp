#include <x86_sha.hpp>

#ifdef X86_COMPILED
    extern "C" {
        #include <sha256_sse41_sha_x86.h>
        #include "cpuinfo_x86.h"
    }

    using namespace cpu_features;

    static const X86Features features = GetX86Info().features;
#endif

int isAvailable_x86() {
    #ifdef X86_COMPILED
        if (!features.sse4_1) {
            return NoSSE41;
        }

        if (!features.sha) {
            return NoSha;
        }

        return Available;
    #else
        return NotCompiled;
    #endif
}
    
void iterateHash_x86(const int numIter, uint32_t* const startingHash) {
    #ifdef X86_COMPILED
        sha256_iter(numIter, startingHash);
    #else
        return;
    #endif
}
