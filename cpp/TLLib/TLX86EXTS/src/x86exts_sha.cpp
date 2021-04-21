#include <x86exts_sha.hpp>

extern "C" {
#include <sha256-sse41-sha-x86.h>
#include "cpuinfo_x86.h"
}
using namespace cpu_features;

static const X86Features features = GetX86Info().features;

bool X86ExtsSHA::is_available() {
    //bool sha_supported = sha_ext_support();
    //bool sse41_supported = sse41_ext_support();
    //features.sha && features.sse4_1
    return features.sse4_1 && features.sha;
}