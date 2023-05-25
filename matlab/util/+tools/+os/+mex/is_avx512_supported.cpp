#include <cpuid.h>

bool is_avx512_supported() {
    unsigned int eax = 0, ebx = 0, ecx = 0, edx = 0;

    // Check for AVX512F support
    __cpuid_count(7, 0, eax, ebx, ecx, edx);
    if (!(ebx & bit_AVX512F)) {
        return false;
    }

    // Check for AVX512DQ support
    if (!(ebx & bit_AVX512DQ)) {
        return false;
    }

    // Check for AVX512BW and AVX512VL support (these are important for many applications)
    if (!(ebx & bit_AVX512BW) || !(ebx & bit_AVX512VL)) {
        return false;
    }

    // If we reach this point, all required AVX-512 features are supported
    return true;
}
