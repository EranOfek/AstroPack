#include "mex.h"

#ifdef _WIN32

#include <intrin.h>
#include <array>

bool is_avx512_supported() 
{
    //mexPrintf("WIN32 is_avx512_supported\n");    
    
    int cpuInfo[4];
    __cpuid(cpuInfo, 0);

    if (cpuInfo[0] >= 7) {
        std::array<int, 4> cpui;
        __cpuidex(cpui.data(), 7, 0);

        bool avx512f = (cpui[1] & (1 << 16)) != 0; // AVX-512 Foundation
        bool avx512cd = (cpui[1] & (1 << 28)) != 0; // AVX-512 Conflict Detection
        bool avx512pf = (cpui[1] & (1 << 26)) != 0; // AVX-512 Prefetch
        bool avx512er = (cpui[1] & (1 << 27)) != 0; // AVX-512 Exponential and Reciprocal
        bool avx512vl = (cpui[1] & (1 << 31)) != 0; // AVX-512 Vector Length extensions

        return avx512f && avx512cd && avx512pf && avx512er && avx512vl;
    }

    return false;
}

#else

#include <cpuid.h>

bool is_avx512_supported() 
{
    return __builtin_cpu_supports("avx512f");
    
#ifdef never    
    //mexPrintf("Linux is_avx512_supported\n");

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
#endif    
}
#endif

//=========================================================================

void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[]) 
{
    // check for the right number of arguments
    if (nrhs != 0) {
        mexErrMsgIdAndTxt("MyToolbox:arrayProduct:NumInputs", "No input required.");
    }
    if (nlhs > 1) {
        mexErrMsgIdAndTxt("MyToolbox:arrayProduct:NumOutputs", "One output required.");
    }

    bool result = is_avx512_supported();
    plhs[0] = mxCreateLogicalScalar(result);
}
