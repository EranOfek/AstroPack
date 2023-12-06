#include "mex.h"

#ifdef _WIN32

#include <intrin.h>
#include <array>

bool is_avx2_supported() 
{
    //mexPrintf("wIN32 is_avx2_supported\n");

    int cpuInfo[4];
    __cpuid(cpuInfo, 0);

    if (cpuInfo[0] >= 7) {
        std::array<int, 4> cpui;
        __cpuidex(cpui.data(), 7, 0);

        // AVX2 is bit 5 of the returned ebx register when calling cpuid with eax=7
        return (cpui[1] & (1 << 5)) != 0; 
    }

    return false;
}

#else

#include <cpuid.h>
#include <iostream>

bool is_avx2_supported() 
{
    //mexPrintf("Linux is_avx2_supported\n");

    bool avx2_supported = false;

    // EAX=7, ECX=0 is required for AVX2 feature detection
    int eax = 7;
    int ebx, ecx = 0, edx;

    // The AVX2 support bit is the 5th bit in EBX
    const int avx2_bit = 1 << 5;

    __asm__ volatile (
        "cpuid;"
        : "=b" (ebx) 
        : "a" (eax), "c" (ecx), "d" (edx)
    );

    avx2_supported = ebx & avx2_bit;

    return avx2_supported;
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

    bool result = is_avx2_supported();
    plhs[0] = mxCreateLogicalScalar(result);
}
