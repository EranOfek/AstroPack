//
// mex_sincos_include.cpp
//
// Author: Dan Elhanati, July 2023
//
//
// Flags for cmex.py:
//
// 		$dtype: int8, int16, int32, int64, single, double
//


#include "mex.h"
#include <cmath>

typedef long long int64;

void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[])
{
    // Check the number of input and output arguments
    if (nrhs != 2)
        mexErrMsgTxt("Two input arguments are required.");
    if (nlhs != 2)
        mexErrMsgTxt("Two output arguments are required.");

    // Get the input array
    __Type *rads = (__Type*)mxGetData(prhs[0]);

    // Get the number of elements in the input arrays
    int64 numel = mxGetNumberOfElements(prhs[0]);

    // Check argument UseMP
    bool useOpenMP = (*((int*)mxGetData(prhs[1])) != 0);
	if (numel < 256)
		useOpenMP = false;

    // Create output arrays
    plhs[0] = mxCreateDoubleMatrix(1, numel, mxREAL);
    plhs[1] = mxCreateDoubleMatrix(1, numel, mxREAL);
    double *cosArray = mxGetPr(plhs[0]);
    double *sinArray = mxGetPr(plhs[1]);
    
    int64 remainder = numel % 8;
    int64 simd_size = numel - remainder;    
    
// No Loop unrolling:
    if (useOpenMP) {
        #pragma omp parallel for    
        for (mwSize i = 0; i < numel; ++i) {
            sincos(rads[i], &cosArray[i], &sinArray[i]);
        }
    }
    else {
        for (mwSize i = 0; i < numel; ++i) {
            sincos(rads[i], &cosArray[i], &sinArray[i]);
        }        
    }
    
    
// Loop unrolling:
//     if (useOpenMP) {
//         #pragma omp parallel for
//         for (mwSize i = 0; i < simd_size; i += 8) {
//             sincos(rads[i+0], &cosArray[i+0], &sinArray[i+0]);
//             sincos(rads[i+1], &cosArray[i+1], &sinArray[i+1]);
//             sincos(rads[i+2], &cosArray[i+2], &sinArray[i+2]);
//             sincos(rads[i+3], &cosArray[i+3], &sinArray[i+3]);
//             sincos(rads[i+4], &cosArray[i+4], &sinArray[i+4]);
//             sincos(rads[i+5], &cosArray[i+5], &sinArray[i+5]);
//             sincos(rads[i+6], &cosArray[i+6], &sinArray[i+6]);
//             sincos(rads[i+7], &cosArray[i+7], &sinArray[i+7]);
//         }
//     }
//     else {
//         for (mwSize i = 0; i < simd_size; i += 8) {
//             sincos(rads[i+0], &cosArray[i+0], &sinArray[i+0]);
//             sincos(rads[i+1], &cosArray[i+1], &sinArray[i+1]);
//             sincos(rads[i+2], &cosArray[i+2], &sinArray[i+2]);
//             sincos(rads[i+3], &cosArray[i+3], &sinArray[i+3]);
//             sincos(rads[i+4], &cosArray[i+4], &sinArray[i+4]);
//             sincos(rads[i+5], &cosArray[i+5], &sinArray[i+5]);
//             sincos(rads[i+6], &cosArray[i+6], &sinArray[i+6]);
//             sincos(rads[i+7], &cosArray[i+7], &sinArray[i+7]);
//         }    
//     }    
//     
//     if (numel-simd_size > 0) {
//         for (int64 i = simd_size; i < numel;  i++) {
//             sincos(rads[i], &cosArray[i], &sinArray[i]);
//         }
//     }
    
}