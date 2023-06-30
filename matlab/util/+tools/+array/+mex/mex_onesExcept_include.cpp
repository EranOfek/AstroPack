//
// mex_onesExcept_include.cpp
//
// Author: Dan Elhanati, Chen Tishler, May 2023
//
//
// Flags for cmex.py:
//
// 		$dtype: int8, int16, int32, int64, single, double
//

#include "mex.h"
#include <omp.h>

typedef long long int64;

void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[]) {
    // Check number of input/output arguments
    if (nrhs != 4) {
        mexErrMsgIdAndTxt("MATLAB:mexFunctionOMP:input", "Three input arguments required.");
    }
    if (nlhs != 1) {
        mexErrMsgIdAndTxt("MATLAB:mexFunctionOMP:output", "One output argument required.");
    }
    
    // Get input arguments
    __Type *Mat = (__Type*)mxGetData(prhs[0]);
    __Type Scalar = mxGetScalar(prhs[1]);    
    double *Image = mxGetPr(prhs[2]);
    
    // Check argument UseMP
    bool useOpenMP = *((int*)mxGetData(prhs[3]));
	int64 numel = mxGetNumberOfElements(prhs[0]);
    if (numel < 256)
		useOpenMP = false;    
    
    // Get matrix dimensions
    int64 rows = mxGetM(prhs[0]);
    int64 cols = mxGetN(prhs[0]);

    // Create output matrix W
    plhs[0] = mxCreateLogicalMatrix(rows, cols);
    mxLogical *W = mxGetLogicals(plhs[0]);
  
    // Set the elements of W to 0 if the corresponding element of Mat is greater than Scalar
    // Use OpenMP for parallelization
    
    // Loop unrolling version (seems to be slower than stright forward loop):
//     int64 remainder = numel % 8;
//     int64 simd_size = numel - remainder;    
//     if (useOpenMP) {
//         #pragma omp parallel for        
//         for (int64 i = 0; i < simd_size; i += 8) {
//             if (Mat[i+0] > Scalar) {
//                 W[i+0] = false;
//             } else {
//                 W[i+0] = true;
//             }
//             if (Mat[i+1] > Scalar) {
//                 W[i+1] = false;
//             } else {
//                 W[i+1] = true;
//             }
//             if (Mat[i+2] > Scalar) {
//                 W[i+2] = false;
//             } else {
//                 W[i+2] = true;
//             }
//             if (Mat[i+3] > Scalar) {
//                 W[i+3] = false;
//             } else {
//                 W[i+3] = true;
//             }
//             if (Mat[i+4] > Scalar) {
//                 W[i+4] = false;
//             } else {
//                 W[i+4] = true;
//             }
//             if (Mat[i+5] > Scalar) {
//                 W[i+5] = false;
//             } else {
//                 W[i+5] = true;
//             }
//             if (Mat[i+6] > Scalar) {
//                 W[i+6] = false;
//             } else {
//                 W[i+6] = true;
//             }
//             if (Mat[i+7] > Scalar) {
//                 W[i+7] = false;
//             } else {
//                 W[i+7] = true;
//             }
//         }
//     }
//     else {
//         for (int64 i = 0; i < simd_size; i += 8) {
//             if (Mat[i+0] > Scalar) {
//                 W[i+0] = false;
//             } else {
//                 W[i+0] = true;
//             }
//             if (Mat[i+1] > Scalar) {
//                 W[i+1] = false;
//             } else {
//                 W[i+1] = true;
//             }
//             if (Mat[i+2] > Scalar) {
//                 W[i+2] = false;
//             } else {
//                 W[i+2] = true;
//             }
//             if (Mat[i+3] > Scalar) {
//                 W[i+3] = false;
//             } else {
//                 W[i+3] = true;
//             }
//             if (Mat[i+4] > Scalar) {
//                 W[i+4] = false;
//             } else {
//                 W[i+4] = true;
//             }
//             if (Mat[i+5] > Scalar) {
//                 W[i+5] = false;
//             } else {
//                 W[i+5] = true;
//             }
//             if (Mat[i+6] > Scalar) {
//                 W[i+6] = false;
//             } else {
//                 W[i+6] = true;
//             }
//             if (Mat[i+7] > Scalar) {
//                 W[i+7] = false;
//             } else {
//                 W[i+7] = true;
//             }
//         }
//     }
//     
//     if (numel-simd_size > 0) {
//         for (int64 i = simd_size; i < numel;  i++) {
//             if (Mat[i] > Scalar) {
//                 W[i] = false;
//             }
//             else {
//                 W[i] = true;
//             }
//         }
//     }
    
// No Loop unrolling:
    if (useOpenMP) {
        #pragma omp parallel for    
        for (int64 i = 0; i < numel;  i++) {
            if (Mat[i] > Scalar) {
                W[i] = false;
            }
            else {
                W[i] = true;
            }
        } 
    }
    else {
        for (int64 i = 0; i < numel;  i++) {
            if (Mat[i] > Scalar) {
                W[i] = false;
            }
            else {
                W[i] = true;
            }
        }        
    }
    
}