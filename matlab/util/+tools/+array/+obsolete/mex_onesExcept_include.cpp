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
    mwSize ndims = mxGetNumberOfDimensions(prhs[0]);
    const mwSize *dims = mxGetDimensions(prhs[0]);
    mwSize rows, cols, stacks;

    if (ndims == 2) {
        rows = dims[0];
        cols = dims[1];
        stacks = 1; // Treat the 2D matrix as a single stack
    } else if (ndims == 3) {
        rows = dims[0];
        cols = dims[1];
        stacks = dims[2];
    } else {
        rows = dims[0];
        cols = 1;
        stacks = 1; // Treat the 2D matrix as a single stack
    }

    // Create output matrix W    
    mwSize outDims[3] = {rows, cols, stacks};
    plhs[0] = mxCreateLogicalArray(3, outDims);
    mxLogical *W = mxGetLogicals(plhs[0]);  
    // Set the elements of W to 0 if the corresponding element of Mat is greater than Scalar
    // Use OpenMP for parallelization
    
    // Loop unrolling version (seems to be slower than stright forward loop):
    if (useOpenMP) {
        #pragma omp parallel for    
        for (int64 k = 0; k < stacks; k++) {
            int64 offset = k * rows * cols;
            int64 simd_size = rows * cols - (rows * cols) % 8;
            for (int64 i = 0; i < simd_size; i += 8) {
                if (Mat[offset + i + 0] > Scalar) {
                    W[offset + i + 0] = false;
                } else {
                    W[offset + i + 0] = true;
                }
                if (Mat[offset + i + 1] > Scalar) {
                    W[offset + i + 1] = false;
                } else {
                    W[offset + i + 1] = true;
                }
                if (Mat[offset + i + 2] > Scalar) {
                    W[offset + i + 2] = false;
                } else {
                    W[offset + i + 2] = true;
                }
                if (Mat[offset + i + 3] > Scalar) {
                    W[offset + i + 3] = false;
                } else {
                    W[offset + i + 3] = true;
                }
                if (Mat[offset + i + 4] > Scalar) {
                    W[offset + i + 4] = false;
                } else {
                    W[offset + i + 4] = true;
                }
                if (Mat[offset + i + 5] > Scalar) {
                    W[offset + i + 5] = false;
                } else {
                    W[offset + i + 5] = true;
                }
                if (Mat[offset + i + 6] > Scalar) {
                    W[offset + i + 6] = false;
                } else {
                    W[offset + i + 6] = true;
                }
                if (Mat[offset + i + 7] > Scalar) {
                    W[offset + i + 7] = false;
                } else {
                    W[offset + i + 7] = true;
                }
            }

            // Process the remaining elements (if any) without loop unrolling
            for (int64 i = simd_size; i < rows * cols; i++) {
                if (Mat[offset + i] > Scalar) {
                    W[offset + i] = false;
                } else {
                    W[offset + i] = true;
                }
            }
        }
    } else {            
        for (int64 k = 0; k < stacks; k++) {
            int64 offset = k * rows * cols;
            int64 simd_size = rows * cols - (rows * cols) % 8;
            for (int64 i = 0; i < simd_size; i += 8) {
                if (Mat[offset + i + 0] > Scalar) {
                    W[offset + i + 0] = false;
                } else {
                    W[offset + i + 0] = true;
                }
                if (Mat[offset + i + 1] > Scalar) {
                    W[offset + i + 1] = false;
                } else {
                    W[offset + i + 1] = true;
                }
                if (Mat[offset + i + 2] > Scalar) {
                    W[offset + i + 2] = false;
                } else {
                    W[offset + i + 2] = true;
                }
                if (Mat[offset + i + 3] > Scalar) {
                    W[offset + i + 3] = false;
                } else {
                    W[offset + i + 3] = true;
                }
                if (Mat[offset + i + 4] > Scalar) {
                    W[offset + i + 4] = false;
                } else {
                    W[offset + i + 4] = true;
                }
                if (Mat[offset + i + 5] > Scalar) {
                    W[offset + i + 5] = false;
                } else {
                    W[offset + i + 5] = true;
                }
                if (Mat[offset + i + 6] > Scalar) {
                    W[offset + i + 6] = false;
                } else {
                    W[offset + i + 6] = true;
                }
                if (Mat[offset + i + 7] > Scalar) {
                    W[offset + i + 7] = false;
                } else {
                    W[offset + i + 7] = true;
                }
            }

            // Process the remaining elements (if any) without loop unrolling
            for (int64 i = simd_size; i < rows * cols; i++) {
                if (Mat[offset + i] > Scalar) {
                    W[offset + i] = false;
                } else {
                    W[offset + i] = true;
                }
            }
        }
    }
    
    
// No Loop unrolling:
//     if (useOpenMP) {
//         #pragma omp parallel for    
//         for (int64 k = 0; k < stacks; k++) {
//             int64 offset = k * rows * cols;
//             for (int64 i = 0; i < rows * cols; i++) {
//                 if (Mat[offset + i] > Scalar) {
//                     W[offset + i] = false;
//                 }
//                 else {
//                     W[offset + i] = true;
//                 }
//             }
//         }
//     }
//     else {
//         for (int64 k = 0; k < stacks; k++) {
//             int64 offset = k * rows * cols;
//             for (int64 i = 0; i < rows * cols; i++) {
//                 if (Mat[offset + i] > Scalar) {
//                     W[offset + i] = false;
//                 }
//                 else {
//                     W[offset + i] = true;
//                 }
//             }
//         }
//     }    


}