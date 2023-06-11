//
// mex_onesExcept_include.cpp
//
// Author: Chen Tishler, May 2023
//
//
// Flags for cmex.py:
//
// 		$dtype: int8, int16, int32, int64, single, double
//

#include "mex.h"
#include <omp.h>

void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[]) {
    // Check number of input/output arguments
    if (nrhs != 3) {
        mexErrMsgIdAndTxt("MATLAB:mexFunctionOMP:input", "Three input arguments required.");
    }
    if (nlhs != 1) {
        mexErrMsgIdAndTxt("MATLAB:mexFunctionOMP:output", "One output argument required.");
    }

    // Get input arguments
    double *Mat = mxGetPr(prhs[0]);
    double Scalar = mxGetScalar(prhs[1]);
    double *Image = mxGetPr(prhs[2]);

    // Get matrix dimensions
    mwSize rows = mxGetM(prhs[0]);
    mwSize cols = mxGetN(prhs[0]);

    // Create output matrix W
    plhs[0] = mxCreateNumericArray(2, mxGetDimensions(prhs[0]), mxDOUBLE_CLASS, mxREAL);
    double *W = mxGetPr(plhs[0]);

    // Initialize W with 1's
    for (mwSize i = 0; i < rows * cols; ++i) {
        W[i] = 1;
    }

    // Set the elements of W to 0 if the corresponding element of Mat is greater than Scalar
    // Use OpenMP for parallelization
    #pragma omp parallel for
    for (mwSize i = 0; i < rows * cols; ++i) {
        if (Mat[i] > Scalar) {
            W[i] = 0;
        }
    }
}


