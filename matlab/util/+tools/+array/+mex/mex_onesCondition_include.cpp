//
// mex_onesCondition_include.cpp
//
// Author: Dan Elhanati, September 2023
//
//
// Flags for cmex.py:
//
// 		$dtype: single, double
//

#include "string.h"
#include "mex.h"

void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[]) {
    if (nrhs != 3 || nlhs > 1) {
        mexErrMsgIdAndTxt("MATLAB:mex_function:invalidInputOutput",
                          "Usage: Result = my_mex_function(Mat, Radius, Type)");
    }

    // Input arguments
    __Type *Mat = (__Type*)mxGetData(prhs[0]);
    double Radius = mxGetScalar(prhs[1]);
    bool useOpenMP = *((int*)mxGetData(prhs[2]));

    // Get matrix dimensions
    mwSize ndims = mxGetNumberOfDimensions(prhs[0]);
    const mwSize *dims = mxGetDimensions(prhs[0]);
    mwSize m, n, p;

    if (ndims == 2) {
        m = dims[0];
        n = dims[1];
        p = 1; // Treat the 2D matrix as a single stack
    } else if (ndims == 3) {
        m = dims[0];
        n = dims[1];
        p = dims[2];
    } else {
        m = dims[0];
        n = 1;
        p = 1; // Treat the 2D matrix as a single stack
    }

    mwSize outDims[3] = {m, n, p};
    mxArray *Result = mxCreateNumericArray(3, outDims, MEX_TYPE, mxREAL);

    __Type *ResultData = (__Type*)mxGetData(Result);

    if (useOpenMP) {
        #pragma omp parallel for
        for (mwIndex i = 0; i < m * n * p; i++) {
            if (Mat[i] > Radius) {
                ResultData[i] = 0;
            }
            else {
                ResultData[i] = 1;
            }
        }
    } else {  
        for (mwIndex i = 0; i < m * n * p; i++) {
            if (Mat[i] > Radius) {
                ResultData[i] = 0;
            }
            else {
                ResultData[i] = 1;
            }
        }
    }

    plhs[0] = Result;
}