//
// mex_init_include.cpp
//
// Author: Dan Elhanati, October 2023
//
//
// Flags for cmex.py:
//
// 		$dtype: single, double, int8, int16, int32, int64
//

#include "mex.h"

void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[])
{
    // Check the number of input and output arguments
    if (nrhs != 3 || nlhs != 1) {
        mexErrMsgIdAndTxt("MEXFunction:inputError",
            "Three input arguments and one output argument required.");
    }
    
    // Check the data type of the second input argument (Val)
    if (!mxIsDouble(prhs[1]) || mxGetNumberOfElements(prhs[1]) != 1) {
        mexErrMsgIdAndTxt("MEXFunction:inputError",
            "Input argument 'Val' must be a scalar numeric value.");
    }
    
    // Input arguments
    double Val = mxGetScalar(prhs[1]);
    bool useOpenMP = *((int*)mxGetData(prhs[2]));

    double *Size = mxGetPr(prhs[0]);
    mwSize ndims = mxGetNumberOfElements(prhs[0]);

    if (ndims < 1 || ndims > 3) {
        mexErrMsgIdAndTxt("MEXFunction:inputError",
            "Input argument 'Size' must have 1, 2, or 3 dimensions.");
    }

    mwSize dims[3] = {1, 1, 1};
    for (mwSize i = 0; i < ndims; i++) {
        dims[i] = static_cast<mwSize>(Size[i]);
        if (dims[i] <= 0) {
            mexErrMsgIdAndTxt("MEXFunction:inputError",
                "Input argument 'Size' elements must be positive integers.");
        }
    }

    if (ndims == 1) {
        // For 1D case, create a row vector
        dims[1] = dims[0];
        dims[0] = 1;

        ndims = 2;
    }    

    mxArray *Result = mxCreateNumericArray(ndims, dims, MEX_TYPE, mxREAL);
    __Type *ResultData = (__Type*)mxGetData(Result);
    
    // Version without loop unrolling is commented as it appears to be slower
    // 
    // if (useOpenMP) {
    //     #pragma omp parallel for
    //     for (mwIndex i = 0; i < mxGetNumberOfElements(Result); i++) {
    //         ResultData[i] = Val;
    //     }
    // }
    // else {
    //     for (mwIndex i = 0; i < mxGetNumberOfElements(Result); i++) {
    //         ResultData[i] = Val;
    //     }
    // }


    if (useOpenMP) {
        #pragma omp parallel for
        for (mwIndex i = 0; i < mxGetNumberOfElements(Result); i += 8) {
            ResultData[i] = Val;
            ResultData[i + 1] = Val;
            ResultData[i + 2] = Val;
            ResultData[i + 3] = Val;
            ResultData[i + 4] = Val;
            ResultData[i + 5] = Val;
            ResultData[i + 6] = Val;
            ResultData[i + 7] = Val;
        }
    }
    else {
        for (mwIndex i = 0; i < mxGetNumberOfElements(Result); i += 8) {
            ResultData[i] = Val;
            ResultData[i + 1] = Val;
            ResultData[i + 2] = Val;
            ResultData[i + 3] = Val;
            ResultData[i + 4] = Val;
            ResultData[i + 5] = Val;
            ResultData[i + 6] = Val;
            ResultData[i + 7] = Val;
        }
    }


    plhs[0] = Result;

}
