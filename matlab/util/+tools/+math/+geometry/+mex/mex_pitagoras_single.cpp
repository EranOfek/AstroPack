#include "mex.h"
#include <cmath>
#include <omp.h>

// Entry point for the MEX function
void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[]) {
    // Check for the correct number of input arguments
    if (nrhs != 2) {
        mexErrMsgIdAndTxt("MATLAB:mexFunction:invalidNumInputs", "Two input arguments required.");
    }
    // Check for the correct number of output arguments
    if (nlhs != 1) {
        mexErrMsgIdAndTxt("MATLAB:mexFunction:invalidNumOutputs", "One output argument required.");
    }
    
    // Get the input arguments
    const mxArray *X = prhs[0];
    const mxArray *Y = prhs[1];
    
    // Ensure the inputs are single arrays
    if (!mxIsSingle(X) || mxIsComplex(X) || !mxIsSingle(Y) || mxIsComplex(Y)) {
        mexErrMsgIdAndTxt("MATLAB:mexFunction:invalidInputType", "Inputs must be real single arrays.");
    }
    
    // Get the dimensions of the input arrays
    mwSize numDimsX = mxGetNumberOfDimensions(X);
    mwSize numDimsY = mxGetNumberOfDimensions(Y);
    const mwSize *dimsX = mxGetDimensions(X);
    const mwSize *dimsY = mxGetDimensions(Y);
    
    // Check that the dimensions of X and Y match
    if (numDimsX != numDimsY) {
        mexErrMsgIdAndTxt("MATLAB:mexFunction:dimensionMismatch", "Inputs must have the same number of dimensions.");
    }
    for (mwSize i = 0; i < numDimsX; ++i) {
        if (dimsX[i] != dimsY[i]) {
            mexErrMsgIdAndTxt("MATLAB:mexFunction:dimensionMismatch", "Dimensions of inputs must match.");
        }
    }
    
    // Create the output array R
    mxArray *R = mxCreateNumericArray(numDimsX, dimsX, mxSINGLE_CLASS, mxREAL);
    
    // Get pointers to the data in the input and output arrays
    float *XData = (float *)mxGetData(X);
    float *YData = (float *)mxGetData(Y);
    float *RData = (float *)mxGetData(R);
    
    // Calculate the number of elements in the arrays
    mwSize numElements = mxGetNumberOfElements(X);
    
    // Compute R = sqrt(X.^2 + Y.^2) in parallel using OpenMP with dynamic scheduling
    #pragma omp parallel for schedule(dynamic, 1024)
    for (mwSize i = 0; i < numElements; ++i) {
        RData[i] = std::sqrt(XData[i] * XData[i] + YData[i] * YData[i]);
    }
    
    // Set the output
    plhs[0] = R;
}
