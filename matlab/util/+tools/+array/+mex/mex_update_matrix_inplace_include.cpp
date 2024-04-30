//
// mex_update_matrix_inplace_include.cpp
//
// Author: Chen Tishler, April 2024
//
//
// Flags for cmex.py:
//
//      $dtype: int8, int16, int32, int64, single, double
//

#include "mex.h"


void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[])
{
    // Check for the correct number of input and output parameters
    if (nrhs != 8)
        mexErrMsgIdAndTxt("MatrixUpdateInPlace:InvalidNumInputs", "Seven inputs required.");
    if (nlhs != 0)
        mexErrMsgIdAndTxt("MatrixUpdateInPlace:InvalidNumOutputs", "No output expected.");

    mxClassID class_id;
    class_id = mxGetClassID(prhs[0]);
    if ((class_id != MEX_TYPE) && (class_id != MEX_UTYPE))
        mexErrMsgIdAndTxt("MatrixUpdateInPlace:InvalidInput", "A and B must be arrays of matching types");

    class_id = mxGetClassID(prhs[1]);
    if ((class_id != MEX_TYPE) && (class_id != MEX_UTYPE))
        mexErrMsgIdAndTxt("MatrixUpdateInPlace:InvalidInput", "A and B must be arrays of matching types");

    // Ensure all coordinate and size parameters are int32
    for (int i = 2; i < nrhs; i++) {
        if (!mxIsInt32(prhs[i]))
            mexErrMsgIdAndTxt("MatrixUpdateInPlace:InvalidInput", "Coordinates and size parameters must be int32");
    }

    // Pointers to the input matrices
    __Type *A = (__Type *)mxGetData(prhs[0]);
    __Type *B = (__Type *)mxGetData(prhs[1]);

    // Extract matrix dimensions
    const mwSize *dimsA = mxGetDimensions(prhs[0]);
    const mwSize *dimsB = mxGetDimensions(prhs[1]);

    // Extract coordinates and dimensions for the operation
    int x1_A = *((int*)mxGetData(prhs[2]));
    int y1_A = *((int*)mxGetData(prhs[3]));
    int x1_B = *((int*)mxGetData(prhs[4]));
    int y1_B = *((int*)mxGetData(prhs[5]));
    int width_B = *((int*)mxGetData(prhs[6]));
    int height_B = *((int*)mxGetData(prhs[7]));

    // Adjust from 1-based (MATLAB) to 0-based indexing
    x1_A -= 1;
    y1_A -= 1;
    x1_B -= 1;
    y1_B -= 1;

    // Validate dimensions and coordinates
    if (x1_A < 0 || y1_A < 0 || x1_B < 0 || y1_B < 0 ||
        x1_A + width_B > dimsA[1] || y1_A + height_B > dimsA[0] ||
        x1_B + width_B > dimsB[1] || y1_B + height_B > dimsB[0]) {
        mexErrMsgIdAndTxt("MatrixUpdateInPlace:IndexOutOfBounds", "Indices are out of bounds");
    }

    // Perform the update
    for (int i = 0;  i < height_B;  i++) {
        for (int j = 0;  j < width_B;  j++) {
            A[(y1_A + i) + (x1_A + j) * dimsA[0]] += B[(y1_B + i) + (x1_B + j) * dimsB[0]];
        }
    }
}

