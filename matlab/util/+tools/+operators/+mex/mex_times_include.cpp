//
// mex_times_include.cpp
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

typedef long long int64;

void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[]) 
{
	mxClassID class_id;
	
    // Check for the number of arguments
    if (nrhs < 2 || nrhs > 3) {
        mexErrMsgIdAndTxt("MATLAB:mex_times:invalidNumInputs", "Two or three inputs required.");
    }

    /*
    // Check if the input arguments are of the correct type
	class_id = mxGetClassID(prhs[0]);
	if ((class_id != MEX_TYPE) && (class_id != MEX_UTYPE)) 
		mexErrMsgIdAndTxt("MATLAB:bitsetFlag", "Array: Input matrix type does not match this function");  
	if (mxIsSparse(prhs[0])) 
		mexErrMsgIdAndTxt("MATLAB:bitsetFlag", "Array: Sparse array is not supported yet");  
	
	class_id = mxGetClassID(prhs[1]);
	if ((class_id != MEX_TYPE) && (class_id != MEX_UTYPE))
		mexErrMsgIdAndTxt("MATLAB:bitsetFlag", "Array: Input matrix type does not match this function");  
	if (mxIsSparse(prhs[1])) 
		mexErrMsgIdAndTxt("MATLAB:bitsetFlag", "Array: Sparse array is not supported yet");  	

    // Check if the input arguments are of the same size
    if (mxGetNumberOfElements(prhs[0]) != mxGetNumberOfElements(prhs[1])) {
        mexErrMsgIdAndTxt("MATLAB:mex_times:sizeMismatch", "First two inputs must be of the same size.");
    }
    */

    // Get pointers to the data in the input arrays
    __Type *A = (__Type*)mxGetData(prhs[0]);
    __Type *B = (__Type*)mxGetData(prhs[1]);

    // Get the number of elements in the input arrays
    int64 numel = mxGetNumberOfElements(prhs[0]);

    // Check argument UseMP
    bool useOpenMP = (nrhs < 3) || (*((int*)mxGetData(prhs[2])) != 0);
	if (numel < 256)
		useOpenMP = false;
	
    int64 remainder = numel % 8;
    int64 simd_size = numel - remainder;

    // Perform the element-wise multiplication and store the result in A
    if (useOpenMP) {
        #pragma omp parallel for
        for (int64 i = 0; i < simd_size; i += 8) {
            A[i+0] *= B[i+0];
            A[i+1] *= B[i+1];
            A[i+2] *= B[i+2];
            A[i+3] *= B[i+3];
            A[i+4] *= B[i+4];
            A[i+5] *= B[i+5];
            A[i+6] *= B[i+6];
            A[i+7] *= B[i+7];
        }
    } 
    else {
        for (int64 i = 0; i < simd_size; i += 8) {
            A[i+0] *= B[i+0];
            A[i+1] *= B[i+1];
            A[i+2] *= B[i+2];
            A[i+3] *= B[i+3];
            A[i+4] *= B[i+4];
            A[i+5] *= B[i+5];
            A[i+6] *= B[i+6];
            A[i+7] *= B[i+7];
        }
    }

    if (numel-simd_size > 0) {
        //mexPrintf("remainder: %d\n", numel-simd_size);
        for (int64 i = simd_size; i < numel;  i++) {
            //mexPrintf("loop %d: %lf * %lf\n", i, A[i], B[i]);
            A[i] *= B[i];
            //mexPrintf(" = %lf\n", A[i]);
        }    
    }    
}
