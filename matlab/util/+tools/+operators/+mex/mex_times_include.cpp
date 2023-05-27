#include "mex.h"
#include <omp.h>

void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[]) 
{
	mxClassID class_id;
	
    // Check for the number of arguments
    if (nrhs < 2 || nrhs > 3) {
        mexErrMsgIdAndTxt("MATLAB:mex_times:invalidNumInputs", "Two or three inputs required.");
    }

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

    // Get pointers to the data in the input arrays
    double *A = mxGetPr(prhs[0]);
    double *B = mxGetPr(prhs[1]);

    // Get the number of elements in the input arrays
    mwSize numel = mxGetNumberOfElements(prhs[0]);

    // Check if the optional argument is provided and is scalar
    bool useOpenMP = false;
    if (nrhs == 3 && mxIsLogicalScalar(prhs[2])) {
        useOpenMP = mxGetScalar(prhs[2]);
    }

    // Perform the element-wise multiplication and store the result in A
    if (useOpenMP) {
        #pragma omp parallel for
        for (int i = 0; i < numel; i++) {
            A[i] *= B[i];
        }
    } else {
        for (int i = 0; i < numel; i++) {
            A[i] *= B[i];
        }
    }
}
