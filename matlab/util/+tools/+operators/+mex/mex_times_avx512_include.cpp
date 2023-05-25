#include "mex.h"
#include <immintrin.h>
#include <omp.h>

void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[]) {
    // Check for the number of arguments
    if (nrhs < 2 || nrhs > 4) {
        mexErrMsgIdAndTxt("MATLAB:mex_times:invalidNumInputs", "Two to four inputs required.");
    }

    // Check if the input arguments are of the correct type (double)
    if (!mxIsDouble(prhs[0]) || !mxIsDouble(prhs[1])) {
        mexErrMsgIdAndTxt("MATLAB:mex_times:notDouble", "First two inputs must be of type double.");
    }

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
    bool useAVX512 = false;
    if (nrhs >= 3 && mxIsLogicalScalar(prhs[2])) {
        useOpenMP = mxGetScalar(prhs[2]);
    }
    if (nrhs == 4 && mxIsLogicalScalar(prhs[3])) {
        useAVX512 = mxGetScalar(prhs[3]);
    }

    // Perform the element-wise multiplication and store the result in A
    if (useOpenMP) {
        #pragma omp parallel for
        for (mwSize i = 0; i < numel; i++) {
            A[i] *= B[i];
        }
    } else if (useAVX512) {
        mwSize i;
        for (i = 0; i < numel - (numel % 8); i += 8) {
            __m512d vec1 = _mm512_loadu_pd(A + i);
            __m512d vec2 = _mm512_loadu_pd(B + i);
            __m512d vec3 = _mm512_mul_pd(vec1, vec2);
            _mm512_storeu_pd(A + i, vec3);
        }

        // Handle remaining elements if numel is not a multiple of 8
        for (; i < numel; i++) {
            A[i] *= B[i];
        }
    } else {
        for (mwSize i = 0; i < numel; i++) {
            A[i] *= B[i];
        }
    }
}
