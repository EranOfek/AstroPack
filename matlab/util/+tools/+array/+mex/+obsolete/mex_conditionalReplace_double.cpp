#include "mex.h"
#include <immintrin.h> // For AVX intrinsics

void applyOperation(double *M, const double *A, double B, double V, mwSize size) {
    __m256d vecB = _mm256_set1_pd(B);
    __m256d vecV = _mm256_set1_pd(V);
    
    mwSize i;
    for (i = 0; i + 4 <= size; i += 4) {
        // Load 4 elements from A and M
        __m256d vecA = _mm256_loadu_pd(&A[i]);
        
        // Compare A > B
        __m256d mask = _mm256_cmp_pd(vecA, vecB, _CMP_GT_OQ);
        
        // Blend values
        __m256d result = _mm256_blendv_pd(_mm256_loadu_pd(&M[i]), vecV, mask);
        
        // Store result back to M
        _mm256_storeu_pd(&M[i], result);
    }
    
    // Process remaining elements
    for (; i < size; i++) {
        if (A[i] > B) {
            M[i] = V;
        }
    }
}

// The gateway function to be called from MATLAB
void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[]) {
    if (nrhs != 4) {
        mexErrMsgIdAndTxt("MATLAB:matrix_op:invalidNumInputs", "Four inputs required.");
    }

    if (!mxIsDouble(prhs[0]) || !mxIsDouble(prhs[1]) || !mxIsScalar(prhs[2]) || !mxIsScalar(prhs[3])) {
        mexErrMsgIdAndTxt("MATLAB:matrix_op:inputNotDouble", "Inputs must be double arrays and scalars.");
    }

    mxArray *M = mxDuplicateArray(prhs[0]); // Duplicate M to make sure it's writable
    const mxArray *A = prhs[1];
    double B = mxGetScalar(prhs[2]);
    double V = mxGetScalar(prhs[3]);

    double *MData = mxGetPr(M);
    const double *AData = mxGetPr(A);

    mwSize size = mxGetNumberOfElements(A); // Get the total number of elements

    applyOperation(MData, AData, B, V, size);

    plhs[0] = M; // Return the modified matrix M
}
