#include "mex.h"
#include <immintrin.h> // For AVX intrinsics

void applyOperation(double *M, const double *A, double B, double V, mwSize size) {
    __m256d vecB = _mm256_set1_pd(B);
    __m256d vecV = _mm256_set1_pd(V);
    
    mwSize i;
    for (i = 0; i + 8 <= size; i += 8) {
        // Load 8 elements from A and M
        __m256d vecA1 = _mm256_loadu_pd(&A[i]);
        __m256d vecA2 = _mm256_loadu_pd(&A[i + 4]);
        
        // Compare A > B
        __m256d mask1 = _mm256_cmp_pd(vecA1, vecB, _CMP_GT_OQ);
        __m256d mask2 = _mm256_cmp_pd(vecA2, vecB, _CMP_GT_OQ);
        
        // Blend values
        __m256d result1 = _mm256_blendv_pd(_mm256_loadu_pd(&M[i]), vecV, mask1);
        __m256d result2 = _mm256_blendv_pd(_mm256_loadu_pd(&M[i + 4]), vecV, mask2);
        
        // Store result back to M
        _mm256_storeu_pd(&M[i], result1);
        _mm256_storeu_pd(&M[i + 4], result2);
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
