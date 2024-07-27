#include "mex.h"
#include <immintrin.h> // For AVX intrinsics

void applyOperation(float *M, const float *A, float B, float V, mwSize size) {
    __m256 vecB = _mm256_set1_ps(B);
    __m256 vecV = _mm256_set1_ps(V);
    
    mwSize i;
    for (i = 0; i + 8 <= size; i += 8) {
        // Load 8 elements from A and M
        __m256 vecA1 = _mm256_loadu_ps(&A[i]);
        __m256 vecA2 = _mm256_loadu_ps(&A[i + 4]);
        
        // Compare A > B
        __m256 mask1 = _mm256_cmp_ps(vecA1, vecB, _CMP_GT_OQ);
        __m256 mask2 = _mm256_cmp_ps(vecA2, vecB, _CMP_GT_OQ);
        
        // Blend values
        __m256 result1 = _mm256_blendv_ps(_mm256_loadu_ps(&M[i]), vecV, mask1);
        __m256 result2 = _mm256_blendv_ps(_mm256_loadu_ps(&M[i + 4]), vecV, mask2);
        
        // Store result back to M
        _mm256_storeu_ps(&M[i], result1);
        _mm256_storeu_ps(&M[i + 4], result2);
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

    if (!mxIsSingle(prhs[0]) || !mxIsSingle(prhs[1]) || !mxIsScalar(prhs[2]) || !mxIsScalar(prhs[3])) {
        mexErrMsgIdAndTxt("MATLAB:matrix_op:inputNotSingle", "Inputs must be single arrays and scalars.");
    }

    mxArray *M = mxDuplicateArray(prhs[0]); // Duplicate M to make sure it's writable
    const mxArray *A = prhs[1];
    float B = static_cast<float>(mxGetScalar(prhs[2]));
    float V = static_cast<float>(mxGetScalar(prhs[3]));

    float *MData = static_cast<float*>(mxGetData(M));
    const float *AData = static_cast<const float*>(mxGetData(A));

    mwSize size = mxGetNumberOfElements(A); // Get the total number of elements

    applyOperation(MData, AData, B, V, size);

    plhs[0] = M; // Return the modified matrix M
}
