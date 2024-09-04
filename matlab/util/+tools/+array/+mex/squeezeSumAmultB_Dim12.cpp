#include "mex.h"
#include <immintrin.h>
#include <omp.h>
#include <type_traits>

// Performs: squeeze(sum(WInt.*MatXcen,[1 2])).*Norm;
// Input: WInt - A cube.
//        MatXcen - A cube.
//        Norm - A vector.
// Output : A column vector
// mex CXXFLAGS="\$CXXFLAGS -fopenmp -std=c++11 -mavx2" LDFLAGS="\$LDFLAGS -fopenmp" squeezeSumAmultB_Dim12.cpp


// SIMD optimized function for both float and double types
template <typename T>
void computeOutputSIMD(const T* A, const T* B, const T* C, T* output, mwSize rows, mwSize cols, mwSize slices) {
    #pragma omp parallel for
    for (mwSize i = 0; i < slices; ++i) {
        const T* A_plane = A + i * rows * cols;
        const T* B_plane = B + i * rows * cols;
        T sum = 0;

        if (std::is_same<T, double>::value) {
            __m256d sumVec = _mm256_setzero_pd(); // Initialize SIMD register for double precision
            mwSize j = 0;
            for (; j + 3 < rows * cols; j += 4) { // Process 4 elements at a time
                __m256d aVec = _mm256_loadu_pd(reinterpret_cast<const double*>(A_plane + j));
                __m256d bVec = _mm256_loadu_pd(reinterpret_cast<const double*>(B_plane + j));
                sumVec = _mm256_add_pd(sumVec, _mm256_mul_pd(aVec, bVec));
            }
            // Horizontal sum of the SIMD register
            __m256d haddVec = _mm256_hadd_pd(sumVec, sumVec);
            double temp[4];
            _mm256_storeu_pd(temp, haddVec);
            sum += temp[0] + temp[2]; // Only sum two elements due to horizontal addition

            // Remaining elements
            for (; j < rows * cols; ++j) {
                sum += A_plane[j] * B_plane[j];
            }
        } else if (std::is_same<T, float>::value) {
            __m256 sumVec = _mm256_setzero_ps(); // Initialize SIMD register for single precision
            mwSize j = 0;
            for (; j + 7 < rows * cols; j += 8) { // Process 8 elements at a time
                __m256 aVec = _mm256_loadu_ps(reinterpret_cast<const float*>(A_plane + j));
                __m256 bVec = _mm256_loadu_ps(reinterpret_cast<const float*>(B_plane + j));
                sumVec = _mm256_add_ps(sumVec, _mm256_mul_ps(aVec, bVec));
            }
            // Horizontal sum of the SIMD register
            __m256 haddVec = _mm256_hadd_ps(sumVec, sumVec);
            haddVec = _mm256_hadd_ps(haddVec, haddVec);
            float temp[8];
            _mm256_storeu_ps(temp, haddVec);
            sum += temp[0] + temp[4]; // Only sum two elements due to horizontal addition

            // Remaining elements
            for (; j < rows * cols; ++j) {
                sum += A_plane[j] * B_plane[j];
            }
        }

        // Multiply by C[i] and store in the output
        output[i] = sum * C[i];
    }
}

// MEX gateway function
void mexFunction(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[]) {
    if (nrhs != 3) {
        mexErrMsgIdAndTxt("MATLAB:squeezeSumAmultB_Dim12:invalidNumInputs", "Three inputs required.");
    }

    // Determine input types and dimensions
    const mxClassID classID = mxGetClassID(prhs[0]);
    const mwSize* dims = mxGetDimensions(prhs[0]);
    mwSize rows = dims[0], cols = dims[1], slices = dims[2];

    // Validate inputs
    for (int i = 0; i < 3; ++i) {
        if (mxGetNumberOfDimensions(prhs[i]) < 2 || mxGetNumberOfElements(prhs[2]) != slices) {
            mexErrMsgIdAndTxt("MATLAB:squeezeSumAmultB_Dim12:inputSizeMismatch", "Input dimensions are not consistent.");
        }
    }

    // Allocate output
    plhs[0] = mxCreateNumericMatrix(slices, 1, classID, mxREAL);

    // Dispatch based on the input type
    if (classID == mxDOUBLE_CLASS) {
        computeOutputSIMD(mxGetPr(prhs[0]), mxGetPr(prhs[1]), mxGetPr(prhs[2]), mxGetPr(plhs[0]), rows, cols, slices);
    } else if (classID == mxSINGLE_CLASS) {
        computeOutputSIMD(reinterpret_cast<const float*>(mxGetData(prhs[0])),
                          reinterpret_cast<const float*>(mxGetData(prhs[1])),
                          reinterpret_cast<const float*>(mxGetData(prhs[2])),
                          reinterpret_cast<float*>(mxGetData(plhs[0])), rows, cols, slices);
    } else {
        mexErrMsgIdAndTxt("MATLAB:squeezeSumAmultB_Dim12:inputNotSingleOrDouble", "Inputs must be single or double.");
    }
}
