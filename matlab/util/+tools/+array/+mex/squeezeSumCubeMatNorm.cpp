#include "mex.h"
#include <cmath>
#include <algorithm>
#include <omp.h>       // Include OpenMP header
#include <immintrin.h> // Include SIMD intrinsics

// mex CXXFLAGS="\$CXXFLAGS -O3 -mavx" squeezeSumCubeMatNorm.cpp

// Operation: squeeze(sum(WInt.*MatXcen,[1 2],'omitnan')).*Norm;
// tic;for I=1:10000, b=squeeze(sum(WInt.*MatXcen,[1 2],'omitnan')).*Norm; end, toc          
// tic;for I=1:10000, a=squeezeSumCubeMatNorm3(WInt, MatXcen, Norm);end, toc                 

// Template function to perform the computation with SIMD
template <typename T>
void compute(const T* WInt, const T* MatXcen, const T* Norm, T* output, mwSize N, mwSize M);

template <>
void compute<double>(const double* WInt, const double* MatXcen, const double* Norm, double* output, mwSize N, mwSize M) {
    // Parallelize the outer loop with OpenMP
    #pragma omp parallel for
    for (mwSize m = 0; m < M; ++m) {
        double sum = 0;
        // Use SIMD intrinsics for the inner loop
        mwSize i = 0;
        __m256d vec_sum = _mm256_setzero_pd(); // Initialize SIMD accumulator

        // Process data in chunks of 4 doubles
        for (; i + 4 <= N * N; i += 4) {
            __m256d vec_WInt = _mm256_loadu_pd(&WInt[i + m * N * N]);
            __m256d vec_MatXcen = _mm256_loadu_pd(&MatXcen[i]);
            __m256d vec_value = _mm256_mul_pd(vec_WInt, vec_MatXcen);
            vec_sum = _mm256_add_pd(vec_sum, vec_value);
        }

        // Horizontal add to get the sum of the SIMD vector
        double temp[4];
        _mm256_storeu_pd(temp, vec_sum);
        for (int j = 0; j < 4; ++j) {
            sum += temp[j];
        }

        // Process remaining elements
        for (; i < N * N; ++i) {
            double value = WInt[i + m * N * N] * MatXcen[i];
            if (!std::isnan(value)) {
                sum += value;
            }
        }
        
        output[m] = sum * Norm[m];
    }
}

// Specialization for float
template <>
void compute<float>(const float* WInt, const float* MatXcen, const float* Norm, float* output, mwSize N, mwSize M) {
    // Parallelize the outer loop with OpenMP
    #pragma omp parallel for
    for (mwSize m = 0; m < M; ++m) {
        float sum = 0;
        // Use SIMD intrinsics for the inner loop
        mwSize i = 0;
        __m256 vec_sum = _mm256_setzero_ps(); // Initialize SIMD accumulator

        // Process data in chunks of 8 floats
        for (; i + 8 <= N * N; i += 8) {
            __m256 vec_WInt = _mm256_loadu_ps(&WInt[i + m * N * N]);
            __m256 vec_MatXcen = _mm256_loadu_ps(&MatXcen[i]);
            __m256 vec_value = _mm256_mul_ps(vec_WInt, vec_MatXcen);
            vec_sum = _mm256_add_ps(vec_sum, vec_value);
        }

        // Horizontal add to get the sum of the SIMD vector
        float temp[8];
        _mm256_storeu_ps(temp, vec_sum);
        for (int j = 0; j < 8; ++j) {
            sum += temp[j];
        }

        // Process remaining elements
        for (; i < N * N; ++i) {
            float value = WInt[i + m * N * N] * MatXcen[i];
            if (!std::isnan(value)) {
                sum += value;
            }
        }
        
        output[m] = sum * Norm[m];
    }
}

// MEX function entry point
void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[]) {
    if (nrhs != 3) {
        mexErrMsgIdAndTxt("MATLAB:squeezeSumCubeMatNorm:invalidNumInputs",
                          "Three inputs required: WInt, MatXcen, and Norm.");
    }

    // Get inputs
    const mxArray *WInt = prhs[0];
    const mxArray *MatXcen = prhs[1];
    const mxArray *Norm = prhs[2];

    // Check that WInt is a 3D array and MatXcen is a 2D array
    if (mxGetNumberOfDimensions(WInt) != 3 || mxGetNumberOfDimensions(MatXcen) != 2) {
        mexErrMsgIdAndTxt("MATLAB:squeezeSumCubeMatNorm:invalidDimensions",
                          "WInt must be a 3D array and MatXcen must be a 2D matrix.");
    }

    // Get dimensions
    const mwSize* dimsWInt = mxGetDimensions(WInt);
    const mwSize* dimsMatXcen = mxGetDimensions(MatXcen);

    mwSize N1 = dimsMatXcen[0];  // Rows of MatXcen
    mwSize N2 = dimsMatXcen[1];  // Columns of MatXcen
    mwSize N3 = dimsWInt[0];     // Rows of WInt
    mwSize N4 = dimsWInt[1];     // Columns of WInt
    mwSize M = dimsWInt[2];      // Depth of WInt

    // Check if MatXcen is square and dimensions match with WInt
    if (N1 != N3 || N2 != N4) {
        mexErrMsgIdAndTxt("MATLAB:squeezeSumCubeMatNorm:dimensionMismatch",
                          "WInt's first two dimensions (%lu x %lu) do not match MatXcen's dimensions (%lu x %lu).", 
                          (unsigned long)N3, (unsigned long)N4, (unsigned long)N1, (unsigned long)N2);
    }

    // Check that Norm is a column vector of length M
    if (mxGetM(Norm) != M || mxGetN(Norm) != 1) {
        mexErrMsgIdAndTxt("MATLAB:squeezeSumCubeMatNorm:invalidNorm",
                          "Norm must be a column vector of length %lu.", (unsigned long)M);
    }

    // Create output matrix
    mxClassID classID = mxGetClassID(WInt);
    plhs[0] = mxCreateNumericMatrix(M, 1, classID, mxREAL);
    void* output = mxGetData(plhs[0]);

    // Compute based on data type
    if (classID == mxDOUBLE_CLASS) {
        compute(static_cast<double*>(mxGetData(WInt)), 
                static_cast<double*>(mxGetData(MatXcen)), 
                static_cast<double*>(mxGetData(Norm)), 
                static_cast<double*>(output), N1, M);
    } else if (classID == mxSINGLE_CLASS) {
        compute(static_cast<float*>(mxGetData(WInt)), 
                static_cast<float*>(mxGetData(MatXcen)), 
                static_cast<float*>(mxGetData(Norm)), 
                static_cast<float*>(output), N1, M);
    } else {
        mexErrMsgIdAndTxt("MATLAB:squeezeSumCubeMatNorm:invalidType",
                          "All inputs must be of the same data type, either all single or all double.");
    }
}
