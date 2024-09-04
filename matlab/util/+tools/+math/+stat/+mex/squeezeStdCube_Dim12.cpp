#include "mex.h"
#include <cmath>    // For std::sqrt, std::isnan
#include <limits>   // For NaN handling
#include <immintrin.h>  // For AVX2 intrinsics

// mex -R2018a CXXFLAGS="\$CXXFLAGS -fopenmp -mavx2 -std=c++17" LDFLAGS="\$LDFLAGS -fopenmp" squeezeStdCube_Dim12.cpp

// Input : A 3D array
// Output : mean (col vector) - equivalent to squeez(mean(A,[1 2],'omitnan'))
//          std (col vector) - equivalent to squeez(std(A,0,[1 2],'omitnan'))

template <typename T>
void computeMeanAndStdPerSliceSIMD(const T* input, T* meanOutput, T* stdOutput, mwSize rows, mwSize cols, mwSize slices) {
    #pragma omp parallel for
    for (mwSize k = 0; k < slices; ++k) {
        T sum = 0;
        T sumSq = 0;
        mwSize count = 0;

        const T* sliceData = input + k * rows * cols;

        if constexpr (std::is_same<T, double>::value) {
            // Use SIMD vectors for accumulation with double precision
            __m256d vec_sum = _mm256_setzero_pd();
            __m256d vec_sumSq = _mm256_setzero_pd();
            __m256d vec_count = _mm256_setzero_pd();

            mwSize i = 0;
            for (; i + 3 < rows * cols; i += 4) {
                __m256d vec_val = _mm256_loadu_pd(&sliceData[i]);

                // Mask to check for NaN
                __m256d mask = _mm256_cmp_pd(vec_val, vec_val, _CMP_ORD_Q);

                // Only process non-NaN values
                vec_sum = _mm256_add_pd(vec_sum, _mm256_and_pd(mask, vec_val));
                vec_sumSq = _mm256_add_pd(vec_sumSq, _mm256_and_pd(mask, _mm256_mul_pd(vec_val, vec_val)));
                vec_count = _mm256_add_pd(vec_count, _mm256_and_pd(mask, _mm256_set1_pd(1.0)));
            }

            // Accumulate results from the SIMD vectors
            double sum_array[4], sumSq_array[4], count_array[4];
            _mm256_storeu_pd(sum_array, vec_sum);
            _mm256_storeu_pd(sumSq_array, vec_sumSq);
            _mm256_storeu_pd(count_array, vec_count);

            for (int j = 0; j < 4; ++j) {
                sum += sum_array[j];
                sumSq += sumSq_array[j];
                count += static_cast<mwSize>(count_array[j]);
            }

            // Process remaining elements
            for (; i < rows * cols; ++i) {
                T value = sliceData[i];
                if (!std::isnan(value)) {
                    sum += value;
                    sumSq += value * value;
                    ++count;
                }
            }
        } else if constexpr (std::is_same<T, float>::value) {
            // Use SIMD vectors for accumulation with single precision
            __m256 vec_sum = _mm256_setzero_ps();
            __m256 vec_sumSq = _mm256_setzero_ps();
            __m256 vec_count = _mm256_setzero_ps();

            mwSize i = 0;
            for (; i + 7 < rows * cols; i += 8) {
                __m256 vec_val = _mm256_loadu_ps(&sliceData[i]);

                // Mask to check for NaN
                __m256 mask = _mm256_cmp_ps(vec_val, vec_val, _CMP_ORD_Q);

                // Only process non-NaN values
                vec_sum = _mm256_add_ps(vec_sum, _mm256_and_ps(mask, vec_val));
                vec_sumSq = _mm256_add_ps(vec_sumSq, _mm256_and_ps(mask, _mm256_mul_ps(vec_val, vec_val)));
                vec_count = _mm256_add_ps(vec_count, _mm256_and_ps(mask, _mm256_set1_ps(1.0f)));
            }

            // Accumulate results from the SIMD vectors
            float sum_array[8], sumSq_array[8], count_array[8];
            _mm256_storeu_ps(sum_array, vec_sum);
            _mm256_storeu_ps(sumSq_array, vec_sumSq);
            _mm256_storeu_ps(count_array, vec_count);

            for (int j = 0; j < 8; ++j) {
                sum += sum_array[j];
                sumSq += sumSq_array[j];
                count += static_cast<mwSize>(count_array[j]);
            }

            // Process remaining elements
            for (; i < rows * cols; ++i) {
                T value = sliceData[i];
                if (!std::isnan(value)) {
                    sum += value;
                    sumSq += value * value;
                    ++count;
                }
            }
        }

        if (count > 1) {
            T mean = sum / count;
            meanOutput[k] = mean;
            stdOutput[k] = std::sqrt((sumSq - sum * mean) / (count - 1));
        } else {
            meanOutput[k] = std::numeric_limits<T>::quiet_NaN();
            stdOutput[k] = std::numeric_limits<T>::quiet_NaN();
        }
    }
}

void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[]) {
    if (nrhs != 1) {
        mexErrMsgIdAndTxt("mexFunction:InvalidNumInputs", "One input required.");
    }
    if (nlhs != 2) {
        mexErrMsgIdAndTxt("mexFunction:InvalidNumOutputs", "Two outputs required.");
    }

    const mxArray* inputArray = prhs[0];
    mwSize rows = mxGetDimensions(inputArray)[0];
    mwSize cols = mxGetDimensions(inputArray)[1];
    mwSize slices = mxGetDimensions(inputArray)[2];
    mwSize numSlices = slices;

    mxClassID classID = mxGetClassID(inputArray);
    mxArray* meanArray = mxCreateNumericMatrix(numSlices, 1, classID, mxREAL);
    mxArray* stdArray = mxCreateNumericMatrix(numSlices, 1, classID, mxREAL);

    plhs[0] = meanArray;
    plhs[1] = stdArray;

    if (classID == mxDOUBLE_CLASS) {
        double* inputData = mxGetPr(inputArray);
        double* meanData = mxGetPr(meanArray);
        double* stdData = mxGetPr(stdArray);

        computeMeanAndStdPerSliceSIMD(inputData, meanData, stdData, rows, cols, slices);
    } else if (classID == mxSINGLE_CLASS) {
        float* inputData = (float*)mxGetData(inputArray);
        float* meanData = (float*)mxGetData(meanArray);
        float* stdData = (float*)mxGetData(stdArray);

        computeMeanAndStdPerSliceSIMD(inputData, meanData, stdData, rows, cols, slices);
    } else {
        mexErrMsgIdAndTxt("mexFunction:InvalidInputType", "Input must be of type 'single' or 'double'.");
    }
}
