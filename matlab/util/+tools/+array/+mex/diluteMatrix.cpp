#include "mex.h"
#include <omp.h>
#include <immintrin.h> // For SIMD instructions (AVX)

// mex diluteMatrix.cpp CXXFLAGS="\$CXXFLAGS -O3 -march=native -fopenmp" LDFLAGS="\$LDFLAGS -fopenmp"

// Main function: Dilute a flattened array by sampling elements with StepSize
void mexFunction(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[]) {
    // Validate the number of inputs
    if (nrhs != 2) {
        mexErrMsgIdAndTxt("MATLAB:diluteMatrix:InvalidInput",
                          "Usage: output = diluteMatrix(inputArray, StepSize)");
    }

    // Validate the input array
    const mxArray* inputArray = prhs[0];
    if (!mxIsSingle(inputArray) && !mxIsDouble(inputArray)) {
        mexErrMsgIdAndTxt("MATLAB:diluteMatrix:InvalidType",
                          "Input must be a single or double array.");
    }

    // Get input data and size
    const mwSize totalInputElements = mxGetNumberOfElements(inputArray);
    const void* inputData = mxGetData(inputArray);
    const mxClassID inputClass = mxGetClassID(inputArray);

    // Validate StepSize
    if (!mxIsDouble(prhs[1]) || mxGetNumberOfElements(prhs[1]) != 1) {
        mexErrMsgIdAndTxt("MATLAB:diluteMatrix:InvalidStepSize",
                          "StepSize must be a scalar double.");
    }
    double stepSize = mxGetScalar(prhs[1]);
    if (stepSize <= 0) {
        mexErrMsgIdAndTxt("MATLAB:diluteMatrix:InvalidStepSize",
                          "StepSize must be greater than zero.");
    }

    // Calculate output size
    mwSize stepSizeInt = static_cast<mwSize>(stepSize);
    mwSize totalOutputElements = (totalInputElements + stepSizeInt - 1) / stepSizeInt;

    // Create output array (column vector)
    mxArray* outputArray = mxCreateNumericMatrix(totalOutputElements, 1, inputClass, mxREAL);
    plhs[0] = outputArray;
    void* outputData = mxGetData(outputArray);

    // SIMD and OpenMP for faster processing
    #pragma omp parallel
    {
        if (inputClass == mxDOUBLE_CLASS) {
            const double* input = static_cast<const double*>(inputData);
            double* output = static_cast<double*>(outputData);

            #pragma omp for
            for (mwSize j = 0; j < totalOutputElements; j += 4) { // Process 4 elements at a time
                __m256d simd_input = _mm256_set_pd(
                    input[j * stepSizeInt],
                    input[(j + 1) * stepSizeInt],
                    input[(j + 2) * stepSizeInt],
                    input[(j + 3) * stepSizeInt]);
                _mm256_storeu_pd(&output[j], simd_input);
            }
        } else if (inputClass == mxSINGLE_CLASS) {
            const float* input = static_cast<const float*>(inputData);
            float* output = static_cast<float*>(outputData);

            #pragma omp for
            for (mwSize j = 0; j < totalOutputElements; j += 8) { // Process 8 elements at a time
                __m256 simd_input = _mm256_set_ps(
                    input[j * stepSizeInt],
                    input[(j + 1) * stepSizeInt],
                    input[(j + 2) * stepSizeInt],
                    input[(j + 3) * stepSizeInt],
                    input[(j + 4) * stepSizeInt],
                    input[(j + 5) * stepSizeInt],
                    input[(j + 6) * stepSizeInt],
                    input[(j + 7) * stepSizeInt]);
                _mm256_storeu_ps(&output[j], simd_input);
            }
        }
    }
}
