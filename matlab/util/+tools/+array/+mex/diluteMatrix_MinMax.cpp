#include "mex.h"
#include <omp.h>

// Main function: Dilute a flattened array by sampling elements with StepSize and filtering
void mexFunction(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[]) {
    // Validate the number of inputs
    if (nrhs != 4) {
        mexErrMsgIdAndTxt("MATLAB:diluteMatrix:InvalidInput",
                          "Usage: output = diluteMatrix(inputArray, StepSize, MinVal, MaxVal)");
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

    // Validate MinVal and MaxVal
    if (!mxIsDouble(prhs[2]) || mxGetNumberOfElements(prhs[2]) != 1 ||
        !mxIsDouble(prhs[3]) || mxGetNumberOfElements(prhs[3]) != 1) {
        mexErrMsgIdAndTxt("MATLAB:diluteMatrix:InvalidRange",
                          "MinVal and MaxVal must be scalar doubles.");
    }
    double minVal = mxGetScalar(prhs[2]);
    double maxVal = mxGetScalar(prhs[3]);
    if (minVal > maxVal) {
        mexErrMsgIdAndTxt("MATLAB:diluteMatrix:InvalidRange",
                          "MinVal must be less than or equal to MaxVal.");
    }

    // Calculate tentative output size (before filtering)
    mwSize stepSizeInt = static_cast<mwSize>(stepSize);
    mwSize tentativeOutputSize = (totalInputElements + stepSizeInt - 1) / stepSizeInt;

    // Preallocate the output array
    mxArray* outputArray = mxCreateNumericMatrix(tentativeOutputSize, 1, mxDOUBLE_CLASS, mxREAL);
    double* output = static_cast<double*>(mxGetData(outputArray));

    // Counter for valid elements
    mwSize validCount = 0;

    // Parallelized sampling and filtering using OpenMP
    #pragma omp parallel
    {
        // Thread-local counter for valid elements
        mwSize localCount = 0;
        double* localBuffer = new double[tentativeOutputSize];

        if (inputClass == mxDOUBLE_CLASS) {
            const double* input = static_cast<const double*>(inputData);

            #pragma omp for nowait
            for (mwSize j = 0; j < tentativeOutputSize; ++j) {
                mwSize idx = j * stepSizeInt;
                if (idx < totalInputElements && input[idx] >= minVal && input[idx] <= maxVal) {
                    localBuffer[localCount++] = input[idx];
                }
            }
        } else if (inputClass == mxSINGLE_CLASS) {
            const float* input = static_cast<const float*>(inputData);

            #pragma omp for nowait
            for (mwSize j = 0; j < tentativeOutputSize; ++j) {
                mwSize idx = j * stepSizeInt;
                if (idx < totalInputElements && input[idx] >= static_cast<float>(minVal) &&
                    input[idx] <= static_cast<float>(maxVal)) {
                    localBuffer[localCount++] = static_cast<double>(input[idx]);
                }
            }
        }

        // Merge thread-local results into the global output array
        #pragma omp critical
        {
            for (mwSize i = 0; i < localCount; ++i) {
                output[validCount++] = localBuffer[i];
            }
        }

        delete[] localBuffer;
    }

    // Resize the output array to the actual number of valid elements
    mxSetM(outputArray, validCount); // Adjust the number of rows in the output array
    mxSetN(outputArray, 1);          // Ensure it's a column vector
    plhs[0] = outputArray;
}
