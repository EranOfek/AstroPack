#include "mex.h"
#include <algorithm>  // For std::min and std::max
#include <omp.h>      // For OpenMP

// Compile with: mex -O CXXFLAGS="\$CXXFLAGS -fopenmp -mavx" LDFLAGS="\$LDFLAGS -fopenmp" conv1.cpp

template <typename T>
void conv1D(const T* input, const T* kernel, T* output, mwSize inputSize, mwSize kernelSize) {
    mwSize halfKernel = kernelSize / 2;

    #pragma omp parallel for
    for (mwSize i = 0; i < inputSize; ++i) {
        T sum = 0;

        // Handle boundaries separately
        for (mwSize j = 0; j < kernelSize; ++j) {
            mwSize inputIndex = i + j - halfKernel;
            if (inputIndex >= 0 && inputIndex < inputSize) {
                sum += input[inputIndex] * kernel[j];
            }
        }

        output[i] = sum;
    }
}

void mexFunction(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[]) {
    if (nrhs != 2) {
        mexErrMsgIdAndTxt("MATLAB:conv1D:invalidNumInputs", "Two input arguments required.");
    }
    if (nlhs > 1) {
        mexErrMsgIdAndTxt("MATLAB:conv1D:invalidNumOutputs", "Too many output arguments.");
    }

    const mxArray* inputArray = prhs[0];
    const mxArray* kernelArray = prhs[1];

    if (!mxIsNumeric(inputArray) || !mxIsNumeric(kernelArray) ||
        mxIsComplex(inputArray) || mxIsComplex(kernelArray)) {
        mexErrMsgIdAndTxt("MATLAB:conv1D:inputNotReal", "Input and kernel must be non-complex numeric arrays.");
    }

    mwSize inputSize = mxGetNumberOfElements(inputArray);
    mwSize kernelSize = mxGetNumberOfElements(kernelArray);

    if (kernelSize > inputSize) {
        mexErrMsgIdAndTxt("MATLAB:conv1D:kernelTooLarge", "Kernel size cannot be larger than input size.");
    }

    mxClassID inputClass = mxGetClassID(inputArray);
    mxClassID kernelClass = mxGetClassID(kernelArray);

    if (inputClass != kernelClass) {
        mexErrMsgIdAndTxt("MATLAB:conv1D:classMismatch", "Input and kernel must be of the same class.");
    }

    mxArray* outputArray = mxCreateNumericArray(mxGetNumberOfDimensions(inputArray),
                                                mxGetDimensions(inputArray),
                                                inputClass, mxREAL);

    if (inputClass == mxDOUBLE_CLASS) {
        const double* input = mxGetPr(inputArray);
        const double* kernel = mxGetPr(kernelArray);
        double* output = mxGetPr(outputArray);
        conv1D(input, kernel, output, inputSize, kernelSize);
    } else if (inputClass == mxSINGLE_CLASS) {
        const float* input = reinterpret_cast<const float*>(mxGetData(inputArray));
        const float* kernel = reinterpret_cast<const float*>(mxGetData(kernelArray));
        float* output = reinterpret_cast<float*>(mxGetData(outputArray));
        conv1D(input, kernel, output, inputSize, kernelSize);
    } else {
        mexErrMsgIdAndTxt("MATLAB:conv1D:unsupportedClass", "Only single and double classes are supported.");
    }

    plhs[0] = outputArray;
}
