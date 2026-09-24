#include "mex.h"
#include <cmath>
#include <omp.h>

// Template function to unwrap along a specific dimension
// mex unwrap_mex2.cpp CXXFLAGS="\$CXXFLAGS -O3 -march=native -fopenmp" LDFLAGS="\$LDFLAGS -fopenmp"

template <typename T>
void unwrapAlongDimension(const T* input, T* output, const mwSize* dims, mwSize numDims, mwSize unwrapDim, T cutoff, int numThreads) {
    mwSize totalElements = 1;     // Total number of elements in the array
    mwSize strides[64] = {1};     // Strides for all dimensions

    // Calculate strides and total number of elements
    for (mwSize i = 0; i < numDims; ++i) {
        totalElements *= dims[i];
        if (i > 0) {
            strides[i] = strides[i - 1] * dims[i - 1];
        }
    }

    mwSize sliceSize = dims[unwrapDim]; // Number of elements along the unwrapping dimension
    mwSize totalSlices = totalElements / sliceSize; // Total number of slices

    // Parallelize over slices
    #pragma omp parallel for num_threads(numThreads)
    for (mwSize slice = 0; slice < totalSlices; ++slice) {
        T phaseOffset = 0;

        // Compute the base index for the slice
        mwSize baseIndex = 0;
        mwSize tempSlice = slice;
        for (mwSize d = 0; d < numDims; ++d) {
            if (d == unwrapDim) {
                continue; // Skip the unwrapping dimension
            }
            baseIndex += (tempSlice % dims[d]) * strides[d];
            tempSlice /= dims[d];
        }

        // Unwrap along the specified dimension
        for (mwSize i = 0; i < sliceSize; ++i) {
            mwSize idx = baseIndex + i * strides[unwrapDim];
            if (i > 0) {
                mwSize prevIdx = baseIndex + (i - 1) * strides[unwrapDim];
                T diff = input[idx] - input[prevIdx];
                if (diff > cutoff) {
                    phaseOffset -= static_cast<T>(2 * M_PI);
                } else if (diff < -cutoff) {
                    phaseOffset += static_cast<T>(2 * M_PI);
                }
            }
            output[idx] = input[idx] + phaseOffset;
        }
    }
}

void mexFunction(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[]) {
    // Validate inputs
    if (nrhs < 1 || nrhs > 3) {
        mexErrMsgIdAndTxt("MATLAB:unwrap_mex:InvalidInput",
                          "Usage: output = unwrap_mex(input, cutoff, dim)");
    }
    if (nlhs != 1) {
        mexErrMsgIdAndTxt("MATLAB:unwrap_mex:InvalidOutput",
                          "One output required.");
    }
    if (!mxIsSingle(prhs[0]) && !mxIsDouble(prhs[0])) {
        mexErrMsgIdAndTxt("MATLAB:unwrap_mex:InvalidType",
                          "Input must be single or double.");
    }

    // Get input data
    const mxArray* inputArray = prhs[0];
    mwSize numDims = mxGetNumberOfDimensions(inputArray);
    const mwSize* dims = mxGetDimensions(inputArray);
    void* inputData = mxGetData(inputArray);

    // Get cutoff (default: pi)
    double cutoff = M_PI;
    if (nrhs >= 2) {
        if (!mxIsDouble(prhs[1]) || mxGetNumberOfElements(prhs[1]) != 1) {
            mexErrMsgIdAndTxt("MATLAB:unwrap_mex:InvalidCutoff",
                              "Cutoff must be a scalar double.");
        }
        cutoff = mxGetScalar(prhs[1]);
    }

    // Get dimension (default: first non-singleton dimension)
    mwSize unwrapDim = 0;
    if (nrhs == 3) {
        if (!mxIsDouble(prhs[2]) || mxGetNumberOfElements(prhs[2]) != 1) {
            mexErrMsgIdAndTxt("MATLAB:unwrap_mex:InvalidDim",
                              "Dimension must be a scalar double.");
        }
        unwrapDim = static_cast<mwSize>(mxGetScalar(prhs[2]) - 1); // Convert 1-based MATLAB indexing to 0-based C++ indexing
        if (unwrapDim >= numDims) {
            mexErrMsgIdAndTxt("MATLAB:unwrap_mex:InvalidDim",
                              "Dimension exceeds input dimensions.");
        }
    } else {
        // Find the first non-singleton dimension
        for (mwSize d = 0; d < numDims; ++d) {
            if (dims[d] > 1) {
                unwrapDim = d;
                break;
            }
        }
    }

    // Get number of threads for OpenMP
    int numThreads = omp_get_max_threads();

    // Create output array
    mxArray* outputArray = mxCreateNumericArray(numDims, dims, mxGetClassID(inputArray), mxREAL);
    void* outputData = mxGetData(outputArray);

    // Process input along the specified dimension
    if (mxIsDouble(inputArray)) {
        unwrapAlongDimension(static_cast<const double*>(inputData),
                             static_cast<double*>(outputData),
                             dims, numDims, unwrapDim, static_cast<double>(cutoff), numThreads);
    } else if (mxIsSingle(inputArray)) {
        unwrapAlongDimension(static_cast<const float*>(inputData),
                             static_cast<float*>(outputData),
                             dims, numDims, unwrapDim, static_cast<float>(cutoff), numThreads);
    }

    // Return the output
    plhs[0] = outputArray;
}
