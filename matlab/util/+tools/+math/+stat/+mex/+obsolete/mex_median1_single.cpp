#include "mex.h"
#include "matrix.h"  // For MATLAB matrix operations
#include <algorithm>
#include <vector>
#include <immintrin.h>
#include <cfloat>

// Helper function to partition the array around the pivot
size_t partition(float *data, size_t left, size_t right, size_t pivotIndex) {
    float pivotValue = data[pivotIndex];
    std::swap(data[pivotIndex], data[right]);
    size_t storeIndex = left;
    for (size_t i = left; i < right; ++i) {
        if (data[i] < pivotValue) {
            std::swap(data[i], data[storeIndex]);
            ++storeIndex;
        }
    }
    std::swap(data[storeIndex], data[right]);
    return storeIndex;
}

// Quickselect algorithm to find the k-th smallest element
float quickselect(float *data, size_t left, size_t right, size_t k) {
    if (left == right) {
        return data[left];
    }

    size_t pivotIndex = left + (right - left) / 2;
    pivotIndex = partition(data, left, right, pivotIndex);

    if (k == pivotIndex) {
        return data[k];
    } else if (k < pivotIndex) {
        return quickselect(data, left, pivotIndex - 1, k);
    } else {
        return quickselect(data, pivotIndex + 1, right, k);
    }
}

// SIMD-based helper function to compute the median
float computeMedianSIMD(float *data, size_t n) {
    if (n % 2 == 1) {
        return quickselect(data, 0, n - 1, n / 2);
    } else {
        float leftMedian = quickselect(data, 0, n - 1, n / 2 - 1);
        float rightMedian = quickselect(data, 0, n - 1, n / 2);
        return (leftMedian + rightMedian) / 2.0f;
    }
}

// MEX gateway function
void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[]) {
    if (nrhs != 1) {
        mexErrMsgIdAndTxt("MATLAB:median_vector:invalidNumInputs", "One input required.");
    }
    if (!mxIsSingle(prhs[0]) || mxIsComplex(prhs[0]) || mxGetNumberOfDimensions(prhs[0]) != 2) {
        mexErrMsgIdAndTxt("MATLAB:median_vector:inputNotRealVector", "Input must be a real single precision vector.");
    }

    size_t n = mxGetNumberOfElements(prhs[0]);
    if (n == 0) {
        mexErrMsgIdAndTxt("MATLAB:median_vector:emptyInput", "Input vector is empty.");
    }

    float *dataPtr = (float*)mxGetData(prhs[0]);

    float median = computeMedianSIMD(dataPtr, n);

    // Cast to double before creating the output scalar
    plhs[0] = mxCreateDoubleScalar(static_cast<double>(median));
}
