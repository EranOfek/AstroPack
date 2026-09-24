#include "mex.h"
#include <algorithm>
#include <vector>
#include <immintrin.h>

// Helper function to partition the array around the pivot
template <typename T>
size_t partition(T *data, size_t left, size_t right, size_t pivotIndex) {
    T pivotValue = data[pivotIndex];
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
template <typename T>
T quickselect(T *data, size_t left, size_t right, size_t k) {
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
template <typename T>
T computeMedianSIMD(T *data, size_t n) {
    if (n % 2 == 1) {
        return quickselect(data, 0, n - 1, n / 2);
    } else {
        T leftMedian = quickselect(data, 0, n - 1, n / 2 - 1);
        T rightMedian = quickselect(data, 0, n - 1, n / 2);
        return (leftMedian + rightMedian) / static_cast<T>(2.0);
    }
}

// MEX gateway function
void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[]) {
    if (nrhs != 1) {
        mexErrMsgIdAndTxt("MATLAB:median_vector:invalidNumInputs", "One input required.");
    }
    if (mxIsComplex(prhs[0]) || mxGetNumberOfDimensions(prhs[0]) != 2) {
        mexErrMsgIdAndTxt("MATLAB:median_vector:inputNotRealVector", "Input must be a real vector.");
    }

    size_t n = mxGetNumberOfElements(prhs[0]);
    if (n == 0) {
        mexErrMsgIdAndTxt("MATLAB:median_vector:emptyInput", "Input vector is empty.");
    }

    if (mxIsDouble(prhs[0])) {
        double *dataPtr = mxGetPr(prhs[0]);
        double median = computeMedianSIMD(dataPtr, n);
        plhs[0] = mxCreateDoubleScalar(median);
    } else if (mxIsSingle(prhs[0])) {
        float *dataPtr = static_cast<float*>(mxGetData(prhs[0]));
        float median = computeMedianSIMD(dataPtr, n);
        plhs[0] = mxCreateNumericMatrix(1, 1, mxSINGLE_CLASS, mxREAL);
        *static_cast<float*>(mxGetData(plhs[0])) = median;
    } else {
        mexErrMsgIdAndTxt("MATLAB:median_vector:inputNotSupported", "Input type not supported. Must be single or double.");
    }
}
