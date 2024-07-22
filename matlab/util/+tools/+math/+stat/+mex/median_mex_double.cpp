#include "mex.h"
#include <algorithm>  // For std::nth_element

// Quickselect algorithm to find the k-th smallest element
template <typename T>
T quickselect(T* arr, int left, int right, int k) {
    if (left == right) return arr[left];
    
    int pivotIndex = left + (right - left) / 2;
    T pivotValue = arr[pivotIndex];
    
    // Move pivot to the end
    std::swap(arr[pivotIndex], arr[right]);
    
    int storeIndex = left;
    for (int i = left; i < right; ++i) {
        if (arr[i] < pivotValue) {
            std::swap(arr[i], arr[storeIndex]);
            ++storeIndex;
        }
    }
    
    // Move pivot to its final place
    std::swap(arr[storeIndex], arr[right]);
    
    if (k == storeIndex) {
        return arr[k];
    } else if (k < storeIndex) {
        return quickselect(arr, left, storeIndex - 1, k);
    } else {
        return quickselect(arr, storeIndex + 1, right, k);
    }
}

// The MEX function
void mexFunction(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[]) {
    // Check for proper number of arguments
    if (nrhs != 1) {
        mexErrMsgIdAndTxt("MATLAB:median:invalidNumInputs", "One input required.");
    }
    if (nlhs > 1) {
        mexErrMsgIdAndTxt("MATLAB:median:invalidNumOutputs", "Too many output arguments.");
    }
    
    // Get the input array
    const mxArray* inputArray = prhs[0];
    if (!mxIsNumeric(inputArray) || mxIsComplex(inputArray)) {
        mexErrMsgIdAndTxt("MATLAB:median:inputNotReal", "Input must be a non-complex numeric array.");
    }
    
    // Get dimensions and data
    mwSize numel = mxGetNumberOfElements(inputArray);
    double* data = mxGetPr(inputArray);
    
    // Find the median
    mwSize medianIndex = numel / 2;
    double median = quickselect(data, 0, numel - 1, medianIndex);
    
    if (numel % 2 == 0) {
        double median2 = quickselect(data, 0, numel - 1, medianIndex - 1);
        median = (median + median2) / 2.0;
    }
    
    // Create output array
    plhs[0] = mxCreateDoubleScalar(median);
}
