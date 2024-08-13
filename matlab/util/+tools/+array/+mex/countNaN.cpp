#include "mex.h"
#include <cmath>    // For std::isnan
#include <omp.h>    // For OpenMP
#include <xmmintrin.h>  // For SSE instructions
#include <cstdint>  // For uint64_t

// mex CXXFLAGS="\$CXXFLAGS -fopenmp" LDFLAGS="\$LDFLAGS -fopenmp" countNaN.cpp

// count NaN elements in a single or double array
// equivalent to: sum(isnan(Array))
// faster than matlab for Array with >~1e4 elements

// Inline function to check for NaN without std::isnan (for double)
inline bool isNaN(double value) {
    uint64_t bits = *reinterpret_cast<uint64_t*>(&value);
    return (bits & 0x7FF8000000000000ULL) == 0x7FF8000000000000ULL &&
           (bits & 0x000FFFFFFFFFFFFFULL) != 0;
}

// Inline function to check for NaN (for single precision)
inline bool isNaN(float value) {
    uint32_t bits = *reinterpret_cast<uint32_t*>(&value);
    return (bits & 0x7FC00000U) == 0x7FC00000U && (bits & 0x003FFFFFU) != 0;
}

// Optimized function to count NaN elements in double precision array
void countNaNDouble(const double *data, mwSize numElements, double &count) {
    count = 0;

    #pragma omp parallel
    {
        double localCount = 0;

        #pragma omp for nowait
        for (mwSize i = 0; i < numElements; ++i) {
            if (isNaN(data[i])) {
                ++localCount;
            }
        }

        #pragma omp atomic
        count += localCount;
    }
}

// Optimized function to count NaN elements in single precision array
void countNaNFloat(const float *data, mwSize numElements, double &count) {
    count = 0;

    #pragma omp parallel
    {
        double localCount = 0;

        #pragma omp for nowait
        for (mwSize i = 0; i < numElements; ++i) {
            if (isNaN(data[i])) {
                ++localCount;
            }
        }

        #pragma omp atomic
        count += localCount;
    }
}

// Main MEX function
void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[]) {
    // Check number of inputs and outputs
    if (nrhs != 1) {
        mexErrMsgIdAndTxt("MATLAB:count_nan:invalidNumInputs", "One input required.");
    }
    if (nlhs > 1) {
        mexErrMsgIdAndTxt("MATLAB:count_nan:invalidNumOutputs", "One output required.");
    }

    // Get input array
    const mxArray *inputArray = prhs[0];
    
    // Check if input is numeric
    if (!mxIsNumeric(inputArray)) {
        mexErrMsgIdAndTxt("MATLAB:count_nan:inputNotNumeric", "Input must be a numeric array.");
    }

    // Determine the type and size of input array
    mwSize numElements = mxGetNumberOfElements(inputArray);
    double nanCount = 0;

    // Handle double precision arrays
    if (mxIsDouble(inputArray)) {
        double *data = mxGetPr(inputArray);
        countNaNDouble(data, numElements, nanCount);
    } else if (mxIsSingle(inputArray)) {
        float *data = static_cast<float*>(mxGetData(inputArray));
        countNaNFloat(data, numElements, nanCount);
    } else {
        mexErrMsgIdAndTxt("MATLAB:count_nan:unsupportedType", "Unsupported input type. Only double and single precision are supported.");
    }

    // Create output array
    plhs[0] = mxCreateDoubleScalar(nanCount);
}
