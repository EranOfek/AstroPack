#include "mex.h"
#include <immintrin.h>
#include <type_traits>

// mex CXXFLAGS="\$CXXFLAGS -mavx2" LDFLAGS="\$LDFLAGS" findSmallerFirst.cpp

// Template function to handle both single and double precision arrays
template <typename T>
void findSmallerFirst(const T* array, mwSize numElements, T value, mwIndex& index) {
    index = 0; // Default index to 0 if not found

    // SIMD processing
    if (std::is_same<T, double>::value) {
        __m256d valVec = _mm256_set1_pd(static_cast<double>(value));

        for (mwSize i = 0; i <= numElements - 4; i += 4) {
            __m256d vec = _mm256_loadu_pd(reinterpret_cast<const double*>(&array[i]));
            __m256d mask = _mm256_cmp_pd(vec, valVec, _CMP_LT_OQ);
            int maskBits = _mm256_movemask_pd(mask);

            if (maskBits != 0) {
                index = i + __builtin_ctz(maskBits) + 1; // 1-based index
                return; // Exit function if found
            }
        }
    } else if (std::is_same<T, float>::value) {
        __m256 valVec = _mm256_set1_ps(static_cast<float>(value));

        for (mwSize i = 0; i <= numElements - 8; i += 8) {
            __m256 vec = _mm256_loadu_ps(reinterpret_cast<const float*>(&array[i]));
            __m256 mask = _mm256_cmp_ps(vec, valVec, _CMP_LT_OQ);
            int maskBits = _mm256_movemask_ps(mask);

            if (maskBits != 0) {
                index = i + __builtin_ctz(maskBits) + 1; // 1-based index
                return; // Exit function if found
            }
        }
    }

    // Fallback to scalar processing for the remaining elements (if any)
    for (mwSize i = numElements - (numElements % 8); i < numElements; ++i) {
        if (array[i] < value) {
            index = i + 1; // 1-based index
            break;
        }
    }
}

// Gateway function
void mexFunction(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[]) {
    // Check for proper number of arguments
    if (nrhs != 2) {
        mexErrMsgIdAndTxt("MATLAB:findSmallerFirst:invalidNumInputs", "Two input arguments required.");
    }
    if (nlhs > 1) {
        mexErrMsgIdAndTxt("MATLAB:findSmallerFirst:maxlhs", "Too many output arguments.");
    }

    // Get the input array
    const mxArray* array = prhs[0];
    const mxArray* value = prhs[1];

    // Check that value is a scalar
    if (mxGetNumberOfElements(value) != 1) {
        mexErrMsgIdAndTxt("MATLAB:findSmallerFirst:inputNotScalar", "Value must be a scalar.");
    }

    mwSize numElements = mxGetNumberOfElements(array);
    mwIndex index = 0;

    // Determine the type of the array and value
    if (mxIsDouble(array) && mxIsDouble(value)) {
        double* arrayData = mxGetPr(array);
        double valueData = mxGetScalar(value);
        findSmallerFirst(arrayData, numElements, valueData, index);
    } else if (mxIsSingle(array) && mxIsSingle(value)) {
        float* arrayData = (float*)mxGetData(array);
        float valueData = static_cast<float>(mxGetScalar(value));
        findSmallerFirst(arrayData, numElements, valueData, index);
    } else {
        mexErrMsgIdAndTxt("MATLAB:findSmallerFirst:typeMismatch", "Array and Value must be of the same type.");
    }

    // Create the output
    plhs[0] = mxCreateDoubleScalar(static_cast<double>(index));
}
