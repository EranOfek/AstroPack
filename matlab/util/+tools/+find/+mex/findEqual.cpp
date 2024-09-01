#include "mex.h"
#include <vector>
#include <omp.h>
#include <immintrin.h> // For SIMD intrinsics

// mex -O -largeArrayDims CXXFLAGS="\$CXXFLAGS -fopenmp -march=native" LDFLAGS="\$LDFLAGS -fopenmp" findEqual.cpp

// Input  : - Array (double, single, unit32)
//          - Value
// Output : - Indices found (double)

// Template for finding elements
template <typename T>
void findElements(const T *data, T value, size_t numElements, std::vector<double> &indices);

// Specialization for double
template <>
void findElements<double>(const double *data, double value, size_t numElements, std::vector<double> &indices) {
    #pragma omp parallel
    {
        std::vector<double> localIndices;
        localIndices.reserve(numElements / omp_get_num_threads());

        #pragma omp for
        for (size_t i = 0; i < numElements; i += 4) {
            if (i + 4 <= numElements) {
                __m256d valueVector = _mm256_set1_pd(value);
                __m256d dataVector = _mm256_loadu_pd(&data[i]);
                __m256d cmpResult = _mm256_cmp_pd(dataVector, valueVector, _CMP_EQ_OQ);
                int mask = _mm256_movemask_pd(cmpResult);

                for (int j = 0; j < 4; ++j) {
                    if (i + j < numElements && (mask & (1 << j))) {
                        localIndices.push_back(static_cast<double>(i + j + 1));
                    }
                }
            } else {
                // Handle remaining elements
                for (size_t j = i; j < numElements; ++j) {
                    if (data[j] == value) {
                        localIndices.push_back(static_cast<double>(j + 1));
                    }
                }
            }
        }

        #pragma omp critical
        {
            indices.insert(indices.end(), localIndices.begin(), localIndices.end());
        }
    }
}

// Specialization for single
template <>
void findElements<float>(const float *data, float value, size_t numElements, std::vector<double> &indices) {
    #pragma omp parallel
    {
        std::vector<double> localIndices;
        localIndices.reserve(numElements / omp_get_num_threads());

        #pragma omp for
        for (size_t i = 0; i < numElements; i += 8) {
            if (i + 8 <= numElements) {
                __m256 valueVector = _mm256_set1_ps(value);
                __m256 dataVector = _mm256_loadu_ps(&data[i]);
                __m256 cmpResult = _mm256_cmp_ps(dataVector, valueVector, _CMP_EQ_OQ);
                int mask = _mm256_movemask_ps(cmpResult);

                for (int j = 0; j < 8; ++j) {
                    if (i + j < numElements && (mask & (1 << j))) {
                        localIndices.push_back(static_cast<double>(i + j + 1));
                    }
                }
            } else {
                // Handle remaining elements
                for (size_t j = i; j < numElements; ++j) {
                    if (data[j] == value) {
                        localIndices.push_back(static_cast<double>(j + 1));
                    }
                }
            }
        }

        #pragma omp critical
        {
            indices.insert(indices.end(), localIndices.begin(), localIndices.end());
        }
    }
}

// Specialization for uint32_T
template <>
void findElements<uint32_T>(const uint32_T *data, uint32_T value, size_t numElements, std::vector<double> &indices) {
    #pragma omp parallel
    {
        std::vector<double> localIndices;
        localIndices.reserve(numElements / omp_get_num_threads());

        #pragma omp for
        for (size_t i = 0; i < numElements; i += 8) {
            if (i + 8 <= numElements) {
                __m256i valueVector = _mm256_set1_epi32(value);
                __m256i dataVector = _mm256_loadu_si256(reinterpret_cast<const __m256i*>(&data[i]));
                __m256i cmpResult = _mm256_cmpeq_epi32(dataVector, valueVector);
                int mask = _mm256_movemask_epi8(cmpResult);

                for (int j = 0; j < 8; ++j) {
                    if (i + j < numElements && (mask & (1 << (j * 4)))) {
                        localIndices.push_back(static_cast<double>(i + j + 1));
                    }
                }
            } else {
                // Handle remaining elements
                for (size_t j = i; j < numElements; ++j) {
                    if (data[j] == value) {
                        localIndices.push_back(static_cast<double>(j + 1));
                    }
                }
            }
        }

        #pragma omp critical
        {
            indices.insert(indices.end(), localIndices.begin(), localIndices.end());
        }
    }
}

// The gateway function
void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[]) {
    if (nrhs != 2) {
        mexErrMsgIdAndTxt("MATLAB:find_elements:invalidNumInputs",
                          "Two inputs required.");
    }
    if (nlhs > 1) {
        mexErrMsgIdAndTxt("MATLAB:find_elements:invalidNumOutputs",
                          "Too many output arguments.");
    }

    // Check that the first input is a numeric array
    if (!mxIsNumeric(prhs[0]) || mxIsComplex(prhs[0])) {
        mexErrMsgIdAndTxt("MATLAB:find_elements:inputNotNumeric",
                          "First input must be a numeric array.");
    }

    // Check that the second input is a scalar
    if (!mxIsNumeric(prhs[1]) || mxIsComplex(prhs[1]) || mxGetNumberOfElements(prhs[1]) != 1) {
        mexErrMsgIdAndTxt("MATLAB:find_elements:inputNotScalar",
                          "Second input must be a numeric scalar.");
    }

    // Get the input array and scalar value
    const mxArray *array = prhs[0];
    double value = mxGetScalar(prhs[1]);

    // Vector to hold the indices
    std::vector<double> indices;
    size_t numElements = mxGetNumberOfElements(array);

    // Determine the data type and call the appropriate function
    if (mxIsDouble(array)) {
        findElements(mxGetPr(array), value, numElements, indices);
    } else if (mxIsSingle(array)) {
        findElements(static_cast<float*>(mxGetData(array)), static_cast<float>(value), numElements, indices);
    } else if (mxIsUint32(array)) {
        findElements(static_cast<uint32_T*>(mxGetData(array)), static_cast<uint32_T>(value), numElements, indices);
    } else {
        mexErrMsgIdAndTxt("MATLAB:find_elements:inputType",
                          "Unsupported data type.");
    }

    // Create the output array as a column vector
    mwSize numIndices = indices.size();
    plhs[0] = mxCreateDoubleMatrix(numIndices, 1, mxREAL);
    double *output = mxGetPr(plhs[0]);

    // Copy indices to output array
    for (mwSize i = 0; i < numIndices; ++i) {
        output[i] = indices[i];
    }
}
