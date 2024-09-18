#include "mex.h"
#include <omp.h>
#include <immintrin.h>
#include <cmath>  // For isnan

// mex CXXFLAGS="\$CXXFLAGS -fopenmp -mavx" LDFLAGS="\$LDFLAGS -fopenmp" sum2InRange.cpp

// Function to compute sum, count, and sum of squares for double array
void computeSumDouble(const mxArray* arr, double minVal, double maxVal, double& sum, double& count, double& sum2) {
    const double* data = reinterpret_cast<const double*>(mxGetData(arr));
    mwSize numElements = mxGetNumberOfElements(arr);

    double local_sum = 0.0;
    double local_count = 0.0;
    double local_sum2 = 0.0;

    #pragma omp parallel reduction(+:local_sum, local_count, local_sum2)
    {
        #pragma omp for
        for (mwSize i = 0; i < numElements; i += 4) {
            __m256d v = _mm256_loadu_pd(&data[i]);
            double temp[4];
            _mm256_storeu_pd(temp, v);

            for (int j = 0; j < 4 && i + j < numElements; ++j) {
                if (!std::isnan(temp[j]) && temp[j] > minVal && temp[j] < maxVal) {
                    local_sum += temp[j];
                    local_count++;
                    local_sum2 += temp[j] * temp[j];
                }
            }
        }
    }

    sum = local_sum;
    count = local_count;
    sum2 = local_sum2;
}

// Function to compute sum, count, and sum of squares for float array
void computeSumFloat(const mxArray* arr, float minVal, float maxVal, float& sum, double& count, float& sum2) {
    const float* data = reinterpret_cast<const float*>(mxGetData(arr));
    mwSize numElements = mxGetNumberOfElements(arr);

    float local_sum = 0.0f;
    double local_count = 0.0;
    float local_sum2 = 0.0f;

    #pragma omp parallel reduction(+:local_sum, local_count, local_sum2)
    {
        #pragma omp for
        for (mwSize i = 0; i < numElements; i += 8) {
            __m256 v = _mm256_loadu_ps(&data[i]);
            float temp[8];
            _mm256_storeu_ps(temp, v);

            for (int j = 0; j < 8 && i + j < numElements; ++j) {
                if (!std::isnan(temp[j]) && temp[j] > minVal && temp[j] < maxVal) {
                    local_sum += temp[j];
                    local_count++;
                    local_sum2 += temp[j] * temp[j];
                }
            }
        }
    }

    sum = local_sum;
    count = local_count;
    sum2 = local_sum2;
}

// Entry point of the MEX function
void mexFunction(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[]) {
    if (nrhs != 3) {
        mexErrMsgIdAndTxt("MATLAB:sum_array_optimized:invalidNumInputs", "Three inputs required.");
    }
    if (nlhs > 3) {
        mexErrMsgIdAndTxt("MATLAB:sum_array_optimized:invalidNumOutputs", "Too many output arguments.");
    }
    
    if (!mxIsDouble(prhs[0]) && !mxIsSingle(prhs[0])) {
        mexErrMsgIdAndTxt("MATLAB:sum_array_optimized:inputNotNumeric", "Input array must be numeric.");
    }
    
    if (!mxIsDouble(prhs[1]) || !mxIsDouble(prhs[2]) || mxGetNumberOfElements(prhs[1]) != 1 || mxGetNumberOfElements(prhs[2]) != 1) {
        mexErrMsgIdAndTxt("MATLAB:sum_array_optimized:invalidRangeInputs", "MinVal and MaxVal must be double scalars.");
    }

    double minVal = mxGetScalar(prhs[1]);
    double maxVal = mxGetScalar(prhs[2]);

    mwSize numElements = mxGetNumberOfElements(prhs[0]);
    mxClassID classID = mxGetClassID(prhs[0]);

    mxArray* outSum2Array = mxCreateNumericMatrix(1, 1, classID, mxREAL);
    mxArray* outSumArray = mxCreateNumericMatrix(1, 1, classID, mxREAL);
    mxArray* outCountArray = mxCreateNumericMatrix(1, 1, mxDOUBLE_CLASS, mxREAL);

    if (classID == mxDOUBLE_CLASS) {
        double sum;
        double count;
        double sum2;
        computeSumDouble(prhs[0], minVal, maxVal, sum, count, sum2);
        *reinterpret_cast<double*>(mxGetData(outSum2Array)) = sum2;
        *reinterpret_cast<double*>(mxGetData(outSumArray)) = sum;
        *reinterpret_cast<double*>(mxGetData(outCountArray)) = count;
    } else if (classID == mxSINGLE_CLASS) {
        float sum;
        double count;
        float sum2;
        computeSumFloat(prhs[0], static_cast<float>(minVal), static_cast<float>(maxVal), sum, count, sum2);
        *reinterpret_cast<float*>(mxGetData(outSum2Array)) = sum2;
        *reinterpret_cast<float*>(mxGetData(outSumArray)) = sum;
        *reinterpret_cast<double*>(mxGetData(outCountArray)) = count;
    }

    plhs[0] = outSum2Array;
    if (nlhs > 1) {
        plhs[1] = outSumArray;
    }
    if (nlhs > 2) {
        plhs[2] = outCountArray;
    }
}