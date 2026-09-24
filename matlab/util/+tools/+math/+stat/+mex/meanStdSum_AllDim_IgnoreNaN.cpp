#include "mex.h"
#include <cmath>       // For sqrt
#include <limits>      // For quiet_NaN()
#include <omp.h>       // For OpenMP
#include <immintrin.h> // For SIMD intrinsics (AVX, SSE)

// mex CXXFLAGS="\$CXXFLAGS -fopenmp -mavx -mavx2 -std=c++17" LDFLAGS="\$LDFLAGS -fopenmp" meanStdSum_AllDim_IgnoreNaN.cpp
// Return: Mean, Std, Sum
// About 2 times faster than matlab, ~10 times for large matrices
//
// A=rand(100,1);
// tic; for I=1:100000, [Mean, Std, Sum]=meanStdSum_AllDim_IgnoreNaN(A); end, toc
//Elapsed time is 0.366008 seconds.
// tic;for I=1:100000, Mean1=mean(A,'all','omitnan'); Std1=std(A,[],'all','omitnan'); Sum1=sum(A,'all','omitnan'); end,toc



inline bool is_nan(double x) {
    return x != x;
}

inline bool is_nan(float x) {
    return x != x;
}

template <typename T>
void calculateStatistics(const T* input, mwSize numElements, T& mean, T& stdDev, T& sum) {
    sum = 0;
    T sumOfSquares = 0;
    mwSize validCount = 0;

    // Parallel loop with OpenMP, with manual loop unrolling
    #pragma omp parallel for reduction(+:sum, sumOfSquares, validCount) schedule(static)
    for (mwSize i = 0; i < numElements; i += 4) {
        T localSum = 0;
        T localSumOfSquares = 0;
        mwSize localCount = 0;

        for (int j = 0; j < 4 && i + j < numElements; ++j) {
            T value = input[i + j];
            if (!is_nan(value)) {
                localSum += value;
                localSumOfSquares += value * value;
                ++localCount;
            }
        }

        sum += localSum;
        sumOfSquares += localSumOfSquares;
        validCount += localCount;
    }

    if (validCount > 0) {
        mean = sum / validCount;
        stdDev = std::sqrt((sumOfSquares / validCount) - (mean * mean));
    } else {
        mean = std::numeric_limits<T>::quiet_NaN();
        stdDev = std::numeric_limits<T>::quiet_NaN();
        sum = std::numeric_limits<T>::quiet_NaN();
    }
}

void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[]) {
    if (nrhs != 1) {
        mexErrMsgIdAndTxt("mexFunction:InvalidNumInputs", "One input required.");
    }
    if (nlhs != 3) {
        mexErrMsgIdAndTxt("mexFunction:InvalidNumOutputs", "Three outputs required.");
    }

    const mxArray* inputArray = prhs[0];
    mwSize numElements = mxGetNumberOfElements(inputArray);
    mxClassID classID = mxGetClassID(inputArray);

    mxArray* meanArray = mxCreateNumericMatrix(1, 1, classID, mxREAL);
    mxArray* stdArray = mxCreateNumericMatrix(1, 1, classID, mxREAL);
    mxArray* sumArray = mxCreateNumericMatrix(1, 1, classID, mxREAL);

    plhs[0] = meanArray;
    plhs[1] = stdArray;
    plhs[2] = sumArray;

    if (classID == mxDOUBLE_CLASS) {
        double* inputData = mxGetPr(inputArray);
        double* meanData = mxGetPr(meanArray);
        double* stdData = mxGetPr(stdArray);
        double* sumData = mxGetPr(sumArray);

        calculateStatistics(inputData, numElements, *meanData, *stdData, *sumData);
    }
    else if (classID == mxSINGLE_CLASS) {
        float* inputData = (float*)mxGetData(inputArray);
        float* meanData = (float*)mxGetData(meanArray);
        float* stdData = (float*)mxGetData(stdArray);
        float* sumData = (float*)mxGetData(sumArray);

        calculateStatistics(inputData, numElements, *meanData, *stdData, *sumData);
    }
    else {
        mexErrMsgIdAndTxt("mexFunction:InvalidInputType", "Input must be of type 'single' or 'double'.");
    }
}
