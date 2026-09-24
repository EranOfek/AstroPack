#include "mex.h"
#include <cmath>       // For sqrt
#include <limits>      // For quiet_NaN()
#include <omp.h>       // For OpenMP
#include <immintrin.h> // For SIMD intrinsics (AVX, SSE)

// mex CXXFLAGS="\$CXXFLAGS -fopenmp -mavx -mavx2 -std=c++17" LDFLAGS="\$LDFLAGS -fopenmp" meanStdSum_2Dcols_IgnoreNaN.cpp
// Return: Vectors of Mean, Std, Sum for each column

// tic;for I=1:1000, Mean1=mean(A,1,'omitnan'); Std1=std(A,[],1,'omitnan'); Sum1=sum(A,1,'omitnan'); end,toc
//Elapsed time is 3.112748 seconds.
// tic; for I=1:1000, [Mean, Std, Sum]=meanStdSum_2Dcols(A); end, toc                                       
//Elapsed time is 0.364047 seconds.
// [max(abs(Mean-Mean1)), max(abs(Std-Std1)), max(abs(Sum-Sum1))]




inline bool is_nan(double x) {
    return x != x;
}

inline bool is_nan(float x) {
    return x != x;
}

template <typename T>
void calculateColumnStatistics(const T* input, mwSize numRows, mwSize numCols, T* means, T* stdDevs, T* sums) {
    #pragma omp parallel for schedule(static)
    for (mwSize col = 0; col < numCols; ++col) {
        T sum = 0;
        T sumOfSquares = 0;
        mwSize validCount = 0;

        // Iterate over each element in the column
        for (mwSize row = 0; row < numRows; ++row) {
            T value = input[row + col * numRows];
            if (!is_nan(value)) {
                sum += value;
                sumOfSquares += value * value;
                ++validCount;
            }
        }

        if (validCount > 0) {
            T mean = sum / validCount;
            means[col] = mean;
            stdDevs[col] = std::sqrt((sumOfSquares / validCount) - (mean * mean));
            sums[col] = sum;
        } else {
            means[col] = std::numeric_limits<T>::quiet_NaN();
            stdDevs[col] = std::numeric_limits<T>::quiet_NaN();
            sums[col] = std::numeric_limits<T>::quiet_NaN();
        }
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
    mwSize numRows = mxGetM(inputArray);
    mwSize numCols = mxGetN(inputArray);
    mxClassID classID = mxGetClassID(inputArray);

    // Create output arrays
    mxArray* meanArray = mxCreateNumericMatrix(1, numCols, classID, mxREAL);
    mxArray* stdArray = mxCreateNumericMatrix(1, numCols, classID, mxREAL);
    mxArray* sumArray = mxCreateNumericMatrix(1, numCols, classID, mxREAL);

    plhs[0] = meanArray;
    plhs[1] = stdArray;
    plhs[2] = sumArray;

    if (classID == mxDOUBLE_CLASS) {
        double* inputData = mxGetPr(inputArray);
        double* meanData = mxGetPr(meanArray);
        double* stdData = mxGetPr(stdArray);
        double* sumData = mxGetPr(sumArray);

        calculateColumnStatistics(inputData, numRows, numCols, meanData, stdData, sumData);
    }
    else if (classID == mxSINGLE_CLASS) {
        float* inputData = (float*)mxGetData(inputArray);
        float* meanData = (float*)mxGetData(meanArray);
        float* stdData = (float*)mxGetData(stdArray);
        float* sumData = (float*)mxGetData(sumArray);

        calculateColumnStatistics(inputData, numRows, numCols, meanData, stdData, sumData);
    }
    else {
        mexErrMsgIdAndTxt("mexFunction:InvalidInputType", "Input must be of type 'single' or 'double'.");
    }
}
