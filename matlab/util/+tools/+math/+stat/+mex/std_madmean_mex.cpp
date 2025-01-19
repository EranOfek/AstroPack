#include "mex.h"
#include <cmath>
#include <algorithm>
#include <omp.h>

template <typename T>
void computeScaledMADAndMean(const T* X, T* madOutput, T* meanOutput, const mwSize* dims, mwSize ndims, int dim, bool omitnan, mwSize* outDims, bool computeMean) {
    mwSize totalElements = 1;
    for (mwSize i = 0; i < ndims; ++i) totalElements *= dims[i];

    mwSize sizeAlongDim = dims[dim];
    mwSize sizeRest = totalElements / sizeAlongDim;

    #pragma omp parallel for
    for (mwSize idx = 0; idx < sizeRest; ++idx) {
        T sumMean = 0, sumMAD = 0;
        mwSize countMean = 0, countMAD = 0;

        // Calculate the mean along the dimension
        for (mwSize i = 0; i < sizeAlongDim; ++i) {
            mwSize linearIdx = idx + i * sizeRest;
            T value = X[linearIdx];
            if (omitnan && std::isnan(value)) continue;
            sumMean += value;
            countMean++;
        }
        T mean = (countMean > 0) ? sumMean / countMean : std::numeric_limits<T>::quiet_NaN();

        // Calculate the mean absolute deviation along the dimension
        for (mwSize i = 0; i < sizeAlongDim; ++i) {
            mwSize linearIdx = idx + i * sizeRest;
            T value = X[linearIdx];
            if (omitnan && std::isnan(value)) continue;
            sumMAD += std::abs(value - mean);
            countMAD++;
        }
        T mad = (countMAD > 0) ? sumMAD / countMAD : std::numeric_limits<T>::quiet_NaN();

        madOutput[idx] = static_cast<T>(1.253) * mad;
        if (computeMean) {
            meanOutput[idx] = mean;
        }
    }
}

void mexFunction(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[]) {
    // Validate input
    if (nrhs < 2 || nrhs > 3) {
        mexErrMsgTxt("Usage: [mad, mean] = mad_mex(X, Dim, NanFlag)");
    }
    if (nlhs < 1 || nlhs > 2) {
        mexErrMsgTxt("This function requires one or two output arguments: mad and optionally mean.");
    }
    if (!mxIsSingle(prhs[0]) && !mxIsDouble(prhs[0])) {
        mexErrMsgTxt("Input X must be single or double.");
    }
    if (!mxIsDouble(prhs[1]) || mxGetNumberOfElements(prhs[1]) != 1) {
        mexErrMsgTxt("Dim must be a scalar.");
    }

    const mxArray* X = prhs[0];
    int dim = static_cast<int>(mxGetScalar(prhs[1])) - 1; // MATLAB dims are 1-based
    int nanflag = 1; // Default: omit NaNs

    // Optional third argument (NanFlag)
    if (nrhs == 3) {
        if (!mxIsDouble(prhs[2]) || mxGetNumberOfElements(prhs[2]) != 1) {
            mexErrMsgTxt("NanFlag must be a scalar (0 or 1).");
        }
        nanflag = static_cast<int>(mxGetScalar(prhs[2])); // 0: include NaNs, 1: omit NaNs
        if (nanflag < 0 || nanflag > 1) {
            mexErrMsgTxt("NanFlag must be 0 (include NaNs) or 1 (omit NaNs).");
        }
    }

    if (dim < 0 || dim >= mxGetNumberOfDimensions(X)) {
        mexErrMsgTxt("Dim is out of range.");
    }

    const mwSize* dims = mxGetDimensions(X);
    mwSize ndims = mxGetNumberOfDimensions(X);

    // Determine output dimensions
    mwSize outDims[32]; // MATLAB allows up to 32 dimensions
    for (mwSize i = 0; i < ndims; ++i) {
        outDims[i] = dims[i];
    }
    outDims[dim] = 1; // Collapse the specified dimension to 1

    // Create output array for MAD
    plhs[0] = mxCreateNumericArray(ndims, outDims, mxGetClassID(X), mxREAL);

    // Create output array for mean only if requested
    mxArray* meanOutputArray = nullptr;
    if (nlhs > 1) {
        meanOutputArray = mxCreateNumericArray(ndims, outDims, mxGetClassID(X), mxREAL);
        plhs[1] = meanOutputArray;
    }

    if (mxIsDouble(X)) {
        computeScaledMADAndMean(mxGetPr(X), mxGetPr(plhs[0]),
                                (nlhs > 1) ? mxGetPr(plhs[1]) : nullptr,
                                dims, ndims, dim, nanflag == 1, outDims, nlhs > 1);
    } else if (mxIsSingle(X)) {
        computeScaledMADAndMean(reinterpret_cast<float*>(mxGetData(X)),
                                reinterpret_cast<float*>(mxGetData(plhs[0])),
                                (nlhs > 1) ? reinterpret_cast<float*>(mxGetData(plhs[1])) : nullptr,
                                dims, ndims, dim, nanflag == 1, outDims, nlhs > 1);
    }
}
