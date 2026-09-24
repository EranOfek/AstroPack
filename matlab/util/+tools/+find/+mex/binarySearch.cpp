#include "mex.h"
#include <algorithm>
#include <cmath>
#include <omp.h>
#include <cstdint>

#define DIR_IF_FOUND_DEFAULT     -1
#define DIR_IF_NOT_FOUND_DEFAULT  2
#define CHECK_IF_INPUT_SORTED_DEFAULT 0

// =====================================================
// Utility
// =====================================================

template<typename T>
bool isSorted(const T* data, mwSize N)
{
    for (mwSize i = 1; i < N; ++i)
        if (data[i] < data[i-1])
            return false;
    return true;
}

// =====================================================
// Core Search
// =====================================================

template<typename T>
double searchOne(
    const T* data,
    mwSize N,
    T item,
    double dirIfFound,
    double dirIfNotFound)
{
    if (N == 0)
        return 0.0;

    const T* begin = data;
    const T* end   = data + N;

    const T* it = std::lower_bound(begin, end, item);
    bool found = (it != end && *it == item);

    // FOUND
    if (found)
    {
        if (dirIfFound == -1)
            while (it != begin && *(it-1) == item)
                --it;
        else if (dirIfFound == 1)
        {
            const T* it2 = it;
            while ((it2+1) != end && *(it2+1) == item)
                ++it2;
            it = it2;
        }

        return static_cast<double>((it - begin) + 1);
    }

    // NOT FOUND
    mwSize idxUp   = (it != end) ? (it - begin) : N;
    mwSize idxDown = (it != begin) ? (idxUp - 1) : 0;

    if (dirIfNotFound == 0)
        return 0.0;

    if (dirIfNotFound == -1)
    {
        if (it == begin) return 1.0;
        return static_cast<double>(idxDown + 1);
    }

    if (dirIfNotFound == 1)
    {
        if (it == end) return static_cast<double>(N);
        return static_cast<double>(idxUp + 1);
    }

    if (dirIfNotFound == 2)
    {
        if (it == begin) return 1.0;
        if (it == end)   return static_cast<double>(N);

        T upVal   = data[idxUp];
        T downVal = data[idxDown];

        if (std::abs(upVal - item) < std::abs(item - downVal))
            return static_cast<double>(idxUp + 1);
        else
            return static_cast<double>(idxDown + 1);
    }

    if (dirIfNotFound == 0.5)
    {
        if (it == begin) return 1.0;
        if (it == end)   return static_cast<double>(N);

        T upVal   = data[idxUp];
        T downVal = data[idxDown];
        T denom   = upVal - downVal;

        if (denom == 0)
            return static_cast<double>(idxDown + 1);

        double frac =
            static_cast<double>(item - downVal) /
            static_cast<double>(denom);

        return static_cast<double>(idxDown + 1) + frac;
    }

    return 0.0;
}

// =====================================================
// MEX ENTRY
// =====================================================

void mexFunction(
    int nlhs,
    mxArray* plhs[],
    int nrhs,
    const mxArray* prhs[])
{
    if (nrhs < 2)
        mexErrMsgTxt("At least 2 inputs required.");
    if (nrhs > 6)
        mexErrMsgTxt("Maximum 6 inputs allowed.");

    const mxArray* dataArg  = prhs[0];
    const mxArray* itemsArg = prhs[1];

    if (!mxIsNumeric(dataArg) || mxIsComplex(dataArg) || mxIsEmpty(dataArg))
        mexErrMsgTxt("Data must be real numeric vector.");

    if (!mxIsNumeric(itemsArg) || mxIsComplex(itemsArg))
        mexErrMsgTxt("Items must be real numeric.");

    mwSize N      = mxGetNumberOfElements(dataArg);
    mwSize nItems = mxGetNumberOfElements(itemsArg);

    bool dataIsDouble  = mxIsDouble(dataArg);
    bool itemsIsDouble = mxIsDouble(itemsArg);

    double dirIfFound    = DIR_IF_FOUND_DEFAULT;
    double dirIfNotFound = DIR_IF_NOT_FOUND_DEFAULT;
    bool checkSorted     = CHECK_IF_INPUT_SORTED_DEFAULT;
    bool outputAsDouble  = true;   // NEW DEFAULT

    if (nrhs >= 3 && !mxIsEmpty(prhs[2]))
        dirIfFound = mxGetScalar(prhs[2]);

    if (nrhs >= 4 && !mxIsEmpty(prhs[3]))
        dirIfNotFound = mxGetScalar(prhs[3]);

    if (nrhs >= 5 && !mxIsEmpty(prhs[4]))
        checkSorted = true;

    if (nrhs == 6 && !mxIsEmpty(prhs[5]))
        outputAsDouble = (mxGetScalar(prhs[5]) != 0);

    // fractional mode must return double
    if (dirIfNotFound == 0.5)
        outputAsDouble = true;

    // ========================================
    // Create output
    // ========================================

    if (outputAsDouble)
        plhs[0] = mxCreateDoubleMatrix(mxGetM(itemsArg), mxGetN(itemsArg), mxREAL);
    else
        plhs[0] = mxCreateNumericMatrix(mxGetM(itemsArg), mxGetN(itemsArg),
                                        mxUINT32_CLASS, mxREAL);

    // ========================================
    // Dispatch
    // ========================================

    if (dataIsDouble)
    {
        const double* data = mxGetPr(dataArg);

        if (checkSorted && !isSorted(data, N))
            mexErrMsgTxt("Data must be sorted.");

        if (outputAsDouble)
        {
            double* out = mxGetPr(plhs[0]);

#pragma omp parallel for schedule(static)
            for (mwSize i = 0; i < nItems; ++i)
            {
                double item = itemsIsDouble ?
                              mxGetPr(itemsArg)[i] :
                              static_cast<double>(
                                static_cast<float*>(mxGetData(itemsArg))[i]);

                out[i] = searchOne(data, N, item,
                                   dirIfFound, dirIfNotFound);
            }
        }
        else
        {
            uint32_T* out =
                static_cast<uint32_T*>(mxGetData(plhs[0]));

#pragma omp parallel for schedule(static)
            for (mwSize i = 0; i < nItems; ++i)
            {
                double item = itemsIsDouble ?
                              mxGetPr(itemsArg)[i] :
                              static_cast<double>(
                                static_cast<float*>(mxGetData(itemsArg))[i]);

                double result =
                    searchOne(data, N, item,
                              dirIfFound, dirIfNotFound);

                if (result < 0) result = 0;
                if (result > 4294967295.0)
                    mexErrMsgTxt("Index exceeds uint32 range.");

                out[i] = static_cast<uint32_T>(result);
            }
        }
    }
    else
    {
        const float* data =
            static_cast<float*>(mxGetData(dataArg));

        if (checkSorted && !isSorted(data, N))
            mexErrMsgTxt("Data must be sorted.");

        if (outputAsDouble)
        {
            double* out = mxGetPr(plhs[0]);

#pragma omp parallel for schedule(static)
            for (mwSize i = 0; i < nItems; ++i)
            {
                float item = itemsIsDouble ?
                             static_cast<float>(mxGetPr(itemsArg)[i]) :
                             static_cast<float*>(
                               mxGetData(itemsArg))[i];

                out[i] = searchOne(data, N, item,
                                   dirIfFound, dirIfNotFound);
            }
        }
        else
        {
            uint32_T* out =
                static_cast<uint32_T*>(mxGetData(plhs[0]));

#pragma omp parallel for schedule(static)
            for (mwSize i = 0; i < nItems; ++i)
            {
                float item = itemsIsDouble ?
                             static_cast<float>(mxGetPr(itemsArg)[i]) :
                             static_cast<float*>(
                               mxGetData(itemsArg))[i];

                double result =
                    searchOne(data, N, item,
                              dirIfFound, dirIfNotFound);

                if (result < 0) result = 0;
                if (result > 4294967295.0)
                    mexErrMsgTxt("Index exceeds uint32 range.");

                out[i] = static_cast<uint32_T>(result);
            }
        }
    }
}
