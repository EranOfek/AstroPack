// findLocalMax1.cpp
// [Ind, YInd] = findLocalMax1(Y, Threshold)
//
// Fast local maxima finder on a 1-D vector (row or column), single/double.
// Peak criterion at interior point i (1<i<N):
//   Y(i) >= Threshold && Y(i) > Y(i-1) && Y(i) >= Y(i+1)
// NaN handling: any NaN in Y(i-1), Y(i), or Y(i+1) -> not a peak.
//
// Outputs:
//   Ind  : double column vector of 1-based indices of local maxima
//   YInd : values at those indices, same class as Y
//
// Build:
//   mex -O -R2018a findLocalMax1.cpp

#include "mex.h"
#include <cmath>
#include <vector>
#include <type_traits>

template<typename T>
static inline bool isFiniteT(T x){ return std::isfinite(static_cast<double>(x)); }

template<typename T>
static void core_find(const T* y, mwSize n, T thr, std::vector<mwIndex>& outIdx, std::vector<T>& outVal)
{
    if (n < 3) return;

    // Reserve a safe upper bound: at most ~ (n-2)/2 peaks in strict alternation
    outIdx.reserve(n/2 + 2);
    outVal.reserve(n/2 + 2);

    // Scan interior points only
    for (mwIndex i = 1; i+1 < n; ++i){
        const T yi = y[i];
        // Fast threshold + NaN check first (skip most)
        if (!(yi >= thr) || !(yi==yi)) continue;

        const T yl = y[i-1];
        const T yr = y[i+1];

        // If either neighbor is NaN -> not a peak
        // (Comparisons with NaN will be false anyway, but this is explicit & clear.)
        if (!(yl==yl) || !(yr==yr)) continue;

        // Peak criterion: left strict, right non-strict
        if ((yi > yl) && (yi >= yr)){
            outIdx.push_back(i);   // 0-based for now
            outVal.push_back(yi);
        }
    }
}

void mexFunction(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[])
{
    if (nrhs < 2)
        mexErrMsgIdAndTxt("findLocalMax1:args", "Usage: [Ind, YInd] = findLocalMax1(Y, Threshold)");

    const mxArray* Y = prhs[0];
    const mxArray* Th = prhs[1];

    if (mxIsComplex(Y) || (!mxIsDouble(Y) && !mxIsSingle(Y)))
        mexErrMsgIdAndTxt("findLocalMax1:type", "Y must be real single or double.");
    if (mxGetNumberOfElements(Th) != 1 || mxIsComplex(Th) || (!mxIsDouble(Th) && !mxIsSingle(Th)))
        mexErrMsgIdAndTxt("findLocalMax1:thr", "Threshold must be a real scalar (single/double).");

    // Ensure Y is a vector
    const mwSize nd = mxGetNumberOfDimensions(Y);
    const mwSize* dims = mxGetDimensions(Y);
    mwSize n = mxGetNumberOfElements(Y);
    if (!( (nd==2 && (dims[0]==1 || dims[1]==1)) ))
        mexErrMsgIdAndTxt("findLocalMax1:shape", "Y must be a 1-D vector (row or column).");

    // Prepare outputs (we'll create actual mxArrays after collecting)
    std::vector<mwIndex> idx;
    if (mxIsDouble(Y)){
        const double* y = mxGetPr(Y);
        const double thr = (mxIsDouble(Th) ? mxGetPr(Th)[0]
                                           : static_cast<double>(reinterpret_cast<const float*>(mxGetData(Th))[0]));
        std::vector<double> vals;
        core_find<double>(y, n, thr, idx, vals);

        // Create outputs
        plhs[0] = mxCreateDoubleMatrix(static_cast<mwSize>(idx.size()), 1, mxREAL);
        double* oInd = mxGetPr(plhs[0]);
        for (mwSize k=0;k<idx.size();++k) oInd[k] = static_cast<double>(idx[k] + 1); // 1-based

        if (nlhs >= 2){
            plhs[1] = mxCreateNumericMatrix(static_cast<mwSize>(vals.size()), 1, mxDOUBLE_CLASS, mxREAL);
            double* oVal = reinterpret_cast<double*>(mxGetData(plhs[1]));
            for (mwSize k=0;k<vals.size();++k) oVal[k] = vals[k];
        }
    } else { // single
        const float* y = reinterpret_cast<const float*>(mxGetData(Y));
        const float thr = (mxIsSingle(Th) ? reinterpret_cast<const float*>(mxGetData(Th))[0]
                                          : static_cast<float>(mxGetPr(Th)[0]));
        std::vector<float> vals;
        core_find<float>(y, n, thr, idx, vals);

        // Indices always double (MATLAB convention)
        plhs[0] = mxCreateDoubleMatrix(static_cast<mwSize>(idx.size()), 1, mxREAL);
        double* oInd = mxGetPr(plhs[0]);
        for (mwSize k=0;k<idx.size();++k) oInd[k] = static_cast<double>(idx[k] + 1);

        if (nlhs >= 2){
            plhs[1] = mxCreateNumericMatrix(static_cast<mwSize>(vals.size()), 1, mxSINGLE_CLASS, mxREAL);
            float* oVal = reinterpret_cast<float*>(mxGetData(plhs[1]));
            for (mwSize k=0;k<vals.size();++k) oVal[k] = vals[k];
        }
    }
}
