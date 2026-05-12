/*
 * array_stats.cpp
 * MEX function: [mean, std_dev, min_val, max_val, element_count] = array_stats(A)
 *
 * Input:  A — 1D or 2D double array (real, non-sparse, non-empty)
 * Output: five scalars (mean, standard deviation, min, max, element count)
 *
 * Build:  mex array_stats.cpp  (MSVC 2022 on Windows / MATLAB R2023a)
 *
 * MEX API notes:
 *   - #include "mex.h" pulls in both matrix.h and the mexFunction prototype.
 *   - We use the R2018a typed API (mxGetDoubles) which returns double* directly
 *     and is preferred over the legacy mxGetPr (returns double* too but is
 *     officially deprecated as of R2018a).
 *   - plhs = pointer-to-left-hand-side  (outputs MATLAB receives)
 *   - prhs = pointer-to-right-hand-side (inputs  MATLAB passes in)
 *   - nlhs / nrhs = number of those arrays actually requested / provided.
 */

#include "mex.h"
#include <cmath>    // std::sqrt, std::isnan
#include <limits>   // numeric_limits

/* -----------------------------------------------------------------------
 * computeStats — pure C++ function; completely independent of the MEX API.
 * Keeping business logic out of mexFunction makes it unit-testable and
 * clearly separates "MEX glue" from "algorithm".
 * ----------------------------------------------------------------------- */
struct Stats {
    double mean;
    double std_dev;
    double min_val;
    double max_val;
    mwSize count;       // mwSize is MATLAB's size_t (64-bit on 64-bit MATLAB)
};

Stats computeStats(const double* data, mwSize n)
{
    Stats s;
    s.count = n;
    s.min_val =  std::numeric_limits<double>::infinity();
    s.max_val = -std::numeric_limits<double>::infinity();

    double sum = 0.0;
    for (mwSize i = 0; i < n; ++i) {
        double v = data[i];
        if (std::isnan(v)) continue;   // NaN: skip for min/max/mean
        sum += v;
        if (v < s.min_val) s.min_val = v;
        if (v > s.max_val) s.max_val = v;
    }

    // Count non-NaN elements for mean / std
    mwSize valid = 0;
    for (mwSize i = 0; i < n; ++i)
        if (!std::isnan(data[i])) ++valid;

    if (valid == 0) {
        // All NaN: return NaN for everything numeric
        s.mean    = std::numeric_limits<double>::quiet_NaN();
        s.std_dev = std::numeric_limits<double>::quiet_NaN();
        s.min_val = std::numeric_limits<double>::quiet_NaN();
        s.max_val = std::numeric_limits<double>::quiet_NaN();
        return s;
    }

    s.mean = sum / static_cast<double>(valid);

    // Two-pass variance (numerically stable for this POC)
    double sq_sum = 0.0;
    for (mwSize i = 0; i < n; ++i) {
        if (std::isnan(data[i])) continue;
        double d = data[i] - s.mean;
        sq_sum += d * d;
    }
    // Population std dev (matches MATLAB std(x,1) — i.e. divide by N, not N-1)
    // To match MATLAB's default std (N-1), change valid to (valid-1) below.
    s.std_dev = (valid > 1) ? std::sqrt(sq_sum / static_cast<double>(valid - 1))
                             : 0.0;   // single valid element → std = 0

    return s;
}

/* -----------------------------------------------------------------------
 * mexFunction — the entry point MATLAB calls.
 *   nlhs  : number of output arguments requested by caller
 *   plhs  : array of mxArray* where we place our outputs
 *   nrhs  : number of input arguments the caller provided
 *   prhs  : array of const mxArray* holding the inputs
 * ----------------------------------------------------------------------- */
void mexFunction(int nlhs, mxArray* plhs[],
                 int nrhs, const mxArray* prhs[])
{
    /* --- Input count check -------------------------------------------- */
    if (nrhs != 1)
        mexErrMsgIdAndTxt("array_stats:badNrhs",
            "array_stats requires exactly 1 input argument, got %d.", nrhs);
    // mexErrMsgIdAndTxt: preferred over legacy mexErrMsgTxt because the
    // identifier (first arg) allows callers to catch this specific error
    // with try/catch and strcmp(ME.identifier, 'array_stats:badNrhs').

    /* --- Type checks --------------------------------------------------- */
    const mxArray* A = prhs[0];   // convenience alias

    if (!mxIsDouble(A))
        mexErrMsgIdAndTxt("array_stats:notDouble",
            "Input must be a double array.");

    // mxIsComplex: R2018a+ returns true if the array has an imaginary part.
    if (mxIsComplex(A))
        mexErrMsgIdAndTxt("array_stats:isComplex",
            "Input must be real (non-complex).");

    // Sparse matrices store only non-zero elements; our pointer loop would
    // miss the implicit zeros, so reject them explicitly.
    if (mxIsSparse(A))
        mexErrMsgIdAndTxt("array_stats:isSparse",
            "Input must be a full (non-sparse) array.");

    /* --- Size / empty check ------------------------------------------- */
    mwSize n = mxGetNumberOfElements(A);   // total elements (rows * cols * ...)
    if (n == 0)
        mexErrMsgIdAndTxt("array_stats:emptyArray",
            "Input array must not be empty.");

    /* --- Get data pointer ---------------------------------------------- */
    // mxGetDoubles (R2018a+): returns double* directly for real arrays.
    // Prefer over mxGetPr (legacy, still works but deprecated in R2018a docs).
    const double* data = mxGetDoubles(A);

    /* --- Compute ------------------------------------------------------- */
    Stats s = computeStats(data, n);

    /* --- Build outputs ------------------------------------------------- */
    // mxCreateDoubleScalar allocates a 1x1 real double mxArray.
    // MATLAB owns the memory of plhs[i] after we assign it — do NOT free.
    plhs[0] = mxCreateDoubleScalar(s.mean);
    if (nlhs > 1) plhs[1] = mxCreateDoubleScalar(s.std_dev);
    if (nlhs > 2) plhs[2] = mxCreateDoubleScalar(s.min_val);
    if (nlhs > 3) plhs[3] = mxCreateDoubleScalar(s.max_val);
    if (nlhs > 4) plhs[4] = mxCreateDoubleScalar(static_cast<double>(s.count));
}
