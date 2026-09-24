// rstd_mex.cpp
// Result = rstd_mex(Array, Type, Dim)
// - Array: real single|double matrix
// - Type : 0 (default; sample, multiply by sqrt(n/(n-1))), or 1 (population).
//          If empty, treated as 0.
// - Dim  : 1 (down columns; default) or 2 (across rows).
//
// Robust std definition:
//   sigma_robust = IQR / 1.3489795003921634, where IQR = Q75 - Q25
//   Qp computed with Hyndman–Fan #7 ("linear") interpolation:
//       r = 1 + (n-1)p,  iL=floor(r), iU=ceil(r), Q = (1-g)*x_(iL) + g*x_(iU)
//   Then, if Type==0 (sample) and n>1, multiply by sqrt(n/(n-1)).
//
// Implementation notes:
//   * Omits NaNs/Inf automatically (only finite values kept).
//   * Quantiles obtained by up to four independent std::nth_element calls on the same buffer.
//   * OpenMP parallel across slices; Dim=2 copies row into contiguous scratch for speed.

#include "mex.h"
#include <algorithm>
#include <cmath>
#include <limits>
#include <vector>

#if defined(_OPENMP)
  #include <omp.h>
#endif

template<typename T> inline bool finiteT(T x){ return std::isfinite(static_cast<double>(x)); }
template<typename T> inline T   NaN()        { return std::numeric_limits<T>::quiet_NaN(); }

// --- Hyndman–Fan #7 quantile positions (1-based indices) ---
struct HF7Pos {
    size_t il; // floor index in [1..n]
    size_t iu; // ceil  index in [1..n]
    double g;  // fractional part (r - floor(r))
};
static inline HF7Pos hf7_pos(double p, size_t n){
    const double r  = 1.0 + (n - 1) * p;
    const double fl = std::floor(r);
    HF7Pos s;
    s.il = (size_t)fl; if (s.il < 1) s.il = 1; if (s.il > n) s.il = n;
    s.iu = (size_t)std::ceil(r);     if (s.iu < 1) s.iu = 1; if (s.iu > n) s.iu = n;
    s.g  = r - fl;
    return s;
}

// --- Independent selections for Q25 & Q75 (correct & O(n) expected) ---
template<typename T>
static inline void quantile25_75(std::vector<T>& buf, T& q25, T& q75)
{
    const size_t n = buf.size();
    if (n == 0){ q25 = NaN<T>(); q75 = NaN<T>(); return; }
    if (n == 1){ q25 = buf[0];   q75 = buf[0];   return; }

    const HF7Pos p25 = hf7_pos(0.25, n);
    const HF7Pos p75 = hf7_pos(0.75, n);

    const size_t aL = p25.il - 1, aU = p25.iu - 1;
    const size_t bL = p75.il - 1, bU = p75.iu - 1;

    // Q25 lower & upper order stats
    std::nth_element(buf.begin(), buf.begin()+aL, buf.end());
    const T v_aL = buf[aL];
    T v_aU = v_aL;
    if (aU != aL){
        std::nth_element(buf.begin(), buf.begin()+aU, buf.end());
        v_aU = buf[aU];
    }

    // Q75 lower & upper order stats
    std::nth_element(buf.begin(), buf.begin()+bL, buf.end());
    const T v_bL = buf[bL];
    T v_bU = v_bL;
    if (bU != bL){
        std::nth_element(buf.begin(), buf.begin()+bU, buf.end());
        v_bU = buf[bU];
    }

    // HF#7 linear interpolation
    q25 = (p25.g == 0.0) ? v_aL : (T)((1.0 - p25.g) * (double)v_aL + p25.g * (double)v_aU);
    q75 = (p75.g == 0.0) ? v_bL : (T)((1.0 - p75.g) * (double)v_bL + p75.g * (double)v_bU);
}

// --- Per-slice robust std ---
template<typename T>
static inline T rstd_slice(const T* x, mwSize n, int Type)
{
    // Gather finite values
    std::vector<T> buf; buf.reserve(n);
    for (mwSize i=0;i<n;++i){
        const T xi = x[i];
        if (finiteT(xi)) buf.push_back(xi);
    }

    const size_t m = buf.size();
    if (m == 0) return NaN<T>();
    if (m == 1){
        // Mirror std semantics: population->0, sample->NaN
        return (Type==1) ? (T)0 : NaN<T>();
    }

    T q25, q75;
    quantile25_75(buf, q25, q75);
    const double IQR = (double)q75 - (double)q25;

    // Robust sigma from IQR (normal-consistent)
    double s = IQR / 1.3489795003921634;  // = 2*norminv(0.75)

    // std-style normalization
    if (Type == 0){
        const double mm = (double)m;
        s = (mm > 1.0) ? s * std::sqrt(mm/(mm-1.0)) : std::numeric_limits<double>::quiet_NaN();
    }
    return (T)s;
}

// --- Drivers along Dim ---
template<typename T>
static void run_dim1(const mxArray* A, mxArray* out, int Type)
{
    const mwSize M = mxGetM(A), N = mxGetN(A);
    const T* Ax = (const T*)mxGetData(A);
    T* Or = (T*)mxGetData(out);

    #pragma omp parallel for if (N>8) schedule(static)
    for (mwIndex j=0;j<N;++j){
        Or[j] = rstd_slice(Ax + j*M, M, Type);
    }
}

template<typename T>
static void run_dim2(const mxArray* A, mxArray* out, int Type)
{
    const mwSize M = mxGetM(A), N = mxGetN(A);
    const T* Ax = (const T*)mxGetData(A);
    T* Or = (T*)mxGetData(out);

    #pragma omp parallel
    {
        std::vector<T> row; row.resize(N);

        #pragma omp for schedule(static)
        for (mwIndex i=0;i<M;++i){
            // Gather row into contiguous memory
            for (mwIndex j=0;j<N;++j){
                row[j] = Ax[i + j*M];
            }
            Or[i] = rstd_slice(row.data(), N, Type);
        }
    }
}

// --- MEX entry ---
void mexFunction(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[])
{
    if (nrhs < 1)
        mexErrMsgIdAndTxt("rstd_mex:args", "Usage: Result = rstd_mex(Array, Type, Dim)");

    const mxArray* A = prhs[0];
    if ((!mxIsDouble(A) && !mxIsSingle(A)) || mxIsComplex(A))
        mexErrMsgIdAndTxt("rstd_mex:type", "Array must be real single or double.");

    // Type (default 0; empty -> 0)
    int Type = 0;
    if (nrhs >= 2 && !mxIsEmpty(prhs[1])){
        if (!mxIsDouble(prhs[1]) || mxIsComplex(prhs[1]) || mxGetNumberOfElements(prhs[1])!=1)
            mexErrMsgIdAndTxt("rstd_mex:typeArg", "Type must be scalar 0 or 1 (or empty).");
        Type = (int)mxGetScalar(prhs[1]);
        if (Type!=0 && Type!=1)
            mexErrMsgIdAndTxt("rstd_mex:typeArg", "Type must be 0 or 1.");
    }

    // Dim (default 1)
    int Dim = 1;
    if (nrhs >= 3 && !mxIsEmpty(prhs[2])){
        if (!mxIsDouble(prhs[2]) || mxIsComplex(prhs[2]) || mxGetNumberOfElements(prhs[2])!=1)
            mexErrMsgIdAndTxt("rstd_mex:dim", "Dim must be scalar 1 or 2.");
        Dim = (int)mxGetScalar(prhs[2]);
        if (Dim!=1 && Dim!=2)
            mexErrMsgIdAndTxt("rstd_mex:dim", "Dim must be 1 or 2.");
    }

    const mwSize M   = mxGetM(A);
    const mwSize N   = mxGetN(A);
    const mxClassID cls = mxGetClassID(A);

    // Output allocation
    if (Dim==1)
        plhs[0] = mxCreateNumericMatrix(1, N, cls, mxREAL);
    else
        plhs[0] = mxCreateNumericMatrix(M, 1, cls, mxREAL);

    if (cls == mxDOUBLE_CLASS){
        if (Dim==1) run_dim1<double>(A, plhs[0], Type);
        else        run_dim2<double>(A, plhs[0], Type);
    } else {
        if (Dim==1) run_dim1<float >(A, plhs[0], Type);
        else        run_dim2<float >(A, plhs[0], Type);
    }
}
