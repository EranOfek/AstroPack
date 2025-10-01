// quantile_mex.cpp  (fast native-type selection; logical fixed w/ alias)
// y = quantile_mex(Array, q)

#include "mex.h"
#include <algorithm>
#include <vector>
#include <type_traits>
#include <cmath>
#include <cstdint>

#if defined(_OPENMP)
  #include <omp.h>
#endif

// ---------- type helpers ----------
template <typename T> struct IsFloat : std::false_type {};
template <> struct IsFloat<float>  : std::true_type {};
template <> struct IsFloat<double> : std::true_type {};

template <typename T>
inline bool is_nan(T) { return false; }
template <>
inline bool is_nan<float>(float x)  { return std::isnan(x); }
template <>
inline bool is_nan<double>(double x){ return std::isnan(x); }

// Map mxLogical/bool → uint8_T to avoid std::vector<bool> specialization
template <typename T>
using BufT = typename std::conditional<
    std::is_same<T, mxLogical>::value || std::is_same<T, bool>::value,
    uint8_T, T
>::type;

// Count kept elements (skip NaN for floats; else keep all)
template <typename T>
static size_t count_kept(const T* p, size_t n) {
    if constexpr (IsFloat<T>::value) {
        size_t cnt = 0;
        for (size_t i=0;i<n;++i) if (!is_nan<T>(p[i])) ++cnt;
        return cnt;
    } else {
        return n;
    }
}

// Copy into preallocated dst, skipping NaNs for float/double; casting for others.
// dst is BufT<T>*
template <typename T>
static void copy_keep(const T* src, size_t n, BufT<T>* dst) {
    if constexpr (IsFloat<T>::value) {
        size_t w = 0;
        for (size_t i=0;i<n;++i) {
            T v = src[i];
            if (!is_nan<T>(v)) dst[w++] = static_cast<BufT<T>>(v);
        }
    } else {
        for (size_t i=0;i<n;++i) dst[i] = static_cast<BufT<T>>(src[i]);
    }
}

// Gather into native-type buffer (OpenMP acceleration for very large arrays)
template <typename T>
static std::vector<BufT<T>> gather_native(const mxArray* A) {
    const T* p = static_cast<const T*>(mxGetData(A));
    const size_t n = static_cast<size_t>(mxGetNumberOfElements(A));

#if defined(_OPENMP)
    if (n > (1u<<22)) { // ~4M elements threshold
        int P = omp_get_max_threads();
        std::vector<size_t> counts(P,0);
        #pragma omp parallel
        {
            int tid = omp_get_thread_num();
            size_t n0 = (n*tid)/P, n1 = (n*(tid+1))/P;
            counts[tid] = count_kept<T>(p+n0, n1-n0);
        }
        std::vector<size_t> off(P+1,0);
        for (int i=0;i<P;i++) off[i+1] = off[i] + counts[i];
        std::vector<BufT<T>> out(off[P]);
        #pragma omp parallel
        {
            int tid = omp_get_thread_num();
            size_t n0 = (n*tid)/P, n1 = (n*(tid+1))/P;
            size_t w0 = off[tid];
            if constexpr (IsFloat<T>::value) {
                size_t w = w0;
                for (size_t i=n0;i<n1;++i) {
                    T v = p[i];
                    if (!is_nan<T>(v)) out[w++] = static_cast<BufT<T>>(v);
                }
            } else {
                for (size_t i=0;i<n1-n0;++i)
                    out[w0 + i] = static_cast<BufT<T>>(p[n0 + i]);
            }
        }
        return out;
    }
#endif
    size_t kept = count_kept<T>(p, n);
    std::vector<BufT<T>> out(kept);
    copy_keep<T>(p, n, out.data());
    return out;
}

// Quantile on vector<T> (T = native or uint8 for logical)
// Type-7 (MATLAB/R): h=(n-1)q+1; interpolate between floor/ceil
template <typename T>
static double compute_quantile(std::vector<T>& x, double q) {
    const size_t n = x.size();
    if (n == 0) return mxGetNaN();
    if (q <= 0.0) return static_cast<double>(*std::min_element(x.begin(), x.end()));
    if (q >= 1.0) return static_cast<double>(*std::max_element(x.begin(), x.end()));

    const double h  = (static_cast<double>(n) - 1.0) * q + 1.0;
    const double hf = h - 1.0;
    size_t lo = static_cast<size_t>(std::floor(hf));
    size_t hi = static_cast<size_t>(std::ceil(hf));
    if (hi >= n) hi = n-1;
    const double frac = hf - static_cast<double>(lo);

    std::nth_element(x.begin(), x.begin()+lo, x.end());
    const double xL = static_cast<double>(x[lo]);
    if (hi == lo) return xL;

    std::nth_element(x.begin()+lo+1, x.begin()+hi, x.end());
    const double xU = static_cast<double>(x[hi]);
    return xL + frac*(xU - xL);
}

static void mexErr(const char* id, const char* msg) {
    mexErrMsgIdAndTxt(id, "%s", msg);
}

void mexFunction(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[]) {
    if (nrhs != 2) mexErr("quantile_mex:Args", "Usage: y = quantile_mex(Array, q).");
    const mxArray* A = prhs[0];
    const mxArray* Q = prhs[1];

    if (!mxIsNumeric(A) && !mxIsLogical(A))
        mexErr("quantile_mex:Type", "Array must be numeric or logical.");
    if (mxIsComplex(A))
        mexErr("quantile_mex:Complex", "Complex arrays are not supported.");
    if (!mxIsDouble(Q) || mxGetNumberOfElements(Q)!=1)
        mexErr("quantile_mex:qType", "q must be a scalar double in [0,1].");

    const double q = *mxGetPr(Q);
    if (!(q>=0.0 && q<=1.0)) mexErr("quantile_mex:qRange", "q must be in [0,1].");

    double y = mxGetNaN();
    switch (mxGetClassID(A)) {
        case mxDOUBLE_CLASS: { auto buf = gather_native<double>(A);   y = compute_quantile(buf, q); break; }
        case mxSINGLE_CLASS: { auto buf = gather_native<float>(A);    y = compute_quantile(buf, q); break; }
        case mxINT8_CLASS:   { auto buf = gather_native<int8_T>(A);   y = compute_quantile(buf, q); break; }
        case mxUINT8_CLASS:  { auto buf = gather_native<uint8_T>(A);  y = compute_quantile(buf, q); break; }
        case mxINT16_CLASS:  { auto buf = gather_native<int16_T>(A);  y = compute_quantile(buf, q); break; }
        case mxUINT16_CLASS: { auto buf = gather_native<uint16_T>(A); y = compute_quantile(buf, q); break; }
        case mxINT32_CLASS:  { auto buf = gather_native<int32_T>(A);  y = compute_quantile(buf, q); break; }
        case mxUINT32_CLASS: { auto buf = gather_native<uint32_T>(A); y = compute_quantile(buf, q); break; }
#if defined(mxINT64_CLASS)
        case mxINT64_CLASS:  { auto buf = gather_native<int64_T>(A);  y = compute_quantile(buf, q); break; }
        case mxUINT64_CLASS: { auto buf = gather_native<uint64_T>(A); y = compute_quantile(buf, q); break; }
#endif
        case mxLOGICAL_CLASS:{ auto buf = gather_native<mxLogical>(A); y = compute_quantile(buf, q); break; }
        default:
            mexErr("quantile_mex:Class", "Unsupported input class.");
    }

    plhs[0] = mxCreateDoubleScalar(y);
}
