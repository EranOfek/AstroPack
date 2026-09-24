// wmedianStd_mex.cpp
//
// [Med, WStd] = wmedianStd_mex(Array, Weights, Dim, Method)
//
// Inputs:
//   Array, Weights : real single or double, same size, 2-D only
//   Dim            : 1 (default, operate down columns) or 2 (across rows)
//   Method         : 'median' (default) or 'quantile'
//
// Behavior:
//   * Ignore elements where Array or Weights is NaN/Inf
//   * Ignore weights <= 0
//   * If any weight == +Inf with finite Array value:
//       operate only on those finite-Array, +Inf-weight values
//       - 'median'   : lower median for even count
//       - 'quantile' : midpoint of the middle two for even count
//       - WStd       : unweighted population std over those values
//   * WStd is weighted population std:
//       sqrt( sum(w*x^2)/sum(w) - (sum(w*x)/sum(w))^2 )
//
// Notes:
//   * Portable: standard C++ + MEX only
//   * OpenMP is optional; compile without it for maximum portability
//

#include "mex.h"

#include <algorithm>
#include <cmath>
#include <cctype>
#include <limits>
#include <string>
#include <utility>
#include <vector>

#if defined(_OPENMP)
  #include <omp.h>
#endif

template<typename T> inline bool isFiniteT(T x) {
    return std::isfinite(static_cast<double>(x));
}

template<typename T> inline bool isInfT(T x) {
    return std::isinf(static_cast<double>(x));
}

template<typename T> inline T nanT() {
    return std::numeric_limits<T>::quiet_NaN();
}

template<typename T> inline T avg2_safe(T a, T b) {
    return a + (b - a) / static_cast<T>(2);
}

enum MethodKind {
    METHOD_MEDIAN   = 0,
    METHOD_QUANTILE = 1
};

// ========================= Unweighted helpers =========================

template<typename T>
static T unweighted_median_lower(std::vector<T>& x) {
    const size_t n = x.size();
    if (n == 0) return nanT<T>();
    const size_t k = (n - 1) >> 1;
    std::nth_element(x.begin(), x.begin() + k, x.end());
    return x[k];
}

template<typename T>
static T unweighted_quantile_half(std::vector<T>& x) {
    const size_t n = x.size();
    if (n == 0) return nanT<T>();

    const size_t k1 = (n - 1) >> 1;
    std::nth_element(x.begin(), x.begin() + k1, x.end());
    T a = x[k1];

    if (n & 1) {
        return a;
    } else {
        T b = *std::min_element(x.begin() + k1 + 1, x.end());
        return avg2_safe(a, b);
    }
}

template<typename T>
static T unweighted_std_pop(const std::vector<T>& x) {
    const size_t n = x.size();
    if (n == 0) return nanT<T>();

    long double s = 0.0L;
    for (size_t i = 0; i < n; ++i) {
        s += static_cast<long double>(x[i]);
    }
    const long double mu = s / static_cast<long double>(n);

    long double acc = 0.0L;
    for (size_t i = 0; i < n; ++i) {
        const long double d = static_cast<long double>(x[i]) - mu;
        acc += d * d;
    }

    return static_cast<T>(std::sqrt(acc / static_cast<long double>(n)));
}

// ========================= Pivot / partition =========================

template<typename T>
inline T median3(T a, T b, T c) {
    if (a > b) std::swap(a, b);
    if (b > c) std::swap(b, c);
    if (a > b) std::swap(a, b);
    return b;
}

template<typename T>
static T choose_pivot_ninther(const std::vector<T>& x, size_t L, size_t R) {
    const size_t n = R - L;
    if (n < 9) {
        return x[L + (n >> 1)];
    }

    const size_t a1 = L + n / 16;
    const size_t a2 = L + n / 8;
    const size_t a3 = L + 3 * n / 16;

    const size_t b1 = L + 7 * n / 16;
    const size_t b2 = L + n / 2;
    const size_t b3 = L + 9 * n / 16;

    const size_t c1 = L + 13 * n / 16;
    const size_t c2 = L + 7 * n / 8;
    const size_t c3 = L + 15 * n / 16;

    return median3(
        median3(x[a1], x[a2], x[a3]),
        median3(x[b1], x[b2], x[b3]),
        median3(x[c1], x[c2], x[c3])
    );
}

// Partition [L,R) into < pivot | == pivot | > pivot
template<typename T>
static void partition3_inplace(
    std::vector<T>& x,
    std::vector<T>& w,
    size_t L,
    size_t R,
    T pivot,
    size_t& ltEnd,
    size_t& eqEnd,
    long double& wLess,
    long double& wEqual)
{
    size_t i = L;
    size_t lt = L;
    size_t gt = R;

    wLess  = 0.0L;
    wEqual = 0.0L;

    while (i < gt) {
        if (x[i] < pivot) {
            wLess += static_cast<long double>(w[i]);
            std::swap(x[i], x[lt]);
            std::swap(w[i], w[lt]);
            ++i;
            ++lt;
        } else if (x[i] > pivot) {
            --gt;
            std::swap(x[i], x[gt]);
            std::swap(w[i], w[gt]);
        } else {
            wEqual += static_cast<long double>(w[i]);
            ++i;
        }
    }

    ltEnd = lt;
    eqEnd = gt;
}

// ========================= Gather / slice compute =========================

template<typename T>
struct SliceBuffers {
    std::vector<T> xValid;
    std::vector<T> wValid;
    std::vector<T> xInf;
    std::vector<size_t> idx;

    void clear() {
        xValid.clear();
        wValid.clear();
        xInf.clear();
        idx.clear();
    }

    void reserve(size_t n) {
        xValid.reserve(n);
        wValid.reserve(n);
        xInf.reserve((n < 8) ? n : 8);
        idx.reserve(n);
    }
};

template<typename T>
static T weighted_std_from_sums(long double S1, long double Sx, long double Sxx) {
    if (!(S1 > 0.0L)) return nanT<T>();

    const long double mu  = Sx / S1;
    long double var = (Sxx / S1) - mu * mu;
    if (var < 0.0L) var = 0.0L;
    return static_cast<T>(std::sqrt(var));
}

template<typename T>
static T weighted_median_select_inplace(std::vector<T>& x, std::vector<T>& w, long double half) {
    size_t L = 0;
    size_t R = x.size();

    while (true) {
        const size_t len = R - L;

        if (len == 1) {
            return x[L];
        }

        // Tiny slices: sort indices, then scan
        if (len <= 32) {
            std::vector<size_t> idx(len);
            for (size_t i = 0; i < len; ++i) idx[i] = L + i;

            std::sort(idx.begin(), idx.end(),
                [&](size_t a, size_t b) { return x[a] < x[b]; });

            long double c = 0.0L;
            for (size_t k = 0; k < len; ++k) {
                c += static_cast<long double>(w[idx[k]]);
                if (c >= half) {
                    return x[idx[k]];
                }
            }
            return x[idx.back()];
        }

        const T pivot = (len >= 64)
            ? choose_pivot_ninther(x, L, R)
            : x[L + (len >> 1)];

        size_t ltEnd, eqEnd;
        long double wLess, wEqual;
        partition3_inplace(x, w, L, R, pivot, ltEnd, eqEnd, wLess, wEqual);

        if (half <= wLess) {
            R = ltEnd;
        } else if (half <= wLess + wEqual) {
            return pivot;
        } else {
            half -= (wLess + wEqual);
            L = eqEnd;
        }
    }
}

template<typename T>
static T weighted_quantile_half_sorted(
    const std::vector<T>& x,
    const std::vector<T>& w,
    std::vector<size_t>& idx,
    long double half)
{
    const size_t n = x.size();
    if (n == 0) return nanT<T>();

    idx.resize(n);
    for (size_t i = 0; i < n; ++i) idx[i] = i;

    std::sort(idx.begin(), idx.end(),
        [&](size_t a, size_t b) { return x[a] < x[b]; });

    long double cum = 0.0L;
    for (size_t j = 0; j < n; ++j) {
        const long double prev = cum;
        cum += static_cast<long double>(w[idx[j]]);

        if (cum > half) {
            return x[idx[j]];
        }
        if (cum == half) {
            if (j + 1 < n) {
                return avg2_safe(x[idx[j]], x[idx[j + 1]]);
            } else {
                return x[idx[j]];
            }
        }
    }

    return x[idx[n - 1]];
}

template<typename T>
static void compute_slice(
    const T* x,
    const T* w,
    mwSize n,
    MethodKind method,
    SliceBuffers<T>& buf,
    T& outMed,
    T& outStd)
{
    buf.clear();
    buf.reserve(static_cast<size_t>(n));

    long double S1  = 0.0L;
    long double Sx  = 0.0L;
    long double Sxx = 0.0L;

    // Gather valid finite positive-weight values.
    // If any finite x has +Inf weight, store those in xInf and ignore all finite-weight values.
    for (mwSize i = 0; i < n; ++i) {
        const T xi = x[i];
        const T wi = w[i];

        if (!isFiniteT(xi)) continue;

        if (isInfT(wi)) {
            if (wi > static_cast<T>(0)) {
                buf.xInf.push_back(xi);
            }
            continue;
        }

        if (!isFiniteT(wi)) continue;
        if (!(wi > static_cast<T>(0))) continue;

        buf.xValid.push_back(xi);
        buf.wValid.push_back(wi);

        const long double wl = static_cast<long double>(wi);
        const long double xl = static_cast<long double>(xi);

        S1  += wl;
        Sx  += wl * xl;
        Sxx += wl * xl * xl;
    }

    // +Inf-weight collapse
    if (!buf.xInf.empty()) {
        if (method == METHOD_MEDIAN) {
            outMed = unweighted_median_lower(buf.xInf);
        } else {
            outMed = unweighted_quantile_half(buf.xInf);
        }
        outStd = unweighted_std_pop(buf.xInf);
        return;
    }

    if (buf.xValid.empty() || !(S1 > 0.0L)) {
        outMed = nanT<T>();
        outStd = nanT<T>();
        return;
    }

    outStd = weighted_std_from_sums<T>(S1, Sx, Sxx);

    const long double half = 0.5L * S1;

    if (method == METHOD_MEDIAN) {
        outMed = weighted_median_select_inplace(buf.xValid, buf.wValid, half);
    } else {
        outMed = weighted_quantile_half_sorted(buf.xValid, buf.wValid, buf.idx, half);
    }
}

// ========================= Drivers =========================

template<typename T>
static void run_dim1(
    const mxArray* A,
    const mxArray* W,
    mxArray* outMedArr,
    mxArray* outStdArr,
    MethodKind method)
{
    const mwSize M = mxGetM(A);
    const mwSize N = mxGetN(A);

    const T* Ax = static_cast<const T*>(mxGetData(A));
    const T* Aw = static_cast<const T*>(mxGetData(W));

    T* outMed = static_cast<T*>(mxGetData(outMedArr));
    T* outStd = outStdArr ? static_cast<T*>(mxGetData(outStdArr)) : nullptr;

    #if defined(_OPENMP)
    #pragma omp parallel
    #endif
    {
        SliceBuffers<T> buf;
        buf.reserve(static_cast<size_t>(M));

        #if defined(_OPENMP)
        #pragma omp for schedule(static)
        #endif
        for (mwIndex j = 0; j < N; ++j) {
            T med, sd;
            compute_slice(Ax + j * M, Aw + j * M, M, method, buf, med, sd);
            outMed[j] = med;
            if (outStd) outStd[j] = sd;
        }
    }
}

template<typename T>
static void run_dim2(
    const mxArray* A,
    const mxArray* W,
    mxArray* outMedArr,
    mxArray* outStdArr,
    MethodKind method)
{
    const mwSize M = mxGetM(A);
    const mwSize N = mxGetN(A);

    const T* Ax = static_cast<const T*>(mxGetData(A));
    const T* Aw = static_cast<const T*>(mxGetData(W));

    T* outMed = static_cast<T*>(mxGetData(outMedArr));
    T* outStd = outStdArr ? static_cast<T*>(mxGetData(outStdArr)) : nullptr;

    #if defined(_OPENMP)
    #pragma omp parallel
    #endif
    {
        std::vector<T> rowX(static_cast<size_t>(N));
        std::vector<T> rowW(static_cast<size_t>(N));
        SliceBuffers<T> buf;
        buf.reserve(static_cast<size_t>(N));

        #if defined(_OPENMP)
        #pragma omp for schedule(static)
        #endif
        for (mwIndex i = 0; i < M; ++i) {
            for (mwIndex j = 0; j < N; ++j) {
                rowX[j] = Ax[i + j * M];
                rowW[j] = Aw[i + j * M];
            }

            T med, sd;
            compute_slice(rowX.data(), rowW.data(), N, method, buf, med, sd);
            outMed[i] = med;
            if (outStd) outStd[i] = sd;
        }
    }
}

// ========================= Input validation =========================

static MethodKind parse_method(const mxArray* in) {
    if (in == nullptr || mxIsEmpty(in)) {
        return METHOD_MEDIAN;
    }

    if (!mxIsChar(in)) {
        mexErrMsgIdAndTxt("wmedianStd_mex:method",
                          "Method must be 'median' or 'quantile'.");
    }

    char buf[32];
    buf[0] = '\0';
    mxGetString(in, buf, sizeof(buf));

    std::string s(buf);
    for (size_t i = 0; i < s.size(); ++i) {
        s[i] = static_cast<char>(std::tolower(static_cast<unsigned char>(s[i])));
    }

    if (s == "median")   return METHOD_MEDIAN;
    if (s == "quantile") return METHOD_QUANTILE;

    mexErrMsgIdAndTxt("wmedianStd_mex:method",
                      "Unknown Method. Use 'median' or 'quantile'.");
    return METHOD_MEDIAN;
}

static int parse_dim(const mxArray* in) {
    if (in == nullptr || mxIsEmpty(in)) {
        return 1;
    }

    if (!mxIsDouble(in) || mxIsComplex(in) || mxGetNumberOfElements(in) != 1) {
        mexErrMsgIdAndTxt("wmedianStd_mex:dim", "Dim must be scalar 1 or 2.");
    }

    const int d = static_cast<int>(mxGetScalar(in));
    if (d != 1 && d != 2) {
        mexErrMsgIdAndTxt("wmedianStd_mex:dim", "Dim must be 1 or 2.");
    }
    return d;
}

static void validate_inputs(const mxArray* A, const mxArray* W) {
    if ((!mxIsDouble(A) && !mxIsSingle(A)) || mxIsComplex(A)) {
        mexErrMsgIdAndTxt("wmedianStd_mex:type",
                          "Array must be real single or double.");
    }

    if ((!mxIsDouble(W) && !mxIsSingle(W)) || mxIsComplex(W)) {
        mexErrMsgIdAndTxt("wmedianStd_mex:type",
                          "Weights must be real single or double.");
    }

    if (mxGetClassID(A) != mxGetClassID(W)) {
        mexErrMsgIdAndTxt("wmedianStd_mex:type",
                          "Array and Weights must have the same class.");
    }

    if (mxGetNumberOfDimensions(A) != 2 || mxGetNumberOfDimensions(W) != 2) {
        mexErrMsgIdAndTxt("wmedianStd_mex:ndims",
                          "Array and Weights must be 2-D.");
    }

    if (mxGetM(A) != mxGetM(W) || mxGetN(A) != mxGetN(W)) {
        mexErrMsgIdAndTxt("wmedianStd_mex:size",
                          "Array and Weights must have the same size.");
    }
}

// ========================= MEX entry =========================

void mexFunction(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[]) {
    if (nrhs < 2) {
        mexErrMsgIdAndTxt("wmedianStd_mex:args",
                          "Usage: [Med, WStd] = wmedianStd_mex(Array, Weights, Dim, Method)");
    }
    if (nlhs > 2) {
        mexErrMsgIdAndTxt("wmedianStd_mex:nlhs",
                          "Too many output arguments.");
    }

    const mxArray* A = prhs[0];
    const mxArray* W = prhs[1];

    validate_inputs(A, W);

    const int dim = (nrhs >= 3) ? parse_dim(prhs[2]) : 1;
    const MethodKind method = (nrhs >= 4) ? parse_method(prhs[3]) : METHOD_MEDIAN;

    const mwSize M = mxGetM(A);
    const mwSize N = mxGetN(A);
    const mxClassID cls = mxGetClassID(A);

    if (dim == 1) {
        plhs[0] = mxCreateNumericMatrix(1, N, cls, mxREAL);
        if (nlhs >= 2) {
            plhs[1] = mxCreateNumericMatrix(1, N, cls, mxREAL);
        }
    } else {
        plhs[0] = mxCreateNumericMatrix(M, 1, cls, mxREAL);
        if (nlhs >= 2) {
            plhs[1] = mxCreateNumericMatrix(M, 1, cls, mxREAL);
        }
    }

    mxArray* outStdArr = (nlhs >= 2) ? plhs[1] : nullptr;

    if (cls == mxDOUBLE_CLASS) {
        if (dim == 1) {
            run_dim1<double>(A, W, plhs[0], outStdArr, method);
        } else {
            run_dim2<double>(A, W, plhs[0], outStdArr, method);
        }
    } else {
        if (dim == 1) {
            run_dim1<float>(A, W, plhs[0], outStdArr, method);
        } else {
            run_dim2<float>(A, W, plhs[0], outStdArr, method);
        }
    }
}
