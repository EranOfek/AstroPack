#include "mex.h"
#include <algorithm>
#include <vector>
#include <string>
#include <cmath>
#include <limits>

#if defined(_OPENMP)
  #include <omp.h>
#endif
// mex -O -R2018a CXXFLAGS="\$CXXFLAGS -fopenmp -O3 -march=native" LDFLAGS="\$LDFLAGS -fopenmp" wmedian_mex.cpp 

// ------------------------------ Helpers ------------------------------

template<typename T>
inline bool finiteT(T x){ return std::isfinite(static_cast<double>(x)); }

template<typename T> inline T NaN() { return std::numeric_limits<T>::quiet_NaN(); }
template<typename T> inline T avg2(const T& a, const T& b){ return (a + b) / static_cast<T>(2); }

// Lower median for Inf-weight case
template<typename T>
static T median_unweighted(std::vector<T>& x){
    size_t n = x.size();
    if (!n) return NaN<T>();
    size_t k = (n - 1) >> 1;
    std::nth_element(x.begin(), x.begin()+k, x.end());
    T a = x[k];
    if (n & 1) return a;
    std::nth_element(x.begin()+k+1, x.begin()+k, x.end());
    return avg2(a, x[k+1]);
}

// Tukey ninther pivot from subrange [L,R)
template<typename T>
inline T tukey_ninther(const T* x, size_t L, size_t R){
    size_t n = R - L;
    size_t a = L + n/8, b = L + n/2, c = L + (7*n)/8;
    size_t a1 = L + n/16, a3 = L + 3*n/16;
    size_t b1 = L + 7*n/16, b3 = L + 9*n/16;
    size_t c1 = L + 13*n/16, c3 = L + 15*n/16;

    auto med3 = [&](T A, T B, T C)->T{
        if (A > B) std::swap(A,B);
        if (B > C) std::swap(B,C);
        if (A > B) std::swap(A,B);
        return B;
    };
    T m1 = med3(x[a1], x[a],  x[a3]);
    T m2 = med3(x[b1], x[b],  x[b3]);
    T m3 = med3(x[c1], x[c],  x[c3]);
    return med3(m1,m2,m3);
}

// In-place 3-way partition around pivot value pv on [L,R)
// Layout on return: [L,lt) < pv, [lt,eq) == pv, [eq,R) > pv
// Also returns cumulative weights of < and == segments (double precision).
template<typename T>
static void partition_inplace(T* x, T* w, size_t L, size_t R, T pv, double& w_lt, double& w_eq, size_t& lt, size_t& eq)
{
    size_t i = L;
    lt = L;
    size_t gt = R;
    w_lt = 0.0; w_eq = 0.0;

    while (i < gt){
        if (x[i] < pv){
            w_lt += static_cast<double>(w[i]);
            std::swap(x[i], x[lt]);
            std::swap(w[i], w[lt]);
            ++i; ++lt;
        } else if (x[i] > pv){
            --gt;
            std::swap(x[i], x[gt]);
            std::swap(w[i], w[gt]);
        } else { // equal
            w_eq += static_cast<double>(w[i]);
            ++i;
        }
    }
    eq = gt; // [lt, eq) equals
}

// Core: weighted median / 0.5-quantile for one slice (NaNs already removed).
// method=0 -> discrete L1 (smallest x s.t. CDF>=0.5); method=1 -> quantile (linear only for exact 0.5 gap).
template<typename T>
static T wmed_slice(T* xbuf, T* wbuf, size_t n, int method)
{
    if (n == 0) return NaN<T>();

    // Gather Inf-weight values and drop nonpositive/NaN/Inf weights.
    std::vector<T> x_inf; x_inf.reserve(4);

    size_t m = 0;
    double sumW = 0.0;
    for (size_t i=0;i<n;++i){
        T xi = xbuf[i];
        T wi = wbuf[i];
        if (!finiteT(xi) || !finiteT(wi)) {
            if (finiteT(xi) && std::isinf(static_cast<double>(wi))) {
                x_inf.push_back(xi);
            }
            continue;
        }
        if (wi > static_cast<T>(0)){
            xbuf[m] = xi;
            wbuf[m] = wi;
            sumW += static_cast<double>(wi);
            ++m;
        }
    }

    if (!x_inf.empty()){
        return median_unweighted(x_inf);
    }
    if (m == 0) return NaN<T>();

    // Tiny-n fallback: sort+scan (fast for cache)
    if (m <= 32){
        std::vector<size_t> idx(m);
        for (size_t i=0;i<m;++i) idx[i]=i;
        std::sort(idx.begin(), idx.end(), [&](size_t a, size_t b){ return xbuf[a] < xbuf[b]; });
        double c = 0.0, half = 0.5*sumW;
        for (size_t j=0;j<m;++j){
            c += static_cast<double>(wbuf[idx[j]]);
            if (c >= half){
                if (method==0) return xbuf[idx[j]];
                const double c_prev = c - static_cast<double>(wbuf[idx[j]]);
                if (std::abs(c_prev - half) <= 1e-15 && j>0){
                    return avg2(xbuf[idx[j-1]], xbuf[idx[j]]);
                } else {
                    return xbuf[idx[j]];
                }
            }
        }
        return xbuf[idx.back()];
    }

    // Quickselect-like loop, in-place on [L,R)
    size_t L = 0, R = m;
    double need = 0.5 * sumW;

    while (true){
        const size_t len = R - L;

        // Choose pivot
        T pv;
        if (len >= 64){
            pv = tukey_ninther(xbuf, L, R);
        } else {
            pv = xbuf[L + len/2];
        }

        // Partition
        double w_lt, w_eq;
        size_t lt, eq;
        partition_inplace(xbuf, wbuf, L, R, pv, w_lt, w_eq, lt, eq);

        if (w_lt > need + 1e-15){
            // go left
            R = lt;
            continue;
        } else if (w_lt + w_eq >= need - 1e-15){
            // pv is a valid answer
            if (method==0) return pv;

            // quantile: interpolate ONLY if exactly between left and right with no mass at pivot
            const bool exact = std::abs(w_lt - need) <= 1e-15;
            const bool hasEq = (w_eq > 0.0);
            if (exact && !hasEq){
                // find max left (largest in [L,lt)) and min right (smallest in [eq,R))
                T maxL = xbuf[L];
                for (size_t i=L+1;i<lt;++i) if (xbuf[i] > maxL) maxL = xbuf[i];
                T minR = xbuf[eq];
                for (size_t i=eq+1;i<R;++i) if (xbuf[i] < minR) minR = xbuf[i];
                return avg2(maxL, minR);
            } else {
                return pv;
            }
        } else {
            // go right
            need -= (w_lt + w_eq);
            L = eq;
            continue;
        }
    }
}

// Thread-local reusable buffers for a slice (to avoid repeated mallocs).
template<typename T>
struct SliceBuf {
    std::vector<T> x, w;
    void clear_reserve(size_t n){
        x.clear(); w.clear();
        x.reserve(n); w.reserve(n);
    }
};

// ------------------------------ Drivers ------------------------------

template<typename T>
static void run_dim1(const mxArray* A, const mxArray* W, mxArray* out, int method)
{
    const mwSize M = mxGetM(A);
    const mwSize N = mxGetN(A);
    const T* Adata = static_cast<const T*>(mxGetData(A));
    const T* Wdata = static_cast<const T*>(mxGetData(W));
    T* Odata = static_cast<T*>(mxGetData(out)); // 1 x N

    #pragma omp parallel
    {
        SliceBuf<T> buf;
        buf.clear_reserve(M);

        #pragma omp for schedule(static)
        for (mwIndex j=0; j<N; ++j){
            buf.x.clear(); buf.w.clear();
            const T* xc = Adata + j*M;
            const T* wc = Wdata + j*M;

            // Pre-filter NaNs/Infs (values) and NaNs (weights). Keep Infs for weights (handled inside)
            for (mwIndex i=0;i<M;++i){
                T xi = xc[i], wi = wc[i];
                if (!finiteT(xi) || std::isnan(static_cast<double>(wi))) continue;
                buf.x.push_back(xi);
                buf.w.push_back(wi);
            }

            T* xptr = buf.x.data();
            T* wptr = buf.w.data();
            Odata[j] = wmed_slice(xptr, wptr, buf.x.size(), method);
        }
    }
}

template<typename T>
static void run_dim2(const mxArray* A, const mxArray* W, mxArray* out, int method)
{
    const mwSize M = mxGetM(A);
    const mwSize N = mxGetN(A);
    const T* Adata = static_cast<const T*>(mxGetData(A));
    const T* Wdata = static_cast<const T*>(mxGetData(W));
    T* Odata = static_cast<T*>(mxGetData(out)); // M x 1

    #pragma omp parallel
    {
        SliceBuf<T> buf;
        buf.clear_reserve(N);

        #pragma omp for schedule(static)
        for (mwIndex i=0; i<M; ++i){
            buf.x.clear(); buf.w.clear();
            for (mwIndex j=0;j<N;++j){
                const T xi = Adata[i + j*M];
                const T wi = Wdata[i + j*M];
                if (!finiteT(xi) || std::isnan(static_cast<double>(wi))) continue;
                buf.x.push_back(xi);
                buf.w.push_back(wi);
            }
            T* xptr = buf.x.data();
            T* wptr = buf.w.data();
            Odata[i] = wmed_slice(xptr, wptr, buf.x.size(), method);
        }
    }
}

// ------------------------------ MEX entry ------------------------------

void mexFunction(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[])
{
    if (nrhs < 2) mexErrMsgIdAndTxt("wmedian_mex:args", "Usage: Med = wmedian_mex(Array,Weights,Dim,Method)");

    const mxArray* A = prhs[0];
    const mxArray* W = prhs[1];

    if ((!mxIsDouble(A) && !mxIsSingle(A)) || mxIsComplex(A))
        mexErrMsgIdAndTxt("wmedian_mex:type", "Array must be real single or double.");
    if (mxGetClassID(A) != mxGetClassID(W))
        mexErrMsgIdAndTxt("wmedian_mex:type", "Array and Weights must have the same class.");
    if (mxGetM(A)!=mxGetM(W) || mxGetN(A)!=mxGetN(W))
        mexErrMsgIdAndTxt("wmedian_mex:size", "Array and Weights must have the same size.");

    // Dim (default 1)
    int Dim = 1;
    if (nrhs >= 3 && !mxIsEmpty(prhs[2])){
        if (!mxIsDouble(prhs[2]) || mxIsComplex(prhs[2]) || mxGetNumberOfElements(prhs[2])!=1)
            mexErrMsgIdAndTxt("wmedian_mex:dim", "Dim must be scalar 1 or 2.");
        Dim = static_cast<int>(mxGetScalar(prhs[2]));
        if (Dim!=1 && Dim!=2)
            mexErrMsgIdAndTxt("wmedian_mex:dim", "Dim must be 1 or 2.");
    }

    // Method (default 'median')
    int method = 0; // 0=median, 1=quantile
    if (nrhs >= 4 && !mxIsEmpty(prhs[3])){
        if (!mxIsChar(prhs[3])) mexErrMsgIdAndTxt("wmedian_mex:method", "Method must be 'median' or 'quantile'.");
        char buf[32]; buf[0]='\0';
        mxGetString(prhs[3], buf, sizeof(buf));
        std::string s(buf);
        for (char& c : s) c = static_cast<char>(::tolower(c));
        if (s=="median") method = 0;
        else if (s=="quantile") method = 1;
        else mexErrMsgIdAndTxt("wmedian_mex:method", "Unknown Method.");
    }

    const mwSize M = mxGetM(A), N = mxGetN(A);
    mxClassID cls = mxGetClassID(A);
    if (Dim==1) plhs[0] = mxCreateNumericMatrix(1, N, cls, mxREAL);
    else        plhs[0] = mxCreateNumericMatrix(M, 1, cls, mxREAL);

    if (cls == mxDOUBLE_CLASS){
        if (Dim==1) run_dim1<double>(A, W, plhs[0], method);
        else        run_dim2<double>(A, W, plhs[0], method);
    } else {
        if (Dim==1) run_dim1<float>(A, W, plhs[0], method);
        else        run_dim2<float>(A, W, plhs[0], method);
    }
}
