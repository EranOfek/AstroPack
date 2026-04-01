// wmedianStd_mex.cpp
// [Med, WStd] = wmedianStd_mex(Array, Weights, Dim, Method)
//   Array, Weights: real single|double, same size
//   Dim:    1 (columns, default) or 2 (rows)
//   Method: 'median' (default) | 'quantile'
// Behavior:
//   * Omits NaNs/Infs (either Array or Weights)
//   * Ignores weights <= 0
//   * If any weight==Inf with finite value: operate only on those values
//       - Med  = unweighted median (lower-of-two for even count)
//       - WStd = unweighted population std
//   * WStd = population weighted std: sqrt( sum(w*x^2) - 2*mu*sum(w*x) + mu^2*sum(w) ) / sum(w)

#include "mex.h"
#include <cmath>
#include <limits>
#include <vector>
#include <string>
#include <algorithm>

#if defined(_OPENMP)
  #include <omp.h>
#endif

template<typename T> inline bool finiteT(T x){ return std::isfinite(static_cast<double>(x)); }
template<typename T> inline T   NaN()        { return std::numeric_limits<T>::quiet_NaN(); }
template<typename T> inline T   avg2(T a, T b){ return (a + b) / (T)2; }

// ---------- Tiny helpers (unweighted fallback for Inf-weight cases) ----------
template<typename T>
static T median_unweighted(std::vector<T>& x){
    const size_t n = x.size();
    if (!n) return NaN<T>();
    const size_t k = (n-1)>>1;  // lower median
    std::nth_element(x.begin(), x.begin()+k, x.end());
    T a = x[k];
    if (n & 1) return a;
    std::nth_element(x.begin()+k+1, x.begin()+k, x.end());
    return avg2(a, x[k+1]);
}

template<typename T>
static T std_unweighted_pop(const std::vector<T>& x){
    const size_t n = x.size();
    if (!n) return NaN<T>();
    long double s=0.0L;
    for (size_t i=0;i<n;++i) s += (long double)x[i];
    const long double mu = s / (long double)n;
    long double acc=0.0L;
    for (size_t i=0;i<n;++i){
        long double d = (long double)x[i] - mu;
        acc += d*d;
    }
    return (T)std::sqrt(acc / (long double)n);
}

// ---------- Pivot & partition for weighted median selection ----------
template<typename T>
inline T tukey_ninther_buf(const std::vector<T>& x, size_t L, size_t R){
    const size_t n = R - L;
    const auto med3 = [](T A,T B,T C)->T{
        if (A>B) std::swap(A,B);
        if (B>C) std::swap(B,C);
        if (A>B) std::swap(A,B);
        return B;
    };
    size_t a = L + n/8, b = L + n/2, c = L + (7*n)/8;
    size_t a1 = L + n/16, a3 = L + 3*n/16;
    size_t b1 = L + 7*n/16, b3 = L + 9*n/16;
    size_t c1 = L + 13*n/16, c3 = L + 15*n/16;

    T m1 = med3(x[a1], x[a],  x[a3]);
    T m2 = med3(x[b1], x[b],  x[b3]);
    T m3 = med3(x[c1], x[c],  x[c3]);
    return med3(m1,m2,m3);
}

// In-place 3-way partition on [L,R): <pv | ==pv | >pv
template<typename T>
static void partition_inplace(std::vector<T>& x, std::vector<T>& w,
                              size_t L, size_t R, T pv,
                              double& w_lt, double& w_eq,
                              size_t& lt, size_t& eq)
{
    size_t i=L; lt=L; size_t gt=R;
    w_lt=0.0; w_eq=0.0;
    while (i<gt){
        if (x[i] < pv){
            w_lt += (double)w[i];
            std::swap(x[i], x[lt]); std::swap(w[i], w[lt]);
            ++i; ++lt;
        } else if (x[i] > pv){
            --gt;
            std::swap(x[i], x[gt]); std::swap(w[i], w[gt]);
        } else {
            w_eq += (double)w[i];
            ++i;
        }
    }
    eq = gt; // [lt,eq) are equals
}

// ---------- Weighted median (single slice), also compute weighted std via sums ----------
template<typename T>
static void wmedian_wstd_slice(const T* x, const T* w, mwSize n, int method,
                               T& outMed, T& outStd)
{
    // Gather finite + (w>0) to buffers; detect Inf-weight values; accumulate sums for std.
    std::vector<T> xv; xv.reserve(n);
    std::vector<T> wv; wv.reserve(n);
    std::vector<T> xInf; xInf.reserve(4);

    long double S1=0.0L, Sx=0.0L, Sxx=0.0L;

    for (mwSize i=0;i<n;++i){
        const T xi = x[i];
        const T wi = w[i];
        if (!finiteT(xi) || !finiteT(wi)){
            if (finiteT(xi) && std::isinf((double)wi))
                xInf.push_back(xi);
            continue;
        }
        if (wi > (T)0){
            xv.push_back(xi);
            wv.push_back(wi);
            const long double wl = (long double)wi;
            const long double xl = (long double)xi;
            S1  += wl;
            Sx  += wl*xl;
            Sxx += wl*xl*xl;
        }
    }

    // Inf-weight collapse (no extra loops: reuse vectors)
    if (!xInf.empty()){
        std::vector<T> tmp = xInf; // median modifies
        outMed = median_unweighted(tmp);
        outStd = std_unweighted_pop(xInf);
        return;
    }

    if (!(S1 > 0.0L) || xv.empty()){
        outMed = NaN<T>(); outStd = NaN<T>(); return;
    }

    // Weighted std from the accumulated sums (no extra pass)
    const long double mu = Sx / S1;
    long double var = (Sxx - 2.0L*mu*Sx + mu*mu*S1) / S1;
    if (var < 0.0L) var = 0.0L;
    outStd = (T)std::sqrt(var);

    // Weighted median:
    const double half = 0.5 * (double)S1;

    // Tiny-n: sort+scan (fast)
    if (xv.size() <= 32){
        std::vector<size_t> idx(xv.size());
        for (size_t i=0;i<idx.size();++i) idx[i]=i;
        std::sort(idx.begin(), idx.end(), [&](size_t a,size_t b){return xv[a] < xv[b];});
        double c=0.0;
        for (size_t j=0;j<idx.size();++j){
            c += (double)wv[idx[j]];
            if (c >= half){
                if (method==0){
                    outMed = xv[idx[j]];
                }else{
                    const double c_prev = c - (double)wv[idx[j]];
                    if (std::abs(c_prev - half) <= 1e-15 && j>0)
                        outMed = avg2(xv[idx[j-1]], xv[idx[j]]);
                    else
                        outMed = xv[idx[j]];
                }
                return;
            }
        }
        outMed = xv[idx.back()];
        return;
    }

    // Quickselect-like, in-place on buffers
    size_t L=0, R=xv.size();
    double need = half;

    while (true){
        const size_t len = R - L;
        T pv = (len >= 64) ? tukey_ninther_buf(xv, L, R) : xv[L + len/2];

        double w_lt, w_eq; size_t lt, eq;
        partition_inplace(xv, wv, L, R, pv, w_lt, w_eq, lt, eq);

        if (w_lt > need + 1e-15){
            R = lt;
        } else if (w_lt + w_eq >= need - 1e-15){
            if (method==0){
                outMed = pv; return;
            } else {
                const bool exact = std::abs(w_lt - need) <= 1e-15;
                const bool hasEq = (w_eq > 0.0);
                if (exact && !hasEq){
                    // need neighbor maxima/minima
                    T maxL = xv[L];
                    for (size_t i=L+1;i<lt;++i) if (xv[i] > maxL) maxL = xv[i];
                    T minR = xv[eq];
                    for (size_t i=eq+1;i<R;++i) if (xv[i] < minR) minR = xv[i];
                    outMed = avg2(maxL, minR);
                } else {
                    outMed = pv;
                }
                return;
            }
        } else {
            need -= (w_lt + w_eq);
            L = eq;
        }
    }
}

// ---------- Drivers (Dim=1/2) ----------
template<typename T>
static void run_dim1(const mxArray* A, const mxArray* W, mxArray* oMed, mxArray* oStd, int method)
{
    const mwSize M = mxGetM(A), N = mxGetN(A);
    const T* Ax = (const T*)mxGetData(A);
    const T* Aw = (const T*)mxGetData(W);
    T* Om = (T*)mxGetData(oMed);
    T* Os = (T*)mxGetData(oStd);

    #pragma omp parallel for if(N>8) schedule(static)
    for (mwIndex j=0;j<N;++j){
        T med, sd;
        wmedian_wstd_slice(Ax + j*M, Aw + j*M, M, method, med, sd);
        Om[j] = med; Os[j] = sd;
    }
}

template<typename T>
static void run_dim2(const mxArray* A, const mxArray* W, mxArray* oMed, mxArray* oStd, int method)
{
    const mwSize M = mxGetM(A), N = mxGetN(A);
    const T* Ax = (const T*)mxGetData(A);
    const T* Aw = (const T*)mxGetData(W);
    T* Om = (T*)mxGetData(oMed);
    T* Os = (T*)mxGetData(oStd);

    #pragma omp parallel
    {
        std::vector<T> xb; xb.resize(N);
        std::vector<T> wb; wb.resize(N);

        #pragma omp for schedule(static)
        for (mwIndex i=0;i<M;++i){
            for (mwIndex j=0;j<N;++j){
                xb[j] = Ax[i + j*M];
                wb[j] = Aw[i + j*M];
            }
            T med, sd;
            wmedian_wstd_slice(xb.data(), wb.data(), N, method, med, sd);
            Om[i] = med; Os[i] = sd;
        }
    }
}

// ---------- MEX entry ----------
void mexFunction(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[])
{
    if (nrhs < 2)
        mexErrMsgIdAndTxt("wmedianStd_mex:args", "Usage: [Med,WStd] = wmedianStd_mex(Array,Weights,Dim,Method)");

    const mxArray* A = prhs[0];
    const mxArray* W = prhs[1];

    if ((!mxIsDouble(A) && !mxIsSingle(A)) || mxIsComplex(A))
        mexErrMsgIdAndTxt("wmedianStd_mex:type", "Array must be real single or double.");
    if (mxGetClassID(A) != mxGetClassID(W))
        mexErrMsgIdAndTxt("wmedianStd_mex:type", "Array and Weights must have the same class.");
    if (mxGetM(A)!=mxGetM(W) || mxGetN(A)!=mxGetN(W))
        mexErrMsgIdAndTxt("wmedianStd_mex:size", "Array and Weights must have the same size.");

    // Dim (default 1)
    int Dim = 1;
    if (nrhs >= 3 && !mxIsEmpty(prhs[2])){
        if (!mxIsDouble(prhs[2]) || mxIsComplex(prhs[2]) || mxGetNumberOfElements(prhs[2])!=1)
            mexErrMsgIdAndTxt("wmedianStd_mex:dim", "Dim must be scalar 1 or 2.");
        Dim = (int)mxGetScalar(prhs[2]);
        if (Dim!=1 && Dim!=2)
            mexErrMsgIdAndTxt("wmedianStd_mex:dim", "Dim must be 1 or 2.");
    }

    // Method (default 'median')
    int method = 0; // 0=median (discrete L1), 1=quantile (interp on exact 0.5 gap)
    if (nrhs >= 4 && !mxIsEmpty(prhs[3])){
        if (!mxIsChar(prhs[3]))
            mexErrMsgIdAndTxt("wmedianStd_mex:method", "Method must be 'median' or 'quantile'.");
        char buf[32]; buf[0]='\0';
        mxGetString(prhs[3], buf, sizeof(buf));
        std::string s(buf);
        for (char& c : s) c = (char)std::tolower((unsigned char)c);
        if (s=="median") method = 0;
        else if (s=="quantile") method = 1;
        else mexErrMsgIdAndTxt("wmedianStd_mex:method", "Unknown Method.");
    }

    const mwSize M = mxGetM(A), N = mxGetN(A);
    const mxClassID cls = mxGetClassID(A);

    // Outputs
    if (Dim==1){
        plhs[0] = mxCreateNumericMatrix(1, N, cls, mxREAL);
        plhs[1] = mxCreateNumericMatrix(1, N, cls, mxREAL);
    } else {
        plhs[0] = mxCreateNumericMatrix(M, 1, cls, mxREAL);
        plhs[1] = mxCreateNumericMatrix(M, 1, cls, mxREAL);
    }

    if (cls == mxDOUBLE_CLASS){
        if (Dim==1) run_dim1<double>(A,W,plhs[0],plhs[1],method);
        else        run_dim2<double>(A,W,plhs[0],plhs[1],method);
    } else {
        if (Dim==1) run_dim1<float >(A,W,plhs[0],plhs[1],method);
        else        run_dim2<float >(A,W,plhs[0],plhs[1],method);
    }
}
