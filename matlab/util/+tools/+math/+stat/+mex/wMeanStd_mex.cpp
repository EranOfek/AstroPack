// wMeanStd_mex.cpp
// [WMean, WStd, WError] = wmeanStd_mex(Array, Weights, Dim)
// - Array, Weights: real single|double, same size
// - Dim: 1 (columns, default) or 2 (rows)
// Behavior:
//   * Omits NaNs/Infs in Array or Weights
//   * Ignores weights <= 0
//   * If any weight==Inf (and value finite) in a slice: collapse to those values,
//     WMean/WStd become the unweighted population mean/std of those values; WError=0
//   * WStd = population weighted std: sqrt( sum(w*(x-mu)^2) / sum(w) )
//   * WError = sqrt(1/sum(w))  (inverse-variance convention)

#include "mex.h"
#include <cmath>
#include <limits>

#if defined(_OPENMP)
  #include <omp.h>
#endif

template<typename T> inline bool finiteT(T x){ return std::isfinite(static_cast<double>(x)); }
template<typename T> inline T   NaN()        { return std::numeric_limits<T>::quiet_NaN(); }

// --- SIMD-friendly three-reduction core (all finite, w>0) ---
template<typename T>
inline void fast_reductions(const T* x, const T* w, mwSize n,
                            long double& S1, long double& Sx, long double& Sxx)
{
    long double s1=0.0L, sx=0.0L, sxx=0.0L;
    #if defined(_OPENMP)
    #pragma omp simd reduction(+:s1,sx,sxx)
    #endif
    for (mwSize i=0;i<n;++i){
        const long double wi = (long double)w[i];
        const long double xi = (long double)x[i];
        s1  += wi;
        sx  += wi * xi;
        sxx += wi * xi * xi;
    }
    S1 = s1; Sx = sx; Sxx = sxx;
}

// Unweighted population mean/std (Inf-weight fallback)
template<typename T>
static void mean_std_unweighted(const T* v, mwSize n, T& mu, T& sd)
{
    if (n==0){ mu=NaN<T>(); sd=NaN<T>(); return; }
    long double s=0.0L;
    #if defined(_OPENMP)
    #pragma omp simd reduction(+:s)
    #endif
    for (mwSize i=0;i<n;++i) s += (long double)v[i];
    const long double mean = s / (long double)n;

    long double acc=0.0L;
    #if defined(_OPENMP)
    #pragma omp simd reduction(+:acc)
    #endif
    for (mwSize i=0;i<n;++i){
        const long double d = (long double)v[i] - mean;
        acc += d*d;
    }
    mu = (T)mean;
    sd = (T)std::sqrt(acc/(long double)n);
}

// SIMD-friendly pre-scan: true iff all x,w are finite and w>0
template<typename T>
inline bool all_finite_pos(const T* x, const T* w, mwSize n){
#if defined(_OPENMP)
    // Some compilers dislike bitwise-AND reductions; use sum of "bad" flags.
    int bad = 0;
    #pragma omp simd reduction(+:bad)
    for (mwSize i=0; i<n; ++i){
        bad += (!std::isfinite((double)x[i]) ||
                !std::isfinite((double)w[i]) ||
                w[i] <= (T)0) ? 1 : 0;
    }
    return bad == 0;
#else
    for (mwSize i=0; i<n; ++i){
        if (!std::isfinite((double)x[i]) ||
            !std::isfinite((double)w[i]) ||
            w[i] <= (T)0)
            return false;
    }
    return true;
#endif
}

// Robust reductions: handle NaN/Inf, <=0, and Inf weights (with scratch for Inf values)
template<typename T>
static void robust_reductions(const T* x, const T* w, mwSize n,
                              bool& infOnly, long double& S1, long double& Sx, long double& Sxx,
                              T*& infBuf, mwSize& nInf)
{
    S1=Sx=Sxx=0.0L; infOnly=false; nInf=0; infBuf=nullptr;

    // Count positives and Inf-weighted finite values
    mwSize cntPos=0, cntInf=0;
    for (mwSize i=0;i<n;++i){
        const T xi=x[i], wi=w[i];
        if (!finiteT(xi) || !finiteT(wi)) {
            if (finiteT(xi) && std::isinf((double)wi)) ++cntInf;
            continue;
        }
        if (wi > (T)0) ++cntPos;
    }

    if (cntInf>0 && cntPos==0){
        infOnly = true;
        nInf = cntInf;
        infBuf = (T*)mxCalloc(nInf, sizeof(T));
        mwSize k=0;
        for (mwSize i=0;i<n;++i){
            const T xi=x[i], wi=w[i];
            if (finiteT(xi) && std::isinf((double)wi)) infBuf[k++]=xi;
        }
        return;
    }

    if (cntInf>0){
        // Mixed finite + Inf: Inf dominates (IV convention)
        infOnly = true;
        nInf = cntInf;
        infBuf = (T*)mxCalloc(nInf, sizeof(T));
        mwSize k=0;
        for (mwSize i=0;i<n;++i){
            const T xi=x[i], wi=w[i];
            if (finiteT(xi) && std::isinf((double)wi)) infBuf[k++]=xi;
        }
        return;
    }

    long double s1=0.0L, sx_=0.0L, sxx_=0.0L;
    #if defined(_OPENMP)
    #pragma omp simd reduction(+:s1,sx_,sxx_)
    #endif
    for (mwSize i=0;i<n;++i){
        const T xi=x[i], wi=w[i];
        if (!finiteT(xi) || !finiteT(wi) || wi <= (T)0) continue;
        const long double wl = (long double)wi;
        const long double xl = (long double)xi;
        s1   += wl;
        sx_  += wl*xl;
        sxx_ += wl*xl*xl;
    }
    S1=s1; Sx=sx_; Sxx=sxx_;
}

// Compute one contiguous slice
template<typename T>
static inline void slice_compute(const T* x, const T* w, mwSize n,
                                 T& oMean, T& oStd, T& oErr)
{
    const bool hot = all_finite_pos(x, w, n);
    if (hot){
        long double S1,Sx,Sxx;
        fast_reductions(x,w,n,S1,Sx,Sxx);
        if (!(S1 > 0.0L)){ oMean=NaN<T>(); oStd=NaN<T>(); oErr=NaN<T>(); return; }
        const long double mu  = Sx / S1;
        long double var = (Sxx - 2.0L*mu*Sx + mu*mu*S1) / S1;
        if (var < 0.0L) var = 0.0L;
        oMean = (T)mu;
        oStd  = (T)std::sqrt(var);
        oErr  = (T)std::sqrt(1.0L / S1);
        return;
    }

    // Robust path (NaNs omitted, w<=0 ignored, Inf handled)
    bool infOnly=false;
    long double S1=0.0L,Sx=0.0L,Sxx=0.0L;
    T* infBuf=nullptr; mwSize nInf=0;
    robust_reductions(x,w,n,infOnly,S1,Sx,Sxx,infBuf,nInf);

    if (infOnly){
        T mu, sd;
        mean_std_unweighted(infBuf, nInf, mu, sd);
        mxFree(infBuf);
        oMean = mu; oStd = sd; oErr = (T)0;
        return;
    }

    if (!(S1>0.0L)){ oMean=NaN<T>(); oStd=NaN<T>(); oErr=NaN<T>(); return; }

    const long double mu  = Sx / S1;
    long double var = (Sxx - 2.0L*mu*Sx + mu*mu*S1) / S1;
    if (var < 0.0L) var = 0.0L;
    oMean = (T)mu;
    oStd  = (T)std::sqrt(var);
    oErr  = (T)std::sqrt(1.0L / S1);
}

// Dim=1: columns (contiguous)
template<typename T>
static void run_dim1(const mxArray* A, const mxArray* W, mxArray* OM, mxArray* OS, mxArray* OE)
{
    const mwSize M = mxGetM(A), N = mxGetN(A);
    const T* Ax = (const T*)mxGetData(A);
    const T* Aw = (const T*)mxGetData(W);
    T* oM = (T*)mxGetData(OM);
    T* oS = OS ? (T*)mxGetData(OS) : nullptr;
    T* oE = OE ? (T*)mxGetData(OE) : nullptr;

    #pragma omp parallel for if (N>8) schedule(static)
    for (mwIndex j=0;j<N;++j){
        T mu, sd, se;
        slice_compute(Ax + j*M, Aw + j*M, M, mu, sd, se);
        oM[j] = mu;
        if (oS) oS[j] = sd;
        if (oE) oE[j] = se;
    }
}

// Dim=2: rows (gather to contiguous scratch for SIMD)
template<typename T>
static void run_dim2(const mxArray* A, const mxArray* W, mxArray* OM, mxArray* OS, mxArray* OE)
{
    const mwSize M = mxGetM(A), N = mxGetN(A);
    const T* Ax = (const T*)mxGetData(A);
    const T* Aw = (const T*)mxGetData(W);
    T* oM = (T*)mxGetData(OM);
    T* oS = OS ? (T*)mxGetData(OS) : nullptr;
    T* oE = OE ? (T*)mxGetData(OE) : nullptr;

    #pragma omp parallel
    {
        T* xbuf = (T*)mxCalloc(N, sizeof(T));
        T* wbuf = (T*)mxCalloc(N, sizeof(T));

        #pragma omp for schedule(static)
        for (mwIndex i=0;i<M;++i){
            for (mwIndex j=0;j<N;++j){
                xbuf[j] = Ax[i + j*M];
                wbuf[j] = Aw[i + j*M];
            }
            T mu, sd, se;
            slice_compute(xbuf, wbuf, N, mu, sd, se);
            oM[i] = mu;
            if (oS) oS[i] = sd;
            if (oE) oE[i] = se;
        }
        mxFree(xbuf); mxFree(wbuf);
    }
}

// ---------------- MEX entry ----------------
void mexFunction(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[])
{
    if (nrhs < 2)
        mexErrMsgIdAndTxt("wmeanStd_mex:args", "Usage: [WMean,WStd,WError] = wmeanStd_mex(Array,Weights,Dim)");

    const mxArray* A = prhs[0];
    const mxArray* W = prhs[1];

    if ((!mxIsDouble(A) && !mxIsSingle(A)) || mxIsComplex(A))
        mexErrMsgIdAndTxt("wmeanStd_mex:type", "Array must be real single or double.");
    if (mxGetClassID(A) != mxGetClassID(W))
        mexErrMsgIdAndTxt("wmeanStd_mex:type", "Array and Weights must have the same class.");
    if (mxGetM(A)!=mxGetM(W) || mxGetN(A)!=mxGetN(W))
        mexErrMsgIdAndTxt("wmeanStd_mex:size", "Array and Weights must have the same size.");

    int Dim = 1;
    if (nrhs >= 3 && !mxIsEmpty(prhs[2])){
        if (!mxIsDouble(prhs[2]) || mxIsComplex(prhs[2]) || mxGetNumberOfElements(prhs[2])!=1)
            mexErrMsgIdAndTxt("wmeanStd_mex:dim", "Dim must be scalar 1 or 2.");
        Dim = (int)mxGetScalar(prhs[2]);
        if (Dim!=1 && Dim!=2)
            mexErrMsgIdAndTxt("wmeanStd_mex:dim", "Dim must be 1 or 2.");
    }

    const mwSize M = mxGetM(A), N = mxGetN(A);
    mxClassID cls = mxGetClassID(A);

    // outputs (only first mandatory)
    if (Dim==1){
        plhs[0] = mxCreateNumericMatrix(1, N, cls, mxREAL);
        if (nlhs>=2) plhs[1] = mxCreateNumericMatrix(1, N, cls, mxREAL);
        if (nlhs>=3) plhs[2] = mxCreateNumericMatrix(1, N, cls, mxREAL);
    } else {
        plhs[0] = mxCreateNumericMatrix(M, 1, cls, mxREAL);
        if (nlhs>=2) plhs[1] = mxCreateNumericMatrix(M, 1, cls, mxREAL);
        if (nlhs>=3) plhs[2] = mxCreateNumericMatrix(M, 1, cls, mxREAL);
    }

    mxArray* OS = (nlhs>=2) ? plhs[1] : nullptr;
    mxArray* OE = (nlhs>=3) ? plhs[2] : nullptr;

    if (cls == mxDOUBLE_CLASS){
        if (Dim==1) run_dim1<double>(A,W,plhs[0],OS,OE);
        else        run_dim2<double>(A,W,plhs[0],OS,OE);
    } else {
        if (Dim==1) run_dim1<float>(A,W,plhs[0],OS,OE);
        else        run_dim2<float>(A,W,plhs[0],OS,OE);
    }
}
