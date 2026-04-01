// wmean_mex.cpp
//
// [Mean, WStd, WErr] = wmean_mex(Array, Weights, Dim)
//
// Weighted mean, weighted std, and weighted error of the mean.
//
// Inputs:
//   Array, Weights : real single or double, same size, 2-D only
//   Dim            : 1 (default) or 2
//
// Outputs:
//   Mean : weighted mean
//   WStd : weighted population std
//   WErr : weighted error of the mean using
//          Neff = (sum w)^2 / sum(w^2),
//          WErr = WStd / sqrt(Neff)
//
// Behavior:
//   * Omits NaN/Inf in Array or Weights
//   * Ignores weights <= 0
//   * If no valid data remain, outputs NaN
//
// Notes:
//   * Two-pass algorithm: usually faster than Welford here
//   * AVX2 / AVX512 branches for contiguous slices
//   * Portable fallback scalar path
//   * Optional OpenMP
//

#include "mex.h"
#include <cmath>
#include <cstdint>
#include <limits>
#include <type_traits>
#include <new>

#if defined(_OPENMP)
  #include <omp.h>
#endif

#if defined(__AVX2__) || defined(__AVX512F__)
  #include <immintrin.h>
#endif

template<typename T> inline bool finiteT(T x) {
    return std::isfinite(static_cast<double>(x));
}
template<typename T> inline T NaN() {
    return std::numeric_limits<T>::quiet_NaN();
}

template<typename T>
struct Accum {
    long double SumW;
    long double SumWX;
    long double SumW2;
    long double SumWDX2;
    Accum() : SumW(0.0L), SumWX(0.0L), SumW2(0.0L), SumWDX2(0.0L) {}
};

// ============================================================
// Scalar kernels
// ============================================================

template<typename T>
static inline void pass1_scalar(const T* X, const T* W, mwSize N, Accum<T>& A)
{
    for (mwSize i = 0; i < N; ++i) {
        const T Xi = X[i];
        const T Wi = W[i];

        if (!finiteT(Xi) || !finiteT(Wi) || !(Wi > (T)0)) {
            continue;
        }

        const long double WL = (long double)Wi;
        const long double XL = (long double)Xi;

        A.SumW  += WL;
        A.SumWX += WL * XL;
        A.SumW2 += WL * WL;
    }
}

template<typename T>
static inline void pass2_scalar(const T* X, const T* W, mwSize N, long double Mu, Accum<T>& A)
{
    for (mwSize i = 0; i < N; ++i) {
        const T Xi = X[i];
        const T Wi = W[i];

        if (!finiteT(Xi) || !finiteT(Wi) || !(Wi > (T)0)) {
            continue;
        }

        const long double D  = (long double)Xi - Mu;
        const long double WL = (long double)Wi;
        A.SumWDX2 += WL * D * D;
    }
}

// ============================================================
// AVX2 / AVX512 helpers for double
// We accumulate vector partial sums into double lanes, then
// fold to long double at the end.
// ============================================================

#if defined(__AVX2__) || defined(__AVX512F__)

static inline double hsum256_pd(__m256d v)
{
    __m128d lo = _mm256_castpd256_pd128(v);
    __m128d hi = _mm256_extractf128_pd(v, 1);
    __m128d s  = _mm_add_pd(lo, hi);
    __m128d t  = _mm_unpackhi_pd(s, s);
    s = _mm_add_sd(s, t);
    return _mm_cvtsd_f64(s);
}

static inline float hsum256_ps(__m256 v)
{
    __m128 lo = _mm256_castps256_ps128(v);
    __m128 hi = _mm256_extractf128_ps(v, 1);
    __m128 s  = _mm_add_ps(lo, hi);
    s = _mm_hadd_ps(s, s);
    s = _mm_hadd_ps(s, s);
    return _mm_cvtss_f32(s);
}

#if defined(__AVX512F__)
static inline double hsum512_pd(__m512d v)
{
    return _mm512_reduce_add_pd(v);
}
static inline float hsum512_ps(__m512 v)
{
    return _mm512_reduce_add_ps(v);
}
#endif

#endif

// ============================================================
// AVX2 double
// ============================================================

#if defined(__AVX2__)

static inline void pass1_avx2_double(const double* X, const double* W, mwSize N, Accum<double>& A)
{
    const mwSize Step = 4;
    mwSize i = 0;

    __m256d VsumW  = _mm256_setzero_pd();
    __m256d VsumWX = _mm256_setzero_pd();
    __m256d VsumW2 = _mm256_setzero_pd();

    const __m256d Vzero = _mm256_setzero_pd();

    for (; i + Step <= N; i += Step) {
        const __m256d VX = _mm256_loadu_pd(X + i);
        const __m256d VW = _mm256_loadu_pd(W + i);

        const __m256d Xeq = _mm256_cmp_pd(VX, VX, _CMP_ORD_Q);
        const __m256d Weq = _mm256_cmp_pd(VW, VW, _CMP_ORD_Q);
        const __m256d Wgt = _mm256_cmp_pd(VW, Vzero, _CMP_GT_OQ);
        const __m256d M   = _mm256_and_pd(_mm256_and_pd(Xeq, Weq), Wgt);

        const __m256d Wm  = _mm256_and_pd(VW, M);
        const __m256d Xm  = _mm256_and_pd(VX, M);

        VsumW  = _mm256_add_pd(VsumW,  Wm);
        VsumWX = _mm256_add_pd(VsumWX, _mm256_mul_pd(Wm, Xm));
        VsumW2 = _mm256_add_pd(VsumW2, _mm256_mul_pd(Wm, Wm));
    }

    A.SumW  += (long double)hsum256_pd(VsumW);
    A.SumWX += (long double)hsum256_pd(VsumWX);
    A.SumW2 += (long double)hsum256_pd(VsumW2);

    for (; i < N; ++i) {
        const double Xi = X[i];
        const double Wi = W[i];
        if (!finiteT(Xi) || !finiteT(Wi) || !(Wi > 0.0)) continue;
        const long double WL = (long double)Wi;
        const long double XL = (long double)Xi;
        A.SumW  += WL;
        A.SumWX += WL * XL;
        A.SumW2 += WL * WL;
    }
}

static inline void pass2_avx2_double(const double* X, const double* W, mwSize N, long double Mu, Accum<double>& A)
{
    const mwSize Step = 4;
    mwSize i = 0;

    __m256d Vsum = _mm256_setzero_pd();
    const __m256d Vzero = _mm256_setzero_pd();
    const __m256d Vmu   = _mm256_set1_pd((double)Mu);

    for (; i + Step <= N; i += Step) {
        const __m256d VX = _mm256_loadu_pd(X + i);
        const __m256d VW = _mm256_loadu_pd(W + i);

        const __m256d Xeq = _mm256_cmp_pd(VX, VX, _CMP_ORD_Q);
        const __m256d Weq = _mm256_cmp_pd(VW, VW, _CMP_ORD_Q);
        const __m256d Wgt = _mm256_cmp_pd(VW, Vzero, _CMP_GT_OQ);
        const __m256d M   = _mm256_and_pd(_mm256_and_pd(Xeq, Weq), Wgt);

        const __m256d D   = _mm256_sub_pd(VX, Vmu);
        const __m256d D2  = _mm256_mul_pd(D, D);
        const __m256d Wm  = _mm256_and_pd(VW, M);
        const __m256d T   = _mm256_mul_pd(Wm, D2);

        Vsum = _mm256_add_pd(Vsum, T);
    }

    A.SumWDX2 += (long double)hsum256_pd(Vsum);

    for (; i < N; ++i) {
        const double Xi = X[i];
        const double Wi = W[i];
        if (!finiteT(Xi) || !finiteT(Wi) || !(Wi > 0.0)) continue;
        const long double D  = (long double)Xi - Mu;
        const long double WL = (long double)Wi;
        A.SumWDX2 += WL * D * D;
    }
}

static inline void pass1_avx2_float(const float* X, const float* W, mwSize N, Accum<float>& A)
{
    const mwSize Step = 8;
    mwSize i = 0;

    __m256 VsumW  = _mm256_setzero_ps();
    __m256 VsumWX = _mm256_setzero_ps();
    __m256 VsumW2 = _mm256_setzero_ps();

    const __m256 Vzero = _mm256_setzero_ps();

    for (; i + Step <= N; i += Step) {
        const __m256 VX = _mm256_loadu_ps(X + i);
        const __m256 VW = _mm256_loadu_ps(W + i);

        const __m256 Xeq = _mm256_cmp_ps(VX, VX, _CMP_ORD_Q);
        const __m256 Weq = _mm256_cmp_ps(VW, VW, _CMP_ORD_Q);
        const __m256 Wgt = _mm256_cmp_ps(VW, Vzero, _CMP_GT_OQ);
        const __m256 M   = _mm256_and_ps(_mm256_and_ps(Xeq, Weq), Wgt);

        const __m256 Wm  = _mm256_and_ps(VW, M);
        const __m256 Xm  = _mm256_and_ps(VX, M);

        VsumW  = _mm256_add_ps(VsumW,  Wm);
        VsumWX = _mm256_add_ps(VsumWX, _mm256_mul_ps(Wm, Xm));
        VsumW2 = _mm256_add_ps(VsumW2, _mm256_mul_ps(Wm, Wm));
    }

    A.SumW  += (long double)hsum256_ps(VsumW);
    A.SumWX += (long double)hsum256_ps(VsumWX);
    A.SumW2 += (long double)hsum256_ps(VsumW2);

    for (; i < N; ++i) {
        const float Xi = X[i];
        const float Wi = W[i];
        if (!finiteT(Xi) || !finiteT(Wi) || !(Wi > 0.0f)) continue;
        const long double WL = (long double)Wi;
        const long double XL = (long double)Xi;
        A.SumW  += WL;
        A.SumWX += WL * XL;
        A.SumW2 += WL * WL;
    }
}

static inline void pass2_avx2_float(const float* X, const float* W, mwSize N, long double Mu, Accum<float>& A)
{
    const mwSize Step = 8;
    mwSize i = 0;

    __m256 Vsum = _mm256_setzero_ps();
    const __m256 Vzero = _mm256_setzero_ps();
    const __m256 Vmu   = _mm256_set1_ps((float)Mu);

    for (; i + Step <= N; i += Step) {
        const __m256 VX = _mm256_loadu_ps(X + i);
        const __m256 VW = _mm256_loadu_ps(W + i);

        const __m256 Xeq = _mm256_cmp_ps(VX, VX, _CMP_ORD_Q);
        const __m256 Weq = _mm256_cmp_ps(VW, VW, _CMP_ORD_Q);
        const __m256 Wgt = _mm256_cmp_ps(VW, Vzero, _CMP_GT_OQ);
        const __m256 M   = _mm256_and_ps(_mm256_and_ps(Xeq, Weq), Wgt);

        const __m256 D   = _mm256_sub_ps(VX, Vmu);
        const __m256 D2  = _mm256_mul_ps(D, D);
        const __m256 Wm  = _mm256_and_ps(VW, M);
        const __m256 T   = _mm256_mul_ps(Wm, D2);

        Vsum = _mm256_add_ps(Vsum, T);
    }

    A.SumWDX2 += (long double)hsum256_ps(Vsum);

    for (; i < N; ++i) {
        const float Xi = X[i];
        const float Wi = W[i];
        if (!finiteT(Xi) || !finiteT(Wi) || !(Wi > 0.0f)) continue;
        const long double D  = (long double)Xi - Mu;
        const long double WL = (long double)Wi;
        A.SumWDX2 += WL * D * D;
    }
}

#endif

// ============================================================
// AVX512
// ============================================================

#if defined(__AVX512F__)

static inline void pass1_avx512_double(const double* X, const double* W, mwSize N, Accum<double>& A)
{
    const mwSize Step = 8;
    mwSize i = 0;

    __m512d VsumW  = _mm512_setzero_pd();
    __m512d VsumWX = _mm512_setzero_pd();
    __m512d VsumW2 = _mm512_setzero_pd();
    const __m512d Vzero = _mm512_setzero_pd();

    for (; i + Step <= N; i += Step) {
        const __m512d VX = _mm512_loadu_pd(X + i);
        const __m512d VW = _mm512_loadu_pd(W + i);

        const __mmask8 Mx = _mm512_cmp_pd_mask(VX, VX, _CMP_ORD_Q);
        const __mmask8 Mw = _mm512_cmp_pd_mask(VW, VW, _CMP_ORD_Q);
        const __mmask8 Mp = _mm512_cmp_pd_mask(VW, Vzero, _CMP_GT_OQ);
        const __mmask8 M  = Mx & Mw & Mp;

        VsumW  = _mm512_mask_add_pd(VsumW,  M, VsumW,  VW);
        VsumWX = _mm512_mask_add_pd(VsumWX, M, VsumWX, _mm512_mul_pd(VW, VX));
        VsumW2 = _mm512_mask_add_pd(VsumW2, M, VsumW2, _mm512_mul_pd(VW, VW));
    }

    A.SumW  += (long double)hsum512_pd(VsumW);
    A.SumWX += (long double)hsum512_pd(VsumWX);
    A.SumW2 += (long double)hsum512_pd(VsumW2);

    for (; i < N; ++i) {
        const double Xi = X[i];
        const double Wi = W[i];
        if (!finiteT(Xi) || !finiteT(Wi) || !(Wi > 0.0)) continue;
        const long double WL = (long double)Wi;
        const long double XL = (long double)Xi;
        A.SumW  += WL;
        A.SumWX += WL * XL;
        A.SumW2 += WL * WL;
    }
}

static inline void pass2_avx512_double(const double* X, const double* W, mwSize N, long double Mu, Accum<double>& A)
{
    const mwSize Step = 8;
    mwSize i = 0;

    __m512d Vsum = _mm512_setzero_pd();
    const __m512d Vzero = _mm512_setzero_pd();
    const __m512d Vmu   = _mm512_set1_pd((double)Mu);

    for (; i + Step <= N; i += Step) {
        const __m512d VX = _mm512_loadu_pd(X + i);
        const __m512d VW = _mm512_loadu_pd(W + i);

        const __mmask8 Mx = _mm512_cmp_pd_mask(VX, VX, _CMP_ORD_Q);
        const __mmask8 Mw = _mm512_cmp_pd_mask(VW, VW, _CMP_ORD_Q);
        const __mmask8 Mp = _mm512_cmp_pd_mask(VW, Vzero, _CMP_GT_OQ);
        const __mmask8 M  = Mx & Mw & Mp;

        const __m512d D  = _mm512_sub_pd(VX, Vmu);
        const __m512d T  = _mm512_mul_pd(VW, _mm512_mul_pd(D, D));

        Vsum = _mm512_mask_add_pd(Vsum, M, Vsum, T);
    }

    A.SumWDX2 += (long double)hsum512_pd(Vsum);

    for (; i < N; ++i) {
        const double Xi = X[i];
        const double Wi = W[i];
        if (!finiteT(Xi) || !finiteT(Wi) || !(Wi > 0.0)) continue;
        const long double D  = (long double)Xi - Mu;
        const long double WL = (long double)Wi;
        A.SumWDX2 += WL * D * D;
    }
}

static inline void pass1_avx512_float(const float* X, const float* W, mwSize N, Accum<float>& A)
{
    const mwSize Step = 16;
    mwSize i = 0;

    __m512 VsumW  = _mm512_setzero_ps();
    __m512 VsumWX = _mm512_setzero_ps();
    __m512 VsumW2 = _mm512_setzero_ps();
    const __m512 Vzero = _mm512_setzero_ps();

    for (; i + Step <= N; i += Step) {
        const __m512 VX = _mm512_loadu_ps(X + i);
        const __m512 VW = _mm512_loadu_ps(W + i);

        const __mmask16 Mx = _mm512_cmp_ps_mask(VX, VX, _CMP_ORD_Q);
        const __mmask16 Mw = _mm512_cmp_ps_mask(VW, VW, _CMP_ORD_Q);
        const __mmask16 Mp = _mm512_cmp_ps_mask(VW, Vzero, _CMP_GT_OQ);
        const __mmask16 M  = Mx & Mw & Mp;

        VsumW  = _mm512_mask_add_ps(VsumW,  M, VsumW,  VW);
        VsumWX = _mm512_mask_add_ps(VsumWX, M, VsumWX, _mm512_mul_ps(VW, VX));
        VsumW2 = _mm512_mask_add_ps(VsumW2, M, VsumW2, _mm512_mul_ps(VW, VW));
    }

    A.SumW  += (long double)hsum512_ps(VsumW);
    A.SumWX += (long double)hsum512_ps(VsumWX);
    A.SumW2 += (long double)hsum512_ps(VsumW2);

    for (; i < N; ++i) {
        const float Xi = X[i];
        const float Wi = W[i];
        if (!finiteT(Xi) || !finiteT(Wi) || !(Wi > 0.0f)) continue;
        const long double WL = (long double)Wi;
        const long double XL = (long double)Xi;
        A.SumW  += WL;
        A.SumWX += WL * XL;
        A.SumW2 += WL * WL;
    }
}

static inline void pass2_avx512_float(const float* X, const float* W, mwSize N, long double Mu, Accum<float>& A)
{
    const mwSize Step = 16;
    mwSize i = 0;

    __m512 Vsum = _mm512_setzero_ps();
    const __m512 Vzero = _mm512_setzero_ps();
    const __m512 Vmu   = _mm512_set1_ps((float)Mu);

    for (; i + Step <= N; i += Step) {
        const __m512 VX = _mm512_loadu_ps(X + i);
        const __m512 VW = _mm512_loadu_ps(W + i);

        const __mmask16 Mx = _mm512_cmp_ps_mask(VX, VX, _CMP_ORD_Q);
        const __mmask16 Mw = _mm512_cmp_ps_mask(VW, VW, _CMP_ORD_Q);
        const __mmask16 Mp = _mm512_cmp_ps_mask(VW, Vzero, _CMP_GT_OQ);
        const __mmask16 M  = Mx & Mw & Mp;

        const __m512 D = _mm512_sub_ps(VX, Vmu);
        const __m512 T = _mm512_mul_ps(VW, _mm512_mul_ps(D, D));

        Vsum = _mm512_mask_add_ps(Vsum, M, Vsum, T);
    }

    A.SumWDX2 += (long double)hsum512_ps(Vsum);

    for (; i < N; ++i) {
        const float Xi = X[i];
        const float Wi = W[i];
        if (!finiteT(Xi) || !finiteT(Wi) || !(Wi > 0.0f)) continue;
        const long double D  = (long double)Xi - Mu;
        const long double WL = (long double)Wi;
        A.SumWDX2 += WL * D * D;
    }
}

#endif

// ============================================================
// Dispatch for contiguous slices
// ============================================================

template<typename T>
static inline void pass1_best(const T* X, const T* W, mwSize N, Accum<T>& A)
{
#if defined(__AVX512F__)
    if (std::is_same<T,double>::value) {
        pass1_avx512_double((const double*)X, (const double*)W, N, (Accum<double>&)A);
        return;
    }
    if (std::is_same<T,float>::value) {
        pass1_avx512_float((const float*)X, (const float*)W, N, (Accum<float>&)A);
        return;
    }
#elif defined(__AVX2__)
    if (std::is_same<T,double>::value) {
        pass1_avx2_double((const double*)X, (const double*)W, N, (Accum<double>&)A);
        return;
    }
    if (std::is_same<T,float>::value) {
        pass1_avx2_float((const float*)X, (const float*)W, N, (Accum<float>&)A);
        return;
    }
#endif
    pass1_scalar(X, W, N, A);
}

template<typename T>
static inline void pass2_best(const T* X, const T* W, mwSize N, long double Mu, Accum<T>& A)
{
#if defined(__AVX512F__)
    if (std::is_same<T,double>::value) {
        pass2_avx512_double((const double*)X, (const double*)W, N, Mu, (Accum<double>&)A);
        return;
    }
    if (std::is_same<T,float>::value) {
        pass2_avx512_float((const float*)X, (const float*)W, N, Mu, (Accum<float>&)A);
        return;
    }
#elif defined(__AVX2__)
    if (std::is_same<T,double>::value) {
        pass2_avx2_double((const double*)X, (const double*)W, N, Mu, (Accum<double>&)A);
        return;
    }
    if (std::is_same<T,float>::value) {
        pass2_avx2_float((const float*)X, (const float*)W, N, Mu, (Accum<float>&)A);
        return;
    }
#endif
    pass2_scalar(X, W, N, Mu, A);
}

// ============================================================
// Slice compute
// ============================================================

template<typename T>
static inline void wmean_slice_2pass(const T* X, const T* W, mwSize N,
                                     T& OutMean, T& OutStd, T& OutErr)
{
    Accum<T> A;

    pass1_best(X, W, N, A);

    if (!(A.SumW > 0.0L) || !(A.SumW2 > 0.0L)) {
        OutMean = NaN<T>();
        OutStd  = NaN<T>();
        OutErr  = NaN<T>();
        return;
    }

    const long double Mu = A.SumWX / A.SumW;
    OutMean = (T)Mu;

    pass2_best(X, W, N, Mu, A);

    long double Var = A.SumWDX2 / A.SumW;
    if (Var < 0.0L) Var = 0.0L;

    const long double Std  = std::sqrt(Var);
    const long double Neff = (A.SumW * A.SumW) / A.SumW2;

    OutStd = (T)Std;
    OutErr = (Neff > 0.0L) ? (T)(Std / std::sqrt(Neff)) : NaN<T>();
}

// ============================================================
// Drivers
// ============================================================

template<typename T>
static void run_dim1(const mxArray* A, const mxArray* W,
                     mxArray* OMean, mxArray* OStd, mxArray* OErr)
{
    const mwSize M = mxGetM(A);
    const mwSize N = mxGetN(A);

    const T* Ax = (const T*)mxGetData(A);
    const T* Aw = (const T*)mxGetData(W);

    T* Om = (T*)mxGetData(OMean);
    T* Os = (T*)mxGetData(OStd);
    T* Oe = (T*)mxGetData(OErr);

    #if defined(_OPENMP)
    #pragma omp parallel for if(N > 8) schedule(static)
    #endif
    for (mwIndex j = 0; j < N; ++j) {
        T Mean, Std, Err;
        wmean_slice_2pass(Ax + j*M, Aw + j*M, M, Mean, Std, Err);
        Om[j] = Mean;
        Os[j] = Std;
        Oe[j] = Err;
    }
}

template<typename T>
static void run_dim2(const mxArray* A, const mxArray* W,
                     mxArray* OMean, mxArray* OStd, mxArray* OErr)
{
    const mwSize M = mxGetM(A);
    const mwSize N = mxGetN(A);

    const T* Ax = (const T*)mxGetData(A);
    const T* Aw = (const T*)mxGetData(W);

    T* Om = (T*)mxGetData(OMean);
    T* Os = (T*)mxGetData(OStd);
    T* Oe = (T*)mxGetData(OErr);

    #if defined(_OPENMP)
    #pragma omp parallel
    #endif
    {
        T* Xbuf = new(std::nothrow) T[N];
        T* Wbuf = new(std::nothrow) T[N];

        if (Xbuf == nullptr || Wbuf == nullptr) {
            delete[] Xbuf;
            delete[] Wbuf;
            mexErrMsgIdAndTxt("wmean_mex:alloc", "Memory allocation failed.");
        }

        #if defined(_OPENMP)
        #pragma omp for schedule(static)
        #endif
        for (mwIndex i = 0; i < M; ++i) {
            for (mwIndex j = 0; j < N; ++j) {
                Xbuf[j] = Ax[i + j*M];
                Wbuf[j] = Aw[i + j*M];
            }

            T Mean, Std, Err;
            wmean_slice_2pass(Xbuf, Wbuf, N, Mean, Std, Err);
            Om[i] = Mean;
            Os[i] = Std;
            Oe[i] = Err;
        }

        delete[] Xbuf;
        delete[] Wbuf;
    }
}

// ============================================================
// MEX entry
// ============================================================

void mexFunction(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[])
{
    if (nrhs < 2) {
        mexErrMsgIdAndTxt("wmean_mex:args",
            "Usage: [Mean,WStd,WErr] = wmean_mex(Array,Weights,Dim)");
    }
    if (nlhs > 3) {
        mexErrMsgIdAndTxt("wmean_mex:nlhs", "Too many output arguments.");
    }

    const mxArray* A = prhs[0];
    const mxArray* W = prhs[1];

    if ((!mxIsDouble(A) && !mxIsSingle(A)) || mxIsComplex(A)) {
        mexErrMsgIdAndTxt("wmean_mex:type", "Array must be real single or double.");
    }
    if ((!mxIsDouble(W) && !mxIsSingle(W)) || mxIsComplex(W)) {
        mexErrMsgIdAndTxt("wmean_mex:type", "Weights must be real single or double.");
    }
    if (mxGetClassID(A) != mxGetClassID(W)) {
        mexErrMsgIdAndTxt("wmean_mex:type", "Array and Weights must have the same class.");
    }
    if (mxGetNumberOfDimensions(A) != 2 || mxGetNumberOfDimensions(W) != 2) {
        mexErrMsgIdAndTxt("wmean_mex:ndims", "Array and Weights must be 2-D.");
    }
    if (mxGetM(A) != mxGetM(W) || mxGetN(A) != mxGetN(W)) {
        mexErrMsgIdAndTxt("wmean_mex:size", "Array and Weights must have the same size.");
    }

    int Dim = 1;
    if (nrhs >= 3 && !mxIsEmpty(prhs[2])) {
        if (!mxIsDouble(prhs[2]) || mxIsComplex(prhs[2]) || mxGetNumberOfElements(prhs[2]) != 1) {
            mexErrMsgIdAndTxt("wmean_mex:dim", "Dim must be scalar 1 or 2.");
        }
        Dim = (int)mxGetScalar(prhs[2]);
        if (Dim != 1 && Dim != 2) {
            mexErrMsgIdAndTxt("wmean_mex:dim", "Dim must be 1 or 2.");
        }
    }

    const mwSize M = mxGetM(A);
    const mwSize N = mxGetN(A);
    const mxClassID Cls = mxGetClassID(A);

    if (Dim == 1) {
        plhs[0] = mxCreateNumericMatrix(1, N, Cls, mxREAL);
        if (nlhs >= 2) plhs[1] = mxCreateNumericMatrix(1, N, Cls, mxREAL);
        if (nlhs >= 3) plhs[2] = mxCreateNumericMatrix(1, N, Cls, mxREAL);
    } else {
        plhs[0] = mxCreateNumericMatrix(M, 1, Cls, mxREAL);
        if (nlhs >= 2) plhs[1] = mxCreateNumericMatrix(M, 1, Cls, mxREAL);
        if (nlhs >= 3) plhs[2] = mxCreateNumericMatrix(M, 1, Cls, mxREAL);
    }

    mxArray* TmpStd = (nlhs >= 2) ? plhs[1] :
        ((Dim == 1) ? mxCreateNumericMatrix(1, N, Cls, mxREAL)
                    : mxCreateNumericMatrix(M, 1, Cls, mxREAL));

    mxArray* TmpErr = (nlhs >= 3) ? plhs[2] :
        ((Dim == 1) ? mxCreateNumericMatrix(1, N, Cls, mxREAL)
                    : mxCreateNumericMatrix(M, 1, Cls, mxREAL));

    if (Cls == mxDOUBLE_CLASS) {
        if (Dim == 1) run_dim1<double>(A, W, plhs[0], TmpStd, TmpErr);
        else          run_dim2<double>(A, W, plhs[0], TmpStd, TmpErr);
    } else {
        if (Dim == 1) run_dim1<float >(A, W, plhs[0], TmpStd, TmpErr);
        else          run_dim2<float >(A, W, plhs[0], TmpStd, TmpErr);
    }

    if (nlhs < 2) mxDestroyArray(TmpStd);
    if (nlhs < 3) mxDestroyArray(TmpErr);
}
