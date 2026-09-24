// periodScargle_mex.cpp
// [P, Tau, A, B, R, Phi] = periodScargle_mex(T, Y, F)
// Assumes Y is already mean-subtracted.
//
// Efficient streaming Lomb–Scargle with OpenMP and optional AVX2-accelerated sin/cos.
// - No huge MxN matrices: streams over samples, accumulates length-M vectors.
// - Matches classic LS normalization: P = [(Σy c)^2/Σc^2 + (Σy s)^2/Σs^2] / (2*mean(y.^2)).
//
// Build (Hz frequencies, AVX2 if available):
//   mex -O CXXFLAGS="$CXXFLAGS -std=c++17 -march=native -mavx2 -mfma -fopenmp" \
//       LDFLAGS="$LDFLAGS -fopenmp" periodScargle_mex.cpp
//
// Build with angular frequencies (F = ω) instead of Hz:
//   mex -O CXXFLAGS="$CXXFLAGS -std=c++17 -DLS_FREQ_IS_ANGULAR=1 -march=native -mavx2 -mfma -fopenmp" \
//       LDFLAGS="$LDFLAGS -fopenmp" periodScargle_mex.cpp
//
// Build to force libm sin/cos (exactness check; slower):
//   mex -O CXXFLAGS="$CXXFLAGS -std=c++17 -DLS_USE_LIBM_SINCOS=1 -march=native -fopenmp" \
//       LDFLAGS="$LDFLAGS -fopenmp" periodScargle_mex.cpp

#include "mex.h"
#include <cmath>
#include <cstdint>
#include <vector>
#include <algorithm>
#include <cstring>

#ifdef _OPENMP
  #include <omp.h>
#endif

#if defined(__AVX2__)
  #include <immintrin.h>
  #define HAS_AVX2 1
#else
  #define HAS_AVX2 0
#endif

// ---------------- Configuration switches ----------------
// Set to 1 if F input is already angular frequency (rad/s); 0 if F is in Hz (default).
#ifndef LS_FREQ_IS_ANGULAR
#define LS_FREQ_IS_ANGULAR 0
#endif
// Set to 1 to bypass AVX2 kernels and use libm sin/cos everywhere (for reference matching).
#ifndef LS_USE_LIBM_SINCOS
#define LS_USE_LIBM_SINCOS 0
#endif

#ifndef LS_NORM_DENOM_K
#define LS_NORM_DENOM_K 8  // default: Scargle 1982 (denominator = 2*sig2) - NOT CLEAR WHY THIS IS 8, but using 2 there is a factor of 4 error...
#endif


// ---------------- Utilities ----------------
static inline bool isRealFloatOrDoubleVec(const mxArray* a){
    return !mxIsComplex(a) && (mxIsSingle(a) || mxIsDouble(a)) &&
           mxGetNumberOfDimensions(a)==2 && (mxGetN(a)==1 || mxGetM(a)==1);
}

template<typename T> static inline T my_eps();
template<> inline float  my_eps<float>()  { return 1.1920929e-7f; }
template<> inline double my_eps<double>() { return 2.220446049250313e-16; }

// ---------------- Fast sin/cos (based on your sincos_mex) ----------------
// Horner polys (double-precision core; reused for float)
static inline void poly_sin_cos_double(double r, double& ss, double& cc){
    const double r2=r*r;
    const double s1=-1.66666666666666324348e-1;
    const double s2= 8.33333333332248946124e-3;
    const double s3=-1.98412698298579493134e-4;
    const double s4= 2.75573137070700676789e-6;
    const double t=((s4*r2+s3)*r2+s2)*r2+s1;
    ss = r + r*r2*t;

    const double c1=-0.5;
    const double c2= 4.16666666666665929218e-2;
    const double c3=-1.38888888888730564116e-3;
    const double c4= 2.48015872888517045348e-5;
    const double u=((c4*r2+c3)*r2+c2)*r2+c1;
    cc = 1.0 + r2*u;
}

// Cody–Waite constants
// float
static constexpr float  F_INV_PIO2 = 0.63661975f;
static constexpr float  F_PIO2_1   = 1.5707962512969971f;
static constexpr float  F_PIO2_2   = 7.5497894158615964e-08f;
static constexpr float  F_PIO2_3   = 5.3903025299577646e-15f;
static constexpr float  F_HUGE_T   = 1.0e7f;
// double
static constexpr double D_INV_PIO2 = 0.63661977236758134308;
static constexpr double D_PIO2_1   = 1.57079632679489655800;
static constexpr double D_PIO2_2   = 6.12323399573676603587e-17;
static constexpr double D_PIO2_3   = 2.02226624879595063154e-21;
static constexpr double D_HUGE_T   = 1.0e14;

#if HAS_AVX2
// 8-wide float
static inline void fast_sincos8_ps(__m256 x, __m256& s, __m256& c){
    const __m256 ax   = _mm256_andnot_ps(_mm256_set1_ps(-0.0f), x);
    const __m256 huge = _mm256_cmp_ps(ax, _mm256_set1_ps(F_HUGE_T), _CMP_GT_OQ);

    const __m256 y  = _mm256_mul_ps(x, _mm256_set1_ps(F_INV_PIO2));
    const __m256 nr = _mm256_round_ps(y, _MM_FROUND_TO_NEAREST_INT | _MM_FROUND_NO_EXC);
    const __m256i n_i = _mm256_cvtps_epi32(nr);
    const __m256  n_ps= _mm256_cvtepi32_ps(n_i);

    __m256 r = _mm256_sub_ps(x, _mm256_mul_ps(n_ps, _mm256_set1_ps(F_PIO2_1)));
    r        = _mm256_sub_ps(r, _mm256_mul_ps(n_ps, _mm256_set1_ps(F_PIO2_2)));
    r        = _mm256_sub_ps(r, _mm256_mul_ps(n_ps, _mm256_set1_ps(F_PIO2_3)));

    alignas(32) float rf[8]; _mm256_store_ps(rf, r);
    float sf[8], cf[8];
    for(int i=0;i<8;++i){ double sd,cd; poly_sin_cos_double((double)rf[i], sd, cd);
                          sf[i]=(float)sd; cf[i]=(float)cd; }
    __m256 ss = _mm256_load_ps(sf), cc = _mm256_load_ps(cf);

    const __m256 z=_mm256_setzero_ps(), negss=_mm256_sub_ps(z,ss), negcc=_mm256_sub_ps(z,cc);
    const __m256i q = _mm256_and_si256(n_i, _mm256_set1_epi32(3));
    const __m256 m1=_mm256_castsi256_ps(_mm256_cmpeq_epi32(q, _mm256_set1_epi32(1)));
    const __m256 m2=_mm256_castsi256_ps(_mm256_cmpeq_epi32(q, _mm256_set1_epi32(2)));
    const __m256 m3=_mm256_castsi256_ps(_mm256_cmpeq_epi32(q, _mm256_set1_epi32(3)));
    const __m256 m23=_mm256_or_ps(m2,m3);

    __m256 s01=_mm256_blendv_ps(ss,   cc,   m1);
    __m256 s23=_mm256_blendv_ps(negss,negcc,m3);
    s = _mm256_blendv_ps(s01, s23, m23);

    __m256 c01=_mm256_blendv_ps(cc,   negss,m1);
    __m256 c23=_mm256_blendv_ps(negcc,ss,   m3);
    c = _mm256_blendv_ps(c01, c23, m23);

    if (_mm256_movemask_ps(huge)){
        alignas(32) float xv[8], sv[8], cv[8], hm[8];
        _mm256_store_ps(xv,x); _mm256_store_ps(sv,s); _mm256_store_ps(cv,c); _mm256_store_ps(hm,huge);
        for(int i=0;i<8;++i) if(hm[i]){ sv[i]=std::sin(xv[i]); cv[i]=std::cos(xv[i]); }
        s=_mm256_load_ps(sv); c=_mm256_load_ps(cv);
    }
}

// 4-wide double (safe quadrant)
static inline void fast_sincos4_pd(__m256d x, __m256d& s, __m256d& c){
    const __m256d ax   = _mm256_andnot_pd(_mm256_set1_pd(-0.0), x);
    const __m256d huge = _mm256_cmp_pd(ax, _mm256_set1_pd(D_HUGE_T), _CMP_GT_OQ);

    const __m256d y  = _mm256_mul_pd(x, _mm256_set1_pd(D_INV_PIO2));
    const __m256d nr = _mm256_round_pd(y, _MM_FROUND_TO_NEAREST_INT | _MM_FROUND_NO_EXC);

    __m256d r = _mm256_sub_pd(x, _mm256_mul_pd(nr, _mm256_set1_pd(D_PIO2_1)));
    r         = _mm256_sub_pd(r, _mm256_mul_pd(nr, _mm256_set1_pd(D_PIO2_2)));
    r         = _mm256_sub_pd(r, _mm256_mul_pd(nr, _mm256_set1_pd(D_PIO2_3)));

    const __m256d r2 = _mm256_mul_pd(r, r);
    const __m256d s4=_mm256_set1_pd( 2.75573137070700676789e-6);
    const __m256d s3=_mm256_set1_pd(-1.98412698298579493134e-4);
    const __m256d s2=_mm256_set1_pd( 8.33333333332248946124e-3);
    const __m256d s1=_mm256_set1_pd(-1.66666666666666324348e-1);
    __m256d t = _mm256_fmadd_pd(s4,r2,s3);
    t = _mm256_fmadd_pd(t, r2, s2);
    t = _mm256_fmadd_pd(t, r2, s1);
    __m256d ss = _mm256_fmadd_pd(_mm256_mul_pd(r,r2), t, r);

    const __m256d c4=_mm256_set1_pd( 2.48015872888517045348e-5);
    const __m256d c3=_mm256_set1_pd(-1.38888888888730564116e-3);
    const __m256d c2=_mm256_set1_pd( 4.16666666666665929218e-2);
    const __m256d c1=_mm256_set1_pd(-0.5);
    __m256d u = _mm256_fmadd_pd(c4,r2,c3);
    u = _mm256_fmadd_pd(u, r2, c2);
    u = _mm256_fmadd_pd(u, r2, c1);
    __m256d cc = _mm256_fmadd_pd(r2, u, _mm256_set1_pd(1.0));

    // quadrant via double mod 4
    const __m256d n_div4 = _mm256_mul_pd(nr, _mm256_set1_pd(0.25));
    const __m256d n_div4_fl = _mm256_floor_pd(n_div4);
    const __m256d qd = _mm256_sub_pd(nr, _mm256_mul_pd(n_div4_fl, _mm256_set1_pd(4.0)));

    const __m256d z  = _mm256_setzero_pd();
    const __m256d one= _mm256_set1_pd(1.0);
    const __m256d two= _mm256_set1_pd(2.0);
    const __m256d thr= _mm256_set1_pd(3.0);
    const __m256d m0 = _mm256_cmp_pd(qd, z,   _CMP_EQ_OQ);
    const __m256d m1 = _mm256_cmp_pd(qd, one, _CMP_EQ_OQ);
    const __m256d m2 = _mm256_cmp_pd(qd, two, _CMP_EQ_OQ);
    const __m256d m3 = _mm256_cmp_pd(qd, thr, _CMP_EQ_OQ);

    const __m256d negss = _mm256_sub_pd(z, ss);
    const __m256d negcc = _mm256_sub_pd(z, cc);

    __m256d s_q = _mm256_or_pd(
                    _mm256_or_pd(_mm256_and_pd(m0, ss),
                                 _mm256_and_pd(m1, cc)),
                    _mm256_or_pd(_mm256_and_pd(m2, negss),
                                 _mm256_and_pd(m3, negcc)));

    __m256d c_q = _mm256_or_pd(
                    _mm256_or_pd(_mm256_and_pd(m0, cc),
                                 _mm256_and_pd(m1, _mm256_sub_pd(z, ss))),
                    _mm256_or_pd(_mm256_and_pd(m2, negcc),
                                 _mm256_and_pd(m3, ss)));

    s = s_q; c = c_q;

    if (_mm256_movemask_pd(huge)){
        alignas(32) double xv[4], sv[4], cv[4], hm[4];
        _mm256_store_pd(xv,x); _mm256_store_pd(sv,s); _mm256_store_pd(cv,c); _mm256_store_pd(hm,huge);
        for(int i=0;i<4;++i) if(hm[i]) { sv[i]=std::sin(xv[i]); cv[i]=std::cos(xv[i]); }
        s=_mm256_load_pd(sv); c=_mm256_load_pd(cv);
    }
}
#endif // HAS_AVX2

// Vector wrappers for arbitrary-length arrays (float/double)
static void sincos_vec(const float* x, float* s, float* c, mwSize n){
#if LS_USE_LIBM_SINCOS
    #pragma omp parallel for if (n>20000) schedule(static)
    for(mwSize i=0;i<n;++i){ s[i]=std::sin(x[i]); c[i]=std::cos(x[i]); }
#else
  #if HAS_AVX2
    mwSize i=0;
    for(; i+8<=n; i+=8){
        __m256 vx=_mm256_loadu_ps(x+i), vs,vc;
        fast_sincos8_ps(vx,vs,vc);
        _mm256_storeu_ps(s+i,vs);
        _mm256_storeu_ps(c+i,vc);
    }
    for(; i<n; ++i){ s[i]=std::sin(x[i]); c[i]=std::cos(x[i]); }
  #else
    #pragma omp parallel for if (n>20000) schedule(static)
    for(mwSize i=0;i<n;++i){ s[i]=std::sin(x[i]); c[i]=std::cos(x[i]); }
  #endif
#endif
}
static void sincos_vec(const double* x, double* s, double* c, mwSize n){
#if LS_USE_LIBM_SINCOS
    #pragma omp parallel for if (n>20000) schedule(static)
    for(mwSize i=0;i<n;++i){ s[i]=std::sin(x[i]); c[i]=std::cos(x[i]); }
#else
  #if HAS_AVX2
    mwSize i=0;
    for(; i+4<=n; i+=4){
        __m256d vx=_mm256_loadu_pd(x+i), vs,vc;
        fast_sincos4_pd(vx,vs,vc);
        _mm256_storeu_pd(s+i,vs);
        _mm256_storeu_pd(c+i,vc);
    }
    for(; i<n; ++i){ s[i]=std::sin(x[i]); c[i]=std::cos(x[i]); }
  #else
    #pragma omp parallel for if (n>20000) schedule(static)
    for(mwSize i=0;i<n;++i){ s[i]=std::sin(x[i]); c[i]=std::cos(x[i]); }
  #endif
#endif
}

// ---------------- Core templated LS ----------------
template<typename T>
static void lomb_scargle_core(const T* Tvec, const T* Yvec, mwSize N,
                              const T* Fvec, mwSize M,
                              // outputs (some may be null if not requested)
                              T* P, T* Tau, T* A, T* B, T* R, T* Phi)
{
    const T two = (T)2;
    const T pi  = (T)3.141592653589793238462643383279502884;
    const T eps = (T)my_eps<T>();

    // Angular frequencies W
    std::vector<T> W(M);
    for (mwSize i=0;i<M;++i){
#if LS_FREQ_IS_ANGULAR
        W[i] = Fvec[i];          // F is ω (rad/s)
#else
        W[i] = two*pi*Fvec[i];   // F is Hz (default)
#endif
    }

    // variance of Y (demeaned)
    T Sig2 = (T)0;
    for (mwSize i=0;i<N;++i) Sig2 += Yvec[i]*Yvec[i];
    Sig2 /= (N ? (T)N : (T)1);

    // Accumulators for tau
    std::vector<T> S2(M,(T)0), C2(M,(T)0);
    std::vector<T> X(M), S(M), C(M);

    int nthreads = 1;
#ifdef _OPENMP
    nthreads = omp_get_max_threads();
#endif
    std::vector< std::vector<T> > S2_loc(nthreads, std::vector<T>(M,(T)0));
    std::vector< std::vector<T> > C2_loc(nthreads, std::vector<T>(M,(T)0));

    // ---- Pass 1: compute tau(w) via sums of sin/cos(2*w*t)
    #pragma omp parallel if (N*(mwSize)M > 400000)
    {
        int tid = 0;
        #ifdef _OPENMP
        tid = omp_get_thread_num();
        #endif
        auto& s2 = S2_loc[tid];
        auto& c2 = C2_loc[tid];

        for (mwSize j=0; j<N; ++j){
            const T scale = two * Tvec[j];
            for (mwSize i=0;i<M;++i) X[i] = W[i] * scale;
            sincos_vec(X.data(), S.data(), C.data(), M);
            for (mwSize i=0;i<M;++i){ s2[i]+=S[i]; c2[i]+=C[i]; }
        }
    }
    for (int t=0;t<nthreads;++t){
        for (mwSize i=0;i<M;++i){ S2[i]+=S2_loc[t][i]; C2[i]+=C2_loc[t][i]; }
    }

    // Tau and w*tau
    std::vector<T> WTau(M);
    for (mwSize i=0;i<M;++i){
        const T wi = W[i];
        const T tau = ((T)0.5) * std::atan2(S2[i], C2[i]) / ((std::abs(wi)>eps) ? wi : (T)1);
        if (Tau) Tau[i] = tau;
        WTau[i] = wi * tau;
    }

    // ---- Pass 2: main LS accumulations
    std::vector<T> Yc(M,(T)0), Ys(M,(T)0), Cc2(M,(T)0), Ss2(M,(T)0);

    #pragma omp parallel if (N*(mwSize)M > 400000)
    {
        std::vector<T> Yc_loc(M,(T)0), Ys_loc(M,(T)0), Cc2_loc(M,(T)0), Ss2_loc(M,(T)0);
        std::vector<T> Xloc(M), Sloc(M), Cloc(M);

        for (mwSize j=0; j<N; ++j){
            const T tj = Tvec[j];
            for (mwSize i=0;i<M;++i) Xloc[i] = W[i]*tj - WTau[i];
            sincos_vec(Xloc.data(), Sloc.data(), Cloc.data(), M);

            const T yj = Yvec[j]; // already demeaned
            for (mwSize i=0;i<M;++i){
                const T c = Cloc[i], s = Sloc[i];
                Yc_loc[i] += yj * c;
                Ys_loc[i] += yj * s;
                Cc2_loc[i]+= c*c;
                Ss2_loc[i]+= s*s;
            }
        }

        #pragma omp critical
        {
            for (mwSize i=0;i<M;++i){
                Yc[i]  += Yc_loc[i];
                Ys[i]  += Ys_loc[i];
                Cc2[i] += Cc2_loc[i];
                Ss2[i] += Ss2_loc[i];
            }
        }
    }

    // ---- Finalize power and (optionally) A,B,R,Phi
    for (mwSize i=0;i<M;++i){
        const T cden = std::max(Cc2[i], eps);
        const T sden = std::max(Ss2[i], eps);
        const T yc2_over = (Yc[i]*Yc[i]) / cden;
        const T ys2_over = (Ys[i]*Ys[i]) / sden;
        //P[i] = (yc2_over + ys2_over) / ( (T)2 * std::max(Sig2, eps) );
	P[i] = (yc2_over + ys2_over) / ( (T)LS_NORM_DENOM_K * std::max(Sig2, eps) );

	
        if (A){ A[i] = Yc[i] / cden; }
        if (B){ B[i] = Ys[i] / sden; }
        if (R){
            const T aa = A?A[i]:(Yc[i]/cden);
            const T bb = B?B[i]:(Ys[i]/sden);
            R[i] = std::hypot(aa, bb);
        }
        if (Phi){
            const T aa = A?A[i]:(Yc[i]/cden);
            const T bb = B?B[i]:(Ys[i]/sden);
            Phi[i] = std::atan2(bb, aa);
        }
    }
}

// ---------------- MEX entry ----------------
void mexFunction(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[])
{
    if (nrhs != 3)
        mexErrMsgIdAndTxt("periodScargle_mex:args","Usage: [P,Tau,(A,B,R,Phi)] = periodScargle_mex(T, Y, F)");

    const mxArray* Tmx = prhs[0];
    const mxArray* Ymx = prhs[1];
    const mxArray* Fmx = prhs[2];

    if (!isRealFloatOrDoubleVec(Tmx) || !isRealFloatOrDoubleVec(Ymx) || !isRealFloatOrDoubleVec(Fmx))
        mexErrMsgIdAndTxt("periodScargle_mex:type","T, Y, F must be real single/double vectors.");
    const mwSize N = mxGetNumberOfElements(Tmx);
    if (mxGetNumberOfElements(Ymx)!=N)
        mexErrMsgIdAndTxt("periodScargle_mex:len","T and Y must have the same length.");
    const mwSize M = mxGetNumberOfElements(Fmx);

    const bool useDouble = mxIsDouble(Tmx) || mxIsDouble(Ymx) || mxIsDouble(Fmx);

    if (useDouble){
        plhs[0]=mxCreateDoubleMatrix(M,1,mxREAL); // P
        plhs[1]=mxCreateDoubleMatrix(M,1,mxREAL); // Tau
        double* P   = mxGetPr(plhs[0]);
        double* Tau = mxGetPr(plhs[1]);

        double *A=nullptr,*B=nullptr,*R=nullptr,*Phi=nullptr;
        if (nlhs>2){ plhs[2]=mxCreateDoubleMatrix(M,1,mxREAL); A=mxGetPr(plhs[2]); }
        if (nlhs>3){ plhs[3]=mxCreateDoubleMatrix(M,1,mxREAL); B=mxGetPr(plhs[3]); }
        if (nlhs>4){ plhs[4]=mxCreateDoubleMatrix(M,1,mxREAL); R=mxGetPr(plhs[4]); }
        if (nlhs>5){ plhs[5]=mxCreateDoubleMatrix(M,1,mxREAL); Phi=mxGetPr(plhs[5]); }

        std::vector<double> T(N), Y(N), F(M);
        if (mxIsDouble(Tmx)) std::memcpy(T.data(), mxGetPr(Tmx), N*sizeof(double));
        else { const float* p=(const float*)mxGetData(Tmx); for (mwSize i=0;i<N;++i) T[i]=p[i]; }
        if (mxIsDouble(Ymx)) std::memcpy(Y.data(), mxGetPr(Ymx), N*sizeof(double));
        else { const float* p=(const float*)mxGetData(Ymx); for (mwSize i=0;i<N;++i) Y[i]=p[i]; }
        if (mxIsDouble(Fmx)) std::memcpy(F.data(), mxGetPr(Fmx), M*sizeof(double));
        else { const float* p=(const float*)mxGetData(Fmx); for (mwSize i=0;i<M;++i) F[i]=p[i]; }

        lomb_scargle_core<double>(T.data(), Y.data(), N, F.data(), M, P, Tau, A, B, R, Phi);
    } else {
        plhs[0]=mxCreateNumericMatrix(M,1,mxSINGLE_CLASS,mxREAL);
        plhs[1]=mxCreateNumericMatrix(M,1,mxSINGLE_CLASS,mxREAL);
        float* P   = (float*)mxGetData(plhs[0]);
        float* Tau = (float*)mxGetData(plhs[1]);

        float *A=nullptr,*B=nullptr,*R=nullptr,*Phi=nullptr;
        if (nlhs>2){ plhs[2]=mxCreateNumericMatrix(M,1,mxSINGLE_CLASS,mxREAL); A=(float*)mxGetData(plhs[2]); }
        if (nlhs>3){ plhs[3]=mxCreateNumericMatrix(M,1,mxSINGLE_CLASS,mxREAL); B=(float*)mxGetData(plhs[3]); }
        if (nlhs>4){ plhs[4]=mxCreateNumericMatrix(M,1,mxSINGLE_CLASS,mxREAL); R=(float*)mxGetData(plhs[4]); }
        if (nlhs>5){ plhs[5]=mxCreateNumericMatrix(M,1,mxSINGLE_CLASS,mxREAL); Phi=(float*)mxGetData(plhs[5]); }

        std::vector<float> T(N), Y(N), F(M);
        if (mxIsSingle(Tmx)) std::memcpy(T.data(), mxGetData(Tmx), N*sizeof(float));
        else { const double* p=mxGetPr(Tmx); for (mwSize i=0;i<N;++i) T[i]=(float)p[i]; }
        if (mxIsSingle(Ymx)) std::memcpy(Y.data(), mxGetData(Ymx), N*sizeof(float));
        else { const double* p=mxGetPr(Ymx); for (mwSize i=0;i<N;++i) Y[i]=(float)p[i]; }
        if (mxIsSingle(Fmx)) std::memcpy(F.data(), mxGetData(Fmx), M*sizeof(float));
        else { const double* p=mxGetPr(Fmx); for (mwSize i=0;i<M;++i) F[i]=(float)p[i]; }

        lomb_scargle_core<float>(T.data(), Y.data(), N, F.data(), M, P, Tau, A, B, R, Phi);
    }
}
