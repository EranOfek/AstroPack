// sincos_mex.cpp
// [S, C] = sincos_mex(X)
// X: real array (single or double). Outputs match X's class/size.
// AVX2 vector kernels for both float (8-wide) and double (4-wide).
// FIX: double quadrant uses n mod 4 computed in double (no 32-bit overflow).

#include "mex.h"
#include <cmath>
#include <cstdint>
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

static inline bool isRealFloatOrDouble(const mxArray* a){
    return !mxIsComplex(a) && (mxIsSingle(a) || mxIsDouble(a));
}

// Cody–Waite constants (decimal; C++14-safe)
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

// Horner polys on r in ~[-pi/4, pi/4] (double accuracy, reused for float)
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

#if HAS_AVX2
// ---------------- FLOAT: fully vectorized (8 lanes) ----------------
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

// ---------------- DOUBLE: fully vectorized (4 lanes) ----------------
// KEY FIX: compute q = n mod 4 using double math: q = n - 4*floor(n/4).
static inline void fast_sincos4_pd(__m256d x, __m256d& s, __m256d& c){
    const __m256d ax   = _mm256_andnot_pd(_mm256_set1_pd(-0.0), x);
    const __m256d huge = _mm256_cmp_pd(ax, _mm256_set1_pd(D_HUGE_T), _CMP_GT_OQ);

    const __m256d y  = _mm256_mul_pd(x, _mm256_set1_pd(D_INV_PIO2));
    const __m256d nr = _mm256_round_pd(y, _MM_FROUND_TO_NEAREST_INT | _MM_FROUND_NO_EXC);

    // r = x - n*pio2 (n as exact double integer)
    __m256d r = _mm256_sub_pd(x, _mm256_mul_pd(nr, _mm256_set1_pd(D_PIO2_1)));
    r         = _mm256_sub_pd(r, _mm256_mul_pd(nr, _mm256_set1_pd(D_PIO2_2)));
    r         = _mm256_sub_pd(r, _mm256_mul_pd(nr, _mm256_set1_pd(D_PIO2_3)));

    // Polynomials
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

    // q = n mod 4 in double (safe up to 2^52)
    const __m256d n_div4 = _mm256_mul_pd(nr, _mm256_set1_pd(0.25));
    const __m256d n_div4_fl = _mm256_floor_pd(n_div4);
    const __m256d qd = _mm256_sub_pd(nr, _mm256_mul_pd(n_div4_fl, _mm256_set1_pd(4.0))); // 0,1,2,3 as double

    // Build masks from qd
    const __m256d z  = _mm256_setzero_pd();
    const __m256d one= _mm256_set1_pd(1.0);
    const __m256d two= _mm256_set1_pd(2.0);
    const __m256d thr= _mm256_set1_pd(3.0);
    const __m256d m0 = _mm256_cmp_pd(qd, z,   _CMP_EQ_OQ);
    const __m256d m1 = _mm256_cmp_pd(qd, one, _CMP_EQ_OQ);
    const __m256d m2 = _mm256_cmp_pd(qd, two, _CMP_EQ_OQ);
    const __m256d m3 = _mm256_cmp_pd(qd, thr, _CMP_EQ_OQ);

    // Combine per-quadrant results
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

    // Libm fallback for huge |x| lanes only
    if (_mm256_movemask_pd(huge)){
        alignas(32) double xv[4], sv[4], cv[4], hm[4];
        _mm256_store_pd(xv,x); _mm256_store_pd(sv,s); _mm256_store_pd(cv,c); _mm256_store_pd(hm,huge);
        for(int i=0;i<4;++i) if(hm[i]) { sv[i]=std::sin(xv[i]); cv[i]=std::cos(xv[i]); }
        s=_mm256_load_pd(sv); c=_mm256_load_pd(cv);
    }
}
#endif // HAS_AVX2

// ---------------- Top-level kernels ----------------
static void kernel_float(const float* in, float* s, float* c, mwSize N){
#if HAS_AVX2
    mwSize i=0;
    for(; i+8<=N; i+=8){
        __m256 vx=_mm256_loadu_ps(in+i), vs,vc;
        fast_sincos8_ps(vx,vs,vc);
        _mm256_storeu_ps(s+i,vs);
        _mm256_storeu_ps(c+i,vc);
    }
    for(; i<N; ++i){
        float x=in[i];
        if (std::fabs(x)>F_HUGE_T){ s[i]=std::sin(x); c[i]=std::cos(x); continue; }
        long n = lrintf(x*F_INV_PIO2);
        float r=x; r-=n*F_PIO2_1; r-=n*F_PIO2_2; r-=n*F_PIO2_3;
        double sd,cd; poly_sin_cos_double((double)r, sd, cd);
        float ss=(float)sd, cc=(float)cd;
        switch(n&3){ case 0: s[i]= ss; c[i]= cc; break;
                     case 1: s[i]= cc; c[i]=-ss; break;
                     case 2: s[i]=-ss; c[i]=-cc; break;
                     default:s[i]=-cc; c[i]= ss; break; }
    }
#else
    #pragma omp parallel for if (N>20000) schedule(static)
    for(mwSize i=0;i<N;++i){ s[i]=std::sin(in[i]); c[i]=std::cos(in[i]); }
#endif
}

static void kernel_double(const double* in, double* s, double* c, mwSize N){
#if HAS_AVX2
    mwSize i=0;
    for(; i+4<=N; i+=4){
        __m256d vx=_mm256_loadu_pd(in+i), vs,vc;
        fast_sincos4_pd(vx,vs,vc);
        _mm256_storeu_pd(s+i,vs);
        _mm256_storeu_pd(c+i,vc);
    }
    for(; i<N; ++i){
        double x=in[i];
        if (std::fabs(x)>D_HUGE_T){ s[i]=std::sin(x); c[i]=std::cos(x); continue; }
        long long n = llround(x*D_INV_PIO2);
        double r=x; r-=n*D_PIO2_1; r-=n*D_PIO2_2; r-=n*D_PIO2_3;
        double ss,cc; poly_sin_cos_double(r,ss,cc);
        switch((int)(n&3)){ case 0: s[i]= ss; c[i]= cc; break;
                             case 1: s[i]= cc; c[i]=-ss; break;
                             case 2: s[i]=-ss; c[i]=-cc; break;
                             default:s[i]=-cc; c[i]= ss; break; }
    }
#else
    #pragma omp parallel for if (N>20000) schedule(static)
    for(mwSize i=0;i<N;++i){ s[i]=std::sin(in[i]); c[i]=std::cos(in[i]); }
#endif
}

// ---------------- MEX entry ----------------
void mexFunction(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[]){
    if (nrhs!=1) mexErrMsgIdAndTxt("sincos_mex:nrhs","Usage: [S,C] = sincos_mex(X)");
    if (nlhs!=2) mexErrMsgIdAndTxt("sincos_mex:nlhs","This function returns both outputs: [S,C].");
    const mxArray* X=prhs[0];
    if (!isRealFloatOrDouble(X)) mexErrMsgIdAndTxt("sincos_mex:type","X must be real single or double.");

    const mwSize nd=mxGetNumberOfDimensions(X);
    const mwSize* dims=mxGetDimensions(X);
    const mwSize N=mxGetNumberOfElements(X);

    if (mxIsSingle(X)){
        plhs[0]=mxCreateNumericArray(nd,dims,mxSINGLE_CLASS,mxREAL);
        plhs[1]=mxCreateNumericArray(nd,dims,mxSINGLE_CLASS,mxREAL);
        const float* in = reinterpret_cast<const float*>(mxGetData(X));
        float* outS = reinterpret_cast<float*>(mxGetData(plhs[0]));
        float* outC = reinterpret_cast<float*>(mxGetData(plhs[1]));
        kernel_float(in,outS,outC,N);
    } else {
        plhs[0]=mxCreateNumericArray(nd,dims,mxDOUBLE_CLASS,mxREAL);
        plhs[1]=mxCreateNumericArray(nd,dims,mxDOUBLE_CLASS,mxREAL);
        const double* in = mxGetPr(X);
        double* outS = mxGetPr(plhs[0]);
        double* outC = mxGetPr(plhs[1]);
        kernel_double(in,outS,outC,N);
    }
}
