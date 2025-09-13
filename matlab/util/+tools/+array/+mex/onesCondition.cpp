#include "mex.h"
#include <algorithm>
#include <limits>
#include <cctype>
#include <cstring>
#include <cmath>
#if defined(_OPENMP)
  #include <omp.h>
#endif
#if defined(__AVX2__)
  #include <immintrin.h>
#endif

// --------- helpers ---------
static inline void tolower_inplace(char* s){
    for (; *s; ++s) *s = static_cast<char>(std::tolower(*s));
}
static mxClassID parse_class(const mxArray* a){
    if (!mxIsChar(a)) mexErrMsgIdAndTxt("onesCondition:VarClass","VarClass must be a char string.");
    char buf[32]; mxGetString(a, buf, sizeof(buf)); buf[sizeof(buf)-1]=0;
    tolower_inplace(buf);
    if (!std::strcmp(buf,"double")) return mxDOUBLE_CLASS;
    if (!std::strcmp(buf,"single")) return mxSINGLE_CLASS;
    if (!std::strcmp(buf,"uint8"))  return mxUINT8_CLASS;
    if (!std::strcmp(buf,"uint16") || !std::strcmp(buf,"unit16")) return mxUINT16_CLASS; // accept typo
    if (!std::strcmp(buf,"uint32")) return mxUINT32_CLASS;
    if (!std::strcmp(buf,"uint64")) return mxUINT64_CLASS;
    mexErrMsgIdAndTxt("onesCondition:VarClass","Unsupported VarClass '%s'.", buf);
    return mxDOUBLE_CLASS;
}

template<typename T> struct limits { static inline T max(){return std::numeric_limits<T>::max();} };
template<typename T> inline T sat_cast(double v){ return static_cast<T>(v); } // floats

// Unsigned integer specializations (round to nearest; clamp to [0,max], NaN->0)
template<> inline uint8_T  sat_cast<uint8_T >(double v){
    if (!(v==v)) return 0; if (v<=0.0) return 0; if (v>=limits<uint8_T>::max()) return limits<uint8_T>::max();
    return static_cast<uint8_T>(std::llround(v));
}
template<> inline uint16_T sat_cast<uint16_T>(double v){
    if (!(v==v)) return 0; if (v<=0.0) return 0; if (v>=limits<uint16_T>::max()) return limits<uint16_T>::max();
    return static_cast<uint16_T>(std::llround(v));
}
template<> inline uint32_T sat_cast<uint32_T>(double v){
    if (!(v==v)) return 0; if (v<=0.0) return 0; if (v>=static_cast<double>(limits<uint32_T>::max())) return limits<uint32_T>::max();
    return static_cast<uint32_T>(std::llround(v));
}
template<> inline uint64_T sat_cast<uint64_T>(double v){
    if (!(v==v)) return 0; if (v<=0.0) return 0;
    const long double vmax = static_cast<long double>(std::numeric_limits<uint64_T>::max());
    if (static_cast<long double>(v) >= vmax) return std::numeric_limits<uint64_T>::max();
    return static_cast<uint64_T>(std::llround(v));
}

// --------- scalar single-pass fallback (all types) ---------
template<typename OutT, typename AMatT, typename ARadT>
static void kernel_scalar(const AMatT* __restrict A,
                          const ARadT* __restrict R,
                          bool radScalar, double rscalar,
                          OutT* __restrict Out, size_t N,
                          OutT oneC, OutT valC)
{
    const bool use_omp =
    #if defined(_OPENMP)
        (N >= (1u<<18)) && (omp_get_max_threads()>1);
    #else
        false;
    #endif
    if (use_omp){
    #if defined(_OPENMP)
      #pragma omp parallel for schedule(static)
    #endif
      for (mwIndex i=0;i<(mwIndex)N;++i){
          const double ai = static_cast<double>(A[i]);
          const double ri = radScalar ? rscalar : static_cast<double>(R[i]);
          Out[i] = (ai > ri) ? valC : oneC;
      }
    } else {
      for (mwIndex i=0;i<(mwIndex)N;++i){
          const double ai = static_cast<double>(A[i]);
          const double ri = radScalar ? rscalar : static_cast<double>(R[i]);
          Out[i] = (ai > ri) ? valC : oneC;
      }
    }
}

#if defined(__AVX2__)
// --------- AVX2 fast paths (float/float and double/double to same-type output) ---------

// double/double -> double
static void avx2_dd_to_d(const double* __restrict A,
                         const double* __restrict R, bool radScalar, double rscalar,
                         double* __restrict Out, size_t N,
                         double oneC, double valC)
{
    const size_t V = 4;
    size_t i=0;
    const __m256d vOne = _mm256_set1_pd(oneC);
    const __m256d vVal = _mm256_set1_pd(valC);
    const __m256d vR   = radScalar ? _mm256_set1_pd(rscalar) : _mm256_setzero_pd();

    if (radScalar){
        for (; i+V<=N; i+=V){
            __m256d va = _mm256_loadu_pd(A+i);
            __m256d m  = _mm256_cmp_pd(va, vR, _CMP_GT_OQ);
            __m256d vr = _mm256_blendv_pd(vOne, vVal, m);
            _mm256_storeu_pd(Out+i, vr);
        }
    }else{
        for (; i+V<=N; i+=V){
            __m256d va = _mm256_loadu_pd(A+i);
            __m256d vrad = _mm256_loadu_pd(R+i);
            __m256d m  = _mm256_cmp_pd(va, vrad, _CMP_GT_OQ);
            __m256d vr = _mm256_blendv_pd(vOne, vVal, m);
            _mm256_storeu_pd(Out+i, vr);
        }
    }
    for (; i<N; ++i){
        Out[i] = (A[i] > (radScalar ? rscalar : R[i])) ? valC : oneC;
    }
}

// single/single -> single
static void avx2_ss_to_s(const float* __restrict A,
                         const float* __restrict R, bool radScalar, float rscalar,
                         float* __restrict Out, size_t N,
                         float oneC, float valC)
{
    const size_t V = 8;
    size_t i=0;
    const __m256 vOne = _mm256_set1_ps(oneC);
    const __m256 vVal = _mm256_set1_ps(valC);
    const __m256 vR   = radScalar ? _mm256_set1_ps(rscalar) : _mm256_setzero_ps();

    if (radScalar){
        for (; i+V<=N; i+=V){
            __m256 va = _mm256_loadu_ps(A+i);
            __m256 m  = _mm256_cmp_ps(va, vR, _CMP_GT_OQ);
            __m256 vr = _mm256_blendv_ps(vOne, vVal, m);
            _mm256_storeu_ps(Out+i, vr);
        }
    }else{
        for (; i+V<=N; i+=V){
            __m256 va = _mm256_loadu_ps(A+i);
            __m256 vrad = _mm256_loadu_ps(R+i);
            __m256 m  = _mm256_cmp_ps(va, vrad, _CMP_GT_OQ);
            __m256 vr = _mm256_blendv_ps(vOne, vVal, m);
            _mm256_storeu_ps(Out+i, vr);
        }
    }
    for (; i<N; ++i){
        Out[i] = (A[i] > (radScalar ? rscalar : R[i])) ? valC : oneC;
    }
}
#endif // __AVX2__

// --------- dispatch over output type ---------
template<typename OutT>
static void dispatch_out(const mxArray* MatR2, const mxArray* Rad2,
                         mxArray* OutArr, double value_cast)
{
    const size_t N = mxGetNumberOfElements(MatR2);
    OutT* outp = reinterpret_cast<OutT*>(mxGetData(OutArr));
    const OutT oneC = sat_cast<OutT>(1.0);
    const OutT valC = sat_cast<OutT>(value_cast);
    const bool radScalar = (mxGetNumberOfElements(Rad2) == 1);

    if (mxIsDouble(MatR2) && mxIsDouble(Rad2)){
        const double* A = mxGetDoubles(MatR2);
        const double* R = mxGetDoubles(Rad2);
        const double rscalar = radScalar ? R[0] : 0.0;
        #if defined(__AVX2__)
        if (std::is_same<OutT,double>::value){
            avx2_dd_to_d(A, R, radScalar, rscalar, reinterpret_cast<double*>(outp), N,
                         static_cast<double>(oneC), static_cast<double>(valC));
            return;
        }
        #endif
        kernel_scalar<OutT,double,double>(A,R,radScalar,rscalar,outp,N,oneC,valC);
        return;
    }

    if (mxIsSingle(MatR2) && mxIsSingle(Rad2)){
        const float* A = mxGetSingles(MatR2);
        const float* R = mxGetSingles(Rad2);
        const double rscalarD = radScalar ? static_cast<double>(R[0]) : 0.0;
        #if defined(__AVX2__)
        if (std::is_same<OutT,float>::value){
            avx2_ss_to_s(A, R, radScalar, radScalar ? R[0] : 0.0f,
                         reinterpret_cast<float*>(outp), N,
                         static_cast<float>(oneC), static_cast<float>(valC));
            return;
        }
        #endif
        kernel_scalar<OutT,float,float>(A,R,radScalar,rscalarD,outp,N,oneC,valC);
        return;
    }

    // mixed-type fallback (single pass)
    if (mxIsDouble(MatR2)){
        const double* A = mxGetDoubles(MatR2);
        if (mxIsSingle(Rad2)){
            const float* R = mxGetSingles(Rad2);
            const double rscalar = radScalar ? static_cast<double>(R[0]) : 0.0;
            kernel_scalar<OutT,double,float>(A,R,radScalar,rscalar,outp,N,oneC,valC);
        } else {
            const double* R = mxGetDoubles(Rad2);
            const double rscalar = radScalar ? R[0] : 0.0;
            kernel_scalar<OutT,double,double>(A,R,radScalar,rscalar,outp,N,oneC,valC);
        }
        return;
    } else { // MatR2 single
        const float* A = mxGetSingles(MatR2);
        if (mxIsDouble(Rad2)){
            const double* R = mxGetDoubles(Rad2);
            const double rscalar = radScalar ? R[0] : 0.0;
            kernel_scalar<OutT,float,double>(A,R,radScalar,rscalar,outp,N,oneC,valC);
        } else {
            const float* R = mxGetSingles(Rad2);
            const double rscalar = radScalar ? static_cast<double>(R[0]) : 0.0;
            kernel_scalar<OutT,float,float>(A,R,radScalar,rscalar,outp,N,oneC,valC);
        }
        return;
    }
}

// --------- mex entry ---------
void mexFunction(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[])
{
    if (nrhs < 3 || nrhs > 4)
        mexErrMsgIdAndTxt("onesCondition:usage",
            "Usage: W_Max = onesCondition(MatR2, Rad2, VarClass, [Value=0])");

    const mxArray* MatR2 = prhs[0];
    const mxArray* Rad2  = prhs[1];
    const mxArray* VarClass = prhs[2];

    if (mxIsComplex(MatR2) || !(mxIsSingle(MatR2) || mxIsDouble(MatR2)))
        mexErrMsgIdAndTxt("onesCondition:type","MatR2 must be real single or double.");
    if (mxIsComplex(Rad2) || !(mxIsSingle(Rad2) || mxIsDouble(Rad2)))
        mexErrMsgIdAndTxt("onesCondition:type","Rad2 must be real single or double.");

    // size check: Rad2 scalar or identical size
    if (mxGetNumberOfElements(Rad2) != 1){
        if (mxGetNumberOfDimensions(Rad2) != mxGetNumberOfDimensions(MatR2))
            mexErrMsgIdAndTxt("onesCondition:size","Rad2 must be scalar or match MatR2 size.");
        const mwSize* dA = mxGetDimensions(MatR2);
        const mwSize* dR = mxGetDimensions(Rad2);
        for (mwSize k=0;k<mxGetNumberOfDimensions(MatR2);++k)
            if (dA[k]!=dR[k]) mexErrMsgIdAndTxt("onesCondition:size","Rad2 must be scalar or match MatR2 size.");
    }

    const mxClassID outClass = parse_class(VarClass);
    const mwSize nd = mxGetNumberOfDimensions(MatR2);
    const mwSize* dims = mxGetDimensions(MatR2);
    plhs[0] = mxCreateNumericArray(nd, dims, outClass, mxREAL);

    double ValueD = 0.0;
    if (nrhs >= 4){
        if (!mxIsDouble(prhs[3]) && !mxIsSingle(prhs[3]) &&
            !(mxIsUint8(prhs[3])||mxIsUint16(prhs[3])||mxIsUint32(prhs[3])||mxIsUint64(prhs[3])))
            mexErrMsgIdAndTxt("onesCondition:Value","Value must be a real scalar (single/double/uint*).");
        if (mxGetNumberOfElements(prhs[3])!=1)
            mexErrMsgIdAndTxt("onesCondition:Value","Value must be a scalar.");
        ValueD = mxGetScalar(prhs[3]);
    }

    switch (outClass){
        case mxDOUBLE_CLASS: dispatch_out<double>(MatR2, Rad2, plhs[0], ValueD); break;
        case mxSINGLE_CLASS: dispatch_out<float >(MatR2, Rad2, plhs[0], ValueD); break;
        case mxUINT8_CLASS:  dispatch_out<uint8_T >(MatR2, Rad2, plhs[0], ValueD); break;
        case mxUINT16_CLASS: dispatch_out<uint16_T>(MatR2, Rad2, plhs[0], ValueD); break;
        case mxUINT32_CLASS: dispatch_out<uint32_T>(MatR2, Rad2, plhs[0], ValueD); break;
        case mxUINT64_CLASS: dispatch_out<uint64_T>(MatR2, Rad2, plhs[0], ValueD); break;
        default: mexErrMsgIdAndTxt("onesCondition:VarClass","Unsupported output class.");
    }
}
