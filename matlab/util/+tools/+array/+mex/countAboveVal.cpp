#include "mex.h"
#include <cstdint>
#include <cstring>
#include <cmath>
#include <limits>
#ifdef _OPENMP
  #include <omp.h>
#endif
#ifdef _MSC_VER
  #include <intrin.h>
#else
  #include <immintrin.h>
#endif

static inline double get_scalar_as_double(const mxArray* A){
    if (mxGetNumberOfElements(A)!=1 || mxIsComplex(A)) mexErrMsgIdAndTxt("countAboveVal:Val","Val must be real scalar.");
    const void* p=mxGetData(A);
    switch(mxGetClassID(A)){
        case mxDOUBLE_CLASS: return *(const double*)p;
        case mxSINGLE_CLASS: return (double)*(const float*)p;
        case mxINT8_CLASS:   return (double)*(const int8_T*)p;
        case mxUINT8_CLASS:  return (double)*(const uint8_T*)p;
        case mxINT16_CLASS:  return (double)*(const int16_T*)p;
        case mxUINT16_CLASS: return (double)*(const uint16_T*)p;
        case mxINT32_CLASS:  return (double)*(const int32_T*)p;
        case mxUINT32_CLASS: return (double)*(const uint32_T*)p;
#if defined(mxINT64_CLASS)
        case mxINT64_CLASS:  return (double)*(const int64_T*)p;
        case mxUINT64_CLASS: return (double)*(const uint64_T*)p;
#endif
        case mxLOGICAL_CLASS:return (double)*(const mxLogical*)p;
        default: mexErrMsgIdAndTxt("countAboveVal:ValType","Unsupported Val class."); return 0.0;
    }
}

template <typename IT>
static inline uint64_t count_int_gt_scalar(const IT* a, size_t n, double v){
    using lim=std::numeric_limits<IT>;
    if (std::isnan(v)) return 0;
    if (std::isinf(v) && v>0) return 0;
    if (std::isinf(v) && v<0) return (uint64_t)n;
    long double vf=floor((long double)v);
    if (vf < (long double)lim::min()) return (uint64_t)n;
    if (vf >=(long double)lim::max()) return 0;
    IT thr=(IT)vf;
    uint64_t cnt=0;
    #pragma omp parallel for reduction(+:cnt) if(n>(1u<<16)) schedule(static)
    for (mwIndex i=0;i<(mwIndex)n;++i) cnt += (a[i]>thr);
    return cnt;
}

// AVX2 double
static inline uint64_t count_fp_gt_avx_d(const double* a, size_t n, double v){
#if defined(__AVX2__)
    const size_t V=4; // 4 doubles
    const size_t N=(n/V)*V; // largest multiple of V
    __m256d vv=_mm256_set1_pd(v);
    uint64_t cnt=0;
    #pragma omp parallel
    {
        uint64_t local=0;
        #pragma omp for schedule(static) nowait
        for (size_t i=0;i<N;i+=V){
            __m256d x=_mm256_loadu_pd(a+i);
            __m256d m=_mm256_cmp_pd(x,vv,_CMP_GT_OQ);
            int mask=_mm256_movemask_pd(m);
#if defined(_MSC_VER)
            local += __popcnt(mask);
#else
            local += (uint32_t)__builtin_popcount((unsigned)mask);
#endif
        }
        #pragma omp atomic
        cnt += local;
    }
    for (size_t i=N;i<n;++i) cnt += (a[i]>v);
    return cnt;
#else
    uint64_t cnt=0;
    #pragma omp parallel for reduction(+:cnt) if(n>(1u<<16)) schedule(static)
    for (mwIndex i=0;i<(mwIndex)n;++i) cnt += (a[i]>v);
    return cnt;
#endif
}

// AVX2 float
static inline uint64_t count_fp_gt_avx_f(const float* a, size_t n, float v){
#if defined(__AVX2__)
    const size_t V=8; // 8 floats
    const size_t N=(n/V)*V;
    __m256 vv=_mm256_set1_ps(v);
    uint64_t cnt=0;
    #pragma omp parallel
    {
        uint64_t local=0;
        #pragma omp for schedule(static) nowait
        for (size_t i=0;i<N;i+=V){
            __m256 x=_mm256_loadu_ps(a+i);
            __m256 m=_mm256_cmp_ps(x,vv,_CMP_GT_OQ);
            int mask=_mm256_movemask_ps(m);
#if defined(_MSC_VER)
            local += __popcnt(mask);
#else
            local += (uint32_t)__builtin_popcount((unsigned)mask);
#endif
        }
        #pragma omp atomic
        cnt += local;
    }
    for (size_t i=N;i<n;++i) cnt += (a[i]>v);
    return cnt;
#else
    uint64_t cnt=0;
    #pragma omp parallel for reduction(+:cnt) if(n>(1u<<16)) schedule(static)
    for (mwIndex i=0;i<(mwIndex)n;++i) cnt += (a[i]>v);
    return cnt;
#endif
}

void mexFunction(int nlhs,mxArray* plhs[],int nrhs,const mxArray* prhs[]){
    if (nrhs!=2) mexErrMsgIdAndTxt("countAboveVal:Args","Usage: n = countAboveVal(Array, Val).");
    const mxArray* A=prhs[0];
    const mxArray* V=prhs[1];
    if (mxIsComplex(A)) mexErrMsgIdAndTxt("countAboveVal:Complex","Array must be real.");
    const size_t n=(size_t)mxGetNumberOfElements(A);
    if (n==0){ plhs[0]=mxCreateDoubleScalar(0.0); return; }
    const double v=get_scalar_as_double(V);

    uint64_t cnt=0;
    const void* p=mxGetData(A);
    switch(mxGetClassID(A)){
        case mxDOUBLE_CLASS:  cnt = count_fp_gt_avx_d((const double*)p, n, (double)v); break;
        case mxSINGLE_CLASS:  cnt = count_fp_gt_avx_f((const float*)p,  n, (float)v);  break;
        case mxLOGICAL_CLASS: cnt = count_int_gt_scalar<mxLogical>((const mxLogical*)p, n, v); break;
        case mxINT8_CLASS:    cnt = count_int_gt_scalar<int8_T>((const int8_T*)p, n, v); break;
        case mxUINT8_CLASS:   cnt = count_int_gt_scalar<uint8_T>((const uint8_T*)p, n, v); break;
        case mxINT16_CLASS:   cnt = count_int_gt_scalar<int16_T>((const int16_T*)p, n, v); break;
        case mxUINT16_CLASS:  cnt = count_int_gt_scalar<uint16_T>((const uint16_T*)p, n, v); break;
        case mxINT32_CLASS:   cnt = count_int_gt_scalar<int32_T>((const int32_T*)p, n, v); break;
        case mxUINT32_CLASS:  cnt = count_int_gt_scalar<uint32_T>((const uint32_T*)p, n, v); break;
#if defined(mxINT64_CLASS)
        case mxINT64_CLASS:   cnt = count_int_gt_scalar<int64_T>((const int64_T*)p, n, v); break;
        case mxUINT64_CLASS:  cnt = count_int_gt_scalar<uint64_T>((const uint64_T*)p, n, v); break;
#endif
        default: mexErrMsgIdAndTxt("countAboveVal:Type","Unsupported Array class.");
    }
    plhs[0]=mxCreateDoubleScalar((double)cnt);
}
