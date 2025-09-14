#include "mex.h"
#include <cstddef>
#include <cstring>
#include <vector>
#include <cmath>
#include <limits>
#include <type_traits>

#if defined(_OPENMP)
  #include <omp.h>
#endif

// -------- utils --------
static inline void require_scalar_real(const mxArray* a, const char* id){
    if (mxIsComplex(a) || mxGetNumberOfElements(a)!=1)
        mexErrMsgIdAndTxt(id, "Argument must be a real scalar.");
}
template<typename T> static inline bool isnan_t(T){ return false; }
template<> inline bool isnan_t<double>(double v){ return v!=v; }
template<> inline bool isnan_t<float >(float  v){ return v!=v; }

// Create [M x 1] output like A (same class/complexity)
static mxArray* create_out_col_like(const mxArray* A, size_t M){
    const mxClassID cls = mxGetClassID(A);
    const bool iscx = mxIsComplex(A);
#if defined(mxCreateUninitNumericArray)
    std::vector<size_t> sd(2); sd[0]=M; sd[1]=1;
    return mxCreateUninitNumericArray(2, sd.data(), cls, iscx?mxCOMPLEX:mxREAL);
#else
    mwSize od[2]; od[0]=(mwSize)M; od[1]=1;
    return mxCreateNumericArray(2, od, cls, iscx?mxCOMPLEX:mxREAL);
#endif
}
static mxArray* create_empty_scalar_like(const mxArray* A){
    const mxClassID cls = mxGetClassID(A);
    const bool iscx = mxIsComplex(A);
    mwSize od[2]={0,0};
    return mxCreateNumericArray(2, od, cls, iscx?mxCOMPLEX:mxREAL);
}

// ---- real kernels (copy + min/max in one pass) ----
template<typename T>
static void copy_stride_minmax_real(const T* __restrict src, T* __restrict dst,
                                    size_t M, size_t step, T& outMin, T& outMax, bool& hasNaN)
{
    hasNaN = false;
#if defined(_OPENMP)
    if (M >= (1u<<18) && omp_get_max_threads()>1){
        bool   gNan = false;
        T      gMin = T(0), gMax = T(0); bool gInit=false;
        #pragma omp parallel
        {
            T lMin=T(0), lMax=T(0); bool lInit=false; bool lNan=false;
            #pragma omp for schedule(static)
            for (mwIndex t=0; t<(mwIndex)M; ++t){
                const size_t i = (size_t)t*step;
                const T v = src[i];
                dst[t] = v;
                if (std::is_floating_point<T>::value && isnan_t<T>(v)){ lNan = true; continue; }
                if (!lInit){ lMin=v; lMax=v; lInit=true; }
                else { if (v<lMin) lMin=v; if (v>lMax) lMax=v; }
            }
            #pragma omp critical
            {
                gNan |= lNan;
                if (lInit){
                    if (!gInit){ gMin=lMin; gMax=lMax; gInit=true; }
                    else { if (lMin<gMin) gMin=lMin; if (lMax>gMax) gMax=lMax; }
                }
            }
        }
        hasNaN = gNan;
        if (!gInit){ outMin=T(0); outMax=T(0); }
        else { outMin=gMin; outMax=gMax; }
        return;
    }
#endif
    const T* p = src;
    bool init=false;
    T mn=T(0), mx=T(0);
    for (size_t t=0; t<M; ++t, p+=step){
        T v = *p;
        dst[t] = v;
        if (std::is_floating_point<T>::value && isnan_t<T>(v)){ hasNaN = true; continue; }
        if (!init){ mn=v; mx=v; init=true; }
        else { if (v<mn) mn=v; if (v>mx) mx=v; }
    }
    if (!init){ outMin=T(0); outMax=T(0); }
    else { outMin=mn; outMax=mx; }
}

// ---- real scan-only (used when Step==1 && zero-copy) ----
template<typename T>
static void scan_minmax_real(const T* __restrict src, size_t N, T& outMin, T& outMax, bool& hasNaN)
{
    hasNaN = false;
#if defined(_OPENMP)
    if (N >= (1u<<18) && omp_get_max_threads()>1){
        bool gNan=false; T gMin=T(0), gMax=T(0); bool gInit=false;
        #pragma omp parallel
        {
            bool lNan=false; T lMin=T(0), lMax=T(0); bool lInit=false;
            #pragma omp for schedule(static)
            for (mwIndex i=0; i<(mwIndex)N; ++i){
                T v = src[i];
                if (std::is_floating_point<T>::value && isnan_t<T>(v)){ lNan=true; continue; }
                if (!lInit){ lMin=v; lMax=v; lInit=true; }
                else { if (v<lMin) lMin=v; if (v>lMax) lMax=v; }
            }
            #pragma omp critical
            {
                gNan |= lNan;
                if (lInit){
                    if (!gInit){ gMin=lMin; gMax=lMax; gInit=true; }
                    else { if (lMin<gMin) gMin=lMin; if (lMax>gMax) gMax=lMax; }
                }
            }
        }
        hasNaN = gNan;
        if (!gInit){ outMin=T(0); outMax=T(0); }
        else { outMin=gMin; outMax=gMax; }
        return;
    }
#endif
    bool init=false; T mn=T(0), mx=T(0);
    for (size_t i=0;i<N;++i){
        T v = src[i];
        if (std::is_floating_point<T>::value && isnan_t<T>(v)){ hasNaN=true; continue; }
        if (!init){ mn=v; mx=v; init=true; }
        else { if (v<mn) mn=v; if (v>mx) mx=v; }
    }
    if (!init){ outMin=T(0); outMax=T(0); }
    else { outMin=mn; outMax=mx; }
}

// ---- complex kernels (compare by |z|^2, return element) ----
template<typename C, typename R>
static void copy_stride_minmax_complex(const C* __restrict src, C* __restrict dst,
                                       size_t M, size_t step, C& outMin, C& outMax, bool& hasNaN)
{
    hasNaN=false;
#if defined(_OPENMP)
    if (M >= (1u<<18) && omp_get_max_threads()>1){
        bool gNan=false; R gMinMag=R(0), gMaxMag=R(0); C gMinVal=C{0,0}, gMaxVal=C{0,0}; bool gInit=false;
        #pragma omp parallel
        {
            bool lNan=false; R lMinMag=R(0), lMaxMag=R(0); C lMinVal=C{0,0}, lMaxVal=C{0,0}; bool lInit=false;
            #pragma omp for schedule(static)
            for (mwIndex t=0; t<(mwIndex)M; ++t){
                const size_t i = (size_t)t*step;
                C v = src[i];
                dst[t] = v;
                const R a = v.real; const R b = v.imag;
                if (isnan_t<R>(a) || isnan_t<R>(b)){ lNan=true; continue; }
                const R mag = a*a + b*b;
                if (!lInit){ lMinMag=mag; lMaxMag=mag; lMinVal=v; lMaxVal=v; lInit=true; }
                else { if (mag<lMinMag){ lMinMag=mag; lMinVal=v; } if (mag>lMaxMag){ lMaxMag=mag; lMaxVal=v; } }
            }
            #pragma omp critical
            {
                gNan |= lNan;
                if (lInit){
                    if (!gInit){ gInit=true; gMinMag=lMinMag; gMaxMag=lMaxMag; gMinVal=lMinVal; gMaxVal=lMaxVal; }
                    else { if (lMinMag<gMinMag){ gMinMag=lMinMag; gMinVal=lMinVal; }
                           if (lMaxMag>gMaxMag){ gMaxMag=lMaxMag; gMaxVal=lMaxVal; } }
                }
            }
        }
        hasNaN=gNan; outMin=gMinVal; outMax=gMaxVal;
        return;
    }
#endif
    bool init=false; R minMag=R(0), maxMag=R(0); C minVal=C{0,0}, maxVal=C{0,0};
    const C* p = src;
    for (size_t t=0; t<M; ++t, p+=step){
        C v = *p; dst[t] = v;
        const R a=v.real, b=v.imag;
        if (isnan_t<R>(a) || isnan_t<R>(b)){ hasNaN=true; continue; }
        const R mag = a*a + b*b;
        if (!init){ init=true; minMag=mag; maxMag=mag; minVal=v; maxVal=v; }
        else { if (mag<minMag){ minMag=mag; minVal=v; } if (mag>maxMag){ maxMag=mag; maxVal=v; } }
    }
    outMin=minVal; outMax=maxVal;
}

template<typename C, typename R>
static void scan_minmax_complex(const C* __restrict src, size_t N, C& outMin, C& outMax, bool& hasNaN)
{
    hasNaN=false;
#if defined(_OPENMP)
    if (N >= (1u<<18) && omp_get_max_threads()>1){
        bool gNan=false; R gMinMag=R(0), gMaxMag=R(0); C gMinVal=C{0,0}, gMaxVal=C{0,0}; bool gInit=false;
        #pragma omp parallel
        {
            bool lNan=false; R lMinMag=R(0), lMaxMag=R(0); C lMinVal=C{0,0}, lMaxVal=C{0,0}; bool lInit=false;
            #pragma omp for schedule(static)
            for (mwIndex i=0;i<(mwIndex)N;++i){
                C v = src[i];
                const R a=v.real, b=v.imag;
                if (isnan_t<R>(a) || isnan_t<R>(b)){ lNan=true; continue; }
                const R mag = a*a + b*b;
                if (!lInit){ lInit=true; lMinMag=mag; lMaxMag=mag; lMinVal=v; lMaxVal=v; }
                else { if (mag<lMinMag){ lMinMag=mag; lMinVal=v; } if (mag>lMaxMag){ lMaxMag=mag; lMaxVal=v; } }
            }
            #pragma omp critical
            {
                gNan|=lNan;
                if (lInit){
                    if (!gInit){ gInit=true; gMinMag=lMinMag; gMaxMag=lMaxMag; gMinVal=lMinVal; gMaxVal=lMaxVal; }
                    else { if (lMinMag<gMinMag){ gMinMag=lMinMag; gMinVal=lMinVal; }
                           if (lMaxMag>gMaxMag){ gMaxMag=lMaxMag; gMaxVal=lMaxVal; } }
                }
            }
        }
        hasNaN=gNan; outMin=gMinVal; outMax=gMaxVal; return;
    }
#endif
    bool init=false; R minMag=R(0), maxMag=R(0); C minVal=C{0,0}, maxVal=C{0,0};
    for (size_t i=0;i<N;++i){
        C v=src[i]; const R a=v.real,b=v.imag;
        if (isnan_t<R>(a) || isnan_t<R>(b)){ hasNaN=true; continue; }
        const R mag=a*a+b*b;
        if (!init){ init=true; minMag=mag; maxMag=mag; minVal=v; maxVal=v; }
        else { if (mag<minMag){ minMag=mag; minVal=v; } if (mag>maxMag){ maxMag=mag; maxVal=v; } }
    }
    outMin=minVal; outMax=maxVal;
}

// -------- MEX entry --------
void mexFunction(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[])
{
    if (nrhs != 2)
        mexErrMsgIdAndTxt("diluteArray_MinMax:usage","Usage: [Y,Mn,Mx] = diluteArray_MinMax(A, Step);");
    if (nlhs < 1 || nlhs > 3)
        mexErrMsgIdAndTxt("diluteArray_MinMax:usage","One to three outputs allowed.");

    const mxArray* A = prhs[0];
    if (mxIsSparse(A))
        mexErrMsgIdAndTxt("diluteArray_MinMax:type","Sparse arrays are not supported.");

    require_scalar_real(prhs[1], "diluteArray_MinMax:step");
    const double step_d = mxGetScalar(prhs[1]);
    if (!mxIsFinite(step_d) || step_d < 1.0)
        mexErrMsgIdAndTxt("diluteArray_MinMax:step","Step must be a finite integer >= 1.");
    const size_t step = static_cast<size_t>(step_d);
    if (step_d != static_cast<double>(step))
        mexErrMsgIdAndTxt("diluteArray_MinMax:step","Step must be an integer.");

    const size_t N = (size_t)mxGetNumberOfElements(A);
    const size_t M = (N==0) ? 0 : ((N - 1) / step + 1);
    const mxClassID cls = mxGetClassID(A);

    // Output Y column vector
#if !defined(MX_HAS_INTERLEAVED_COMPLEX) // -R2017b: zero-copy for Step==1
    if (step == 1){
        mxArray* Y = mxCreateSharedDataCopy(const_cast<mxArray*>(A));
        mwSize od[2]; od[0]=(mwSize)N; od[1]=1;
        if (!mxSetDimensions(Y, od, 2)) mexErrMsgIdAndTxt("diluteArray_MinMax:reshape","mxSetDimensions failed.");
        plhs[0] = Y;
    } else {
        plhs[0] = create_out_col_like(A, M);
    }
#else
    plhs[0] = create_out_col_like(A, (step==1?N:M));
#endif

    // Empty case
    if (M == 0){
        if (nlhs>=2) plhs[1] = create_empty_scalar_like(A);
        if (nlhs>=3) plhs[2] = create_empty_scalar_like(A);
        return;
    }

    const bool iscx = mxIsComplex(A);

    if (!iscx){
        switch (cls){
            case mxDOUBLE_CLASS:{
                double mn=0,mx=0; bool hasNaN=false;
            #if !defined(MX_HAS_INTERLEAVED_COMPLEX)
                if (step==1) scan_minmax_real(mxGetPr(A), N, mn, mx, hasNaN);
                else         copy_stride_minmax_real(mxGetPr(A), mxGetPr(plhs[0]), M, step, mn, mx, hasNaN);
                if (nlhs>=2){ plhs[1]=mxCreateDoubleScalar(hasNaN? mxGetNaN() : mn); }
                if (nlhs>=3){ plhs[2]=mxCreateDoubleScalar(hasNaN? mxGetNaN() : mx); }
            #else
                if (step==1) copy_stride_minmax_real(mxGetDoubles(A), mxGetDoubles(plhs[0]), M, 1,    mn, mx, hasNaN);
                else         copy_stride_minmax_real(mxGetDoubles(A), mxGetDoubles(plhs[0]), M, step, mn, mx, hasNaN);
                if (nlhs>=2){ plhs[1]=mxCreateDoubleScalar(hasNaN? std::numeric_limits<double>::quiet_NaN() : mn); }
                if (nlhs>=3){ plhs[2]=mxCreateDoubleScalar(hasNaN? std::numeric_limits<double>::quiet_NaN() : mx); }
            #endif
            } break;
            case mxSINGLE_CLASS:{
                float mn=0,mx=0; bool hasNaN=false;
            #if !defined(MX_HAS_INTERLEAVED_COMPLEX)
                if (step==1){
                    const float* s=(const float*)mxGetData(A);
                    scan_minmax_real(s, N, mn, mx, hasNaN);
                } else {
                    const float* s=(const float*)mxGetData(A);
                    float* d=(float*)mxGetData(plhs[0]);
                    copy_stride_minmax_real(s, d, M, step, mn, mx, hasNaN);
                }
                if (nlhs>=2){ plhs[1]=mxCreateNumericMatrix(1,1,mxSINGLE_CLASS,mxREAL); *mxGetSingles(plhs[1]) = hasNaN? (float)mxGetNaN() : mn; }
                if (nlhs>=3){ plhs[2]=mxCreateNumericMatrix(1,1,mxSINGLE_CLASS,mxREAL); *mxGetSingles(plhs[2]) = hasNaN? (float)mxGetNaN() : mx; }
            #else
                if (step==1) copy_stride_minmax_real(mxGetSingles(A), mxGetSingles(plhs[0]), M, 1,    mn, mx, hasNaN);
                else         copy_stride_minmax_real(mxGetSingles(A), mxGetSingles(plhs[0]), M, step, mn, mx, hasNaN);
                if (nlhs>=2){ plhs[1]=mxCreateNumericMatrix(1,1,mxSINGLE_CLASS,mxREAL); *mxGetSingles(plhs[1]) = hasNaN? std::numeric_limits<float>::quiet_NaN() : mn; }
                if (nlhs>=3){ plhs[2]=mxCreateNumericMatrix(1,1,mxSINGLE_CLASS,mxREAL); *mxGetSingles(plhs[2]) = hasNaN? std::numeric_limits<float>::quiet_NaN() : mx; }
            #endif
            } break;
            case mxINT8_CLASS:   { int8_T mn=0,mx=0;  bool z=false;
                if (step==1){
                #if !defined(MX_HAS_INTERLEAVED_COMPLEX)
                    const int8_T* s=(const int8_T*)mxGetData(A);
                    scan_minmax_real(s,N,mn,mx,z);
                #else
                    copy_stride_minmax_real((const int8_T*)mxGetData(A),(int8_T*)mxGetData(plhs[0]),M,1,mn,mx,z);
                #endif
                } else {
                    copy_stride_minmax_real((const int8_T*)mxGetData(A),(int8_T*)mxGetData(plhs[0]),M,step,mn,mx,z);
                }
                if (nlhs>=2){ plhs[1]=mxCreateNumericMatrix(1,1,mxINT8_CLASS,mxREAL);  *reinterpret_cast<int8_T*>(mxGetData(plhs[1]))=mn; }
                if (nlhs>=3){ plhs[2]=mxCreateNumericMatrix(1,1,mxINT8_CLASS,mxREAL);  *reinterpret_cast<int8_T*>(mxGetData(plhs[2]))=mx; }
            } break;
            case mxUINT8_CLASS:  { uint8_T mn=0,mx=0; bool z=false;
                if (step==1){
                #if !defined(MX_HAS_INTERLEAVED_COMPLEX)
                    const uint8_T* s=(const uint8_T*)mxGetData(A);
                    scan_minmax_real(s,N,mn,mx,z);
                #else
                    copy_stride_minmax_real((const uint8_T*)mxGetData(A),(uint8_T*)mxGetData(plhs[0]),M,1,mn,mx,z);
                #endif
                } else {
                    copy_stride_minmax_real((const uint8_T*)mxGetData(A),(uint8_T*)mxGetData(plhs[0]),M,step,mn,mx,z);
                }
                if (nlhs>=2){ plhs[1]=mxCreateNumericMatrix(1,1,mxUINT8_CLASS,mxREAL); *reinterpret_cast<uint8_T*>(mxGetData(plhs[1]))=mn; }
                if (nlhs>=3){ plhs[2]=mxCreateNumericMatrix(1,1,mxUINT8_CLASS,mxREAL); *reinterpret_cast<uint8_T*>(mxGetData(plhs[2]))=mx; }
            } break;
            case mxINT16_CLASS:  { int16_T mn=0,mx=0; bool z=false;
                if (step==1){
                #if !defined(MX_HAS_INTERLEAVED_COMPLEX)
                    const int16_T* s=(const int16_T*)mxGetData(A);
                    scan_minmax_real(s,N,mn,mx,z);
                #else
                    copy_stride_minmax_real((const int16_T*)mxGetData(A),(int16_T*)mxGetData(plhs[0]),M,1,mn,mx,z);
                #endif
                } else {
                    copy_stride_minmax_real((const int16_T*)mxGetData(A),(int16_T*)mxGetData(plhs[0]),M,step,mn,mx,z);
                }
                if (nlhs>=2){ plhs[1]=mxCreateNumericMatrix(1,1,mxINT16_CLASS,mxREAL); *reinterpret_cast<int16_T*>(mxGetData(plhs[1]))=mn; }
                if (nlhs>=3){ plhs[2]=mxCreateNumericMatrix(1,1,mxINT16_CLASS,mxREAL); *reinterpret_cast<int16_T*>(mxGetData(plhs[2]))=mx; }
            } break;
            case mxUINT16_CLASS: { uint16_T mn=0,mx=0; bool z=false;
                if (step==1){
                #if !defined(MX_HAS_INTERLEAVED_COMPLEX)
                    const uint16_T* s=(const uint16_T*)mxGetData(A);
                    scan_minmax_real(s,N,mn,mx,z);
                #else
                    copy_stride_minmax_real((const uint16_T*)mxGetData(A),(uint16_T*)mxGetData(plhs[0]),M,1,mn,mx,z);
                #endif
                } else {
                    copy_stride_minmax_real((const uint16_T*)mxGetData(A),(uint16_T*)mxGetData(plhs[0]),M,step,mn,mx,z);
                }
                if (nlhs>=2){ plhs[1]=mxCreateNumericMatrix(1,1,mxUINT16_CLASS,mxREAL); *reinterpret_cast<uint16_T*>(mxGetData(plhs[1]))=mn; }
                if (nlhs>=3){ plhs[2]=mxCreateNumericMatrix(1,1,mxUINT16_CLASS,mxREAL); *reinterpret_cast<uint16_T*>(mxGetData(plhs[2]))=mx; }
            } break;
            case mxINT32_CLASS:  { int32_T mn=0,mx=0; bool z=false;
                if (step==1){
                #if !defined(MX_HAS_INTERLEAVED_COMPLEX)
                    const int32_T* s=(const int32_T*)mxGetData(A);
                    scan_minmax_real(s,N,mn,mx,z);
                #else
                    copy_stride_minmax_real((const int32_T*)mxGetData(A),(int32_T*)mxGetData(plhs[0]),M,1,mn,mx,z);
                #endif
                } else {
                    copy_stride_minmax_real((const int32_T*)mxGetData(A),(int32_T*)mxGetData(plhs[0]),M,step,mn,mx,z);
                }
                if (nlhs>=2){ plhs[1]=mxCreateNumericMatrix(1,1,mxINT32_CLASS,mxREAL); *reinterpret_cast<int32_T*>(mxGetData(plhs[1]))=mn; }
                if (nlhs>=3){ plhs[2]=mxCreateNumericMatrix(1,1,mxINT32_CLASS,mxREAL); *reinterpret_cast<int32_T*>(mxGetData(plhs[2]))=mx; }
            } break;
            case mxUINT32_CLASS: { uint32_T mn=0,mx=0; bool z=false;
                if (step==1){
                #if !defined(MX_HAS_INTERLEAVED_COMPLEX)
                    const uint32_T* s=(const uint32_T*)mxGetData(A);
                    scan_minmax_real(s,N,mn,mx,z);
                #else
                    copy_stride_minmax_real((const uint32_T*)mxGetData(A),(uint32_T*)mxGetData(plhs[0]),M,1,mn,mx,z);
                #endif
                } else {
                    copy_stride_minmax_real((const uint32_T*)mxGetData(A),(uint32_T*)mxGetData(plhs[0]),M,step,mn,mx,z);
                }
                if (nlhs>=2){ plhs[1]=mxCreateNumericMatrix(1,1,mxUINT32_CLASS,mxREAL); *reinterpret_cast<uint32_T*>(mxGetData(plhs[1]))=mn; }
                if (nlhs>=3){ plhs[2]=mxCreateNumericMatrix(1,1,mxUINT32_CLASS,mxREAL); *reinterpret_cast<uint32_T*>(mxGetData(plhs[2]))=mx; }
            } break;
            case mxINT64_CLASS:  { int64_T mn=0,mx=0; bool z=false;
                if (step==1){
                #if !defined(MX_HAS_INTERLEAVED_COMPLEX)
                    const int64_T* s=(const int64_T*)mxGetData(A);
                    scan_minmax_real(s,N,mn,mx,z);
                #else
                    copy_stride_minmax_real((const int64_T*)mxGetData(A),(int64_T*)mxGetData(plhs[0]),M,1,mn,mx,z);
                #endif
                } else {
                    copy_stride_minmax_real((const int64_T*)mxGetData(A),(int64_T*)mxGetData(plhs[0]),M,step,mn,mx,z);
                }
                if (nlhs>=2){ plhs[1]=mxCreateNumericMatrix(1,1,mxINT64_CLASS,mxREAL); *reinterpret_cast<int64_T*>(mxGetData(plhs[1]))=mn; }
                if (nlhs>=3){ plhs[2]=mxCreateNumericMatrix(1,1,mxINT64_CLASS,mxREAL); *reinterpret_cast<int64_T*>(mxGetData(plhs[2]))=mx; }
            } break;
            case mxUINT64_CLASS: { uint64_T mn=0,mx=0; bool z=false;
                if (step==1){
                #if !defined(MX_HAS_INTERLEAVED_COMPLEX)
                    const uint64_T* s=(const uint64_T*)mxGetData(A);
                    scan_minmax_real(s,N,mn,mx,z);
                #else
                    copy_stride_minmax_real((const uint64_T*)mxGetData(A),(uint64_T*)mxGetData(plhs[0]),M,1,mn,mx,z);
                #endif
                } else {
                    copy_stride_minmax_real((const uint64_T*)mxGetData(A),(uint64_T*)mxGetData(plhs[0]),M,step,mn,mx,z);
                }
                if (nlhs>=2){ plhs[1]=mxCreateNumericMatrix(1,1,mxUINT64_CLASS,mxREAL); *reinterpret_cast<uint64_T*>(mxGetData(plhs[1]))=mn; }
                if (nlhs>=3){ plhs[2]=mxCreateNumericMatrix(1,1,mxUINT64_CLASS,mxREAL); *reinterpret_cast<uint64_T*>(mxGetData(plhs[2]))=mx; }
            } break;
            case mxLOGICAL_CLASS:{ mxLogical mn=0,mx=0; bool z=false;
                if (step==1){
                #if !defined(MX_HAS_INTERLEAVED_COMPLEX)
                    const mxLogical* s=mxGetLogicals(A);
                    scan_minmax_real(s,N,mn,mx,z);
                #else
                    copy_stride_minmax_real(mxGetLogicals(A), mxGetLogicals(plhs[0]), M, 1, mn, mx, z);
                #endif
                } else {
                    copy_stride_minmax_real(mxGetLogicals(A), mxGetLogicals(plhs[0]), M, step, mn, mx, z);
                }
                if (nlhs>=2) plhs[1]=mxCreateLogicalScalar(mn!=0);
                if (nlhs>=3) plhs[2]=mxCreateLogicalScalar(mx!=0);
            } break;
            case mxCHAR_CLASS:   { mxChar mn=0,mx=0; bool z=false;
                if (step==1){
                #if !defined(MX_HAS_INTERLEAVED_COMPLEX)
                    const mxChar* s=mxGetChars(A);
                    scan_minmax_real(s,N,mn,mx,z);
                #else
                    copy_stride_minmax_real(mxGetChars(A), mxGetChars(plhs[0]), M, 1, mn, mx, z);
                #endif
                } else {
                    copy_stride_minmax_real(mxGetChars(A), mxGetChars(plhs[0]), M, step, mn, mx, z);
                }
                if (nlhs>=2){ mwSize d[2]={1,1}; plhs[1]=mxCreateCharArray(2,d); *mxGetChars(plhs[1])=mn; }
                if (nlhs>=3){ mwSize d[2]={1,1}; plhs[2]=mxCreateCharArray(2,d); *mxGetChars(plhs[2])=mx; }
            } break;
            default:
                mexErrMsgIdAndTxt("diluteArray_MinMax:type","Unsupported class.");
        }
    } else {
        // Complex single/double: compare by magnitude^2, return the element
        if (cls!=mxDOUBLE_CLASS && cls!=mxSINGLE_CLASS)
            mexErrMsgIdAndTxt("diluteArray_MinMax:complex","Complex arrays must be single or double.");
        if (nlhs>=2) plhs[1]=mxCreateNumericMatrix(1,1,cls,mxCOMPLEX);
        if (nlhs>=3) plhs[2]=mxCreateNumericMatrix(1,1,cls,mxCOMPLEX);

    #if defined(MX_HAS_INTERLEAVED_COMPLEX)
        if (cls==mxDOUBLE_CLASS){
            mxComplexDouble mn, mx; bool hasNaN=false;
            if (step==1) copy_stride_minmax_complex<mxComplexDouble,double>(mxGetComplexDoubles(A), mxGetComplexDoubles(plhs[0]), M, 1,    mn, mx, hasNaN);
            else         copy_stride_minmax_complex<mxComplexDouble,double>(mxGetComplexDoubles(A), mxGetComplexDoubles(plhs[0]), M, step, mn, mx, hasNaN);
            if (nlhs>=2){ auto p = mxGetComplexDoubles(plhs[1]);
                          if (hasNaN){ p[0].real = std::numeric_limits<double>::quiet_NaN();
                                       p[0].imag = std::numeric_limits<double>::quiet_NaN(); }
                          else p[0]=mn; }
            if (nlhs>=3){ auto p = mxGetComplexDoubles(plhs[2]);
                          if (hasNaN){ p[0].real = std::numeric_limits<double>::quiet_NaN();
                                       p[0].imag = std::numeric_limits<double>::quiet_NaN(); }
                          else p[0]=mx; }
        } else { // single
            mxComplexSingle mn, mx; bool hasNaN=false;
            if (step==1) copy_stride_minmax_complex<mxComplexSingle,float>(mxGetComplexSingles(A), mxGetComplexSingles(plhs[0]), M, 1,    mn, mx, hasNaN);
            else         copy_stride_minmax_complex<mxComplexSingle,float>(mxGetComplexSingles(A), mxGetComplexSingles(plhs[0]), M, step, mn, mx, hasNaN);
            if (nlhs>=2){ auto p = mxGetComplexSingles(plhs[1]);
                          if (hasNaN){ p[0].real = std::numeric_limits<float>::quiet_NaN();
                                       p[0].imag = std::numeric_limits<float>::quiet_NaN(); }
                          else p[0]=mn; }
            if (nlhs>=3){ auto p = mxGetComplexSingles(plhs[2]);
                          if (hasNaN){ p[0].real = std::numeric_limits<float>::quiet_NaN();
                                       p[0].imag = std::numeric_limits<float>::quiet_NaN(); }
                          else p[0]=mx; }
        }
    #else
        // R2017b split-complex paths omitted here since you’re building with -R2018a
    #endif
    }
}
