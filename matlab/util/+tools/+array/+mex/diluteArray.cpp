#include "mex.h"
#include <cstddef>
#include <cstring>
#include <vector>
#include <cmath>

#if defined(_OPENMP)
  #include <omp.h>
#endif

// ---------- utils ----------
static inline void check_scalar_real(const mxArray* a, const char* id){
    if (mxIsComplex(a) || mxGetNumberOfElements(a)!=1)
        mexErrMsgIdAndTxt(id, "Argument must be a real scalar.");
}

template<typename T>
static void copy_stride_kernel(const T* __restrict src, T* __restrict dst,
                               size_t M, size_t step)
{
#if defined(_OPENMP)
    if (M >= (1u<<18) && omp_get_max_threads()>1){
        #pragma omp parallel for schedule(static)
        for (mwIndex t=0; t<(mwIndex)M; ++t)
            dst[t] = src[(size_t)t * step];
        return;
    }
#endif
    const T* p = src;
    for (size_t t=0; t<M; ++t){ dst[t] = *p; p += step; }
}

// split-complex stride copy (R2017b API)
template<typename T>
static void copy_stride_split_complex(const T* __restrict sr, const T* __restrict si,
                                      T* __restrict dr, T* __restrict di,
                                      size_t M, size_t step)
{
#if defined(_OPENMP)
    if (M >= (1u<<18) && omp_get_max_threads()>1){
        #pragma omp parallel for schedule(static)
        for (mwIndex t=0; t<(mwIndex)M; ++t){
            const size_t i = (size_t)t * step;
            dr[t] = sr[i];
            di[t] = si ? si[i] : T(0);
        }
        return;
    }
#endif
    const T* pr = sr;
    const T* pi = si;
    for (size_t t=0; t<M; ++t){
        dr[t] = *pr;
        if (si) di[t] = *pi; else di[t] = T(0);
        pr += step; if (si) pi += step;
    }
}

static mxArray* create_out_col_like(const mxArray* A, size_t M)
{
    const mxClassID cls = mxGetClassID(A);
    const bool iscx = mxIsComplex(A);
#if defined(mxCreateUninitNumericArray)
    std::vector<size_t> sd(2);
    sd[0] = (size_t)M; sd[1] = 1;
    return mxCreateUninitNumericArray(2, sd.data(), cls, iscx ? mxCOMPLEX : mxREAL);
#else
    mwSize odims[2]; odims[0] = (mwSize)M; odims[1] = 1;
    return mxCreateNumericArray(2, odims, cls, iscx ? mxCOMPLEX : mxREAL);
#endif
}

// ---------- MEX entry ----------
void mexFunction(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[])
{
    if (nrhs != 2)
        mexErrMsgIdAndTxt("diluteArray:usage","Usage: Out = diluteArray(A, Step);");
    if (nlhs > 1)
        mexErrMsgIdAndTxt("diluteArray:usage","One output only.");

    const mxArray* A = prhs[0];
    if (mxIsSparse(A))
        mexErrMsgIdAndTxt("diluteArray:type","Sparse arrays are not supported.");
    const mxClassID cls = mxGetClassID(A);

    // Step: positive integer
    check_scalar_real(prhs[1], "diluteArray:step");
    const double step_d = mxGetScalar(prhs[1]);
    if (!mxIsFinite(step_d)) mexErrMsgIdAndTxt("diluteArray:step","Step must be finite.");
    if (step_d < 1.0)        mexErrMsgIdAndTxt("diluteArray:step","Step must be >= 1.");
    const size_t step = static_cast<size_t>(step_d);
    if (step_d != static_cast<double>(step))
        mexErrMsgIdAndTxt("diluteArray:step","Step must be an integer.");

    const size_t N = (size_t)mxGetNumberOfElements(A);
    const size_t M = (N==0) ? 0 : ((N - 1) / step + 1);

    // ---- Step == 1 : ensure COLUMN VECTOR output ----
#if !defined(MX_HAS_INTERLEAVED_COMPLEX)  // compiling with -R2017b API
    if (step == 1){
        // Zero-copy header clone (shares data with A)
        mxArray* Y = mxCreateSharedDataCopy(const_cast<mxArray*>(A));
        mwSize dims2[2]; dims2[0] = (mwSize)N; dims2[1] = 1; // column vector
        if (!mxSetDimensions(Y, dims2, 2))
            mexErrMsgIdAndTxt("diluteArray:reshape","mxSetDimensions failed.");
        plhs[0] = Y;
        return;
    }
#else
    if (step == 1){
        // Allocate & memcpy; force column vector [N x 1]
        plhs[0] = create_out_col_like(A, N);
        std::memcpy(mxGetData(plhs[0]), mxGetData(A), N * mxGetElementSize(A));
        return;
    }
#endif

    // ---- Step > 1 : allocate column vector and stride-copy ----
    plhs[0] = create_out_col_like(A, M);
    if (M == 0) return;

#if defined(MX_HAS_INTERLEAVED_COMPLEX)  // R2018a+ typed accessors
    if (!mxIsComplex(A)){
        switch (cls){
            case mxDOUBLE_CLASS: { copy_stride_kernel(mxGetDoubles(A), mxGetDoubles(plhs[0]), M, step); } break;
            case mxSINGLE_CLASS: { copy_stride_kernel(mxGetSingles(A), mxGetSingles(plhs[0]), M, step); } break;
            case mxINT8_CLASS:   { copy_stride_kernel((const int8_T*)mxGetData(A),  (int8_T*)mxGetData(plhs[0]),  M, step); } break;
            case mxUINT8_CLASS:  { copy_stride_kernel((const uint8_T*)mxGetData(A), (uint8_T*)mxGetData(plhs[0]), M, step); } break;
            case mxINT16_CLASS:  { copy_stride_kernel((const int16_T*)mxGetData(A), (int16_T*)mxGetData(plhs[0]), M, step); } break;
            case mxUINT16_CLASS: { copy_stride_kernel((const uint16_T*)mxGetData(A),(uint16_T*)mxGetData(plhs[0]),M, step); } break;
            case mxINT32_CLASS:  { copy_stride_kernel((const int32_T*)mxGetData(A), (int32_T*)mxGetData(plhs[0]), M, step); } break;
            case mxUINT32_CLASS: { copy_stride_kernel((const uint32_T*)mxGetData(A),(uint32_T*)mxGetData(plhs[0]),M, step); } break;
            case mxINT64_CLASS:  { copy_stride_kernel((const int64_T*)mxGetData(A), (int64_T*)mxGetData(plhs[0]), M, step); } break;
            case mxUINT64_CLASS: { copy_stride_kernel((const uint64_T*)mxGetData(A),(uint64_T*)mxGetData(plhs[0]),M, step); } break;
            case mxLOGICAL_CLASS:{ copy_stride_kernel(mxGetLogicals(A), mxGetLogicals(plhs[0]), M, step); } break;
            case mxCHAR_CLASS:   { copy_stride_kernel(mxGetChars(A),     mxGetChars(plhs[0]),     M, step); } break;
            default: mexErrMsgIdAndTxt("diluteArray:type","Unsupported class for real array.");
        }
    } else {
        switch (cls){
            case mxDOUBLE_CLASS: {
                copy_stride_kernel(mxGetComplexDoubles(A), mxGetComplexDoubles(plhs[0]), M, step);
            } break;
            case mxSINGLE_CLASS: {
                copy_stride_kernel(mxGetComplexSingles(A), mxGetComplexSingles(plhs[0]), M, step);
            } break;
            default:
                mexErrMsgIdAndTxt("diluteArray:type","Complex arrays must be single or double.");
        }
    }
#else  // ---------- R2017b API (split complex) ----------
    if (!mxIsComplex(A)){
        const void* s = mxGetData(A);
        void* d = mxGetData(plhs[0]);
        const size_t esz = mxGetElementSize(A);
        // dispatch by element size to keep types correct
        switch (cls){
            case mxDOUBLE_CLASS: { copy_stride_kernel((const double*)s, (double*)d, M, step); } break;
            case mxSINGLE_CLASS: { copy_stride_kernel((const float*) s, (float*) d, M, step); } break;
            case mxINT8_CLASS:   { copy_stride_kernel((const int8_T*) s,(int8_T*) d, M, step); } break;
            case mxUINT8_CLASS:  { copy_stride_kernel((const uint8_T*)s,(uint8_T*)d, M, step); } break;
            case mxINT16_CLASS:  { copy_stride_kernel((const int16_T*)s,(int16_T*)d, M, step); } break;
            case mxUINT16_CLASS: { copy_stride_kernel((const uint16_T*)s,(uint16_T*)d, M, step); } break;
            case mxINT32_CLASS:  { copy_stride_kernel((const int32_T*)s,(int32_T*)d, M, step); } break;
            case mxUINT32_CLASS: { copy_stride_kernel((const uint32_T*)s,(uint32_T*)d, M, step); } break;
            case mxINT64_CLASS:  { copy_stride_kernel((const int64_T*)s,(int64_T*)d, M, step); } break;
            case mxUINT64_CLASS: { copy_stride_kernel((const uint64_T*)s,(uint64_T*)d, M, step); } break;
            case mxLOGICAL_CLASS:{ copy_stride_kernel((const mxLogical*)s,(mxLogical*)d, M, step); } break;
            case mxCHAR_CLASS:   { copy_stride_kernel((const mxChar*)   s,(mxChar*)   d, M, step); } break;
            default: mexErrMsgIdAndTxt("diluteArray:type","Unsupported class for real array.");
        }
        (void)esz; // silence unused warning if any
    } else {
        // split complex buffers
        void* pr = mxGetData(A);
        void* pi = mxGetImagData(A);
        void* dr = mxGetData(plhs[0]);
        void* di = mxGetImagData(plhs[0]);
        switch (cls){
            case mxDOUBLE_CLASS: {
                copy_stride_split_complex<double>((const double*)pr,(const double*)pi,
                                                  (double*)dr,(double*)di, M, step);
            } break;
            case mxSINGLE_CLASS: {
                copy_stride_split_complex<float>((const float*)pr,(const float*)pi,
                                                 (float*)dr,(float*)di, M, step);
            } break;
            default:
                mexErrMsgIdAndTxt("diluteArray:type","Complex arrays must be single or double.");
        }
    }
#endif
}
