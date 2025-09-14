#include "mex.h"
#include <cstdint>
#include <cstring>
#include <algorithm>
#if defined(_OPENMP)
  #include <omp.h>
#endif
#if defined(__AVX2__)
  #include <immintrin.h>
#endif

// ------------------- Core kernels -------------------
template<typename T>
static void reduce_dim1(const T* __restrict A, mwSize M, mwSize N, T* __restrict B)
{
    // Each column is contiguous: parallelize over columns
    #if defined(_OPENMP)
    #pragma omp parallel for schedule(static)
    #endif
    for (mwIndex j = 0; j < (mwIndex)N; ++j)
    {
        const T* col = A + (size_t)j * M;
        T acc = 0;

        #if defined(__AVX2__)
        // AVX2 vector reduction across the column
        const size_t vecElems = 32u / sizeof(T);         // elements per 256-bit vector
        size_t i = 0;
        if (M >= vecElems)
        {
            __m256i vacc = _mm256_setzero_si256();
            size_t limit = (M / vecElems) * vecElems;
            for (; i < limit; i += vecElems)
            {
                __m256i v = _mm256_loadu_si256((const __m256i*)(col + i));
                vacc = _mm256_or_si256(vacc, v);
            }
            alignas(32) unsigned char tmp[32];
            _mm256_storeu_si256((__m256i*)tmp, vacc);
            const T* t = reinterpret_cast<const T*>(tmp);
            const size_t K = 32u / sizeof(T);
            T vpart = 0;
            for (size_t k=0; k<K; ++k) vpart |= t[k];
            acc |= vpart;
        }
        // tail
        for (; i < M; ++i) acc |= col[i];
        #else
        // Scalar
        for (mwIndex i=0; i<(mwIndex)M; ++i) acc |= col[i];
        #endif

        B[j] = acc;
    }
}

template<typename T>
static void reduce_dim2(const T* __restrict A, mwSize M, mwSize N, T* __restrict B)
{
    // Each row is strided by M: parallelize over rows
    #if defined(_OPENMP)
    #pragma omp parallel for schedule(static)
    #endif
    for (mwIndex i = 0; i < (mwIndex)M; ++i)
    {
        T acc = 0;
        const T* row = A + i;
        for (mwIndex j=0; j<(mwIndex)N; ++j)
            acc |= row[(size_t)j * M];
        B[i] = acc;
    }
}

// ------------------- Dispatcher -------------------
static void dispatch(const mxArray* A, int dim, mxArray** out)
{
    if (mxIsComplex(A)) mexErrMsgIdAndTxt("bitorArray:type","A must be real.");
    if (mxGetNumberOfDimensions(A) != 2) mexErrMsgIdAndTxt("bitorArray:ndims","A must be a 2-D matrix.");
    if (dim != 1 && dim != 2) mexErrMsgIdAndTxt("bitorArray:dim","Dim must be 1 or 2.");

    const mxClassID cls = mxGetClassID(A);
    const mwSize* dims = mxGetDimensions(A);
    const mwSize M = dims[0];
    const mwSize N = dims[1];

    mwSize odims[2];
    if (dim == 1) { odims[0] = 1; odims[1] = N; }
    else          { odims[0] = M; odims[1] = 1; }

    *out = mxCreateNumericArray(2, odims, cls, mxREAL);

    // Empty behavior: OR-identity is zero → output already zeroed by mxCreateNumericArray
    if (M==0 || N==0) return;

    switch (cls)
    {
        case mxUINT8_CLASS:
        {
            const uint8_T* a = reinterpret_cast<const uint8_T*>(mxGetData(A));
            uint8_T* b = reinterpret_cast<uint8_T*>(mxGetData(*out));
            if (dim==1) reduce_dim1<uint8_T>(a,M,N,b);
            else        reduce_dim2<uint8_T>(a,M,N,b);
            break;
        }
        case mxUINT16_CLASS:
        {
            const uint16_T* a = reinterpret_cast<const uint16_T*>(mxGetData(A));
            uint16_T* b = reinterpret_cast<uint16_T*>(mxGetData(*out));
            if (dim==1) reduce_dim1<uint16_T>(a,M,N,b);
            else        reduce_dim2<uint16_T>(a,M,N,b);
            break;
        }
        case mxUINT32_CLASS:
        {
            const uint32_T* a = reinterpret_cast<const uint32_T*>(mxGetData(A));
            uint32_T* b = reinterpret_cast<uint32_T*>(mxGetData(*out));
            if (dim==1) reduce_dim1<uint32_T>(a,M,N,b);
            else        reduce_dim2<uint32_T>(a,M,N,b);
            break;
        }
        case mxUINT64_CLASS:
        {
            const uint64_T* a = reinterpret_cast<const uint64_T*>(mxGetData(A));
            uint64_T* b = reinterpret_cast<uint64_T*>(mxGetData(*out));
            if (dim==1) reduce_dim1<uint64_T>(a,M,N,b);
            else        reduce_dim2<uint64_T>(a,M,N,b);
            break;
        }
        default:
            mexErrMsgIdAndTxt("bitorArray:type","A must be uint8/uint16/uint32/uint64.");
    }
}

// ------------------- MEX entry -------------------
void mexFunction(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[])
{
    if (nrhs != 2)
        mexErrMsgIdAndTxt("bitorArray:usage","Usage: B = bitorArray(A, Dim);");
    if (nlhs > 1)
        mexErrMsgIdAndTxt("bitorArray:usage","One output only.");

    const mxArray* A = prhs[0];
    if (!mxIsDouble(prhs[1]) || mxIsComplex(prhs[1]) || mxGetNumberOfElements(prhs[1])!=1)
        mexErrMsgIdAndTxt("bitorArray:dim","Dim must be a real scalar 1 or 2.");
    int dim = static_cast<int>(mxGetScalar(prhs[1]));

    dispatch(A, dim, &plhs[0]);
}
