#include "mex.h"
#include <cstdint>
#include <cmath>      // <-- for std::floor

#if defined(_MSC_VER)
  #include <intrin.h>
  #define RESTRICT __restrict
#else
  #include <immintrin.h>
  #define RESTRICT __restrict__
#endif

static inline uint32_T load_size_u32(const mxArray* s, mwSize idx) {
    if (!mxIsNumeric(s) || mxIsComplex(s) || mxGetNumberOfElements(s) <= idx)
        mexErrMsgIdAndTxt("ind2sub:Size", "SizeMat must be real numeric with at least 2 elements: [Size1 Size2].");

    const void* ps = mxGetData(s);
    switch (mxGetClassID(s)) {
        case mxDOUBLE_CLASS: return (uint32_T)((const double*)ps)[idx];
        case mxSINGLE_CLASS: return (uint32_T)((const float*)ps)[idx];
        case mxINT8_CLASS:   return (uint32_T)((const int8_T*)ps)[idx];
        case mxUINT8_CLASS:  return (uint32_T)((const uint8_T*)ps)[idx];
        case mxINT16_CLASS:  return (uint32_T)((const int16_T*)ps)[idx];
        case mxUINT16_CLASS: return (uint32_T)((const uint16_T*)ps)[idx];
        case mxINT32_CLASS:  return (uint32_T)((const int32_T*)ps)[idx];
        case mxUINT32_CLASS: return (uint32_T)((const uint32_T*)ps)[idx];
        case mxINT64_CLASS:  return (uint32_T)((const int64_T*)ps)[idx];
        case mxUINT64_CLASS: return (uint32_T)((const uint64_T*)ps)[idx];
        default:
            mexErrMsgIdAndTxt("ind2sub:SizeType", "SizeMat must be numeric (single/double or integer).");
            return 0;
    }
}

template <typename T>
static inline double load_as_double(const void* p, mwSize idx) {
    return (double)((const T*)p)[idx];
}

static inline double load_double_any(const mxArray* a, const void* p, mwSize idx) {
    switch (mxGetClassID(a)) {
        case mxDOUBLE_CLASS: return load_as_double<double>(p, idx);
        case mxSINGLE_CLASS: return load_as_double<float>(p, idx);
        case mxINT8_CLASS:   return load_as_double<int8_T>(p, idx);
        case mxUINT8_CLASS:  return load_as_double<uint8_T>(p, idx);
        case mxINT16_CLASS:  return load_as_double<int16_T>(p, idx);
        case mxUINT16_CLASS: return load_as_double<uint16_T>(p, idx);
        case mxINT32_CLASS:  return load_as_double<int32_T>(p, idx);
        case mxUINT32_CLASS: return load_as_double<uint32_T>(p, idx);
        case mxINT64_CLASS:  return load_as_double<int64_T>(p, idx);
        case mxUINT64_CLASS: return load_as_double<uint64_T>(p, idx);
        default:
            mexErrMsgIdAndTxt("ind2sub:Type", "LinearInd must be a real numeric array.");
            return 0.0;
    }
}

#if defined(__AVX2__)
// AVX2/FMA fast path for double LinearInd
static inline void simd_double_ind2sub(uint32_T* RESTRICT outI,
                                      uint32_T* RESTRICT outJ,
                                      const double* RESTRICT ind,
                                      mwSize n, double size1d)
{
    const __m256d vOne   = _mm256_set1_pd(1.0);
    const __m256d vSize1 = _mm256_set1_pd(size1d);

    mwSize k = 0;
    for (; k + 4 <= n; k += 4) {
        __m256d vInd = _mm256_loadu_pd(ind + k);
        __m256d vT   = _mm256_sub_pd(vInd, vOne);   // t = ind-1
        __m256d vQ   = _mm256_div_pd(vT, vSize1);   // q = t/size1
        __m256d vQf  = _mm256_floor_pd(vQ);         // floor(q)

        __m256d vJd  = _mm256_add_pd(vQf, vOne);    // J = qf+1

        __m256d vId;
        #if defined(__FMA__)
            // I = t - qf*size1 + 1
            vId = _mm256_fmadd_pd(_mm256_sub_pd(_mm256_setzero_pd(), vQf),
                                  vSize1,
                                  _mm256_add_pd(vT, vOne));
        #else
            vId = _mm256_add_pd(_mm256_sub_pd(vT, _mm256_mul_pd(vQf, vSize1)), vOne);
        #endif

        __m128i vJi = _mm256_cvttpd_epi32(vJd);
        __m128i vIi = _mm256_cvttpd_epi32(vId);

        _mm_storeu_si128((__m128i*)(outJ + k), vJi);
        _mm_storeu_si128((__m128i*)(outI + k), vIi);
    }

    for (; k < n; ++k) {
        const double t  = ind[k] - 1.0;
        const double qf = std::floor(t / size1d);
        outJ[k] = (uint32_T)(qf + 1.0);
        outI[k] = (uint32_T)(t - qf * size1d + 1.0);
    }
}
#endif

void mexFunction(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[]) {
    if (nrhs != 2) mexErrMsgIdAndTxt("ind2sub:nrhs", "Usage: [I,J] = ind2sub_mex(SizeMat, LinearInd)");
    if (nlhs != 2) mexErrMsgIdAndTxt("ind2sub:nlhs", "Two outputs required: [I,J].");

    const mxArray* aSize = prhs[0];
    const mxArray* aInd  = prhs[1];

    if (!mxIsNumeric(aInd) || mxIsComplex(aInd))
        mexErrMsgIdAndTxt("ind2sub:Type", "LinearInd must be a real numeric array.");

    const uint32_T size1 = load_size_u32(aSize, 0);
    (void)load_size_u32(aSize, 1); // Size2 not needed (no bounds checking)

    const mwSize n = mxGetNumberOfElements(aInd);

    const mwSize nd = mxGetNumberOfDimensions(aInd);
    const mwSize* dims = mxGetDimensions(aInd);

    plhs[0] = mxCreateNumericArray(nd, dims, mxUINT32_CLASS, mxREAL);
    plhs[1] = mxCreateNumericArray(nd, dims, mxUINT32_CLASS, mxREAL);

    uint32_T* RESTRICT outI = (uint32_T*)mxGetData(plhs[0]);
    uint32_T* RESTRICT outJ = (uint32_T*)mxGetData(plhs[1]);

    const void* pInd = mxGetData(aInd);
    const double size1d = (double)size1;

#if defined(__AVX2__)
    if (mxGetClassID(aInd) == mxDOUBLE_CLASS) {
        simd_double_ind2sub(outI, outJ, (const double*)pInd, n, size1d);
        return;
    }
#endif

    // Scalar fallback (double math, still fast)
    for (mwSize k = 0; k < n; ++k) {
        const double ind = load_double_any(aInd, pInd, k);
        const double t   = ind - 1.0;
        const double qf  = std::floor(t / size1d);
        outJ[k] = (uint32_T)(qf + 1.0);
        outI[k] = (uint32_T)(t - qf * size1d + 1.0);
    }
}
