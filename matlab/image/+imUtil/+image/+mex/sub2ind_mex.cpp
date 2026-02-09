#include "mex.h"
#include <cstdint>

#if defined(_MSC_VER)
  #include <intrin.h>
  #define RESTRICT __restrict
#else
  #include <immintrin.h>
  #define RESTRICT __restrict__
#endif

// ---------------- Size(1) loader (as uint32) ----------------
static inline uint32_T size1_as_u32(const mxArray* s) {
    if (!mxIsNumeric(s) || mxIsComplex(s) || mxGetNumberOfElements(s) < 1) {
        mexErrMsgIdAndTxt("linind:Size", "Size must be a real numeric array with at least 1 element.");
    }
    const void* ps = mxGetData(s);
    switch (mxGetClassID(s)) {
        case mxDOUBLE_CLASS: return (uint32_T)((const double*)ps)[0];
        case mxSINGLE_CLASS: return (uint32_T)((const float*)ps)[0];

        case mxINT8_CLASS:   return (uint32_T)((const int8_T*)ps)[0];
        case mxUINT8_CLASS:  return (uint32_T)((const uint8_T*)ps)[0];
        case mxINT16_CLASS:  return (uint32_T)((const int16_T*)ps)[0];
        case mxUINT16_CLASS: return (uint32_T)((const uint16_T*)ps)[0];
        case mxINT32_CLASS:  return (uint32_T)((const int32_T*)ps)[0];
        case mxUINT32_CLASS: return (uint32_T)((const uint32_T*)ps)[0];
        case mxINT64_CLASS:  return (uint32_T)((const int64_T*)ps)[0];
        case mxUINT64_CLASS: return (uint32_T)((const uint64_T*)ps)[0];

        default:
            mexErrMsgIdAndTxt("linind:SizeType", "Size must be numeric (single/double or integer).");
            return 0;
    }
}

// ---------------- Generic scalar loader to uint32 ----------------
template <typename T>
static inline uint32_T load_as_u32(const void* p, mwSize idx) {
    return (uint32_T)((const T*)p)[idx]; // trunc for floats via cast in C++ too
}

static inline uint32_T load_u32_any(const mxArray* a, const void* p, mwSize idx) {
    switch (mxGetClassID(a)) {
        case mxDOUBLE_CLASS: return load_as_u32<double>(p, idx);
        case mxSINGLE_CLASS: return load_as_u32<float>(p, idx);

        case mxINT8_CLASS:   return load_as_u32<int8_T>(p, idx);
        case mxUINT8_CLASS:  return load_as_u32<uint8_T>(p, idx);
        case mxINT16_CLASS:  return load_as_u32<int16_T>(p, idx);
        case mxUINT16_CLASS: return load_as_u32<uint16_T>(p, idx);
        case mxINT32_CLASS:  return load_as_u32<int32_T>(p, idx);
        case mxUINT32_CLASS: return load_as_u32<uint32_T>(p, idx);
        case mxINT64_CLASS:  return load_as_u32<int64_T>(p, idx);
        case mxUINT64_CLASS: return load_as_u32<uint64_T>(p, idx);

        default:
            mexErrMsgIdAndTxt("linind:Type", "I and J must be numeric (single/double or integer).");
            return 0;
    }
}

// ---------------- SIMD kernels (AVX2) ----------------
#if defined(__AVX2__)

// uint32/int32 kernel: out = I + (J-1)*size1  (wraps mod 2^32 like uint32 math)
static inline void simd_u32_u32(uint32_T* RESTRICT out,
                               const uint32_T* RESTRICT I,
                               const uint32_T* RESTRICT J,
                               mwSize n, uint32_T size1)
{
    const __m256i vSize1 = _mm256_set1_epi32((int)size1);
    const __m256i vOne   = _mm256_set1_epi32(1);
    mwSize k = 0;

    for (; k + 8 <= n; k += 8) {
        __m256i vI = _mm256_loadu_si256((const __m256i*)(I + k));
        __m256i vJ = _mm256_loadu_si256((const __m256i*)(J + k));
        vJ = _mm256_sub_epi32(vJ, vOne);
        __m256i vBase = _mm256_mullo_epi32(vJ, vSize1);
        __m256i vOut  = _mm256_add_epi32(vI, vBase);
        _mm256_storeu_si256((__m256i*)(out + k), vOut);
    }
    for (; k < n; ++k) {
        out[k] = I[k] + (J[k] - 1u) * size1;
    }
}

static inline void simd_u32_scalarI(uint32_T* RESTRICT out,
                                   uint32_T I0,
                                   const uint32_T* RESTRICT J,
                                   mwSize n, uint32_T size1)
{
    const __m256i vSize1 = _mm256_set1_epi32((int)size1);
    const __m256i vOne   = _mm256_set1_epi32(1);
    const __m256i vI     = _mm256_set1_epi32((int)I0);

    mwSize k = 0;
    for (; k + 8 <= n; k += 8) {
        __m256i vJ = _mm256_loadu_si256((const __m256i*)(J + k));
        vJ = _mm256_sub_epi32(vJ, vOne);
        __m256i vBase = _mm256_mullo_epi32(vJ, vSize1);
        __m256i vOut  = _mm256_add_epi32(vI, vBase);
        _mm256_storeu_si256((__m256i*)(out + k), vOut);
    }
    for (; k < n; ++k) out[k] = I0 + (J[k] - 1u) * size1;
}

static inline void simd_u32_scalarJ(uint32_T* RESTRICT out,
                                   const uint32_T* RESTRICT I,
                                   uint32_T J0,
                                   mwSize n, uint32_T size1)
{
    const uint32_T base = (J0 - 1u) * size1;
    const __m256i vBase = _mm256_set1_epi32((int)base);

    mwSize k = 0;
    for (; k + 8 <= n; k += 8) {
        __m256i vI = _mm256_loadu_si256((const __m256i*)(I + k));
        __m256i vOut = _mm256_add_epi32(vI, vBase);
        _mm256_storeu_si256((__m256i*)(out + k), vOut);
    }
    for (; k < n; ++k) out[k] = I[k] + base;
}

// single kernel: convert trunc to int32 then same math
static inline void simd_single_single(uint32_T* RESTRICT out,
                                      const float* RESTRICT I,
                                      const float* RESTRICT J,
                                      mwSize n, uint32_T size1)
{
    const __m256i vSize1 = _mm256_set1_epi32((int)size1);
    const __m256i vOne   = _mm256_set1_epi32(1);

    mwSize k = 0;
    for (; k + 8 <= n; k += 8) {
        __m256 vIf = _mm256_loadu_ps(I + k);
        __m256 vJf = _mm256_loadu_ps(J + k);
        __m256i vI = _mm256_cvttps_epi32(vIf); // trunc
        __m256i vJ = _mm256_cvttps_epi32(vJf); // trunc
        vJ = _mm256_sub_epi32(vJ, vOne);
        __m256i vBase = _mm256_mullo_epi32(vJ, vSize1);
        __m256i vOut  = _mm256_add_epi32(vI, vBase);
        _mm256_storeu_si256((__m256i*)(out + k), vOut);
    }
    for (; k < n; ++k) {
        const uint32_T Ii = (uint32_T)I[k];
        const uint32_T Jj = (uint32_T)J[k];
        out[k] = Ii + (Jj - 1u) * size1;
    }
}

static inline void simd_single_scalarI(uint32_T* RESTRICT out,
                                       float I0f,
                                       const float* RESTRICT J,
                                       mwSize n, uint32_T size1)
{
    const uint32_T I0 = (uint32_T)I0f;
    // reuse integer scalarI kernel by converting J in SIMD would be extra work; do proper SIMD:
    const __m256i vSize1 = _mm256_set1_epi32((int)size1);
    const __m256i vOne   = _mm256_set1_epi32(1);
    const __m256i vI     = _mm256_set1_epi32((int)I0);

    mwSize k = 0;
    for (; k + 8 <= n; k += 8) {
        __m256 vJf = _mm256_loadu_ps(J + k);
        __m256i vJ = _mm256_cvttps_epi32(vJf);
        vJ = _mm256_sub_epi32(vJ, vOne);
        __m256i vBase = _mm256_mullo_epi32(vJ, vSize1);
        __m256i vOut  = _mm256_add_epi32(vI, vBase);
        _mm256_storeu_si256((__m256i*)(out + k), vOut);
    }
    for (; k < n; ++k) {
        const uint32_T Jj = (uint32_T)J[k];
        out[k] = I0 + (Jj - 1u) * size1;
    }
}

static inline void simd_single_scalarJ(uint32_T* RESTRICT out,
                                       const float* RESTRICT I,
                                       float J0f,
                                       mwSize n, uint32_T size1)
{
    const uint32_T J0 = (uint32_T)J0f;
    const uint32_T base = (J0 - 1u) * size1;
    const __m256i vBase = _mm256_set1_epi32((int)base);

    mwSize k = 0;
    for (; k + 8 <= n; k += 8) {
        __m256 vIf = _mm256_loadu_ps(I + k);
        __m256i vI = _mm256_cvttps_epi32(vIf);
        __m256i vOut = _mm256_add_epi32(vI, vBase);
        _mm256_storeu_si256((__m256i*)(out + k), vOut);
    }
    for (; k < n; ++k) {
        out[k] = (uint32_T)I[k] + base;
    }
}

// double kernel: process 4 doubles at a time (AVX2 converts 4 doubles -> 4 int32)
static inline void simd_double_double(uint32_T* RESTRICT out,
                                      const double* RESTRICT I,
                                      const double* RESTRICT J,
                                      mwSize n, uint32_T size1)
{
    const __m128i vSize1 = _mm_set1_epi32((int)size1);
    const __m128i vOne   = _mm_set1_epi32(1);

    mwSize k = 0;
    for (; k + 4 <= n; k += 4) {
        __m256d vId = _mm256_loadu_pd(I + k);
        __m256d vJd = _mm256_loadu_pd(J + k);
        __m128i vI  = _mm256_cvttpd_epi32(vId); // trunc
        __m128i vJ  = _mm256_cvttpd_epi32(vJd); // trunc
        vJ = _mm_sub_epi32(vJ, vOne);
        __m128i vBase = _mm_mullo_epi32(vJ, vSize1);
        __m128i vOut  = _mm_add_epi32(vI, vBase);
        _mm_storeu_si128((__m128i*)(out + k), vOut);
    }
    for (; k < n; ++k) {
        const uint32_T Ii = (uint32_T)I[k];
        const uint32_T Jj = (uint32_T)J[k];
        out[k] = Ii + (Jj - 1u) * size1;
    }
}

static inline void simd_double_scalarI(uint32_T* RESTRICT out,
                                       double I0d,
                                       const double* RESTRICT J,
                                       mwSize n, uint32_T size1)
{
    const uint32_T I0 = (uint32_T)I0d;
    const __m128i vSize1 = _mm_set1_epi32((int)size1);
    const __m128i vOne   = _mm_set1_epi32(1);
    const __m128i vI     = _mm_set1_epi32((int)I0);

    mwSize k = 0;
    for (; k + 4 <= n; k += 4) {
        __m256d vJd = _mm256_loadu_pd(J + k);
        __m128i vJ  = _mm256_cvttpd_epi32(vJd);
        vJ = _mm_sub_epi32(vJ, vOne);
        __m128i vBase = _mm_mullo_epi32(vJ, vSize1);
        __m128i vOut  = _mm_add_epi32(vI, vBase);
        _mm_storeu_si128((__m128i*)(out + k), vOut);
    }
    for (; k < n; ++k) {
        const uint32_T Jj = (uint32_T)J[k];
        out[k] = I0 + (Jj - 1u) * size1;
    }
}

static inline void simd_double_scalarJ(uint32_T* RESTRICT out,
                                       const double* RESTRICT I,
                                       double J0d,
                                       mwSize n, uint32_T size1)
{
    const uint32_T J0 = (uint32_T)J0d;
    const uint32_T base = (J0 - 1u) * size1;
    const __m128i vBase = _mm_set1_epi32((int)base);

    mwSize k = 0;
    for (; k + 4 <= n; k += 4) {
        __m256d vId = _mm256_loadu_pd(I + k);
        __m128i vI  = _mm256_cvttpd_epi32(vId);
        __m128i vOut = _mm_add_epi32(vI, vBase);
        _mm_storeu_si128((__m128i*)(out + k), vOut);
    }
    for (; k < n; ++k) out[k] = (uint32_T)I[k] + base;
}

#endif // __AVX2__

// ---------------- MEX entry ----------------
void mexFunction(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[]) {
    if (nrhs != 3) mexErrMsgIdAndTxt("linind:nrhs", "Usage: Ind = linind_mex(Size, I, J)");
    if (nlhs > 1)  mexErrMsgIdAndTxt("linind:nlhs", "One output only.");

    const mxArray* aSize = prhs[0];
    const mxArray* aI    = prhs[1];
    const mxArray* aJ    = prhs[2];

    if (!mxIsNumeric(aI) || !mxIsNumeric(aJ) || mxIsComplex(aI) || mxIsComplex(aJ))
        mexErrMsgIdAndTxt("linind:Type", "I and J must be real numeric arrays.");

    const mwSize nI = mxGetNumberOfElements(aI);
    const mwSize nJ = mxGetNumberOfElements(aJ);

    mwSize nOut = 0;
    if (nI == nJ) nOut = nI;
    else if (nI == 1) nOut = nJ;
    else if (nJ == 1) nOut = nI;
    else mexErrMsgIdAndTxt("linind:SizeMismatch", "I and J must match in numel, or one must be scalar.");

    const uint32_T size1 = size1_as_u32(aSize);

    // Output dims follow the non-scalar input (or scalar)
    const mxArray* shapeRef = (nI != 1) ? aI : ((nJ != 1) ? aJ : aI);
    const mwSize nd = mxGetNumberOfDimensions(shapeRef);
    const mwSize* dims = mxGetDimensions(shapeRef);

    plhs[0] = mxCreateNumericArray(nd, dims, mxUINT32_CLASS, mxREAL);
    uint32_T* RESTRICT out = (uint32_T*)mxGetData(plhs[0]);

    const void* pI = mxGetData(aI);
    const void* pJ = mxGetData(aJ);

#if defined(__AVX2__)
    // SIMD fast paths when types match and are supported
    const mxClassID tI = mxGetClassID(aI);
    const mxClassID tJ = mxGetClassID(aJ);

    if (tI == tJ) {
        // uint32
        if (tI == mxUINT32_CLASS) {
            const uint32_T* I = (const uint32_T*)pI;
            const uint32_T* J = (const uint32_T*)pJ;
            if (nI == 1 && nJ == 1) { out[0] = I[0] + (J[0] - 1u) * size1; return; }
            if (nI == 1) { simd_u32_scalarI(out, I[0], J, nOut, size1); return; }
            if (nJ == 1) { simd_u32_scalarJ(out, I, J[0], nOut, size1); return; }
            simd_u32_u32(out, I, J, nOut, size1);
            return;
        }

        // int32 (treat values as indices; cast to uint32 with wrap like C)
        if (tI == mxINT32_CLASS) {
            const int32_T* Ii = (const int32_T*)pI;
            const int32_T* Jj = (const int32_T*)pJ;

            // Reinterpret to uint32 is fine for raw bits; but indices are typically positive anyway.
            const uint32_T* I = (const uint32_T*)Ii;
            const uint32_T* J = (const uint32_T*)Jj;

            if (nI == 1 && nJ == 1) { out[0] = (uint32_T)Ii[0] + ((uint32_T)Jj[0] - 1u) * size1; return; }
            if (nI == 1) { simd_u32_scalarI(out, (uint32_T)Ii[0], J, nOut, size1); return; }
            if (nJ == 1) { simd_u32_scalarJ(out, I, (uint32_T)Jj[0], nOut, size1); return; }
            simd_u32_u32(out, I, J, nOut, size1);
            return;
        }

        // single
        if (tI == mxSINGLE_CLASS) {
            const float* I = (const float*)pI;
            const float* J = (const float*)pJ;
            if (nI == 1 && nJ == 1) { out[0] = (uint32_T)I[0] + ((uint32_T)J[0] - 1u) * size1; return; }
            if (nI == 1) { simd_single_scalarI(out, I[0], J, nOut, size1); return; }
            if (nJ == 1) { simd_single_scalarJ(out, I, J[0], nOut, size1); return; }
            simd_single_single(out, I, J, nOut, size1);
            return;
        }

        // double
        if (tI == mxDOUBLE_CLASS) {
            const double* I = (const double*)pI;
            const double* J = (const double*)pJ;
            if (nI == 1 && nJ == 1) { out[0] = (uint32_T)I[0] + ((uint32_T)J[0] - 1u) * size1; return; }
            if (nI == 1) { simd_double_scalarI(out, I[0], J, nOut, size1); return; }
            if (nJ == 1) { simd_double_scalarJ(out, I, J[0], nOut, size1); return; }
            simd_double_double(out, I, J, nOut, size1);
            return;
        }
    }
#endif

    // Generic scalar fallback (still fast, but no SIMD)
    if (nI == 1 && nJ == 1) {
        const uint32_T Ii = load_u32_any(aI, pI, 0);
        const uint32_T Jj = load_u32_any(aJ, pJ, 0);
        out[0] = Ii + (Jj - 1u) * size1;
        return;
    }
    if (nI == 1) {
        const uint32_T Ii = load_u32_any(aI, pI, 0);
        for (mwSize k = 0; k < nOut; ++k) {
            const uint32_T Jj = load_u32_any(aJ, pJ, k);
            out[k] = Ii + (Jj - 1u) * size1;
        }
        return;
    }
    if (nJ == 1) {
        const uint32_T Jj = load_u32_any(aJ, pJ, 0);
        const uint32_T base = (Jj - 1u) * size1;
        for (mwSize k = 0; k < nOut; ++k) {
            const uint32_T Ii = load_u32_any(aI, pI, k);
            out[k] = Ii + base;
        }
        return;
    }
    for (mwSize k = 0; k < nOut; ++k) {
        const uint32_T Ii = load_u32_any(aI, pI, k);
        const uint32_T Jj = load_u32_any(aJ, pJ, k);
        out[k] = Ii + (Jj - 1u) * size1;
    }
}
