#include "mex.h"
#include <immintrin.h>
#include <cstdint>
#include <cmath>

static void die(const char* msg) {
    mexErrMsgIdAndTxt("countNaN:err", "%s", msg);
}

static inline uint64_t popcnt_u32(unsigned x) {
#if defined(__GNUG__) || defined(__clang__)
    return (uint64_t)__builtin_popcount(x);
#else
    uint64_t c = 0;
    while (x) { x &= (x - 1); ++c; }
    return c;
#endif
}

static uint64_t count_nans_double_real_avx2(const double* x, mwSize n) {
    uint64_t cnt = 0;
    mwSize i = 0;

    for (; i + 8 <= n; i += 8) {
        __m256d v0 = _mm256_loadu_pd(x + i);
        __m256d v1 = _mm256_loadu_pd(x + i + 4);
        __m256d m0 = _mm256_cmp_pd(v0, v0, _CMP_UNORD_Q);
        __m256d m1 = _mm256_cmp_pd(v1, v1, _CMP_UNORD_Q);
        cnt += popcnt_u32((unsigned)_mm256_movemask_pd(m0));
        cnt += popcnt_u32((unsigned)_mm256_movemask_pd(m1));
    }
    for (; i + 4 <= n; i += 4) {
        __m256d v  = _mm256_loadu_pd(x + i);
        __m256d m  = _mm256_cmp_pd(v, v, _CMP_UNORD_Q);
        cnt += popcnt_u32((unsigned)_mm256_movemask_pd(m));
    }
    for (; i < n; ++i) cnt += (uint64_t)std::isnan(x[i]);
    return cnt;
}

static uint64_t count_nans_single_real_avx2(const float* x, mwSize n) {
    uint64_t cnt = 0;
    mwSize i = 0;

    for (; i + 16 <= n; i += 16) {
        __m256 v0 = _mm256_loadu_ps(x + i);
        __m256 v1 = _mm256_loadu_ps(x + i + 8);
        __m256 m0 = _mm256_cmp_ps(v0, v0, _CMP_UNORD_Q);
        __m256 m1 = _mm256_cmp_ps(v1, v1, _CMP_UNORD_Q);
        cnt += popcnt_u32((unsigned)_mm256_movemask_ps(m0));
        cnt += popcnt_u32((unsigned)_mm256_movemask_ps(m1));
    }
    for (; i + 8 <= n; i += 8) {
        __m256 v  = _mm256_loadu_ps(x + i);
        __m256 m  = _mm256_cmp_ps(v, v, _CMP_UNORD_Q);
        cnt += popcnt_u32((unsigned)_mm256_movemask_ps(m));
    }
    for (; i < n; ++i) cnt += (uint64_t)std::isnan(x[i]);
    return cnt;
}

#if MX_HAS_INTERLEAVED_COMPLEX
static uint64_t count_nans_complex_double(const mxComplexDouble* z, mwSize n) {
    uint64_t cnt = 0;
    for (mwSize i = 0; i < n; ++i)
        cnt += (uint64_t)(std::isnan(z[i].real) | std::isnan(z[i].imag));
    return cnt;
}
static uint64_t count_nans_complex_single(const mxComplexSingle* z, mwSize n) {
    uint64_t cnt = 0;
    for (mwSize i = 0; i < n; ++i)
        cnt += (uint64_t)(std::isnan(z[i].real) | std::isnan(z[i].imag));
    return cnt;
}
#else
static uint64_t count_nans_complex_double_sep(const double* re, const double* im, mwSize n) {
    uint64_t cnt = 0;
    for (mwSize i = 0; i < n; ++i)
        cnt += (uint64_t)(std::isnan(re[i]) | std::isnan(im[i]));
    return cnt;
}
static uint64_t count_nans_complex_single_sep(const float* re, const float* im, mwSize n) {
    uint64_t cnt = 0;
    for (mwSize i = 0; i < n; ++i)
        cnt += (uint64_t)(std::isnan(re[i]) | std::isnan(im[i]));
    return cnt;
}
#endif

void mexFunction(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[]) {
    if (nrhs != 1) die("Usage: N = countNaN(A)");
    if (nlhs > 1)  die("One output only.");

    const mxArray* A = prhs[0];

    // Output uint64 scalar
    plhs[0] = mxCreateNumericMatrix(1, 1, mxUINT64_CLASS, mxREAL);
    uint64_t* out = (uint64_t*)mxGetData(plhs[0]);
    *out = 0;

    // Non-numeric/logical => 0 (fast)
    if (!(mxIsNumeric(A) || mxIsLogical(A))) return;

    // Integers/logical can't contain NaN
    const mxClassID cid = mxGetClassID(A);
    if (!(cid == mxDOUBLE_CLASS || cid == mxSINGLE_CLASS)) return;

    // For sparse: count stored values only
    const mwSize n = mxIsSparse(A) ? (mwSize)mxGetNzmax(A)
                                  : (mwSize)mxGetNumberOfElements(A);

    if (!mxIsComplex(A)) {
        if (cid == mxDOUBLE_CLASS) {
            *out = count_nans_double_real_avx2((const double*)mxGetData(A), n);
        } else {
            *out = count_nans_single_real_avx2((const float*)mxGetData(A), n);
        }
        return;
    }

    // Complex
#if MX_HAS_INTERLEAVED_COMPLEX
    if (cid == mxDOUBLE_CLASS) {
        *out = count_nans_complex_double(mxGetComplexDoubles(A), n);
    } else {
        *out = count_nans_complex_single(mxGetComplexSingles(A), n);
    }
#else
    // Separate real/imag API
    if (cid == mxDOUBLE_CLASS) {
        const double* re = (const double*)mxGetData(A);
        const double* im = (const double*)mxGetImagData(A);
        *out = count_nans_complex_double_sep(re, im, n);
    } else {
        const float* re = (const float*)mxGetData(A);
        const float* im = (const float*)mxGetImagData(A);
        *out = count_nans_complex_single_sep(re, im, n);
    }
#endif
}
