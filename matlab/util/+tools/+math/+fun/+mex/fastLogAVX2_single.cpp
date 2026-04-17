#include "mex.h"
#include <immintrin.h>
#include <cmath>
#include <cstdint>
#include <cstring>

// ============================================================
// fastLogAVX2_single_mex.cpp
//
// Approximate natural log for SINGLE arrays only.
// Optimized for speed on AVX2-capable CPUs.
//
// Tricks used:
// 1. AVX2 fast path for 8 floats
// 2. Block-level mask: one branch per 8 lanes
// 3. Unrolled main loop by 2 blocks (16 floats/iter)
// 4. Separate hot scalar path for positive finite normal values
// 5. Slow scalar path only for exceptional values
// 6. 128-entry centered LUT
// 7. Linear residual approximation: log(1+r) ~ r
// ============================================================

// ============================================================
// Configuration
// ============================================================
static constexpr int LUT_BITS_F = 7;   // 128-entry LUT
static constexpr int LUT_SIZE_F = 1 << LUT_BITS_F;
static constexpr float LN2_F = 0.6931471805599453094f;

// ============================================================
// Tables
// ============================================================
static bool TablesInitialized = false;
alignas(64) static float LogTabF[LUT_SIZE_F];
alignas(64) static float InvTabF[LUT_SIZE_F];

static void initTables()
{
    if (TablesInitialized) {
        return;
    }

    for (int k = 0; k < LUT_SIZE_F; ++k) {
        double m0 = 1.0 + (static_cast<double>(k) + 0.5) / static_cast<double>(LUT_SIZE_F);
        LogTabF[k] = static_cast<float>(std::log(m0));
        InvTabF[k] = static_cast<float>(1.0 / m0);
    }

    TablesInitialized = true;
}

// ============================================================
// Bit helpers
// ============================================================
inline uint32_t asUint32(float x)
{
    uint32_t u;
    std::memcpy(&u, &x, sizeof(u));
    return u;
}

inline float asFloat(uint32_t u)
{
    float x;
    std::memcpy(&x, &u, sizeof(x));
    return x;
}

// ============================================================
// Classification
// Fast path iff:
//   sign == 0
//   exponent != 0
//   exponent != all ones
// ============================================================
inline bool isFastPathFloat(uint32_t bits)
{
    const uint32_t sign = bits & 0x80000000u;
    const uint32_t exp  = bits & 0x7F800000u;

    return (sign == 0u) &&
           (exp  != 0u) &&
           (exp  != 0x7F800000u);
}

// ============================================================
// Hot scalar path: positive finite normal only
// ============================================================
inline float fastLogNormalPositiveFloatBits(uint32_t bits)
{
    int e = static_cast<int>((bits >> 23) & 0xFFu) - 127;

    uint32_t frac = bits & 0x007FFFFFu;
    int idx = static_cast<int>(frac >> (23 - LUT_BITS_F));

    float m = asFloat(frac | 0x3F800000u);
    float r = m * InvTabF[idx] - 1.0f;

    return static_cast<float>(e) * LN2_F + LogTabF[idx] + r;
}

// ============================================================
// Slow scalar path
// ============================================================
inline float fastLogSlowFloat(float x)
{
    if (mxIsNaN(x)) {
        return x;
    }

    if (mxIsInf(x)) {
        return (x > 0.0f) ? x : mxGetNaN();
    }

    if (x == 0.0f) {
        return -mxGetInf();
    }

    if (x < 0.0f) {
        return mxGetNaN();
    }

    // positive subnormal
    x *= 8388608.0f; // 2^23
    uint32_t bits = asUint32(x);

    int e = static_cast<int>((bits >> 23) & 0xFFu) - 127 - 23;

    uint32_t frac = bits & 0x007FFFFFu;
    int idx = static_cast<int>(frac >> (23 - LUT_BITS_F));

    float m = asFloat(frac | 0x3F800000u);
    float r = m * InvTabF[idx] - 1.0f;

    return static_cast<float>(e) * LN2_F + LogTabF[idx] + r;
}

// ============================================================
// AVX2 block of 8 floats
// Returns true if fast AVX path used, false if caller should
// do scalar fallback for this block.
// ============================================================
inline bool fastLogVec8Float_try(const float* X, float* Y)
{
    const __m256  one_ps = _mm256_set1_ps(1.0f);
    const __m256  ln2_ps = _mm256_set1_ps(LN2_F);

    const __m256i signMask = _mm256_set1_epi32(0x80000000u);
    const __m256i expMask  = _mm256_set1_epi32(0x7F800000u);
    const __m256i fracMask = _mm256_set1_epi32(0x007FFFFFu);
    const __m256i expBias  = _mm256_set1_epi32(127);
    const __m256i oneBits  = _mm256_set1_epi32(0x3F800000u);
    const __m256i zero_i   = _mm256_setzero_si256();
    const __m256i expAll1  = _mm256_set1_epi32(0x7F800000u);
    const __m256i allOnesI = _mm256_set1_epi32(-1);

    __m256 x   = _mm256_loadu_ps(X);
    __m256i xi = _mm256_castps_si256(x);

    __m256i signBits = _mm256_and_si256(xi, signMask);
    __m256i expBits  = _mm256_and_si256(xi, expMask);

    __m256i signZero = _mm256_cmpeq_epi32(signBits, zero_i);
    __m256i expZero  = _mm256_cmpeq_epi32(expBits, zero_i);
    __m256i expOnes  = _mm256_cmpeq_epi32(expBits, expAll1);

    __m256i badMaskI    = _mm256_or_si256(expZero, expOnes);
    __m256i normalMaskI = _mm256_and_si256(signZero, _mm256_andnot_si256(badMaskI, allOnesI));

    if (_mm256_movemask_ps(_mm256_castsi256_ps(normalMaskI)) != 0xFF) {
        return false;
    }

    __m256i expRaw = _mm256_srli_epi32(expBits, 23);
    __m256i e_i    = _mm256_sub_epi32(expRaw, expBias);

    __m256i frac   = _mm256_and_si256(xi, fracMask);
    __m256i idx_i  = _mm256_srli_epi32(frac, 23 - LUT_BITS_F);
    __m256i mBits  = _mm256_or_si256(frac, oneBits);

    __m256 e_ps    = _mm256_cvtepi32_ps(e_i);
    __m256 m_ps    = _mm256_castsi256_ps(mBits);

    __m256 logtab  = _mm256_i32gather_ps(LogTabF, idx_i, 4);
    __m256 invtab  = _mm256_i32gather_ps(InvTabF, idx_i, 4);

    __m256 r_ps    = _mm256_sub_ps(_mm256_mul_ps(m_ps, invtab), one_ps);
    __m256 y_ps    = _mm256_add_ps(_mm256_add_ps(_mm256_mul_ps(e_ps, ln2_ps), logtab), r_ps);

    _mm256_storeu_ps(Y, y_ps);
    return true;
}

// ============================================================
// Scalar fallback for one 8-float block
// ============================================================
inline void fastLogVec8Float_scalarFallback(const float* X, float* Y)
{
    uint32_t b0 = asUint32(X[0]);
    uint32_t b1 = asUint32(X[1]);
    uint32_t b2 = asUint32(X[2]);
    uint32_t b3 = asUint32(X[3]);
    uint32_t b4 = asUint32(X[4]);
    uint32_t b5 = asUint32(X[5]);
    uint32_t b6 = asUint32(X[6]);
    uint32_t b7 = asUint32(X[7]);

    bool f0 = isFastPathFloat(b0);
    bool f1 = isFastPathFloat(b1);
    bool f2 = isFastPathFloat(b2);
    bool f3 = isFastPathFloat(b3);
    bool f4 = isFastPathFloat(b4);
    bool f5 = isFastPathFloat(b5);
    bool f6 = isFastPathFloat(b6);
    bool f7 = isFastPathFloat(b7);

    Y[0] = f0 ? fastLogNormalPositiveFloatBits(b0) : fastLogSlowFloat(X[0]);
    Y[1] = f1 ? fastLogNormalPositiveFloatBits(b1) : fastLogSlowFloat(X[1]);
    Y[2] = f2 ? fastLogNormalPositiveFloatBits(b2) : fastLogSlowFloat(X[2]);
    Y[3] = f3 ? fastLogNormalPositiveFloatBits(b3) : fastLogSlowFloat(X[3]);
    Y[4] = f4 ? fastLogNormalPositiveFloatBits(b4) : fastLogSlowFloat(X[4]);
    Y[5] = f5 ? fastLogNormalPositiveFloatBits(b5) : fastLogSlowFloat(X[5]);
    Y[6] = f6 ? fastLogNormalPositiveFloatBits(b6) : fastLogSlowFloat(X[6]);
    Y[7] = f7 ? fastLogNormalPositiveFloatBits(b7) : fastLogSlowFloat(X[7]);
}

// ============================================================
// Main processing
// Unrolled by 2 AVX blocks = 16 floats/iter
// ============================================================
void processFloat(const float* __restrict__ X, float* __restrict__ Y, mwSize N)
{
    mwSize i = 0;

    for (; i + 15 < N; i += 16) {
        bool ok0 = fastLogVec8Float_try(X + i,     Y + i);
        bool ok1 = fastLogVec8Float_try(X + i + 8, Y + i + 8);

        if (!ok0) {
            fastLogVec8Float_scalarFallback(X + i, Y + i);
        }
        if (!ok1) {
            fastLogVec8Float_scalarFallback(X + i + 8, Y + i + 8);
        }
    }

    for (; i + 7 < N; i += 8) {
        if (!fastLogVec8Float_try(X + i, Y + i)) {
            fastLogVec8Float_scalarFallback(X + i, Y + i);
        }
    }

    for (; i < N; ++i) {
        uint32_t bits = asUint32(X[i]);
        Y[i] = isFastPathFloat(bits) ? fastLogNormalPositiveFloatBits(bits)
                                     : fastLogSlowFloat(X[i]);
    }
}

// ============================================================
// MEX gateway
// ============================================================
void mexFunction(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[])
{
    initTables();

    if (nrhs != 1) {
        mexErrMsgIdAndTxt("fastLogAVX2_single_mex:Input", "One input required.");
    }

    if (nlhs > 1) {
        mexErrMsgIdAndTxt("fastLogAVX2_single_mex:Output", "One output only.");
    }

    const mxArray* A = prhs[0];

    if (!mxIsSingle(A) || mxIsComplex(A) || mxIsSparse(A)) {
        mexErrMsgIdAndTxt("fastLogAVX2_single_mex:Type",
                          "Input must be a full, real SINGLE array.");
    }

    mwSize N = mxGetNumberOfElements(A);
    mwSize Nd = mxGetNumberOfDimensions(A);
    const mwSize* Dims = mxGetDimensions(A);

    plhs[0] = mxCreateNumericArray(Nd, Dims, mxSINGLE_CLASS, mxREAL);

    const float* X = static_cast<const float*>(mxGetData(A));
    float* Y = static_cast<float*>(mxGetData(plhs[0]));

    processFloat(X, Y, N);
}
