#include "mex.h"
#include <cmath>
#include <cstdint>
#include <cstring>

// ============================================================
// fastLogLUT_double_mex.cpp
//
// Approximate natural log for DOUBLE arrays only.
// - N-D input/output
// - Very fast LUT-based approximation
// - Target accuracy comfortably better than 1e-4
// - Hot path optimized for positive finite normal doubles
// - Slow path handles:
//      negative  -> NaN
//      zero      -> -Inf
//      +Inf      -> +Inf
//      NaN       -> NaN
//      subnormal -> handled correctly
//
// Main tricks used:
// 1. 256-entry centered LUT
// 2. Linear residual approximation: log(1+r) ~ r
// 3. Bit-based fast/slow classification
// 4. Unrolled loop by 4
// 5. Block-level mask: one branch per 4 values in common case
// 6. restrict pointers for better optimization
// ============================================================

// ============================================================
// Configuration
// ============================================================
static constexpr int LUT_BITS_D = 8;
static constexpr int LUT_SIZE_D = 1 << LUT_BITS_D;
static constexpr double LN2_D = 0.693147180559945309417232121458176568;

// ============================================================
// Tables
// ============================================================
static bool TablesInitialized = false;
alignas(64) static double LogTabD[LUT_SIZE_D];
alignas(64) static double InvTabD[LUT_SIZE_D];

static void initTables()
{
    if (TablesInitialized) {
        return;
    }

    for (int k = 0; k < LUT_SIZE_D; ++k) {
        double m0 = 1.0 + (static_cast<double>(k) + 0.5) / static_cast<double>(LUT_SIZE_D);
        LogTabD[k] = std::log(m0);
        InvTabD[k] = 1.0 / m0;
    }

    TablesInitialized = true;
}

// ============================================================
// Bit conversion helpers
// ============================================================
inline uint64_t asUint64(double x)
{
    uint64_t u;
    std::memcpy(&u, &x, sizeof(u));
    return u;
}

inline double asDouble(uint64_t u)
{
    double x;
    std::memcpy(&x, &u, sizeof(x));
    return x;
}

// ============================================================
// Classification helpers
// ============================================================
// Fast path condition:
//   sign == 0
//   exponent != 0          (not zero, not subnormal)
//   exponent != all ones   (not inf, not nan)
//
// bits layout for double:
//   sign: bit 63
//   exp : bits 62:52
//   frac: bits 51:0
// ============================================================
inline bool isFastPathDouble(uint64_t bits)
{
    const uint64_t sign = bits & 0x8000000000000000ULL;
    const uint64_t exp  = bits & 0x7FF0000000000000ULL;

    return (sign == 0ULL) &&
           (exp  != 0ULL) &&
           (exp  != 0x7FF0000000000000ULL);
}

// ============================================================
// Hot path for positive finite normal DOUBLE
// No checks here.
// ============================================================
inline double fastLogNormalPositiveDoubleBits(uint64_t bits)
{
    // exponent
    int e = static_cast<int>((bits >> 52) & 0x7FFu) - 1023;

    // mantissa fraction
    uint64_t frac = bits & 0x000FFFFFFFFFFFFFULL;

    // LUT index from top LUT_BITS_D mantissa bits
    int idx = static_cast<int>(frac >> (52 - LUT_BITS_D));

    // m in [1,2)
    double m = asDouble(frac | 0x3FF0000000000000ULL);

    // residual around LUT center
    double r = m * InvTabD[idx] - 1.0;

    // log(x) ~ e*ln2 + log(m0) + r
    return static_cast<double>(e) * LN2_D + LogTabD[idx] + r;
}

// ============================================================
// Slow path
// Handles all special / rare cases correctly.
// ============================================================
inline double fastLogSlowDouble(double x)
{
    if (mxIsNaN(x)) {
        return x;
    }

    if (mxIsInf(x)) {
        return (x > 0.0) ? x : mxGetNaN();
    }

    if (x == 0.0) {
        return -mxGetInf();
    }

    if (x < 0.0) {
        return mxGetNaN();
    }

    // Positive subnormal:
    // scale by 2^52, then compensate exponent
    x *= 4503599627370496.0; // 2^52
    uint64_t bits = asUint64(x);

    int e = static_cast<int>((bits >> 52) & 0x7FFu) - 1023 - 52;

    uint64_t frac = bits & 0x000FFFFFFFFFFFFFULL;
    int idx = static_cast<int>(frac >> (52 - LUT_BITS_D));

    double m = asDouble(frac | 0x3FF0000000000000ULL);
    double r = m * InvTabD[idx] - 1.0;

    return static_cast<double>(e) * LN2_D + LogTabD[idx] + r;
}

// ============================================================
// Main processing
// Unrolled by 4.
// Uses one block-level branch in the common case.
// ============================================================
void processDouble(const double* __restrict__ X, double* __restrict__ Y, mwSize N)
{
    mwSize i = 0;

    for (; i + 3 < N; i += 4) {
        uint64_t b0 = asUint64(X[i]);
        uint64_t b1 = asUint64(X[i + 1]);
        uint64_t b2 = asUint64(X[i + 2]);
        uint64_t b3 = asUint64(X[i + 3]);

        bool f0 = isFastPathDouble(b0);
        bool f1 = isFastPathDouble(b1);
        bool f2 = isFastPathDouble(b2);
        bool f3 = isFastPathDouble(b3);

        // Common fast block: all 4 values are positive finite normal
        if (f0 & f1 & f2 & f3) {
            Y[i]     = fastLogNormalPositiveDoubleBits(b0);
            Y[i + 1] = fastLogNormalPositiveDoubleBits(b1);
            Y[i + 2] = fastLogNormalPositiveDoubleBits(b2);
            Y[i + 3] = fastLogNormalPositiveDoubleBits(b3);
        } else {
            Y[i]     = f0 ? fastLogNormalPositiveDoubleBits(b0) : fastLogSlowDouble(X[i]);
            Y[i + 1] = f1 ? fastLogNormalPositiveDoubleBits(b1) : fastLogSlowDouble(X[i + 1]);
            Y[i + 2] = f2 ? fastLogNormalPositiveDoubleBits(b2) : fastLogSlowDouble(X[i + 2]);
            Y[i + 3] = f3 ? fastLogNormalPositiveDoubleBits(b3) : fastLogSlowDouble(X[i + 3]);
        }
    }

    for (; i < N; ++i) {
        uint64_t bits = asUint64(X[i]);
        Y[i] = isFastPathDouble(bits) ? fastLogNormalPositiveDoubleBits(bits)
                                      : fastLogSlowDouble(X[i]);
    }
}

// ============================================================
// MEX gateway
// ============================================================
void mexFunction(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[])
{
    initTables();

    if (nrhs != 1) {
        mexErrMsgIdAndTxt("fastLogLUT_double_mex:Input", "One input required.");
    }

    if (nlhs > 1) {
        mexErrMsgIdAndTxt("fastLogLUT_double_mex:Output", "One output only.");
    }

    const mxArray* A = prhs[0];

    if (!mxIsDouble(A) || mxIsComplex(A) || mxIsSparse(A)) {
        mexErrMsgIdAndTxt("fastLogLUT_double_mex:Type",
                          "Input must be a full, real DOUBLE array.");
    }

    mwSize N = mxGetNumberOfElements(A);
    mwSize Nd = mxGetNumberOfDimensions(A);
    const mwSize* Dims = mxGetDimensions(A);

    plhs[0] = mxCreateNumericArray(Nd, Dims, mxDOUBLE_CLASS, mxREAL);

    const double* X = static_cast<const double*>(mxGetData(A));
    double* Y = static_cast<double*>(mxGetData(plhs[0]));

    processDouble(X, Y, N);
}
