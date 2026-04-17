#include "mex.h"
#include <immintrin.h>
#include <cstdint>
#include <cstring>

#ifdef _OPENMP
#include <omp.h>
#endif

// ============================================================
// sum2_mex.cpp
//
// Computes:
//   S2 = sum(Array.^2, Dim, 'omitnan')
//
// Supported input:
//   - full, real, single
//   - full, real, double
//
// Notes:
//   - output class matches input class
//   - optimized special branch for Dim==1
//   - AVX2 + OpenMP in Dim==1 branch
//   - generic fallback for other dimensions
// ============================================================

// ============================================================
// Helpers
// ============================================================
static mwSize getDimArg(const mxArray* A)
{
    if (!mxIsNumeric(A) || mxIsComplex(A) || mxGetNumberOfElements(A) != 1) {
        mexErrMsgIdAndTxt("sum2_mex:Dim", "Dim must be a real numeric scalar.");
    }

    double d = mxGetScalar(A);
    if (!(d >= 1.0)) {
        mexErrMsgIdAndTxt("sum2_mex:Dim", "Dim must be a positive integer.");
    }

    mwSize di = static_cast<mwSize>(d);
    if (static_cast<double>(di) != d) {
        mexErrMsgIdAndTxt("sum2_mex:Dim", "Dim must be a positive integer.");
    }

    return di;
}

static inline double hsum256_pd(__m256d v)
{
    __m128d lo = _mm256_castpd256_pd128(v);
    __m128d hi = _mm256_extractf128_pd(v, 1);
    lo = _mm_add_pd(lo, hi);
    __m128d sh = _mm_unpackhi_pd(lo, lo);
    lo = _mm_add_sd(lo, sh);
    return _mm_cvtsd_f64(lo);
}

static inline float hsum256_ps(__m256 v)
{
    __m128 lo = _mm256_castps256_ps128(v);
    __m128 hi = _mm256_extractf128_ps(v, 1);
    lo = _mm_add_ps(lo, hi);
    __m128 sh = _mm_movehdup_ps(lo);
    lo = _mm_add_ps(lo, sh);
    sh = _mm_movehl_ps(sh, lo);
    lo = _mm_add_ss(lo, sh);
    return _mm_cvtss_f32(lo);
}

// ============================================================
// Generic scalar fallback kernels
// Array is viewed as [Inner x Reduce x Outer]
// linear index = inner + r*Inner + outer*(Reduce*Inner)
// ============================================================
static void reduce_generic_double(const double* A,
                                  double* Out,
                                  mwSize Inner,
                                  mwSize Reduce,
                                  mwSize Outer)
{
    const mwSize Nout = Inner * Outer;

    #ifdef _OPENMP
    #pragma omp parallel for schedule(static)
    #endif
    for (mwSignedIndex outIdx = 0; outIdx < static_cast<mwSignedIndex>(Nout); ++outIdx) {
        mwSize inner = static_cast<mwSize>(outIdx) % Inner;
        mwSize outer = static_cast<mwSize>(outIdx) / Inner;
        mwSize base  = outer * (Reduce * Inner) + inner;

        double Sum = 0.0;
        for (mwSize r = 0; r < Reduce; ++r) {
            double x = A[base + r * Inner];
            if (!mxIsNaN(x)) {
                Sum += x * x;
            }
        }

        Out[outIdx] = Sum;
    }
}

static void reduce_generic_single(const float* A,
                                  float* Out,
                                  mwSize Inner,
                                  mwSize Reduce,
                                  mwSize Outer)
{
    const mwSize Nout = Inner * Outer;

    #ifdef _OPENMP
    #pragma omp parallel for schedule(static)
    #endif
    for (mwSignedIndex outIdx = 0; outIdx < static_cast<mwSignedIndex>(Nout); ++outIdx) {
        mwSize inner = static_cast<mwSize>(outIdx) % Inner;
        mwSize outer = static_cast<mwSize>(outIdx) / Inner;
        mwSize base  = outer * (Reduce * Inner) + inner;

        float Sum = 0.0f;
        for (mwSize r = 0; r < Reduce; ++r) {
            float x = A[base + r * Inner];
            if (!mxIsNaN(x)) {
                Sum += x * x;
            }
        }

        Out[outIdx] = Sum;
    }
}

// ============================================================
// Specialized Dim==1 AVX2 kernels
// Each output element is the sum over one contiguous column block.
// ============================================================
static void reduce_dim1_double(const double* __restrict__ A,
                               double* __restrict__ Out,
                               mwSize Reduce,
                               mwSize Outer)
{
    const mwSize VecWidth = 4;
    const mwSize Nvec = (Reduce / VecWidth) * VecWidth;

    #ifdef _OPENMP
    #pragma omp parallel for schedule(static)
    #endif
    for (mwSignedIndex outer = 0; outer < static_cast<mwSignedIndex>(Outer); ++outer) {
        const double* Ptr = A + static_cast<mwSize>(outer) * Reduce;

        __m256d Acc = _mm256_setzero_pd();

        mwSize i = 0;
        for (; i < Nvec; i += VecWidth) {
            __m256d x = _mm256_loadu_pd(Ptr + i);

            // ordered compare: true only for non-NaN values
            __m256d mask = _mm256_cmp_pd(x, x, _CMP_ORD_Q);
            x = _mm256_and_pd(x, mask);   // NaNs become zero

            Acc = _mm256_add_pd(Acc, _mm256_mul_pd(x, x));
        }

        double Sum = hsum256_pd(Acc);

        for (; i < Reduce; ++i) {
            double x = Ptr[i];
            if (!mxIsNaN(x)) {
                Sum += x * x;
            }
        }

        Out[outer] = Sum;
    }
}

static void reduce_dim1_single(const float* __restrict__ A,
                               float* __restrict__ Out,
                               mwSize Reduce,
                               mwSize Outer)
{
    const mwSize VecWidth = 8;
    const mwSize Nvec = (Reduce / VecWidth) * VecWidth;

    #ifdef _OPENMP
    #pragma omp parallel for schedule(static)
    #endif
    for (mwSignedIndex outer = 0; outer < static_cast<mwSignedIndex>(Outer); ++outer) {
        const float* Ptr = A + static_cast<mwSize>(outer) * Reduce;

        __m256 Acc = _mm256_setzero_ps();

        mwSize i = 0;
        for (; i < Nvec; i += VecWidth) {
            __m256 x = _mm256_loadu_ps(Ptr + i);

            // ordered compare: true only for non-NaN values
            __m256 mask = _mm256_cmp_ps(x, x, _CMP_ORD_Q);
            x = _mm256_and_ps(x, mask);   // NaNs become zero

            Acc = _mm256_add_ps(Acc, _mm256_mul_ps(x, x));
        }

        float Sum = hsum256_ps(Acc);

        for (; i < Reduce; ++i) {
            float x = Ptr[i];
            if (!mxIsNaN(x)) {
                Sum += x * x;
            }
        }

        Out[outer] = Sum;
    }
}

// ============================================================
// Explicit single / double dispatch
// ============================================================
static void run_sum2_double(const mxArray* In, mxArray*& OutMx, mwSize Dim)
{
    const mwSize Nd = mxGetNumberOfDimensions(In);
    const mwSize* DimsIn = mxGetDimensions(In);

    mwSize NdEff = (Dim > Nd) ? Dim : Nd;

    mwSize* DimsOut = static_cast<mwSize*>(mxCalloc(NdEff, sizeof(mwSize)));
    for (mwSize i = 0; i < NdEff; ++i) {
        DimsOut[i] = (i < Nd) ? DimsIn[i] : 1;
    }
    DimsOut[Dim - 1] = 1;

    OutMx = mxCreateNumericArray(NdEff, DimsOut, mxDOUBLE_CLASS, mxREAL);
    mxFree(DimsOut);

    const double* A = static_cast<const double*>(mxGetData(In));
    double* Out = static_cast<double*>(mxGetData(OutMx));

    mwSize Inner = 1;
    for (mwSize i = 0; i < Dim - 1 && i < Nd; ++i) {
        Inner *= DimsIn[i];
    }

    mwSize Reduce = (Dim <= Nd) ? DimsIn[Dim - 1] : 1;

    mwSize Outer = 1;
    for (mwSize i = Dim; i < Nd; ++i) {
        Outer *= DimsIn[i];
    }

    if (Reduce == 1) {
        const mwSize N = mxGetNumberOfElements(OutMx);
        #ifdef _OPENMP
        #pragma omp parallel for schedule(static)
        #endif
        for (mwSignedIndex i = 0; i < static_cast<mwSignedIndex>(N); ++i) {
            double x = A[i];
            Out[i] = mxIsNaN(x) ? 0.0 : x * x;
        }
        return;
    }

    if (Dim == 1) {
        reduce_dim1_double(A, Out, Reduce, Outer);
        return;
    }

    reduce_generic_double(A, Out, Inner, Reduce, Outer);
}

static void run_sum2_single(const mxArray* In, mxArray*& OutMx, mwSize Dim)
{
    const mwSize Nd = mxGetNumberOfDimensions(In);
    const mwSize* DimsIn = mxGetDimensions(In);

    mwSize NdEff = (Dim > Nd) ? Dim : Nd;

    mwSize* DimsOut = static_cast<mwSize*>(mxCalloc(NdEff, sizeof(mwSize)));
    for (mwSize i = 0; i < NdEff; ++i) {
        DimsOut[i] = (i < Nd) ? DimsIn[i] : 1;
    }
    DimsOut[Dim - 1] = 1;

    OutMx = mxCreateNumericArray(NdEff, DimsOut, mxSINGLE_CLASS, mxREAL);
    mxFree(DimsOut);

    const float* A = static_cast<const float*>(mxGetData(In));
    float* Out = static_cast<float*>(mxGetData(OutMx));

    mwSize Inner = 1;
    for (mwSize i = 0; i < Dim - 1 && i < Nd; ++i) {
        Inner *= DimsIn[i];
    }

    mwSize Reduce = (Dim <= Nd) ? DimsIn[Dim - 1] : 1;

    mwSize Outer = 1;
    for (mwSize i = Dim; i < Nd; ++i) {
        Outer *= DimsIn[i];
    }

    if (Reduce == 1) {
        const mwSize N = mxGetNumberOfElements(OutMx);
        #ifdef _OPENMP
        #pragma omp parallel for schedule(static)
        #endif
        for (mwSignedIndex i = 0; i < static_cast<mwSignedIndex>(N); ++i) {
            float x = A[i];
            Out[i] = mxIsNaN(x) ? 0.0f : x * x;
        }
        return;
    }

    if (Dim == 1) {
        reduce_dim1_single(A, Out, Reduce, Outer);
        return;
    }

    reduce_generic_single(A, Out, Inner, Reduce, Outer);
}

// ============================================================
// MEX gateway
// ============================================================
void mexFunction(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[])
{
    
    if (nrhs < 1 || nrhs > 2) {
      mexErrMsgIdAndTxt("sum2_mex:Input", "Usage: S2 = sum2_mex(Array[, Dim]).");
    }

    if (nlhs > 1) {
        mexErrMsgIdAndTxt("sum2_mex:Output", "One output only.");
    }

    const mxArray* In = prhs[0];
    if (mxIsComplex(In) || mxIsSparse(In) || !(mxIsSingle(In) || mxIsDouble(In))) {
        mexErrMsgIdAndTxt("sum2_mex:Type", "Array must be full, real, single or double.");
    }

    mwSize Dim = (nrhs == 1) ? 1 : getDimArg(prhs[1]);
 
    if (mxIsSingle(In)) {
        run_sum2_single(In, plhs[0], Dim);
    } else {
        run_sum2_double(In, plhs[0], Dim);
    }
}
