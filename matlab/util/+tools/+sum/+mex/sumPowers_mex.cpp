#include "mex.h"
#include <immintrin.h>
#include <cstdint>
#include <cstring>
#include <cstdlib>

#ifdef _OPENMP
#include <omp.h>
#endif

// ============================================================
// Helpers
// ============================================================
static inline bool getMaxPower(const mxArray* A, int& MaxPower)
{
    if (!mxIsNumeric(A) || mxIsComplex(A) || mxGetNumberOfElements(A) != 1) {
        return false;
    }

    double v = mxGetScalar(A);
    if (!(v >= 1.0)) {
        return false;
    }

    int vi = static_cast<int>(v);
    if (static_cast<double>(vi) != v) {
        return false;
    }

    if (vi > 1024) {
        return false;
    }

    MaxPower = vi;
    return true;
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
// Specialized DOUBLE kernels
// ============================================================
static void processDoubleP1(const double* __restrict__ X, mwSize N, double* __restrict__ Out)
{
    Out[0] = 0.0;
    const mwSize W = 4;
    const mwSize Nvec = (N / W) * W;

    int nThreads = 1;
    #ifdef _OPENMP
    nThreads = omp_get_max_threads();
    #endif

    double* ThreadSums = static_cast<double*>(std::calloc(static_cast<size_t>(nThreads), sizeof(double)));

    #ifdef _OPENMP
    #pragma omp parallel
    #endif
    {
        int tid = 0;
        #ifdef _OPENMP
        tid = omp_get_thread_num();
        #endif

        __m256d acc1 = _mm256_setzero_pd();

        #ifdef _OPENMP
        #pragma omp for schedule(static)
        #endif
        for (mwSignedIndex i = 0; i < static_cast<mwSignedIndex>(Nvec); i += W) {
            __m256d x = _mm256_loadu_pd(X + i);
            __m256d mask = _mm256_cmp_pd(x, x, _CMP_ORD_Q);
            x = _mm256_and_pd(x, mask);
            acc1 = _mm256_add_pd(acc1, x);
        }

        double sum = hsum256_pd(acc1);

        #ifdef _OPENMP
        #pragma omp for schedule(static)
        #endif
        for (mwSignedIndex i = static_cast<mwSignedIndex>(Nvec); i < static_cast<mwSignedIndex>(N); ++i) {
            double x = X[i];
            if (!mxIsNaN(x)) {
                sum += x;
            }
        }

        ThreadSums[tid] = sum;
    }

    for (int t = 0; t < nThreads; ++t) {
        Out[0] += ThreadSums[t];
    }

    std::free(ThreadSums);
}

static void processDoubleP2(const double* __restrict__ X, mwSize N, double* __restrict__ Out)
{
    Out[0] = 0.0; Out[1] = 0.0;
    const mwSize W = 4;
    const mwSize Nvec = (N / W) * W;

    int nThreads = 1;
    #ifdef _OPENMP
    nThreads = omp_get_max_threads();
    #endif

    double* ThreadSums = static_cast<double*>(std::calloc(static_cast<size_t>(nThreads) * 2, sizeof(double)));

    #ifdef _OPENMP
    #pragma omp parallel
    #endif
    {
        int tid = 0;
        #ifdef _OPENMP
        tid = omp_get_thread_num();
        #endif

        __m256d acc1 = _mm256_setzero_pd();
        __m256d acc2 = _mm256_setzero_pd();

        #ifdef _OPENMP
        #pragma omp for schedule(static)
        #endif
        for (mwSignedIndex i = 0; i < static_cast<mwSignedIndex>(Nvec); i += W) {
            __m256d x = _mm256_loadu_pd(X + i);
            __m256d mask = _mm256_cmp_pd(x, x, _CMP_ORD_Q);
            x = _mm256_and_pd(x, mask);

            __m256d p1 = x;
            __m256d p2 = _mm256_mul_pd(p1, x);

            acc1 = _mm256_add_pd(acc1, p1);
            acc2 = _mm256_add_pd(acc2, p2);
        }

        double s1 = hsum256_pd(acc1);
        double s2 = hsum256_pd(acc2);

        #ifdef _OPENMP
        #pragma omp for schedule(static)
        #endif
        for (mwSignedIndex i = static_cast<mwSignedIndex>(Nvec); i < static_cast<mwSignedIndex>(N); ++i) {
            double x = X[i];
            if (!mxIsNaN(x)) {
                double p1 = x;
                double p2 = p1 * x;
                s1 += p1;
                s2 += p2;
            }
        }

        ThreadSums[2 * tid + 0] = s1;
        ThreadSums[2 * tid + 1] = s2;
    }

    for (int t = 0; t < nThreads; ++t) {
        Out[0] += ThreadSums[2 * t + 0];
        Out[1] += ThreadSums[2 * t + 1];
    }

    std::free(ThreadSums);
}

static void processDoubleP3(const double* __restrict__ X, mwSize N, double* __restrict__ Out)
{
    Out[0] = 0.0; Out[1] = 0.0; Out[2] = 0.0;
    const mwSize W = 4;
    const mwSize Nvec = (N / W) * W;

    int nThreads = 1;
    #ifdef _OPENMP
    nThreads = omp_get_max_threads();
    #endif

    double* ThreadSums = static_cast<double*>(std::calloc(static_cast<size_t>(nThreads) * 3, sizeof(double)));

    #ifdef _OPENMP
    #pragma omp parallel
    #endif
    {
        int tid = 0;
        #ifdef _OPENMP
        tid = omp_get_thread_num();
        #endif

        __m256d acc1 = _mm256_setzero_pd();
        __m256d acc2 = _mm256_setzero_pd();
        __m256d acc3 = _mm256_setzero_pd();

        #ifdef _OPENMP
        #pragma omp for schedule(static)
        #endif
        for (mwSignedIndex i = 0; i < static_cast<mwSignedIndex>(Nvec); i += W) {
            __m256d x = _mm256_loadu_pd(X + i);
            __m256d mask = _mm256_cmp_pd(x, x, _CMP_ORD_Q);
            x = _mm256_and_pd(x, mask);

            __m256d p1 = x;
            __m256d p2 = _mm256_mul_pd(p1, x);
            __m256d p3 = _mm256_mul_pd(p2, x);

            acc1 = _mm256_add_pd(acc1, p1);
            acc2 = _mm256_add_pd(acc2, p2);
            acc3 = _mm256_add_pd(acc3, p3);
        }

        double s1 = hsum256_pd(acc1);
        double s2 = hsum256_pd(acc2);
        double s3 = hsum256_pd(acc3);

        #ifdef _OPENMP
        #pragma omp for schedule(static)
        #endif
        for (mwSignedIndex i = static_cast<mwSignedIndex>(Nvec); i < static_cast<mwSignedIndex>(N); ++i) {
            double x = X[i];
            if (!mxIsNaN(x)) {
                double p1 = x;
                double p2 = p1 * x;
                double p3 = p2 * x;
                s1 += p1;
                s2 += p2;
                s3 += p3;
            }
        }

        ThreadSums[3 * tid + 0] = s1;
        ThreadSums[3 * tid + 1] = s2;
        ThreadSums[3 * tid + 2] = s3;
    }

    for (int t = 0; t < nThreads; ++t) {
        Out[0] += ThreadSums[3 * t + 0];
        Out[1] += ThreadSums[3 * t + 1];
        Out[2] += ThreadSums[3 * t + 2];
    }

    std::free(ThreadSums);
}

static void processDoubleP4(const double* __restrict__ X, mwSize N, double* __restrict__ Out)
{
    Out[0] = 0.0; Out[1] = 0.0; Out[2] = 0.0; Out[3] = 0.0;
    const mwSize W = 4;
    const mwSize Nvec = (N / W) * W;

    int nThreads = 1;
    #ifdef _OPENMP
    nThreads = omp_get_max_threads();
    #endif

    double* ThreadSums = static_cast<double*>(std::calloc(static_cast<size_t>(nThreads) * 4, sizeof(double)));

    #ifdef _OPENMP
    #pragma omp parallel
    #endif
    {
        int tid = 0;
        #ifdef _OPENMP
        tid = omp_get_thread_num();
        #endif

        __m256d acc1 = _mm256_setzero_pd();
        __m256d acc2 = _mm256_setzero_pd();
        __m256d acc3 = _mm256_setzero_pd();
        __m256d acc4 = _mm256_setzero_pd();

        #ifdef _OPENMP
        #pragma omp for schedule(static)
        #endif
        for (mwSignedIndex i = 0; i < static_cast<mwSignedIndex>(Nvec); i += W) {
            __m256d x = _mm256_loadu_pd(X + i);
            __m256d mask = _mm256_cmp_pd(x, x, _CMP_ORD_Q);
            x = _mm256_and_pd(x, mask);

            __m256d p1 = x;
            __m256d p2 = _mm256_mul_pd(p1, x);
            __m256d p3 = _mm256_mul_pd(p2, x);
            __m256d p4 = _mm256_mul_pd(p3, x);

            acc1 = _mm256_add_pd(acc1, p1);
            acc2 = _mm256_add_pd(acc2, p2);
            acc3 = _mm256_add_pd(acc3, p3);
            acc4 = _mm256_add_pd(acc4, p4);
        }

        double s1 = hsum256_pd(acc1);
        double s2 = hsum256_pd(acc2);
        double s3 = hsum256_pd(acc3);
        double s4 = hsum256_pd(acc4);

        #ifdef _OPENMP
        #pragma omp for schedule(static)
        #endif
        for (mwSignedIndex i = static_cast<mwSignedIndex>(Nvec); i < static_cast<mwSignedIndex>(N); ++i) {
            double x = X[i];
            if (!mxIsNaN(x)) {
                double p1 = x;
                double p2 = p1 * x;
                double p3 = p2 * x;
                double p4 = p3 * x;
                s1 += p1;
                s2 += p2;
                s3 += p3;
                s4 += p4;
            }
        }

        ThreadSums[4 * tid + 0] = s1;
        ThreadSums[4 * tid + 1] = s2;
        ThreadSums[4 * tid + 2] = s3;
        ThreadSums[4 * tid + 3] = s4;
    }

    for (int t = 0; t < nThreads; ++t) {
        Out[0] += ThreadSums[4 * t + 0];
        Out[1] += ThreadSums[4 * t + 1];
        Out[2] += ThreadSums[4 * t + 2];
        Out[3] += ThreadSums[4 * t + 3];
    }

    std::free(ThreadSums);
}

static void processDoubleGeneric(const double* __restrict__ X, mwSize N, int MaxPower, double* __restrict__ Out)
{
    for (int k = 0; k < MaxPower; ++k) {
        Out[k] = 0.0;
    }

    const mwSize W = 4;
    const mwSize Nvec = (N / W) * W;

    int nThreads = 1;
    #ifdef _OPENMP
    nThreads = omp_get_max_threads();
    #endif

    double* ThreadSums = static_cast<double*>(std::calloc(static_cast<size_t>(nThreads) * static_cast<size_t>(MaxPower), sizeof(double)));

    #ifdef _OPENMP
    #pragma omp parallel
    #endif
    {
        int tid = 0;
        #ifdef _OPENMP
        tid = omp_get_thread_num();
        #endif

        __m256d* acc = static_cast<__m256d*>(_mm_malloc(sizeof(__m256d) * static_cast<size_t>(MaxPower), 32));
        for (int k = 0; k < MaxPower; ++k) {
            acc[k] = _mm256_setzero_pd();
        }

        #ifdef _OPENMP
        #pragma omp for schedule(static)
        #endif
        for (mwSignedIndex i = 0; i < static_cast<mwSignedIndex>(Nvec); i += W) {
            __m256d x = _mm256_loadu_pd(X + i);
            __m256d mask = _mm256_cmp_pd(x, x, _CMP_ORD_Q);
            x = _mm256_and_pd(x, mask);

            __m256d p = x;
            acc[0] = _mm256_add_pd(acc[0], p);

            for (int k = 1; k < MaxPower; ++k) {
                p = _mm256_mul_pd(p, x);
                acc[k] = _mm256_add_pd(acc[k], p);
            }
        }

        double* local = ThreadSums + static_cast<size_t>(tid) * static_cast<size_t>(MaxPower);
        for (int k = 0; k < MaxPower; ++k) {
            local[k] = hsum256_pd(acc[k]);
        }

        _mm_free(acc);

        #ifdef _OPENMP
        #pragma omp for schedule(static)
        #endif
        for (mwSignedIndex i = static_cast<mwSignedIndex>(Nvec); i < static_cast<mwSignedIndex>(N); ++i) {
            double x = X[i];
            if (!mxIsNaN(x)) {
                double p = x;
                local[0] += p;
                for (int k = 1; k < MaxPower; ++k) {
                    p *= x;
                    local[k] += p;
                }
            }
        }
    }

    for (int t = 0; t < nThreads; ++t) {
        const double* local = ThreadSums + static_cast<size_t>(t) * static_cast<size_t>(MaxPower);
        for (int k = 0; k < MaxPower; ++k) {
            Out[k] += local[k];
        }
    }

    std::free(ThreadSums);
}

// ============================================================
// Specialized SINGLE kernels
// ============================================================
static void processSingleP1(const float* __restrict__ X, mwSize N, float* __restrict__ Out)
{
    Out[0] = 0.0f;
    const mwSize W = 8;
    const mwSize Nvec = (N / W) * W;

    int nThreads = 1;
    #ifdef _OPENMP
    nThreads = omp_get_max_threads();
    #endif

    float* ThreadSums = static_cast<float*>(std::calloc(static_cast<size_t>(nThreads), sizeof(float)));

    #ifdef _OPENMP
    #pragma omp parallel
    #endif
    {
        int tid = 0;
        #ifdef _OPENMP
        tid = omp_get_thread_num();
        #endif

        __m256 acc1 = _mm256_setzero_ps();

        #ifdef _OPENMP
        #pragma omp for schedule(static)
        #endif
        for (mwSignedIndex i = 0; i < static_cast<mwSignedIndex>(Nvec); i += W) {
            __m256 x = _mm256_loadu_ps(X + i);
            __m256 mask = _mm256_cmp_ps(x, x, _CMP_ORD_Q);
            x = _mm256_and_ps(x, mask);
            acc1 = _mm256_add_ps(acc1, x);
        }

        float sum = hsum256_ps(acc1);

        #ifdef _OPENMP
        #pragma omp for schedule(static)
        #endif
        for (mwSignedIndex i = static_cast<mwSignedIndex>(Nvec); i < static_cast<mwSignedIndex>(N); ++i) {
            float x = X[i];
            if (!mxIsNaN(x)) {
                sum += x;
            }
        }

        ThreadSums[tid] = sum;
    }

    for (int t = 0; t < nThreads; ++t) {
        Out[0] += ThreadSums[t];
    }

    std::free(ThreadSums);
}

static void processSingleP2(const float* __restrict__ X, mwSize N, float* __restrict__ Out)
{
    Out[0] = 0.0f; Out[1] = 0.0f;
    const mwSize W = 8;
    const mwSize Nvec = (N / W) * W;

    int nThreads = 1;
    #ifdef _OPENMP
    nThreads = omp_get_max_threads();
    #endif

    float* ThreadSums = static_cast<float*>(std::calloc(static_cast<size_t>(nThreads) * 2, sizeof(float)));

    #ifdef _OPENMP
    #pragma omp parallel
    #endif
    {
        int tid = 0;
        #ifdef _OPENMP
        tid = omp_get_thread_num();
        #endif

        __m256 acc1 = _mm256_setzero_ps();
        __m256 acc2 = _mm256_setzero_ps();

        #ifdef _OPENMP
        #pragma omp for schedule(static)
        #endif
        for (mwSignedIndex i = 0; i < static_cast<mwSignedIndex>(Nvec); i += W) {
            __m256 x = _mm256_loadu_ps(X + i);
            __m256 mask = _mm256_cmp_ps(x, x, _CMP_ORD_Q);
            x = _mm256_and_ps(x, mask);

            __m256 p1 = x;
            __m256 p2 = _mm256_mul_ps(p1, x);

            acc1 = _mm256_add_ps(acc1, p1);
            acc2 = _mm256_add_ps(acc2, p2);
        }

        float s1 = hsum256_ps(acc1);
        float s2 = hsum256_ps(acc2);

        #ifdef _OPENMP
        #pragma omp for schedule(static)
        #endif
        for (mwSignedIndex i = static_cast<mwSignedIndex>(Nvec); i < static_cast<mwSignedIndex>(N); ++i) {
            float x = X[i];
            if (!mxIsNaN(x)) {
                float p1 = x;
                float p2 = p1 * x;
                s1 += p1;
                s2 += p2;
            }
        }

        ThreadSums[2 * tid + 0] = s1;
        ThreadSums[2 * tid + 1] = s2;
    }

    for (int t = 0; t < nThreads; ++t) {
        Out[0] += ThreadSums[2 * t + 0];
        Out[1] += ThreadSums[2 * t + 1];
    }

    std::free(ThreadSums);
}

static void processSingleP3(const float* __restrict__ X, mwSize N, float* __restrict__ Out)
{
    Out[0] = 0.0f; Out[1] = 0.0f; Out[2] = 0.0f;
    const mwSize W = 8;
    const mwSize Nvec = (N / W) * W;

    int nThreads = 1;
    #ifdef _OPENMP
    nThreads = omp_get_max_threads();
    #endif

    float* ThreadSums = static_cast<float*>(std::calloc(static_cast<size_t>(nThreads) * 3, sizeof(float)));

    #ifdef _OPENMP
    #pragma omp parallel
    #endif
    {
        int tid = 0;
        #ifdef _OPENMP
        tid = omp_get_thread_num();
        #endif

        __m256 acc1 = _mm256_setzero_ps();
        __m256 acc2 = _mm256_setzero_ps();
        __m256 acc3 = _mm256_setzero_ps();

        #ifdef _OPENMP
        #pragma omp for schedule(static)
        #endif
        for (mwSignedIndex i = 0; i < static_cast<mwSignedIndex>(Nvec); i += W) {
            __m256 x = _mm256_loadu_ps(X + i);
            __m256 mask = _mm256_cmp_ps(x, x, _CMP_ORD_Q);
            x = _mm256_and_ps(x, mask);

            __m256 p1 = x;
            __m256 p2 = _mm256_mul_ps(p1, x);
            __m256 p3 = _mm256_mul_ps(p2, x);

            acc1 = _mm256_add_ps(acc1, p1);
            acc2 = _mm256_add_ps(acc2, p2);
            acc3 = _mm256_add_ps(acc3, p3);
        }

        float s1 = hsum256_ps(acc1);
        float s2 = hsum256_ps(acc2);
        float s3 = hsum256_ps(acc3);

        #ifdef _OPENMP
        #pragma omp for schedule(static)
        #endif
        for (mwSignedIndex i = static_cast<mwSignedIndex>(Nvec); i < static_cast<mwSignedIndex>(N); ++i) {
            float x = X[i];
            if (!mxIsNaN(x)) {
                float p1 = x;
                float p2 = p1 * x;
                float p3 = p2 * x;
                s1 += p1;
                s2 += p2;
                s3 += p3;
            }
        }

        ThreadSums[3 * tid + 0] = s1;
        ThreadSums[3 * tid + 1] = s2;
        ThreadSums[3 * tid + 2] = s3;
    }

    for (int t = 0; t < nThreads; ++t) {
        Out[0] += ThreadSums[3 * t + 0];
        Out[1] += ThreadSums[3 * t + 1];
        Out[2] += ThreadSums[3 * t + 2];
    }

    std::free(ThreadSums);
}

static void processSingleP4(const float* __restrict__ X, mwSize N, float* __restrict__ Out)
{
    Out[0] = 0.0f; Out[1] = 0.0f; Out[2] = 0.0f; Out[3] = 0.0f;
    const mwSize W = 8;
    const mwSize Nvec = (N / W) * W;

    int nThreads = 1;
    #ifdef _OPENMP
    nThreads = omp_get_max_threads();
    #endif

    float* ThreadSums = static_cast<float*>(std::calloc(static_cast<size_t>(nThreads) * 4, sizeof(float)));

    #ifdef _OPENMP
    #pragma omp parallel
    #endif
    {
        int tid = 0;
        #ifdef _OPENMP
        tid = omp_get_thread_num();
        #endif

        __m256 acc1 = _mm256_setzero_ps();
        __m256 acc2 = _mm256_setzero_ps();
        __m256 acc3 = _mm256_setzero_ps();
        __m256 acc4 = _mm256_setzero_ps();

        #ifdef _OPENMP
        #pragma omp for schedule(static)
        #endif
        for (mwSignedIndex i = 0; i < static_cast<mwSignedIndex>(Nvec); i += W) {
            __m256 x = _mm256_loadu_ps(X + i);
            __m256 mask = _mm256_cmp_ps(x, x, _CMP_ORD_Q);
            x = _mm256_and_ps(x, mask);

            __m256 p1 = x;
            __m256 p2 = _mm256_mul_ps(p1, x);
            __m256 p3 = _mm256_mul_ps(p2, x);
            __m256 p4 = _mm256_mul_ps(p3, x);

            acc1 = _mm256_add_ps(acc1, p1);
            acc2 = _mm256_add_ps(acc2, p2);
            acc3 = _mm256_add_ps(acc3, p3);
            acc4 = _mm256_add_ps(acc4, p4);
        }

        float s1 = hsum256_ps(acc1);
        float s2 = hsum256_ps(acc2);
        float s3 = hsum256_ps(acc3);
        float s4 = hsum256_ps(acc4);

        #ifdef _OPENMP
        #pragma omp for schedule(static)
        #endif
        for (mwSignedIndex i = static_cast<mwSignedIndex>(Nvec); i < static_cast<mwSignedIndex>(N); ++i) {
            float x = X[i];
            if (!mxIsNaN(x)) {
                float p1 = x;
                float p2 = p1 * x;
                float p3 = p2 * x;
                float p4 = p3 * x;
                s1 += p1;
                s2 += p2;
                s3 += p3;
                s4 += p4;
            }
        }

        ThreadSums[4 * tid + 0] = s1;
        ThreadSums[4 * tid + 1] = s2;
        ThreadSums[4 * tid + 2] = s3;
        ThreadSums[4 * tid + 3] = s4;
    }

    for (int t = 0; t < nThreads; ++t) {
        Out[0] += ThreadSums[4 * t + 0];
        Out[1] += ThreadSums[4 * t + 1];
        Out[2] += ThreadSums[4 * t + 2];
        Out[3] += ThreadSums[4 * t + 3];
    }

    std::free(ThreadSums);
}

static void processSingleGeneric(const float* __restrict__ X, mwSize N, int MaxPower, float* __restrict__ Out)
{
    for (int k = 0; k < MaxPower; ++k) {
        Out[k] = 0.0f;
    }

    const mwSize W = 8;
    const mwSize Nvec = (N / W) * W;

    int nThreads = 1;
    #ifdef _OPENMP
    nThreads = omp_get_max_threads();
    #endif

    float* ThreadSums = static_cast<float*>(std::calloc(static_cast<size_t>(nThreads) * static_cast<size_t>(MaxPower), sizeof(float)));

    #ifdef _OPENMP
    #pragma omp parallel
    #endif
    {
        int tid = 0;
        #ifdef _OPENMP
        tid = omp_get_thread_num();
        #endif

        __m256* acc = static_cast<__m256*>(_mm_malloc(sizeof(__m256) * static_cast<size_t>(MaxPower), 32));
        for (int k = 0; k < MaxPower; ++k) {
            acc[k] = _mm256_setzero_ps();
        }

        #ifdef _OPENMP
        #pragma omp for schedule(static)
        #endif
        for (mwSignedIndex i = 0; i < static_cast<mwSignedIndex>(Nvec); i += W) {
            __m256 x = _mm256_loadu_ps(X + i);
            __m256 mask = _mm256_cmp_ps(x, x, _CMP_ORD_Q);
            x = _mm256_and_ps(x, mask);

            __m256 p = x;
            acc[0] = _mm256_add_ps(acc[0], p);

            for (int k = 1; k < MaxPower; ++k) {
                p = _mm256_mul_ps(p, x);
                acc[k] = _mm256_add_ps(acc[k], p);
            }
        }

        float* local = ThreadSums + static_cast<size_t>(tid) * static_cast<size_t>(MaxPower);
        for (int k = 0; k < MaxPower; ++k) {
            local[k] = hsum256_ps(acc[k]);
        }

        _mm_free(acc);

        #ifdef _OPENMP
        #pragma omp for schedule(static)
        #endif
        for (mwSignedIndex i = static_cast<mwSignedIndex>(Nvec); i < static_cast<mwSignedIndex>(N); ++i) {
            float x = X[i];
            if (!mxIsNaN(x)) {
                float p = x;
                local[0] += p;
                for (int k = 1; k < MaxPower; ++k) {
                    p *= x;
                    local[k] += p;
                }
            }
        }
    }

    for (int t = 0; t < nThreads; ++t) {
        const float* local = ThreadSums + static_cast<size_t>(t) * static_cast<size_t>(MaxPower);
        for (int k = 0; k < MaxPower; ++k) {
            Out[k] += local[k];
        }
    }

    std::free(ThreadSums);
}

// ============================================================
// Dispatch
// ============================================================
static void runDouble(const double* X, mwSize N, int MaxPower, double* Out)
{
    switch (MaxPower) {
        case 1: processDoubleP1(X, N, Out); break;
        case 2: processDoubleP2(X, N, Out); break;
        case 3: processDoubleP3(X, N, Out); break;
        case 4: processDoubleP4(X, N, Out); break;
        default: processDoubleGeneric(X, N, MaxPower, Out); break;
    }
}

static void runSingle(const float* X, mwSize N, int MaxPower, float* Out)
{
    switch (MaxPower) {
        case 1: processSingleP1(X, N, Out); break;
        case 2: processSingleP2(X, N, Out); break;
        case 3: processSingleP3(X, N, Out); break;
        case 4: processSingleP4(X, N, Out); break;
        default: processSingleGeneric(X, N, MaxPower, Out); break;
    }
}

// ============================================================
// MEX gateway
// ============================================================
void mexFunction(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[])
{
    if (nrhs != 2) {
        mexErrMsgIdAndTxt("sumPowers_mex:Input", "Two inputs required: X, MaxPower.");
    }

    if (nlhs > 1) {
        mexErrMsgIdAndTxt("sumPowers_mex:Output", "One output only.");
    }

    const mxArray* XIn = prhs[0];
    if (mxIsComplex(XIn) || mxIsSparse(XIn) || !(mxIsSingle(XIn) || mxIsDouble(XIn))) {
        mexErrMsgIdAndTxt("sumPowers_mex:Type", "X must be a full, real, single or double array.");
    }

    int MaxPower = 0;
    if (!getMaxPower(prhs[1], MaxPower)) {
        mexErrMsgIdAndTxt("sumPowers_mex:MaxPower", "MaxPower must be a positive integer scalar.");
    }

    mwSize Dims[2];
    Dims[0] = 1;
    Dims[1] = static_cast<mwSize>(MaxPower);
    mwSize N = mxGetNumberOfElements(XIn);

    if (mxIsDouble(XIn)) {
        plhs[0] = mxCreateNumericArray(2, Dims, mxDOUBLE_CLASS, mxREAL);
        const double* X = static_cast<const double*>(mxGetData(XIn));
        double* Out = static_cast<double*>(mxGetData(plhs[0]));
        runDouble(X, N, MaxPower, Out);
    } else {
        plhs[0] = mxCreateNumericArray(2, Dims, mxSINGLE_CLASS, mxREAL);
        const float* X = static_cast<const float*>(mxGetData(XIn));
        float* Out = static_cast<float*>(mxGetData(plhs[0]));
        runSingle(X, N, MaxPower, Out);
    }
}
