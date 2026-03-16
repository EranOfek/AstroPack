#include "mex.h"
#include <cstddef>
#include <cstdint>
#include <limits>
#include <vector>
#include <type_traits>

#if defined(_OPENMP)
    #include <omp.h>
#endif

#if defined(__AVX2__)
    #include <immintrin.h>
#endif

template <typename T>
struct alignas(64) MinMaxPair {
    T MinVal;
    T MaxVal;
};

template <typename T>
MinMaxPair<T> minmaxScalarRange(const T* Array, std::size_t I1, std::size_t I2)
{
    MinMaxPair<T> Out;
    Out.MinVal = Array[I1];
    Out.MaxVal = Array[I1];

    for (std::size_t I = I1 + 1; I < I2; ++I) {
        T X = Array[I];
        if (X < Out.MinVal) {
            Out.MinVal = X;
        }
        if (X > Out.MaxVal) {
            Out.MaxVal = X;
        }
    }

    return Out;
}

#if defined(__AVX2__)

static inline double horizontalMin256d(__m256d V)
{
    alignas(32) double Tmp[4];
    _mm256_store_pd(Tmp, V);

    double MinVal = Tmp[0];
    if (Tmp[1] < MinVal) MinVal = Tmp[1];
    if (Tmp[2] < MinVal) MinVal = Tmp[2];
    if (Tmp[3] < MinVal) MinVal = Tmp[3];
    return MinVal;
}

static inline double horizontalMax256d(__m256d V)
{
    alignas(32) double Tmp[4];
    _mm256_store_pd(Tmp, V);

    double MaxVal = Tmp[0];
    if (Tmp[1] > MaxVal) MaxVal = Tmp[1];
    if (Tmp[2] > MaxVal) MaxVal = Tmp[2];
    if (Tmp[3] > MaxVal) MaxVal = Tmp[3];
    return MaxVal;
}

static inline float horizontalMin256(__m256 V)
{
    alignas(32) float Tmp[8];
    _mm256_store_ps(Tmp, V);

    float MinVal = Tmp[0];
    for (int I = 1; I < 8; ++I) {
        if (Tmp[I] < MinVal) {
            MinVal = Tmp[I];
        }
    }
    return MinVal;
}

static inline float horizontalMax256(__m256 V)
{
    alignas(32) float Tmp[8];
    _mm256_store_ps(Tmp, V);

    float MaxVal = Tmp[0];
    for (int I = 1; I < 8; ++I) {
        if (Tmp[I] > MaxVal) {
            MaxVal = Tmp[I];
        }
    }
    return MaxVal;
}

// ============================================================
// SIMD kernels: single pass for min and max together
// ============================================================

MinMaxPair<double> minmaxKernelDouble(const double* Array, std::size_t I1, std::size_t I2)
{
    std::size_t N = I2 - I1;
    if (N < 16) {
        return minmaxScalarRange(Array, I1, I2);
    }

    std::size_t I = I1;

    __m256d Min0 = _mm256_loadu_pd(Array + I);
    __m256d Max0 = Min0;
    I += 4;

    __m256d Min1 = _mm256_loadu_pd(Array + I);
    __m256d Max1 = Min1;
    I += 4;

    __m256d Min2 = _mm256_loadu_pd(Array + I);
    __m256d Max2 = Min2;
    I += 4;

    __m256d Min3 = _mm256_loadu_pd(Array + I);
    __m256d Max3 = Min3;
    I += 4;

    for (; I + 15 < I2; I += 16) {
        __m256d X0 = _mm256_loadu_pd(Array + I + 0);
        __m256d X1 = _mm256_loadu_pd(Array + I + 4);
        __m256d X2 = _mm256_loadu_pd(Array + I + 8);
        __m256d X3 = _mm256_loadu_pd(Array + I + 12);

        Min0 = _mm256_min_pd(Min0, X0);
        Max0 = _mm256_max_pd(Max0, X0);

        Min1 = _mm256_min_pd(Min1, X1);
        Max1 = _mm256_max_pd(Max1, X1);

        Min2 = _mm256_min_pd(Min2, X2);
        Max2 = _mm256_max_pd(Max2, X2);

        Min3 = _mm256_min_pd(Min3, X3);
        Max3 = _mm256_max_pd(Max3, X3);
    }

    Min0 = _mm256_min_pd(Min0, Min1);
    Min2 = _mm256_min_pd(Min2, Min3);
    Min0 = _mm256_min_pd(Min0, Min2);

    Max0 = _mm256_max_pd(Max0, Max1);
    Max2 = _mm256_max_pd(Max2, Max3);
    Max0 = _mm256_max_pd(Max0, Max2);

    MinMaxPair<double> Out;
    Out.MinVal = horizontalMin256d(Min0);
    Out.MaxVal = horizontalMax256d(Max0);

    for (; I < I2; ++I) {
        double X = Array[I];
        if (X < Out.MinVal) {
            Out.MinVal = X;
        }
        if (X > Out.MaxVal) {
            Out.MaxVal = X;
        }
    }

    return Out;
}

MinMaxPair<float> minmaxKernelSingle(const float* Array, std::size_t I1, std::size_t I2)
{
    std::size_t N = I2 - I1;
    if (N < 32) {
        return minmaxScalarRange(Array, I1, I2);
    }

    std::size_t I = I1;

    __m256 Min0 = _mm256_loadu_ps(Array + I);
    __m256 Max0 = Min0;
    I += 8;

    __m256 Min1 = _mm256_loadu_ps(Array + I);
    __m256 Max1 = Min1;
    I += 8;

    __m256 Min2 = _mm256_loadu_ps(Array + I);
    __m256 Max2 = Min2;
    I += 8;

    __m256 Min3 = _mm256_loadu_ps(Array + I);
    __m256 Max3 = Min3;
    I += 8;

    for (; I + 31 < I2; I += 32) {
        __m256 X0 = _mm256_loadu_ps(Array + I + 0);
        __m256 X1 = _mm256_loadu_ps(Array + I + 8);
        __m256 X2 = _mm256_loadu_ps(Array + I + 16);
        __m256 X3 = _mm256_loadu_ps(Array + I + 24);

        Min0 = _mm256_min_ps(Min0, X0);
        Max0 = _mm256_max_ps(Max0, X0);

        Min1 = _mm256_min_ps(Min1, X1);
        Max1 = _mm256_max_ps(Max1, X1);

        Min2 = _mm256_min_ps(Min2, X2);
        Max2 = _mm256_max_ps(Max2, X2);

        Min3 = _mm256_min_ps(Min3, X3);
        Max3 = _mm256_max_ps(Max3, X3);
    }

    Min0 = _mm256_min_ps(Min0, Min1);
    Min2 = _mm256_min_ps(Min2, Min3);
    Min0 = _mm256_min_ps(Min0, Min2);

    Max0 = _mm256_max_ps(Max0, Max1);
    Max2 = _mm256_max_ps(Max2, Max3);
    Max0 = _mm256_max_ps(Max0, Max2);

    MinMaxPair<float> Out;
    Out.MinVal = horizontalMin256(Min0);
    Out.MaxVal = horizontalMax256(Max0);

    for (; I < I2; ++I) {
        float X = Array[I];
        if (X < Out.MinVal) {
            Out.MinVal = X;
        }
        if (X > Out.MaxVal) {
            Out.MaxVal = X;
        }
    }

    return Out;
}

#endif

// ============================================================
// Global wrappers
// ============================================================

double reduceMinDouble(const std::vector<MinMaxPair<double>>& ThreadOut)
{
    double MinVal = ThreadOut[0].MinVal;
    for (std::size_t I = 1; I < ThreadOut.size(); ++I) {
        if (ThreadOut[I].MinVal < MinVal) {
            MinVal = ThreadOut[I].MinVal;
        }
    }
    return MinVal;
}

double reduceMaxDouble(const std::vector<MinMaxPair<double>>& ThreadOut)
{
    double MaxVal = ThreadOut[0].MaxVal;
    for (std::size_t I = 1; I < ThreadOut.size(); ++I) {
        if (ThreadOut[I].MaxVal > MaxVal) {
            MaxVal = ThreadOut[I].MaxVal;
        }
    }
    return MaxVal;
}

float reduceMinSingle(const std::vector<MinMaxPair<float>>& ThreadOut)
{
    float MinVal = ThreadOut[0].MinVal;
    for (std::size_t I = 1; I < ThreadOut.size(); ++I) {
        if (ThreadOut[I].MinVal < MinVal) {
            MinVal = ThreadOut[I].MinVal;
        }
    }
    return MinVal;
}

float reduceMaxSingle(const std::vector<MinMaxPair<float>>& ThreadOut)
{
    float MaxVal = ThreadOut[0].MaxVal;
    for (std::size_t I = 1; I < ThreadOut.size(); ++I) {
        if (ThreadOut[I].MaxVal > MaxVal) {
            MaxVal = ThreadOut[I].MaxVal;
        }
    }
    return MaxVal;
}

MinMaxPair<double> runMinMaxDouble(const double* Array, std::size_t N)
{
#if defined(_OPENMP)
    constexpr std::size_t MinPerThread = 1 << 20;
    int MaxThreads = omp_get_max_threads();

    if (MaxThreads > 1 && N >= MinPerThread) {
        std::vector<MinMaxPair<double>> ThreadOut((std::size_t)MaxThreads);

        for (int T = 0; T < MaxThreads; ++T) {
            ThreadOut[(std::size_t)T].MinVal = std::numeric_limits<double>::max();
            ThreadOut[(std::size_t)T].MaxVal = -std::numeric_limits<double>::max();
        }

        #pragma omp parallel
        {
            int Tid = omp_get_thread_num();
            int Nt  = omp_get_num_threads();

            std::size_t I1 = (N * (std::size_t)Tid) / (std::size_t)Nt;
            std::size_t I2 = (N * (std::size_t)(Tid + 1)) / (std::size_t)Nt;

            if (I2 > I1) {
                #if defined(__AVX2__)
                    ThreadOut[(std::size_t)Tid] = minmaxKernelDouble(Array, I1, I2);
                #else
                    ThreadOut[(std::size_t)Tid] = minmaxScalarRange(Array, I1, I2);
                #endif
            }
        }

        MinMaxPair<double> Out;
        Out.MinVal = reduceMinDouble(ThreadOut);
        Out.MaxVal = reduceMaxDouble(ThreadOut);
        return Out;
    }
#endif

#if defined(__AVX2__)
    return minmaxKernelDouble(Array, 0, N);
#else
    return minmaxScalarRange(Array, 0, N);
#endif
}

MinMaxPair<float> runMinMaxSingle(const float* Array, std::size_t N)
{
#if defined(_OPENMP)
    constexpr std::size_t MinPerThread = 1 << 20;
    int MaxThreads = omp_get_max_threads();

    if (MaxThreads > 1 && N >= MinPerThread) {
        std::vector<MinMaxPair<float>> ThreadOut((std::size_t)MaxThreads);

        for (int T = 0; T < MaxThreads; ++T) {
            ThreadOut[(std::size_t)T].MinVal = std::numeric_limits<float>::max();
            ThreadOut[(std::size_t)T].MaxVal = -std::numeric_limits<float>::max();
        }

        #pragma omp parallel
        {
            int Tid = omp_get_thread_num();
            int Nt  = omp_get_num_threads();

            std::size_t I1 = (N * (std::size_t)Tid) / (std::size_t)Nt;
            std::size_t I2 = (N * (std::size_t)(Tid + 1)) / (std::size_t)Nt;

            if (I2 > I1) {
                #if defined(__AVX2__)
                    ThreadOut[(std::size_t)Tid] = minmaxKernelSingle(Array, I1, I2);
                #else
                    ThreadOut[(std::size_t)Tid] = minmaxScalarRange(Array, I1, I2);
                #endif
            }
        }

        MinMaxPair<float> Out;
        Out.MinVal = reduceMinSingle(ThreadOut);
        Out.MaxVal = reduceMaxSingle(ThreadOut);
        return Out;
    }
#endif

#if defined(__AVX2__)
    return minmaxKernelSingle(Array, 0, N);
#else
    return minmaxScalarRange(Array, 0, N);
#endif
}

// ============================================================
// MEX gateway
// ============================================================

void mexFunction(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[])
{
    if (nrhs != 1) {
        mexErrMsgIdAndTxt("minmaxGlobal_mex:InputError",
                          "One input is required: [MinVal, MaxVal] = minmaxGlobal_mex(Array).");
    }

    if (nlhs != 2) {
        mexErrMsgIdAndTxt("minmaxGlobal_mex:OutputError",
                          "Exactly two output arguments are required.");
    }

    const mxArray* ArrayMx = prhs[0];

    if (mxIsComplex(ArrayMx)) {
        mexErrMsgIdAndTxt("minmaxGlobal_mex:TypeError",
                          "Input must be real.");
    }

    mxClassID ClassID = mxGetClassID(ArrayMx);
    if (!(ClassID == mxDOUBLE_CLASS || ClassID == mxSINGLE_CLASS)) {
        mexErrMsgIdAndTxt("minmaxGlobal_mex:TypeError",
                          "Input must be single or double.");
    }

    std::size_t N = (std::size_t)mxGetNumberOfElements(ArrayMx);
    if (N == 0) {
        mexErrMsgIdAndTxt("minmaxGlobal_mex:EmptyInput",
                          "Input must be non-empty.");
    }

    if (ClassID == mxDOUBLE_CLASS) {
        const double* Array = (const double*)mxGetData(ArrayMx);

        MinMaxPair<double> Res = runMinMaxDouble(Array, N);

        plhs[0] = mxCreateNumericMatrix(1, 1, mxDOUBLE_CLASS, mxREAL);
        plhs[1] = mxCreateNumericMatrix(1, 1, mxDOUBLE_CLASS, mxREAL);

        *(double*)mxGetData(plhs[0]) = Res.MinVal;
        *(double*)mxGetData(plhs[1]) = Res.MaxVal;

    } else {
        const float* Array = (const float*)mxGetData(ArrayMx);

        MinMaxPair<float> Res = runMinMaxSingle(Array, N);

        plhs[0] = mxCreateNumericMatrix(1, 1, mxSINGLE_CLASS, mxREAL);
        plhs[1] = mxCreateNumericMatrix(1, 1, mxSINGLE_CLASS, mxREAL);

        *(float*)mxGetData(plhs[0]) = Res.MinVal;
        *(float*)mxGetData(plhs[1]) = Res.MaxVal;
    }
}
