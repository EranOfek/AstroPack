// mex CXXFLAGS='$CXXFLAGS -O3 -march=native -mavx2 -mfma -fopenmp -std=c++17' LDFLAGS='$LDFLAGS -fopenmp' minGlobal_mex.cpp
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
struct alignas(64) MinPair {
    T Val;
    std::size_t Ind;
};

template <typename T>
inline bool betterPair(T ValA, std::size_t IndA, T ValB, std::size_t IndB)
{
    return (ValA < ValB) || ((ValA == ValB) && (IndA < IndB));
}

template <typename T>
T minScalarValueRange(const T* Array, std::size_t I1, std::size_t I2)
{
    T MinVal = Array[I1];
    for (std::size_t I = I1 + 1; I < I2; ++I) {
        T X = Array[I];
        if (X < MinVal) {
            MinVal = X;
        }
    }
    return MinVal;
}

template <typename T>
MinPair<T> minScalarPairRange(const T* Array, std::size_t I1, std::size_t I2)
{
    MinPair<T> Out;
    Out.Val = Array[I1];
    Out.Ind = I1;

    for (std::size_t I = I1 + 1; I < I2; ++I) {
        T X = Array[I];
        if (betterPair(X, I, Out.Val, Out.Ind)) {
            Out.Val = X;
            Out.Ind = I;
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

// ============================================================
// nlhs==1 kernels: value only
// ============================================================

double minValueKernelDouble(const double* Array, std::size_t I1, std::size_t I2)
{
    std::size_t N = I2 - I1;
    if (N < 16) {
        return minScalarValueRange(Array, I1, I2);
    }

    std::size_t I = I1;

    __m256d Min0 = _mm256_loadu_pd(Array + I); I += 4;
    __m256d Min1 = _mm256_loadu_pd(Array + I); I += 4;
    __m256d Min2 = _mm256_loadu_pd(Array + I); I += 4;
    __m256d Min3 = _mm256_loadu_pd(Array + I); I += 4;

    for (; I + 15 < I2; I += 16) {
        __m256d X0 = _mm256_loadu_pd(Array + I + 0);
        __m256d X1 = _mm256_loadu_pd(Array + I + 4);
        __m256d X2 = _mm256_loadu_pd(Array + I + 8);
        __m256d X3 = _mm256_loadu_pd(Array + I + 12);

        Min0 = _mm256_min_pd(Min0, X0);
        Min1 = _mm256_min_pd(Min1, X1);
        Min2 = _mm256_min_pd(Min2, X2);
        Min3 = _mm256_min_pd(Min3, X3);
    }

    Min0 = _mm256_min_pd(Min0, Min1);
    Min2 = _mm256_min_pd(Min2, Min3);
    Min0 = _mm256_min_pd(Min0, Min2);

    double MinVal = horizontalMin256d(Min0);

    for (; I < I2; ++I) {
        double X = Array[I];
        if (X < MinVal) {
            MinVal = X;
        }
    }

    return MinVal;
}

float minValueKernelSingle(const float* Array, std::size_t I1, std::size_t I2)
{
    std::size_t N = I2 - I1;
    if (N < 32) {
        return minScalarValueRange(Array, I1, I2);
    }

    std::size_t I = I1;

    __m256 Min0 = _mm256_loadu_ps(Array + I); I += 8;
    __m256 Min1 = _mm256_loadu_ps(Array + I); I += 8;
    __m256 Min2 = _mm256_loadu_ps(Array + I); I += 8;
    __m256 Min3 = _mm256_loadu_ps(Array + I); I += 8;

    for (; I + 31 < I2; I += 32) {
        __m256 X0 = _mm256_loadu_ps(Array + I + 0);
        __m256 X1 = _mm256_loadu_ps(Array + I + 8);
        __m256 X2 = _mm256_loadu_ps(Array + I + 16);
        __m256 X3 = _mm256_loadu_ps(Array + I + 24);

        Min0 = _mm256_min_ps(Min0, X0);
        Min1 = _mm256_min_ps(Min1, X1);
        Min2 = _mm256_min_ps(Min2, X2);
        Min3 = _mm256_min_ps(Min3, X3);
    }

    Min0 = _mm256_min_ps(Min0, Min1);
    Min2 = _mm256_min_ps(Min2, Min3);
    Min0 = _mm256_min_ps(Min0, Min2);

    float MinVal = horizontalMin256(Min0);

    for (; I < I2; ++I) {
        float X = Array[I];
        if (X < MinVal) {
            MinVal = X;
        }
    }

    return MinVal;
}

// ============================================================
// nlhs==2 kernels: value + first index
// ============================================================

MinPair<double> minPairKernelDouble(const double* Array, std::size_t I1, std::size_t I2)
{
    std::size_t N = I2 - I1;
    if (N < 8) {
        return minScalarPairRange(Array, I1, I2);
    }

    std::size_t I = I1;

    __m256d MinV0 = _mm256_loadu_pd(Array + I);
    __m256i MinI0 = _mm256_setr_epi64x((long long)(I+0), (long long)(I+1), (long long)(I+2), (long long)(I+3));
    I += 4;

    __m256d MinV1 = _mm256_loadu_pd(Array + I);
    __m256i MinI1 = _mm256_setr_epi64x((long long)(I+0), (long long)(I+1), (long long)(I+2), (long long)(I+3));
    I += 4;

    for (; I + 7 < I2; I += 8) {
        __m256d X0 = _mm256_loadu_pd(Array + I + 0);
        __m256d X1 = _mm256_loadu_pd(Array + I + 4);

        __m256i Idx0 = _mm256_setr_epi64x((long long)(I+0), (long long)(I+1), (long long)(I+2), (long long)(I+3));
        __m256i Idx1 = _mm256_setr_epi64x((long long)(I+4), (long long)(I+5), (long long)(I+6), (long long)(I+7));

        __m256d Msk0 = _mm256_cmp_pd(X0, MinV0, _CMP_LT_OQ);
        __m256d Msk1 = _mm256_cmp_pd(X1, MinV1, _CMP_LT_OQ);

        MinV0 = _mm256_blendv_pd(MinV0, X0, Msk0);
        MinV1 = _mm256_blendv_pd(MinV1, X1, Msk1);

        __m256i Msk0i = _mm256_castpd_si256(Msk0);
        __m256i Msk1i = _mm256_castpd_si256(Msk1);

        MinI0 = _mm256_blendv_epi8(MinI0, Idx0, Msk0i);
        MinI1 = _mm256_blendv_epi8(MinI1, Idx1, Msk1i);
    }

    alignas(32) double ValTmp[8];
    alignas(32) std::int64_t IndTmp[8];

    _mm256_store_pd(ValTmp + 0, MinV0);
    _mm256_store_pd(ValTmp + 4, MinV1);
    _mm256_store_si256((__m256i*)(IndTmp + 0), MinI0);
    _mm256_store_si256((__m256i*)(IndTmp + 4), MinI1);

    MinPair<double> Out;
    Out.Val = ValTmp[0];
    Out.Ind = (std::size_t)IndTmp[0];

    for (int K = 1; K < 8; ++K) {
        if (betterPair(ValTmp[K], (std::size_t)IndTmp[K], Out.Val, Out.Ind)) {
            Out.Val = ValTmp[K];
            Out.Ind = (std::size_t)IndTmp[K];
        }
    }

    for (; I < I2; ++I) {
        double X = Array[I];
        if (betterPair(X, I, Out.Val, Out.Ind)) {
            Out.Val = X;
            Out.Ind = I;
        }
    }

    return Out;
}

MinPair<float> minPairKernelSingle(const float* Array, std::size_t I1, std::size_t I2)
{
    return minScalarPairRange(Array, I1, I2);
}

#endif

// ============================================================
// Global wrappers for nlhs==1
// ============================================================

double runMinValueDouble(const double* Array, std::size_t N)
{
#if defined(_OPENMP)
    constexpr std::size_t MinPerThread = 1 << 20;
    int MaxThreads = omp_get_max_threads();

    if (MaxThreads > 1 && N >= MinPerThread) {
        std::vector<double> ThreadMin((std::size_t)MaxThreads, std::numeric_limits<double>::max());

        #pragma omp parallel
        {
            int Tid = omp_get_thread_num();
            int Nt  = omp_get_num_threads();

            std::size_t I1 = (N * (std::size_t)Tid) / (std::size_t)Nt;
            std::size_t I2 = (N * (std::size_t)(Tid + 1)) / (std::size_t)Nt;

            if (I2 > I1) {
                #if defined(__AVX2__)
                    ThreadMin[(std::size_t)Tid] = minValueKernelDouble(Array, I1, I2);
                #else
                    ThreadMin[(std::size_t)Tid] = minScalarValueRange(Array, I1, I2);
                #endif
            }
        }

        double GlobalMin = ThreadMin[0];
        for (int T = 1; T < MaxThreads; ++T) {
            if (ThreadMin[(std::size_t)T] < GlobalMin) {
                GlobalMin = ThreadMin[(std::size_t)T];
            }
        }
        return GlobalMin;
    }
#endif

#if defined(__AVX2__)
    return minValueKernelDouble(Array, 0, N);
#else
    return minScalarValueRange(Array, 0, N);
#endif
}

float runMinValueSingle(const float* Array, std::size_t N)
{
#if defined(_OPENMP)
    constexpr std::size_t MinPerThread = 1 << 20;
    int MaxThreads = omp_get_max_threads();

    if (MaxThreads > 1 && N >= MinPerThread) {
        std::vector<float> ThreadMin((std::size_t)MaxThreads, std::numeric_limits<float>::max());

        #pragma omp parallel
        {
            int Tid = omp_get_thread_num();
            int Nt  = omp_get_num_threads();

            std::size_t I1 = (N * (std::size_t)Tid) / (std::size_t)Nt;
            std::size_t I2 = (N * (std::size_t)(Tid + 1)) / (std::size_t)Nt;

            if (I2 > I1) {
                #if defined(__AVX2__)
                    ThreadMin[(std::size_t)Tid] = minValueKernelSingle(Array, I1, I2);
                #else
                    ThreadMin[(std::size_t)Tid] = minScalarValueRange(Array, I1, I2);
                #endif
            }
        }

        float GlobalMin = ThreadMin[0];
        for (int T = 1; T < MaxThreads; ++T) {
            if (ThreadMin[(std::size_t)T] < GlobalMin) {
                GlobalMin = ThreadMin[(std::size_t)T];
            }
        }
        return GlobalMin;
    }
#endif

#if defined(__AVX2__)
    return minValueKernelSingle(Array, 0, N);
#else
    return minScalarValueRange(Array, 0, N);
#endif
}

// ============================================================
// Global wrappers for nlhs==2
// ============================================================

MinPair<double> runMinPairDouble(const double* Array, std::size_t N)
{
#if defined(_OPENMP)
    constexpr std::size_t MinPerThread = 1 << 20;
    int MaxThreads = omp_get_max_threads();

    if (MaxThreads > 1 && N >= MinPerThread) {
        std::vector<MinPair<double>> ThreadMin((std::size_t)MaxThreads);

        for (int T = 0; T < MaxThreads; ++T) {
            ThreadMin[(std::size_t)T].Val = std::numeric_limits<double>::max();
            ThreadMin[(std::size_t)T].Ind = N;
        }

        #pragma omp parallel
        {
            int Tid = omp_get_thread_num();
            int Nt  = omp_get_num_threads();

            std::size_t I1 = (N * (std::size_t)Tid) / (std::size_t)Nt;
            std::size_t I2 = (N * (std::size_t)(Tid + 1)) / (std::size_t)Nt;

            if (I2 > I1) {
                #if defined(__AVX2__)
                    ThreadMin[(std::size_t)Tid] = minPairKernelDouble(Array, I1, I2);
                #else
                    ThreadMin[(std::size_t)Tid] = minScalarPairRange(Array, I1, I2);
                #endif
            }
        }

        MinPair<double> Global = ThreadMin[0];
        for (int T = 1; T < MaxThreads; ++T) {
            const MinPair<double>& P = ThreadMin[(std::size_t)T];
            if (betterPair(P.Val, P.Ind, Global.Val, Global.Ind)) {
                Global = P;
            }
        }
        return Global;
    }
#endif

#if defined(__AVX2__)
    return minPairKernelDouble(Array, 0, N);
#else
    return minScalarPairRange(Array, 0, N);
#endif
}

MinPair<float> runMinPairSingle(const float* Array, std::size_t N)
{
#if defined(_OPENMP)
    constexpr std::size_t MinPerThread = 1 << 20;
    int MaxThreads = omp_get_max_threads();

    if (MaxThreads > 1 && N >= MinPerThread) {
        std::vector<MinPair<float>> ThreadMin((std::size_t)MaxThreads);

        for (int T = 0; T < MaxThreads; ++T) {
            ThreadMin[(std::size_t)T].Val = std::numeric_limits<float>::max();
            ThreadMin[(std::size_t)T].Ind = N;
        }

        #pragma omp parallel
        {
            int Tid = omp_get_thread_num();
            int Nt  = omp_get_num_threads();

            std::size_t I1 = (N * (std::size_t)Tid) / (std::size_t)Nt;
            std::size_t I2 = (N * (std::size_t)(Tid + 1)) / (std::size_t)Nt;

            if (I2 > I1) {
                #if defined(__AVX2__)
                    ThreadMin[(std::size_t)Tid] = minPairKernelSingle(Array, I1, I2);
                #else
                    ThreadMin[(std::size_t)Tid] = minScalarPairRange(Array, I1, I2);
                #endif
            }
        }

        MinPair<float> Global = ThreadMin[0];
        for (int T = 1; T < MaxThreads; ++T) {
            const MinPair<float>& P = ThreadMin[(std::size_t)T];
            if (betterPair(P.Val, P.Ind, Global.Val, Global.Ind)) {
                Global = P;
            }
        }
        return Global;
    }
#endif

#if defined(__AVX2__)
    return minPairKernelSingle(Array, 0, N);
#else
    return minScalarPairRange(Array, 0, N);
#endif
}

// ============================================================
// MEX gateway
// ============================================================

void mexFunction(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[])
{
    if (nrhs != 1) {
        mexErrMsgIdAndTxt("minGlobal_mex:InputError", "One input is required.");
    }

    if (nlhs > 2) {
        mexErrMsgIdAndTxt("minGlobal_mex:OutputError", "Too many output arguments.");
    }

    const mxArray* ArrayMx = prhs[0];

    if (mxIsComplex(ArrayMx)) {
        mexErrMsgIdAndTxt("minGlobal_mex:TypeError", "Input must be real.");
    }

    mxClassID ClassID = mxGetClassID(ArrayMx);
    if (!(ClassID == mxDOUBLE_CLASS || ClassID == mxSINGLE_CLASS)) {
        mexErrMsgIdAndTxt("minGlobal_mex:TypeError", "Input must be single or double.");
    }

    std::size_t N = (std::size_t)mxGetNumberOfElements(ArrayMx);
    if (N == 0) {
        mexErrMsgIdAndTxt("minGlobal_mex:EmptyInput", "Input must be non-empty.");
    }

    // ========================================================
    // nlhs == 1  --> value-only kernel
    // nlhs == 2  --> value+index kernel
    // ========================================================

    if (ClassID == mxDOUBLE_CLASS) {
        const double* Array = (const double*)mxGetData(ArrayMx);

        if (nlhs <= 1) {
            double MinVal = runMinValueDouble(Array, N);
            plhs[0] = mxCreateNumericMatrix(1, 1, mxDOUBLE_CLASS, mxREAL);
            *(double*)mxGetData(plhs[0]) = MinVal;
        } else {
            MinPair<double> Res = runMinPairDouble(Array, N);
            plhs[0] = mxCreateNumericMatrix(1, 1, mxDOUBLE_CLASS, mxREAL);
            *(double*)mxGetData(plhs[0]) = Res.Val;
            plhs[1] = mxCreateDoubleScalar((double)(Res.Ind + 1));
        }

    } else {
        const float* Array = (const float*)mxGetData(ArrayMx);

        if (nlhs <= 1) {
            float MinVal = runMinValueSingle(Array, N);
            plhs[0] = mxCreateNumericMatrix(1, 1, mxSINGLE_CLASS, mxREAL);
            *(float*)mxGetData(plhs[0]) = MinVal;
        } else {
            MinPair<float> Res = runMinPairSingle(Array, N);
            plhs[0] = mxCreateNumericMatrix(1, 1, mxSINGLE_CLASS, mxREAL);
            *(float*)mxGetData(plhs[0]) = Res.Val;
            plhs[1] = mxCreateDoubleScalar((double)(Res.Ind + 1));
        }
    }
}
