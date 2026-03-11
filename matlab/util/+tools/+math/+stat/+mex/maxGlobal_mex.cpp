//ex CXXFLAGS='$CXXFLAGS -O3 -march=native -mavx2 -mfma -fopenmp -std=c++17' LDFLAGS='$LDFLAGS -fopenmp' maxGlobal_mex.cpp
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
struct alignas(64) MaxPair {
    T Val;
    std::size_t Ind;
};

template <typename T>
inline bool betterPair(T ValA, std::size_t IndA, T ValB, std::size_t IndB)
{
    return (ValA > ValB) || ((ValA == ValB) && (IndA < IndB));
}

template <typename T>
T maxScalarValueRange(const T* Array, std::size_t I1, std::size_t I2)
{
    T MaxVal = Array[I1];
    for (std::size_t I = I1 + 1; I < I2; ++I) {
        T X = Array[I];
        if (X > MaxVal) {
            MaxVal = X;
        }
    }
    return MaxVal;
}

template <typename T>
MaxPair<T> maxScalarPairRange(const T* Array, std::size_t I1, std::size_t I2)
{
    MaxPair<T> Out;
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
// nlhs==1 kernels: value only
// ============================================================

double maxValueKernelDouble(const double* Array, std::size_t I1, std::size_t I2)
{
    std::size_t N = I2 - I1;
    if (N < 16) {
        return maxScalarValueRange(Array, I1, I2);
    }

    std::size_t I = I1;

    __m256d Max0 = _mm256_loadu_pd(Array + I); I += 4;
    __m256d Max1 = _mm256_loadu_pd(Array + I); I += 4;
    __m256d Max2 = _mm256_loadu_pd(Array + I); I += 4;
    __m256d Max3 = _mm256_loadu_pd(Array + I); I += 4;

    for (; I + 15 < I2; I += 16) {
        __m256d X0 = _mm256_loadu_pd(Array + I + 0);
        __m256d X1 = _mm256_loadu_pd(Array + I + 4);
        __m256d X2 = _mm256_loadu_pd(Array + I + 8);
        __m256d X3 = _mm256_loadu_pd(Array + I + 12);

        Max0 = _mm256_max_pd(Max0, X0);
        Max1 = _mm256_max_pd(Max1, X1);
        Max2 = _mm256_max_pd(Max2, X2);
        Max3 = _mm256_max_pd(Max3, X3);
    }

    Max0 = _mm256_max_pd(Max0, Max1);
    Max2 = _mm256_max_pd(Max2, Max3);
    Max0 = _mm256_max_pd(Max0, Max2);

    double MaxVal = horizontalMax256d(Max0);

    for (; I < I2; ++I) {
        double X = Array[I];
        if (X > MaxVal) {
            MaxVal = X;
        }
    }

    return MaxVal;
}

float maxValueKernelSingle(const float* Array, std::size_t I1, std::size_t I2)
{
    std::size_t N = I2 - I1;
    if (N < 32) {
        return maxScalarValueRange(Array, I1, I2);
    }

    std::size_t I = I1;

    __m256 Max0 = _mm256_loadu_ps(Array + I); I += 8;
    __m256 Max1 = _mm256_loadu_ps(Array + I); I += 8;
    __m256 Max2 = _mm256_loadu_ps(Array + I); I += 8;
    __m256 Max3 = _mm256_loadu_ps(Array + I); I += 8;

    for (; I + 31 < I2; I += 32) {
        __m256 X0 = _mm256_loadu_ps(Array + I + 0);
        __m256 X1 = _mm256_loadu_ps(Array + I + 8);
        __m256 X2 = _mm256_loadu_ps(Array + I + 16);
        __m256 X3 = _mm256_loadu_ps(Array + I + 24);

        Max0 = _mm256_max_ps(Max0, X0);
        Max1 = _mm256_max_ps(Max1, X1);
        Max2 = _mm256_max_ps(Max2, X2);
        Max3 = _mm256_max_ps(Max3, X3);
    }

    Max0 = _mm256_max_ps(Max0, Max1);
    Max2 = _mm256_max_ps(Max2, Max3);
    Max0 = _mm256_max_ps(Max0, Max2);

    float MaxVal = horizontalMax256(Max0);

    for (; I < I2; ++I) {
        float X = Array[I];
        if (X > MaxVal) {
            MaxVal = X;
        }
    }

    return MaxVal;
}

// ============================================================
// nlhs==2 kernels: value + first index
// ============================================================

MaxPair<double> maxPairKernelDouble(const double* Array, std::size_t I1, std::size_t I2)
{
    std::size_t N = I2 - I1;
    if (N < 8) {
        return maxScalarPairRange(Array, I1, I2);
    }

    std::size_t I = I1;

    __m256d MaxV0 = _mm256_loadu_pd(Array + I);
    __m256i MaxI0 = _mm256_setr_epi64x((long long)(I+0), (long long)(I+1), (long long)(I+2), (long long)(I+3));
    I += 4;

    __m256d MaxV1 = _mm256_loadu_pd(Array + I);
    __m256i MaxI1 = _mm256_setr_epi64x((long long)(I+0), (long long)(I+1), (long long)(I+2), (long long)(I+3));
    I += 4;

    for (; I + 7 < I2; I += 8) {
        __m256d X0 = _mm256_loadu_pd(Array + I + 0);
        __m256d X1 = _mm256_loadu_pd(Array + I + 4);

        __m256i Idx0 = _mm256_setr_epi64x((long long)(I+0), (long long)(I+1), (long long)(I+2), (long long)(I+3));
        __m256i Idx1 = _mm256_setr_epi64x((long long)(I+4), (long long)(I+5), (long long)(I+6), (long long)(I+7));

        __m256d Msk0 = _mm256_cmp_pd(X0, MaxV0, _CMP_GT_OQ);
        __m256d Msk1 = _mm256_cmp_pd(X1, MaxV1, _CMP_GT_OQ);

        MaxV0 = _mm256_blendv_pd(MaxV0, X0, Msk0);
        MaxV1 = _mm256_blendv_pd(MaxV1, X1, Msk1);

        __m256i Msk0i = _mm256_castpd_si256(Msk0);
        __m256i Msk1i = _mm256_castpd_si256(Msk1);

        MaxI0 = _mm256_blendv_epi8(MaxI0, Idx0, Msk0i);
        MaxI1 = _mm256_blendv_epi8(MaxI1, Idx1, Msk1i);
    }

    alignas(32) double ValTmp[8];
    alignas(32) std::int64_t IndTmp[8];

    _mm256_store_pd(ValTmp + 0, MaxV0);
    _mm256_store_pd(ValTmp + 4, MaxV1);
    _mm256_store_si256((__m256i*)(IndTmp + 0), MaxI0);
    _mm256_store_si256((__m256i*)(IndTmp + 4), MaxI1);

    MaxPair<double> Out;
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

MaxPair<float> maxPairKernelSingle(const float* Array, std::size_t I1, std::size_t I2)
{
    return maxScalarPairRange(Array, I1, I2);
}

#endif

// ============================================================
// Global wrappers for nlhs==1
// ============================================================

double runMaxValueDouble(const double* Array, std::size_t N)
{
#if defined(_OPENMP)
    constexpr std::size_t MinPerThread = 1 << 20;
    int MaxThreads = omp_get_max_threads();

    if (MaxThreads > 1 && N >= MinPerThread) {
        std::vector<double> ThreadMax((std::size_t)MaxThreads, -std::numeric_limits<double>::max());

        #pragma omp parallel
        {
            int Tid = omp_get_thread_num();
            int Nt  = omp_get_num_threads();

            std::size_t I1 = (N * (std::size_t)Tid) / (std::size_t)Nt;
            std::size_t I2 = (N * (std::size_t)(Tid + 1)) / (std::size_t)Nt;

            if (I2 > I1) {
                #if defined(__AVX2__)
                    ThreadMax[(std::size_t)Tid] = maxValueKernelDouble(Array, I1, I2);
                #else
                    ThreadMax[(std::size_t)Tid] = maxScalarValueRange(Array, I1, I2);
                #endif
            }
        }

        double GlobalMax = ThreadMax[0];
        for (int T = 1; T < MaxThreads; ++T) {
            if (ThreadMax[(std::size_t)T] > GlobalMax) {
                GlobalMax = ThreadMax[(std::size_t)T];
            }
        }
        return GlobalMax;
    }
#endif

#if defined(__AVX2__)
    return maxValueKernelDouble(Array, 0, N);
#else
    return maxScalarValueRange(Array, 0, N);
#endif
}

float runMaxValueSingle(const float* Array, std::size_t N)
{
#if defined(_OPENMP)
    constexpr std::size_t MinPerThread = 1 << 20;
    int MaxThreads = omp_get_max_threads();

    if (MaxThreads > 1 && N >= MinPerThread) {
        std::vector<float> ThreadMax((std::size_t)MaxThreads, -std::numeric_limits<float>::max());

        #pragma omp parallel
        {
            int Tid = omp_get_thread_num();
            int Nt  = omp_get_num_threads();

            std::size_t I1 = (N * (std::size_t)Tid) / (std::size_t)Nt;
            std::size_t I2 = (N * (std::size_t)(Tid + 1)) / (std::size_t)Nt;

            if (I2 > I1) {
                #if defined(__AVX2__)
                    ThreadMax[(std::size_t)Tid] = maxValueKernelSingle(Array, I1, I2);
                #else
                    ThreadMax[(std::size_t)Tid] = maxScalarValueRange(Array, I1, I2);
                #endif
            }
        }

        float GlobalMax = ThreadMax[0];
        for (int T = 1; T < MaxThreads; ++T) {
            if (ThreadMax[(std::size_t)T] > GlobalMax) {
                GlobalMax = ThreadMax[(std::size_t)T];
            }
        }
        return GlobalMax;
    }
#endif

#if defined(__AVX2__)
    return maxValueKernelSingle(Array, 0, N);
#else
    return maxScalarValueRange(Array, 0, N);
#endif
}

// ============================================================
// Global wrappers for nlhs==2
// ============================================================

MaxPair<double> runMaxPairDouble(const double* Array, std::size_t N)
{
#if defined(_OPENMP)
    constexpr std::size_t MinPerThread = 1 << 20;
    int MaxThreads = omp_get_max_threads();

    if (MaxThreads > 1 && N >= MinPerThread) {
        std::vector<MaxPair<double>> ThreadMax((std::size_t)MaxThreads);

        for (int T = 0; T < MaxThreads; ++T) {
            ThreadMax[(std::size_t)T].Val = -std::numeric_limits<double>::max();
            ThreadMax[(std::size_t)T].Ind = N;
        }

        #pragma omp parallel
        {
            int Tid = omp_get_thread_num();
            int Nt  = omp_get_num_threads();

            std::size_t I1 = (N * (std::size_t)Tid) / (std::size_t)Nt;
            std::size_t I2 = (N * (std::size_t)(Tid + 1)) / (std::size_t)Nt;

            if (I2 > I1) {
                #if defined(__AVX2__)
                    ThreadMax[(std::size_t)Tid] = maxPairKernelDouble(Array, I1, I2);
                #else
                    ThreadMax[(std::size_t)Tid] = maxScalarPairRange(Array, I1, I2);
                #endif
            }
        }

        MaxPair<double> Global = ThreadMax[0];
        for (int T = 1; T < MaxThreads; ++T) {
            const MaxPair<double>& P = ThreadMax[(std::size_t)T];
            if (betterPair(P.Val, P.Ind, Global.Val, Global.Ind)) {
                Global = P;
            }
        }
        return Global;
    }
#endif

#if defined(__AVX2__)
    return maxPairKernelDouble(Array, 0, N);
#else
    return maxScalarPairRange(Array, 0, N);
#endif
}

MaxPair<float> runMaxPairSingle(const float* Array, std::size_t N)
{
#if defined(_OPENMP)
    constexpr std::size_t MinPerThread = 1 << 20;
    int MaxThreads = omp_get_max_threads();

    if (MaxThreads > 1 && N >= MinPerThread) {
        std::vector<MaxPair<float>> ThreadMax((std::size_t)MaxThreads);

        for (int T = 0; T < MaxThreads; ++T) {
            ThreadMax[(std::size_t)T].Val = -std::numeric_limits<float>::max();
            ThreadMax[(std::size_t)T].Ind = N;
        }

        #pragma omp parallel
        {
            int Tid = omp_get_thread_num();
            int Nt  = omp_get_num_threads();

            std::size_t I1 = (N * (std::size_t)Tid) / (std::size_t)Nt;
            std::size_t I2 = (N * (std::size_t)(Tid + 1)) / (std::size_t)Nt;

            if (I2 > I1) {
                #if defined(__AVX2__)
                    ThreadMax[(std::size_t)Tid] = maxPairKernelSingle(Array, I1, I2);
                #else
                    ThreadMax[(std::size_t)Tid] = maxScalarPairRange(Array, I1, I2);
                #endif
            }
        }

        MaxPair<float> Global = ThreadMax[0];
        for (int T = 1; T < MaxThreads; ++T) {
            const MaxPair<float>& P = ThreadMax[(std::size_t)T];
            if (betterPair(P.Val, P.Ind, Global.Val, Global.Ind)) {
                Global = P;
            }
        }
        return Global;
    }
#endif

#if defined(__AVX2__)
    return maxPairKernelSingle(Array, 0, N);
#else
    return maxScalarPairRange(Array, 0, N);
#endif
}

// ============================================================
// MEX gateway
// ============================================================

void mexFunction(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[])
{
    if (nrhs != 1) {
        mexErrMsgIdAndTxt("maxGlobal_mex:InputError", "One input is required.");
    }

    if (nlhs > 2) {
        mexErrMsgIdAndTxt("maxGlobal_mex:OutputError", "Too many output arguments.");
    }

    const mxArray* ArrayMx = prhs[0];

    if (mxIsComplex(ArrayMx)) {
        mexErrMsgIdAndTxt("maxGlobal_mex:TypeError", "Input must be real.");
    }

    mxClassID ClassID = mxGetClassID(ArrayMx);
    if (!(ClassID == mxDOUBLE_CLASS || ClassID == mxSINGLE_CLASS)) {
        mexErrMsgIdAndTxt("maxGlobal_mex:TypeError", "Input must be single or double.");
    }

    std::size_t N = (std::size_t)mxGetNumberOfElements(ArrayMx);
    if (N == 0) {
        mexErrMsgIdAndTxt("maxGlobal_mex:EmptyInput", "Input must be non-empty.");
    }

    if (ClassID == mxDOUBLE_CLASS) {
        const double* Array = (const double*)mxGetData(ArrayMx);

        if (nlhs <= 1) {
            double MaxVal = runMaxValueDouble(Array, N);
            plhs[0] = mxCreateNumericMatrix(1, 1, mxDOUBLE_CLASS, mxREAL);
            *(double*)mxGetData(plhs[0]) = MaxVal;
        } else {
            MaxPair<double> Res = runMaxPairDouble(Array, N);
            plhs[0] = mxCreateNumericMatrix(1, 1, mxDOUBLE_CLASS, mxREAL);
            *(double*)mxGetData(plhs[0]) = Res.Val;
            plhs[1] = mxCreateDoubleScalar((double)(Res.Ind + 1));
        }

    } else {
        const float* Array = (const float*)mxGetData(ArrayMx);

        if (nlhs <= 1) {
            float MaxVal = runMaxValueSingle(Array, N);
            plhs[0] = mxCreateNumericMatrix(1, 1, mxSINGLE_CLASS, mxREAL);
            *(float*)mxGetData(plhs[0]) = MaxVal;
        } else {
            MaxPair<float> Res = runMaxPairSingle(Array, N);
            plhs[0] = mxCreateNumericMatrix(1, 1, mxSINGLE_CLASS, mxREAL);
            *(float*)mxGetData(plhs[0]) = Res.Val;
            plhs[1] = mxCreateDoubleScalar((double)(Res.Ind + 1));
        }
    }
}
