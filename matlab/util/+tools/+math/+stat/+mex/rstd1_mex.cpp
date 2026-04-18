#include "mex.h"
#include "matrix.h"

#include <algorithm>
#include <cmath>
#include <cstddef>
#include <cstdint>
#include <vector>

#ifdef _OPENMP
#include <omp.h>
#endif

namespace {

constexpr double IQR_TO_SIGMA = 0.74130110925280102553;
constexpr mwSize SMALL_SORT_THRESHOLD = 128;

inline bool is_valid_value(double x) {
    return !std::isnan(x);
}

inline bool is_valid_value(float x) {
    return !std::isnan(static_cast<double>(x));
}

template <typename T>
inline double to_double(T x) {
    return static_cast<double>(x);
}

template <typename T>
void insertion_sort(T* A, mwSize N)
{
    for (mwSize I = 1; I < N; ++I) {
        T Key = A[I];
        mwSize J = I;
        while (J > 0 && A[J - 1] > Key) {
            A[J] = A[J - 1];
            --J;
        }
        A[J] = Key;
    }
}

template <typename T>
double robust_std_small(T* V, mwSize N)
{
    insertion_sort(V, N);

    if (N <= 1) {
        return 0.0;
    }

    const mwIndex K25 = static_cast<mwIndex>(std::floor(0.25 * static_cast<double>(N - 1)));
    const mwIndex K75 = static_cast<mwIndex>(std::floor(0.75 * static_cast<double>(N - 1)));

    const double Q25 = static_cast<double>(V[K25]);
    const double Q75 = static_cast<double>(V[K75]);

    return IQR_TO_SIGMA * (Q75 - Q25);
}

template <typename T>
mwSize count_valid_serial_step(const T* X, mwSize N, mwSize Step)
{
    mwSize Count = 0;
    for (mwSize I = 0; I < N; I += Step) {
        if (is_valid_value(X[I])) {
            ++Count;
        }
    }
    return Count;
}

template <typename T>
mwSize copy_valid_serial_step(const T* X, mwSize N, mwSize Step, T* Out)
{
    mwSize M = 0;
    for (mwSize I = 0; I < N; I += Step) {
        const T Xi = X[I];
        if (is_valid_value(Xi)) {
            Out[M++] = Xi;
        }
    }
    return M;
}

template <typename T>
mwSize copy_valid_openmp_step(const T* X, mwSize N, mwSize Step, T* Out)
{
#ifndef _OPENMP
    return copy_valid_serial_step(X, N, Step, Out);
#else
    const mwSize NStep = (N + Step - 1) / Step;
    const int NT = omp_get_max_threads();

    std::vector<mwSize> Counts(static_cast<size_t>(NT), 0);
    std::vector<mwSize> Starts(static_cast<size_t>(NT), 0);
    std::vector<mwSize> Ends(static_cast<size_t>(NT), 0);

    for (int TID = 0; TID < NT; ++TID) {
        const mwSize B0 = (static_cast<mwSize>(TID) * NStep) / static_cast<mwSize>(NT);
        const mwSize B1 = (static_cast<mwSize>(TID + 1) * NStep) / static_cast<mwSize>(NT);
        Starts[static_cast<size_t>(TID)] = B0;
        Ends[static_cast<size_t>(TID)]   = B1;
    }

    #pragma omp parallel
    {
        const int TID = omp_get_thread_num();
        const mwSize B0 = Starts[static_cast<size_t>(TID)];
        const mwSize B1 = Ends[static_cast<size_t>(TID)];

        mwSize LocalCount = 0;
        for (mwSize B = B0; B < B1; ++B) {
            const mwSize I = B * Step;
            const T Xi = X[I];
            if (is_valid_value(Xi)) {
                ++LocalCount;
            }
        }

        Counts[static_cast<size_t>(TID)] = LocalCount;
    }

    std::vector<mwSize> Offsets(static_cast<size_t>(NT), 0);
    mwSize Total = 0;
    for (int TID = 0; TID < NT; ++TID) {
        Offsets[static_cast<size_t>(TID)] = Total;
        Total += Counts[static_cast<size_t>(TID)];
    }

    #pragma omp parallel
    {
        const int TID = omp_get_thread_num();
        const mwSize B0 = Starts[static_cast<size_t>(TID)];
        const mwSize B1 = Ends[static_cast<size_t>(TID)];
        mwSize Pos = Offsets[static_cast<size_t>(TID)];

        for (mwSize B = B0; B < B1; ++B) {
            const mwSize I = B * Step;
            const T Xi = X[I];
            if (is_valid_value(Xi)) {
                Out[Pos++] = Xi;
            }
        }
    }

    return Total;
#endif
}

template <typename T>
mwSize copy_valid_step(const T* X, mwSize N, mwSize Step, T* Out)
{
#ifdef _OPENMP
    const mwSize NStep = (N + Step - 1) / Step;
    if (NStep >= 1000000) {
        return copy_valid_openmp_step(X, N, Step, Out);
    }
#endif
    return copy_valid_serial_step(X, N, Step, Out);
}

template <typename T>
double robust_std_iqr_exact(T* V, mwSize N)
{
    if (N == 0) {
        return mxGetNaN();
    }
    if (N == 1) {
        return 0.0;
    }

    if (N <= SMALL_SORT_THRESHOLD) {
        return robust_std_small(V, N);
    }

    const mwIndex K25 = static_cast<mwIndex>(std::floor(0.25 * static_cast<double>(N - 1)));
    const mwIndex K75 = static_cast<mwIndex>(std::floor(0.75 * static_cast<double>(N - 1)));

    T* Begin = V;
    T* End   = V + N;

    std::nth_element(Begin, Begin + K75, End);
    const double Q75 = to_double(Begin[K75]);

    std::nth_element(Begin, Begin + K25, Begin + K75 + 1);
    const double Q25 = to_double(Begin[K25]);

    return IQR_TO_SIGMA * (Q75 - Q25);
}

template <typename T>
double compute_rstd_from_input(const T* X, mwSize N, mwSize Step)
{
    if (N == 0) {
        return mxGetNaN();
    }

    const mwSize NStep = (N + Step - 1) / Step;
    if (NStep == 0) {
        return mxGetNaN();
    }

    T* Work = static_cast<T*>(mxMalloc(static_cast<size_t>(NStep) * sizeof(T)));
    if (Work == nullptr) {
        mexErrMsgIdAndTxt("rstd1_mex:AllocationFailed", "Failed to allocate work buffer.");
    }

    double Result;
    try {
        const mwSize M = copy_valid_step(X, N, Step, Work);

        if (M == 0) {
            Result = mxGetNaN();
        } else {
            Result = robust_std_iqr_exact(Work, M);
        }
    } catch (...) {
        mxFree(Work);
        mexErrMsgIdAndTxt("rstd1_mex:InternalError", "Unexpected internal error.");
        return mxGetNaN();
    }

    mxFree(Work);
    return Result;
}

mwSize get_step_argument(const mxArray* StepArg)
{
    if (StepArg == nullptr || mxIsEmpty(StepArg)) {
        return 1;
    }

    if (!mxIsNumeric(StepArg) || mxIsComplex(StepArg) || mxGetNumberOfElements(StepArg) != 1) {
        mexErrMsgIdAndTxt("rstd1_mex:StepType", "Step must be a real numeric scalar.");
    }

    double StepVal = mxGetScalar(StepArg);

    if (std::isnan(StepVal) || std::isinf(StepVal) || StepVal < 1.0) {
        mexErrMsgIdAndTxt("rstd1_mex:StepValue", "Step must be a finite scalar >= 1.");
    }

    double StepIntPart;
    if (std::modf(StepVal, &StepIntPart) != 0.0) {
        mexErrMsgIdAndTxt("rstd1_mex:StepInteger", "Step must be an integer scalar.");
    }

    return static_cast<mwSize>(StepVal);
}

void validate_input(const mxArray* A)
{
    if (A == nullptr) {
        mexErrMsgIdAndTxt("rstd1_mex:NullInput", "Null input.");
    }
    if (mxIsSparse(A)) {
        mexErrMsgIdAndTxt("rstd1_mex:SparseNotSupported", "Sparse input is not supported.");
    }
    if (mxIsComplex(A)) {
        mexErrMsgIdAndTxt("rstd1_mex:ComplexNotSupported", "Complex input is not supported.");
    }
    if (!(mxIsDouble(A) || mxIsSingle(A))) {
        mexErrMsgIdAndTxt("rstd1_mex:TypeError", "Input must be single or double.");
    }
}

} // namespace

void mexFunction(int Nlhs, mxArray* Plhs[], int Nrhs, const mxArray* Prhs[])
{
    if (Nrhs < 1 || Nrhs > 2) {
        mexErrMsgIdAndTxt("rstd1_mex:InputCount", "Usage: Rstd = rstd1_mex(Array, Step);");
    }
    if (Nlhs > 1) {
        mexErrMsgIdAndTxt("rstd1_mex:OutputCount", "One output only.");
    }

    const mxArray* A = Prhs[0];
    validate_input(A);

    const mwSize Step = (Nrhs >= 2) ? get_step_argument(Prhs[1]) : 1;
    const mwSize N = mxGetNumberOfElements(A);

    double Rstd;
    if (mxIsDouble(A)) {
        const double* X = mxGetDoubles(A);
        Rstd = compute_rstd_from_input(X, N, Step);
    } else {
        const float* X = mxGetSingles(A);
        Rstd = compute_rstd_from_input(X, N, Step);
    }

    Plhs[0] = mxCreateDoubleScalar(Rstd);
}
