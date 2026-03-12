#include "mex.h"
#include <cmath>
#include <cstdint>
#include <type_traits>

#ifdef _OPENMP
#include <omp.h>
#endif

bool getIgnoreNaN(int Nrhs, const mxArray *Prhs[]) {
    if (Nrhs < 2) {
        return true;
    }

    const mxArray* In = Prhs[1];

    if (mxIsLogicalScalar(In)) {
        return mxIsLogicalScalarTrue(In);
    }

    if (mxIsNumeric(In) && mxGetNumberOfElements(In) == 1) {
        return mxGetScalar(In) != 0.0;
    }

    mexErrMsgIdAndTxt("meanStd:InvalidIgnoreNaN",
                      "IgnoreNaN must be a logical or numeric scalar.");
    return true;
}

template <typename T>
void computeMeanStdSum(const T* Data,
                       mwSize N,
                       bool IgnoreNaN,
                       double& Mean,
                       double& Std,
                       double& Sum)
{
    if (N == 0) {
        Mean = mxGetNaN();
        Std  = mxGetNaN();
        Sum  = 0.0;
        return;
    }

    double SumSq = 0.0;
    Sum = 0.0;
    std::int64_t Count = 0;

    if (IgnoreNaN) {
        #ifdef _OPENMP
        #pragma omp parallel for simd reduction(+:Sum,SumSq,Count)
        #endif
        for (mwSignedIndex I = 0; I < static_cast<mwSignedIndex>(N); ++I) {
            const double V = static_cast<double>(Data[I]);
            if (!std::isnan(V)) {
                Sum   += V;
                SumSq += V * V;
                Count += 1;
            }
        }
    } else {
        Count = static_cast<std::int64_t>(N);

        #ifdef _OPENMP
        #pragma omp parallel for simd reduction(+:Sum,SumSq)
        #endif
        for (mwSignedIndex I = 0; I < static_cast<mwSignedIndex>(N); ++I) {
            const double V = static_cast<double>(Data[I]);
            Sum   += V;
            SumSq += V * V;
        }
    }

    if (Count == 0) {
        Mean = mxGetNaN();
        Std  = mxGetNaN();
        Sum  = mxGetNaN();
        return;
    }

    Mean = Sum / static_cast<double>(Count);

    if (Count == 1) {
        Std = 0.0;
        return;
    }

    double Var = (SumSq - (Sum * Sum) / static_cast<double>(Count)) /
                 static_cast<double>(Count - 1);

    if (Var < 0.0 && Var > -1e-15 * std::fabs(SumSq)) {
        Var = 0.0;
    }

    Std = (Var >= 0.0) ? std::sqrt(Var) : mxGetNaN();
}

template <typename T>
void runTyped(int Nlhs, mxArray* Plhs[], int Nrhs, const mxArray* Prhs[]) {
    const mxArray* Array = Prhs[0];
    const T* Data = static_cast<const T*>(mxGetData(Array));
    const mwSize N = mxGetNumberOfElements(Array);
    const bool IgnoreNaN = getIgnoreNaN(Nrhs, Prhs);

    double MeanD, StdD, SumD;
    computeMeanStdSum<T>(Data, N, IgnoreNaN, MeanD, StdD, SumD);

    mxClassID ClassID = mxGetClassID(Array);

    Plhs[0] = mxCreateNumericMatrix(1, 1, ClassID, mxREAL);
    Plhs[1] = mxCreateNumericMatrix(1, 1, ClassID, mxREAL);

    if (Nlhs > 2) {
        Plhs[2] = mxCreateNumericMatrix(1, 1, ClassID, mxREAL);
    }

    if constexpr (std::is_same<T, double>::value) {
        *mxGetPr(Plhs[0]) = MeanD;
        *mxGetPr(Plhs[1]) = StdD;
        if (Nlhs > 2) {
            *mxGetPr(Plhs[2]) = SumD;
        }
    } else {
        *static_cast<float*>(mxGetData(Plhs[0])) = static_cast<float>(MeanD);
        *static_cast<float*>(mxGetData(Plhs[1])) = static_cast<float>(StdD);
        if (Nlhs > 2) {
            *static_cast<float*>(mxGetData(Plhs[2])) = static_cast<float>(SumD);
        }
    }
}

void mexFunction(int Nlhs, mxArray* Plhs[], int Nrhs, const mxArray* Prhs[]) {
    if (Nrhs < 1 || Nrhs > 2) {
        mexErrMsgIdAndTxt("meanStd:InvalidNumInputs",
                          "Usage: [Mean, Std, Sum] = meanStd(Array, IgnoreNaN);");
    }

    if (Nlhs > 3) {
        mexErrMsgIdAndTxt("meanStd:InvalidNumOutputs",
                          "Too many output arguments.");
    }

    const mxArray* Array = Prhs[0];

    if (mxIsComplex(Array)) {
        mexErrMsgIdAndTxt("meanStd:ComplexNotSupported",
                          "Complex arrays are not supported.");
    }

    if (!mxIsSingle(Array) && !mxIsDouble(Array)) {
        mexErrMsgIdAndTxt("meanStd:TypeNotSupported",
                          "Array must be single or double.");
    }

    if (Nlhs < 1) {
        mexErrMsgIdAndTxt("meanStd:NoOutput",
                          "At least one output is required.");
    }

    if (mxIsDouble(Array)) {
        runTyped<double>(Nlhs, Plhs, Nrhs, Prhs);
    } else {
        runTyped<float>(Nlhs, Plhs, Nrhs, Prhs);
    }
}
