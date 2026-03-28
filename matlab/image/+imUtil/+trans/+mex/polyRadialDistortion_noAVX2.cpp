// mex -R2018a CXXFLAGS='$CXXFLAGS -O3 -fopenmp -march=native' LDFLAGS='$LDFLAGS -fopenmp' polyRadialDistortion_noAVX2.cpp


#include "mex.h"
#include <cmath>
#include <cstdint>

#ifdef _OPENMP
#include <omp.h>
#endif

// ============================================================================
// Helpers
// ============================================================================

template <typename T>
inline T fastPow(const T Base, const T Exp)
{
    if (Exp == T(0)) return T(1);
    if (Exp == T(1)) return Base;
    if (Exp == T(2)) return Base * Base;
    if (Exp == T(3)) return Base * Base * Base;
    return std::pow(Base, Exp);
}

template <typename T>
inline T powInt0to5(const T Base, const int P)
{
    switch (P)
    {
        case 0: return T(1);
        case 1: return Base;
        case 2: return Base * Base;
        case 3: return Base * Base * Base;
        case 4:
        {
            const T B2 = Base * Base;
            return B2 * B2;
        }
        case 5:
        {
            const T B2 = Base * Base;
            return B2 * B2 * Base;
        }
        default:
            return std::pow(Base, static_cast<T>(P));
    }
}

template <typename T>
bool getSmallIntPowers(const T* Pwr, const mwSize M, int* OutIntPwr)
{
    for (mwSize k = 0; k < M; ++k)
    {
        const T V = Pwr[k];
        const int IV = static_cast<int>(V);

        if (V != static_cast<T>(IV) || IV < 0 || IV > 5)
        {
            return false;
        }

        OutIntPwr[k] = IV;
    }

    return true;
}

template <typename T>
bool getUnitStepSequence01(const T* Pwr, const mwSize M, int& StartVal)
{
    if (M == 0)
    {
        StartVal = 0;
        return true;
    }

    const T V0 = Pwr[0];
    if (V0 == T(0))
    {
        StartVal = 0;
    }
    else if (V0 == T(1))
    {
        StartVal = 1;
    }
    else
    {
        return false;
    }

    for (mwSize k = 0; k < M; ++k)
    {
        const T Expected = static_cast<T>(StartVal + static_cast<int>(k));
        if (Pwr[k] != Expected)
        {
            return false;
        }
    }

    return true;
}

template <typename T>
inline T evalPowerTerm(
    const T Base,
    const bool IsSeq,
    const T SeqVal,
    const bool IsSmallInt,
    const int SmallIntP,
    const T GenericP)
{
    if (IsSeq)
    {
        return SeqVal;
    }
    else if (IsSmallInt)
    {
        return powInt0to5(Base, SmallIntP);
    }
    else
    {
        return fastPow(Base, GenericP);
    }
}

// ============================================================================
// Core
// ============================================================================

template <typename T>
void computePolyRadialDistortion(
    const T* X,
    const T* Y,
    const T* R,
    const bool RIsScalar,
    const T* CoefX,
    const T* X_Xpower,
    const T* X_Ypower,
    const T* X_Rpower,
    const mwSize N,
    const mwSize M,
    T* Xd)
{
    const T RScalar = RIsScalar ? R[0] : T(0);
    const bool RScalarIsOne = RIsScalar && (RScalar == T(1));

    int* XPowInt = static_cast<int*>(mxMalloc(M * sizeof(int)));
    int* YPowInt = static_cast<int*>(mxMalloc(M * sizeof(int)));
    int* RPowInt = static_cast<int*>(mxMalloc(M * sizeof(int)));

    const bool XSmallInt = getSmallIntPowers(X_Xpower, M, XPowInt);
    const bool YSmallInt = getSmallIntPowers(X_Ypower, M, YPowInt);
    const bool RSmallInt = getSmallIntPowers(X_Rpower, M, RPowInt);

    int XSeqStart = 0;
    int YSeqStart = 0;
    int RSeqStart = 0;

    const bool XSeq = getUnitStepSequence01(X_Xpower, M, XSeqStart);
    const bool YSeq = getUnitStepSequence01(X_Ypower, M, YSeqStart);
    const bool RSeq = getUnitStepSequence01(X_Rpower, M, RSeqStart);

    const bool UseSeqBranch = XSeq || YSeq || (!RScalarIsOne && RSeq);

    // ------------------------------------------------------------------------
    // Branch 1: at least one power vector is 0:K or 1:K+1
    // ------------------------------------------------------------------------
    if (UseSeqBranch)
    {
        if (RScalarIsOne)
        {
            #ifdef _OPENMP
            #pragma omp parallel for
            #endif
            for (mwSignedIndex i = 0; i < static_cast<mwSignedIndex>(N); ++i)
            {
                const T Xi = X[i];
                const T Yi = Y[i];

                T XSeqVal = T(1);
                T YSeqVal = T(1);

                if (XSeq)
                {
                    XSeqVal = (XSeqStart == 0) ? T(1) : Xi;
                }
                if (YSeq)
                {
                    YSeqVal = (YSeqStart == 0) ? T(1) : Yi;
                }

                T SumVal = T(0);

                for (mwSize k = 0; k < M; ++k)
                {
                    const T XTerm = evalPowerTerm(
                        Xi, XSeq, XSeqVal, XSmallInt, XPowInt[k], X_Xpower[k]);

                    const T YTerm = evalPowerTerm(
                        Yi, YSeq, YSeqVal, YSmallInt, YPowInt[k], X_Ypower[k]);

                    SumVal += CoefX[k] * XTerm * YTerm;

                    if (XSeq) XSeqVal *= Xi;
                    if (YSeq) YSeqVal *= Yi;
                }

                Xd[i] = SumVal;
            }
        }
        else if (RIsScalar)
        {
            T* CoefEff = static_cast<T*>(mxMalloc(M * sizeof(T)));

            if (RSeq)
            {
                T RSeqVal = (RSeqStart == 0) ? T(1) : RScalar;

                for (mwSize k = 0; k < M; ++k)
                {
                    CoefEff[k] = CoefX[k] * RSeqVal;
                    RSeqVal *= RScalar;
                }
            }
            else if (RSmallInt)
            {
                for (mwSize k = 0; k < M; ++k)
                {
                    CoefEff[k] = CoefX[k] * powInt0to5(RScalar, RPowInt[k]);
                }
            }
            else
            {
                for (mwSize k = 0; k < M; ++k)
                {
                    CoefEff[k] = CoefX[k] * fastPow(RScalar, X_Rpower[k]);
                }
            }

            #ifdef _OPENMP
            #pragma omp parallel for
            #endif
            for (mwSignedIndex i = 0; i < static_cast<mwSignedIndex>(N); ++i)
            {
                const T Xi = X[i];
                const T Yi = Y[i];

                T XSeqVal = T(1);
                T YSeqVal = T(1);

                if (XSeq)
                {
                    XSeqVal = (XSeqStart == 0) ? T(1) : Xi;
                }
                if (YSeq)
                {
                    YSeqVal = (YSeqStart == 0) ? T(1) : Yi;
                }

                T SumVal = T(0);

                for (mwSize k = 0; k < M; ++k)
                {
                    const T XTerm = evalPowerTerm(
                        Xi, XSeq, XSeqVal, XSmallInt, XPowInt[k], X_Xpower[k]);

                    const T YTerm = evalPowerTerm(
                        Yi, YSeq, YSeqVal, YSmallInt, YPowInt[k], X_Ypower[k]);

                    SumVal += CoefEff[k] * XTerm * YTerm;

                    if (XSeq) XSeqVal *= Xi;
                    if (YSeq) YSeqVal *= Yi;
                }

                Xd[i] = SumVal;
            }

            mxFree(CoefEff);
        }
        else
        {
            #ifdef _OPENMP
            #pragma omp parallel for
            #endif
            for (mwSignedIndex i = 0; i < static_cast<mwSignedIndex>(N); ++i)
            {
                const T Xi = X[i];
                const T Yi = Y[i];
                const T Ri = R[i];

                T XSeqVal = T(1);
                T YSeqVal = T(1);
                T RSeqVal = T(1);

                if (XSeq)
                {
                    XSeqVal = (XSeqStart == 0) ? T(1) : Xi;
                }
                if (YSeq)
                {
                    YSeqVal = (YSeqStart == 0) ? T(1) : Yi;
                }
                if (RSeq)
                {
                    RSeqVal = (RSeqStart == 0) ? T(1) : Ri;
                }

                T SumVal = T(0);

                for (mwSize k = 0; k < M; ++k)
                {
                    const T XTerm = evalPowerTerm(
                        Xi, XSeq, XSeqVal, XSmallInt, XPowInt[k], X_Xpower[k]);

                    const T YTerm = evalPowerTerm(
                        Yi, YSeq, YSeqVal, YSmallInt, YPowInt[k], X_Ypower[k]);

                    const T RTerm = evalPowerTerm(
                        Ri, RSeq, RSeqVal, RSmallInt, RPowInt[k], X_Rpower[k]);

                    SumVal += CoefX[k] * XTerm * YTerm * RTerm;

                    if (XSeq) XSeqVal *= Xi;
                    if (YSeq) YSeqVal *= Yi;
                    if (RSeq) RSeqVal *= Ri;
                }

                Xd[i] = SumVal;
            }
        }

        mxFree(XPowInt);
        mxFree(YPowInt);
        mxFree(RPowInt);
        return;
    }

    // ------------------------------------------------------------------------
    // Branch 2: all powers are small integers in [0..5]
    // ------------------------------------------------------------------------
    if (XSmallInt && YSmallInt && RSmallInt)
    {
        if (RScalarIsOne)
        {
            #ifdef _OPENMP
            #pragma omp parallel for
            #endif
            for (mwSignedIndex i = 0; i < static_cast<mwSignedIndex>(N); ++i)
            {
                const T Xi = X[i];
                const T Yi = Y[i];
                T SumVal = T(0);

                for (mwSize k = 0; k < M; ++k)
                {
                    SumVal +=
                        CoefX[k] *
                        powInt0to5(Xi, XPowInt[k]) *
                        powInt0to5(Yi, YPowInt[k]);
                }

                Xd[i] = SumVal;
            }
        }
        else if (RIsScalar)
        {
            T* CoefEff = static_cast<T*>(mxMalloc(M * sizeof(T)));

            for (mwSize k = 0; k < M; ++k)
            {
                CoefEff[k] = CoefX[k] * powInt0to5(RScalar, RPowInt[k]);
            }

            #ifdef _OPENMP
            #pragma omp parallel for
            #endif
            for (mwSignedIndex i = 0; i < static_cast<mwSignedIndex>(N); ++i)
            {
                const T Xi = X[i];
                const T Yi = Y[i];
                T SumVal = T(0);

                for (mwSize k = 0; k < M; ++k)
                {
                    SumVal +=
                        CoefEff[k] *
                        powInt0to5(Xi, XPowInt[k]) *
                        powInt0to5(Yi, YPowInt[k]);
                }

                Xd[i] = SumVal;
            }

            mxFree(CoefEff);
        }
        else
        {
            #ifdef _OPENMP
            #pragma omp parallel for
            #endif
            for (mwSignedIndex i = 0; i < static_cast<mwSignedIndex>(N); ++i)
            {
                const T Xi = X[i];
                const T Yi = Y[i];
                const T Ri = R[i];
                T SumVal = T(0);

                for (mwSize k = 0; k < M; ++k)
                {
                    SumVal +=
                        CoefX[k] *
                        powInt0to5(Xi, XPowInt[k]) *
                        powInt0to5(Yi, YPowInt[k]) *
                        powInt0to5(Ri, RPowInt[k]);
                }

                Xd[i] = SumVal;
            }
        }

        mxFree(XPowInt);
        mxFree(YPowInt);
        mxFree(RPowInt);
        return;
    }

    // ------------------------------------------------------------------------
    // Branch 3: generic
    // ------------------------------------------------------------------------
    if (RScalarIsOne)
    {
        #ifdef _OPENMP
        #pragma omp parallel for
        #endif
        for (mwSignedIndex i = 0; i < static_cast<mwSignedIndex>(N); ++i)
        {
            const T Xi = X[i];
            const T Yi = Y[i];
            T SumVal = T(0);

            for (mwSize k = 0; k < M; ++k)
            {
                SumVal +=
                    CoefX[k] *
                    fastPow(Xi, X_Xpower[k]) *
                    fastPow(Yi, X_Ypower[k]);
            }

            Xd[i] = SumVal;
        }
    }
    else if (RIsScalar)
    {
        T* CoefEff = static_cast<T*>(mxMalloc(M * sizeof(T)));

        for (mwSize k = 0; k < M; ++k)
        {
            CoefEff[k] = CoefX[k] * fastPow(RScalar, X_Rpower[k]);
        }

        #ifdef _OPENMP
        #pragma omp parallel for
        #endif
        for (mwSignedIndex i = 0; i < static_cast<mwSignedIndex>(N); ++i)
        {
            const T Xi = X[i];
            const T Yi = Y[i];
            T SumVal = T(0);

            for (mwSize k = 0; k < M; ++k)
            {
                SumVal +=
                    CoefEff[k] *
                    fastPow(Xi, X_Xpower[k]) *
                    fastPow(Yi, X_Ypower[k]);
            }

            Xd[i] = SumVal;
        }

        mxFree(CoefEff);
    }
    else
    {
        #ifdef _OPENMP
        #pragma omp parallel for
        #endif
        for (mwSignedIndex i = 0; i < static_cast<mwSignedIndex>(N); ++i)
        {
            const T Xi = X[i];
            const T Yi = Y[i];
            const T Ri = R[i];
            T SumVal = T(0);

            for (mwSize k = 0; k < M; ++k)
            {
                SumVal +=
                    CoefX[k] *
                    fastPow(Xi, X_Xpower[k]) *
                    fastPow(Yi, X_Ypower[k]) *
                    fastPow(Ri, X_Rpower[k]);
            }

            Xd[i] = SumVal;
        }
    }

    mxFree(XPowInt);
    mxFree(YPowInt);
    mxFree(RPowInt);
}

// ============================================================================
// Validation helpers
// ============================================================================

static void checkRealFloating(const mxArray* A, const char* Name)
{
    if (!mxIsSingle(A) && !mxIsDouble(A))
    {
        mexErrMsgIdAndTxt("polyRadialDistortion1:Type",
                          "%s must be single or double.", Name);
    }

    if (mxIsComplex(A))
    {
        mexErrMsgIdAndTxt("polyRadialDistortion1:Complex",
                          "%s must be real.", Name);
    }
}

static void checkSameClass(const mxArray* A, const mxArray* B,
                           const char* NameA, const char* NameB)
{
    if (mxGetClassID(A) != mxGetClassID(B))
    {
        mexErrMsgIdAndTxt("polyRadialDistortion1:ClassMismatch",
                          "%s and %s must have the same class.", NameA, NameB);
    }
}

static void checkVectorLength(const mxArray* A, const mwSize ExpectedLen, const char* Name)
{
    if (mxGetNumberOfElements(A) != ExpectedLen)
    {
        mexErrMsgIdAndTxt("polyRadialDistortion1:SizeMismatch",
                          "%s must contain exactly %llu elements.",
                          Name,
                          static_cast<unsigned long long>(ExpectedLen));
    }
}

// ============================================================================
// MEX entry point
// ============================================================================

void mexFunction(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[])
{
    if (nrhs != 7)
    {
        mexErrMsgIdAndTxt("polyRadialDistortion1:NumInputs",
                          "Expected 7 input arguments: X, Y, R, CoefX, X_Xpower, X_Ypower, X_Rpower.");
    }

    if (nlhs > 1)
    {
        mexErrMsgIdAndTxt("polyRadialDistortion1:NumOutputs",
                          "One output expected.");
    }

    const mxArray* X        = prhs[0];
    const mxArray* Y        = prhs[1];
    const mxArray* R        = prhs[2];
    const mxArray* CoefX    = prhs[3];
    const mxArray* X_Xpower = prhs[4];
    const mxArray* X_Ypower = prhs[5];
    const mxArray* X_Rpower = prhs[6];

    checkRealFloating(X,        "X");
    checkRealFloating(Y,        "Y");
    checkRealFloating(R,        "R");
    checkRealFloating(CoefX,    "CoefX");
    checkRealFloating(X_Xpower, "X_Xpower");
    checkRealFloating(X_Ypower, "X_Ypower");
    checkRealFloating(X_Rpower, "X_Rpower");

    checkSameClass(X, Y,        "X", "Y");
    checkSameClass(X, R,        "X", "R");
    checkSameClass(X, CoefX,    "X", "CoefX");
    checkSameClass(X, X_Xpower, "X", "X_Xpower");
    checkSameClass(X, X_Ypower, "X", "X_Ypower");
    checkSameClass(X, X_Rpower, "X", "X_Rpower");

    const mwSize N  = mxGetNumberOfElements(X);
    const mwSize NY = mxGetNumberOfElements(Y);
    const mwSize NR = mxGetNumberOfElements(R);

    if (NY != N)
    {
        mexErrMsgIdAndTxt("polyRadialDistortion1:SizeMismatch",
                          "X and Y must have the same number of elements.");
    }

    const bool RIsScalar = (NR == 1);
    if (!RIsScalar && NR != N)
    {
        mexErrMsgIdAndTxt("polyRadialDistortion1:SizeMismatch",
                          "R must be scalar or have the same number of elements as X.");
    }

    const mwSize M = mxGetNumberOfElements(CoefX);
    checkVectorLength(X_Xpower, M, "X_Xpower");
    checkVectorLength(X_Ypower, M, "X_Ypower");
    checkVectorLength(X_Rpower, M, "X_Rpower");

    plhs[0] = mxCreateNumericMatrix(1, N, mxGetClassID(X), mxREAL);

    if (mxIsDouble(X))
    {
        const double* Xp  = static_cast<const double*>(mxGetData(X));
        const double* Yp  = static_cast<const double*>(mxGetData(Y));
        const double* Rp  = static_cast<const double*>(mxGetData(R));
        const double* Cp  = static_cast<const double*>(mxGetData(CoefX));
        const double* PXp = static_cast<const double*>(mxGetData(X_Xpower));
        const double* PYp = static_cast<const double*>(mxGetData(X_Ypower));
        const double* PRp = static_cast<const double*>(mxGetData(X_Rpower));
        double* Xdp       = static_cast<double*>(mxGetData(plhs[0]));

        computePolyRadialDistortion<double>(
            Xp, Yp, Rp, RIsScalar, Cp, PXp, PYp, PRp, N, M, Xdp);
    }
    else
    {
        const float* Xp  = static_cast<const float*>(mxGetData(X));
        const float* Yp  = static_cast<const float*>(mxGetData(Y));
        const float* Rp  = static_cast<const float*>(mxGetData(R));
        const float* Cp  = static_cast<const float*>(mxGetData(CoefX));
        const float* PXp = static_cast<const float*>(mxGetData(X_Xpower));
        const float* PYp = static_cast<const float*>(mxGetData(X_Ypower));
        const float* PRp = static_cast<const float*>(mxGetData(X_Rpower));
        float* Xdp       = static_cast<float*>(mxGetData(plhs[0]));

        computePolyRadialDistortion<float>(
            Xp, Yp, Rp, RIsScalar, Cp, PXp, PYp, PRp, N, M, Xdp);
    }
}
