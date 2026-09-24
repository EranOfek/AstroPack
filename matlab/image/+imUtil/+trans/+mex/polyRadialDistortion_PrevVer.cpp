#include "mex.h"
#include <cmath>
#include <cstdint>
#include <type_traits>

#ifdef _OPENMP
#include <omp.h>
#endif

#if defined(__AVX2__)
#include <immintrin.h>
#endif

// ============================================================================
// Scalar helpers
// ============================================================================

template <typename T>
inline T fastPowScalar(const T Base, const T Exp)
{
    if (Exp == T(0)) return T(1);
    if (Exp == T(1)) return Base;
    if (Exp == T(2)) return Base * Base;
    if (Exp == T(3)) return Base * Base * Base;
    return std::pow(Base, Exp);
}

template <typename T>
inline T powInt0to5Scalar(const T Base, const int P)
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
        default: return std::pow(Base, static_cast<T>(P));
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

    if (Pwr[0] == T(0))
    {
        StartVal = 0;
    }
    else if (Pwr[0] == T(1))
    {
        StartVal = 1;
    }
    else
    {
        return false;
    }

    for (mwSize k = 0; k < M; ++k)
    {
        if (Pwr[k] != static_cast<T>(StartVal + static_cast<int>(k)))
        {
            return false;
        }
    }

    return true;
}

// ============================================================================
// AVX2 traits
// ============================================================================

#if defined(__AVX2__)

template <typename T>
struct VecTraits;

template <>
struct VecTraits<float>
{
    using Vec = __m256;
    static constexpr int Width = 8;

    static inline Vec loadu(const float* p) { return _mm256_loadu_ps(p); }
    static inline void storeu(float* p, Vec v) { _mm256_storeu_ps(p, v); }
    static inline Vec set1(float x) { return _mm256_set1_ps(x); }
    static inline Vec add(Vec a, Vec b) { return _mm256_add_ps(a, b); }
    static inline Vec mul(Vec a, Vec b) { return _mm256_mul_ps(a, b); }
};

template <>
struct VecTraits<double>
{
    using Vec = __m256d;
    static constexpr int Width = 4;

    static inline Vec loadu(const double* p) { return _mm256_loadu_pd(p); }
    static inline void storeu(double* p, Vec v) { _mm256_storeu_pd(p, v); }
    static inline Vec set1(double x) { return _mm256_set1_pd(x); }
    static inline Vec add(Vec a, Vec b) { return _mm256_add_pd(a, b); }
    static inline Vec mul(Vec a, Vec b) { return _mm256_mul_pd(a, b); }
};

template <typename T>
inline typename VecTraits<T>::Vec powInt0to5Vec(typename VecTraits<T>::Vec Base, const int P)
{
    using VT = VecTraits<T>;
    using V  = typename VT::Vec;

    switch (P)
    {
        case 0: return VT::set1(T(1));
        case 1: return Base;
        case 2: return VT::mul(Base, Base);
        case 3: return VT::mul(VT::mul(Base, Base), Base);
        case 4:
        {
            V B2 = VT::mul(Base, Base);
            return VT::mul(B2, B2);
        }
        case 5:
        {
            V B2 = VT::mul(Base, Base);
            return VT::mul(VT::mul(B2, B2), Base);
        }
        default: return VT::set1(T(0));
    }
}

#endif

// ============================================================================
// Scalar kernels
// ============================================================================

template <typename T>
void scalarKernelSeqR1(
    const T* X, const T* Y,
    const T* CoefX,
    const T* X_Xpower, const T* X_Ypower,
    const int* XPowInt, const int* YPowInt,
    bool XSeq, int XSeqStart,
    bool YSeq, int YSeqStart,
    bool XSmallInt, bool YSmallInt,
    mwSize I0, mwSize I1, mwSize M,
    T* Xd)
{
    for (mwSize i = I0; i < I1; ++i)
    {
        const T Xi = X[i];
        const T Yi = Y[i];

        T XSeqVal = T(1);
        T YSeqVal = T(1);

        if (XSeq) XSeqVal = (XSeqStart == 0) ? T(1) : Xi;
        if (YSeq) YSeqVal = (YSeqStart == 0) ? T(1) : Yi;

        T SumVal = T(0);

        for (mwSize k = 0; k < M; ++k)
        {
            const T XTerm = XSeq ? XSeqVal :
                            (XSmallInt ? powInt0to5Scalar(Xi, XPowInt[k]) : fastPowScalar(Xi, X_Xpower[k]));

            const T YTerm = YSeq ? YSeqVal :
                            (YSmallInt ? powInt0to5Scalar(Yi, YPowInt[k]) : fastPowScalar(Yi, X_Ypower[k]));

            SumVal += CoefX[k] * XTerm * YTerm;

            if (XSeq) XSeqVal *= Xi;
            if (YSeq) YSeqVal *= Yi;
        }

        Xd[i] = SumVal;
    }
}

template <typename T>
void scalarKernelSeqScalarR(
    const T* X, const T* Y,
    const T* CoefEff,
    const T* X_Xpower, const T* X_Ypower,
    const int* XPowInt, const int* YPowInt,
    bool XSeq, int XSeqStart,
    bool YSeq, int YSeqStart,
    bool XSmallInt, bool YSmallInt,
    mwSize I0, mwSize I1, mwSize M,
    T* Xd)
{
    for (mwSize i = I0; i < I1; ++i)
    {
        const T Xi = X[i];
        const T Yi = Y[i];

        T XSeqVal = T(1);
        T YSeqVal = T(1);

        if (XSeq) XSeqVal = (XSeqStart == 0) ? T(1) : Xi;
        if (YSeq) YSeqVal = (YSeqStart == 0) ? T(1) : Yi;

        T SumVal = T(0);

        for (mwSize k = 0; k < M; ++k)
        {
            const T XTerm = XSeq ? XSeqVal :
                            (XSmallInt ? powInt0to5Scalar(Xi, XPowInt[k]) : fastPowScalar(Xi, X_Xpower[k]));

            const T YTerm = YSeq ? YSeqVal :
                            (YSmallInt ? powInt0to5Scalar(Yi, YPowInt[k]) : fastPowScalar(Yi, X_Ypower[k]));

            SumVal += CoefEff[k] * XTerm * YTerm;

            if (XSeq) XSeqVal *= Xi;
            if (YSeq) YSeqVal *= Yi;
        }

        Xd[i] = SumVal;
    }
}

template <typename T>
void scalarKernelSeqRArray(
    const T* X, const T* Y, const T* R,
    const T* CoefX,
    const T* X_Xpower, const T* X_Ypower, const T* X_Rpower,
    const int* XPowInt, const int* YPowInt, const int* RPowInt,
    bool XSeq, int XSeqStart,
    bool YSeq, int YSeqStart,
    bool RSeq, int RSeqStart,
    bool XSmallInt, bool YSmallInt, bool RSmallInt,
    mwSize I0, mwSize I1, mwSize M,
    T* Xd)
{
    for (mwSize i = I0; i < I1; ++i)
    {
        const T Xi = X[i];
        const T Yi = Y[i];
        const T Ri = R[i];

        T XSeqVal = T(1);
        T YSeqVal = T(1);
        T RSeqVal = T(1);

        if (XSeq) XSeqVal = (XSeqStart == 0) ? T(1) : Xi;
        if (YSeq) YSeqVal = (YSeqStart == 0) ? T(1) : Yi;
        if (RSeq) RSeqVal = (RSeqStart == 0) ? T(1) : Ri;

        T SumVal = T(0);

        for (mwSize k = 0; k < M; ++k)
        {
            const T XTerm = XSeq ? XSeqVal :
                            (XSmallInt ? powInt0to5Scalar(Xi, XPowInt[k]) : fastPowScalar(Xi, X_Xpower[k]));

            const T YTerm = YSeq ? YSeqVal :
                            (YSmallInt ? powInt0to5Scalar(Yi, YPowInt[k]) : fastPowScalar(Yi, X_Ypower[k]));

            const T RTerm = RSeq ? RSeqVal :
                            (RSmallInt ? powInt0to5Scalar(Ri, RPowInt[k]) : fastPowScalar(Ri, X_Rpower[k]));

            SumVal += CoefX[k] * XTerm * YTerm * RTerm;

            if (XSeq) XSeqVal *= Xi;
            if (YSeq) YSeqVal *= Yi;
            if (RSeq) RSeqVal *= Ri;
        }

        Xd[i] = SumVal;
    }
}

template <typename T>
void scalarKernelSmallR1(
    const T* X, const T* Y,
    const T* CoefX,
    const int* XPowInt, const int* YPowInt,
    mwSize I0, mwSize I1, mwSize M,
    T* Xd)
{
    for (mwSize i = I0; i < I1; ++i)
    {
        const T Xi = X[i];
        const T Yi = Y[i];
        T SumVal = T(0);

        for (mwSize k = 0; k < M; ++k)
        {
            SumVal += CoefX[k] *
                      powInt0to5Scalar(Xi, XPowInt[k]) *
                      powInt0to5Scalar(Yi, YPowInt[k]);
        }

        Xd[i] = SumVal;
    }
}

template <typename T>
void scalarKernelSmallScalarR(
    const T* X, const T* Y,
    const T* CoefEff,
    const int* XPowInt, const int* YPowInt,
    mwSize I0, mwSize I1, mwSize M,
    T* Xd)
{
    for (mwSize i = I0; i < I1; ++i)
    {
        const T Xi = X[i];
        const T Yi = Y[i];
        T SumVal = T(0);

        for (mwSize k = 0; k < M; ++k)
        {
            SumVal += CoefEff[k] *
                      powInt0to5Scalar(Xi, XPowInt[k]) *
                      powInt0to5Scalar(Yi, YPowInt[k]);
        }

        Xd[i] = SumVal;
    }
}

template <typename T>
void scalarKernelSmallRArray(
    const T* X, const T* Y, const T* R,
    const T* CoefX,
    const int* XPowInt, const int* YPowInt, const int* RPowInt,
    mwSize I0, mwSize I1, mwSize M,
    T* Xd)
{
    for (mwSize i = I0; i < I1; ++i)
    {
        const T Xi = X[i];
        const T Yi = Y[i];
        const T Ri = R[i];
        T SumVal = T(0);

        for (mwSize k = 0; k < M; ++k)
        {
            SumVal += CoefX[k] *
                      powInt0to5Scalar(Xi, XPowInt[k]) *
                      powInt0to5Scalar(Yi, YPowInt[k]) *
                      powInt0to5Scalar(Ri, RPowInt[k]);
        }

        Xd[i] = SumVal;
    }
}

template <typename T>
void scalarKernelGenericR1(
    const T* X, const T* Y,
    const T* CoefX,
    const T* X_Xpower, const T* X_Ypower,
    mwSize I0, mwSize I1, mwSize M,
    T* Xd)
{
    for (mwSize i = I0; i < I1; ++i)
    {
        const T Xi = X[i];
        const T Yi = Y[i];
        T SumVal = T(0);

        for (mwSize k = 0; k < M; ++k)
        {
            SumVal += CoefX[k] *
                      fastPowScalar(Xi, X_Xpower[k]) *
                      fastPowScalar(Yi, X_Ypower[k]);
        }

        Xd[i] = SumVal;
    }
}

template <typename T>
void scalarKernelGenericScalarR(
    const T* X, const T* Y,
    const T* CoefEff,
    const T* X_Xpower, const T* X_Ypower,
    mwSize I0, mwSize I1, mwSize M,
    T* Xd)
{
    for (mwSize i = I0; i < I1; ++i)
    {
        const T Xi = X[i];
        const T Yi = Y[i];
        T SumVal = T(0);

        for (mwSize k = 0; k < M; ++k)
        {
            SumVal += CoefEff[k] *
                      fastPowScalar(Xi, X_Xpower[k]) *
                      fastPowScalar(Yi, X_Ypower[k]);
        }

        Xd[i] = SumVal;
    }
}

template <typename T>
void scalarKernelGenericRArray(
    const T* X, const T* Y, const T* R,
    const T* CoefX,
    const T* X_Xpower, const T* X_Ypower, const T* X_Rpower,
    mwSize I0, mwSize I1, mwSize M,
    T* Xd)
{
    for (mwSize i = I0; i < I1; ++i)
    {
        const T Xi = X[i];
        const T Yi = Y[i];
        const T Ri = R[i];
        T SumVal = T(0);

        for (mwSize k = 0; k < M; ++k)
        {
            SumVal += CoefX[k] *
                      fastPowScalar(Xi, X_Xpower[k]) *
                      fastPowScalar(Yi, X_Ypower[k]) *
                      fastPowScalar(Ri, X_Rpower[k]);
        }

        Xd[i] = SumVal;
    }
}

// ============================================================================
// AVX2 kernels
// ============================================================================

#if defined(__AVX2__)

template <typename T>
void avxKernelSmallR1(
    const T* X, const T* Y,
    const T* CoefX,
    const int* XPowInt, const int* YPowInt,
    mwSize I0, mwSize I1, mwSize M,
    T* Xd)
{
    using VT = VecTraits<T>;
    using V  = typename VT::Vec;

    const mwSize W = VT::Width;
    mwSize i = I0;

    for (; i + W <= I1; i += W)
    {
        V Xv = VT::loadu(X + i);
        V Yv = VT::loadu(Y + i);
        V Sum = VT::set1(T(0));

        for (mwSize k = 0; k < M; ++k)
        {
            V Term = VT::mul(powInt0to5Vec<T>(Xv, XPowInt[k]),
                             powInt0to5Vec<T>(Yv, YPowInt[k]));
            Term = VT::mul(Term, VT::set1(CoefX[k]));
            Sum  = VT::add(Sum, Term);
        }

        VT::storeu(Xd + i, Sum);
    }

    scalarKernelSmallR1<T>(X, Y, CoefX, XPowInt, YPowInt, i, I1, M, Xd);
}

template <typename T>
void avxKernelSmallScalarR(
    const T* X, const T* Y,
    const T* CoefEff,
    const int* XPowInt, const int* YPowInt,
    mwSize I0, mwSize I1, mwSize M,
    T* Xd)
{
    using VT = VecTraits<T>;
    using V  = typename VT::Vec;

    const mwSize W = VT::Width;
    mwSize i = I0;

    for (; i + W <= I1; i += W)
    {
        V Xv = VT::loadu(X + i);
        V Yv = VT::loadu(Y + i);
        V Sum = VT::set1(T(0));

        for (mwSize k = 0; k < M; ++k)
        {
            V Term = VT::mul(powInt0to5Vec<T>(Xv, XPowInt[k]),
                             powInt0to5Vec<T>(Yv, YPowInt[k]));
            Term = VT::mul(Term, VT::set1(CoefEff[k]));
            Sum  = VT::add(Sum, Term);
        }

        VT::storeu(Xd + i, Sum);
    }

    scalarKernelSmallScalarR<T>(X, Y, CoefEff, XPowInt, YPowInt, i, I1, M, Xd);
}

template <typename T>
void avxKernelSmallRArray(
    const T* X, const T* Y, const T* R,
    const T* CoefX,
    const int* XPowInt, const int* YPowInt, const int* RPowInt,
    mwSize I0, mwSize I1, mwSize M,
    T* Xd)
{
    using VT = VecTraits<T>;
    using V  = typename VT::Vec;

    const mwSize W = VT::Width;
    mwSize i = I0;

    for (; i + W <= I1; i += W)
    {
        V Xv = VT::loadu(X + i);
        V Yv = VT::loadu(Y + i);
        V Rv = VT::loadu(R + i);
        V Sum = VT::set1(T(0));

        for (mwSize k = 0; k < M; ++k)
        {
            V Term = VT::mul(powInt0to5Vec<T>(Xv, XPowInt[k]),
                             powInt0to5Vec<T>(Yv, YPowInt[k]));
            Term = VT::mul(Term, powInt0to5Vec<T>(Rv, RPowInt[k]));
            Term = VT::mul(Term, VT::set1(CoefX[k]));
            Sum  = VT::add(Sum, Term);
        }

        VT::storeu(Xd + i, Sum);
    }

    scalarKernelSmallRArray<T>(X, Y, R, CoefX, XPowInt, YPowInt, RPowInt, i, I1, M, Xd);
}

template <typename T>
void avxKernelSeqR1(
    const T* X, const T* Y,
    const T* CoefX,
    const T* X_Xpower, const T* X_Ypower,
    const int* XPowInt, const int* YPowInt,
    bool XSeq, int XSeqStart,
    bool YSeq, int YSeqStart,
    bool XSmallInt, bool YSmallInt,
    mwSize I0, mwSize I1, mwSize M,
    T* Xd)
{
    using VT = VecTraits<T>;
    using V  = typename VT::Vec;

    const mwSize W = VT::Width;
    mwSize i = I0;

    for (; i + W <= I1; i += W)
    {
        V Xv = VT::loadu(X + i);
        V Yv = VT::loadu(Y + i);
        V Sum = VT::set1(T(0));

        V XSeqVal = VT::set1(T(1));
        V YSeqVal = VT::set1(T(1));

        if (XSeq) XSeqVal = (XSeqStart == 0) ? VT::set1(T(1)) : Xv;
        if (YSeq) YSeqVal = (YSeqStart == 0) ? VT::set1(T(1)) : Yv;

        for (mwSize k = 0; k < M; ++k)
        {
            V XTerm = XSeq ? XSeqVal :
                      (XSmallInt ? powInt0to5Vec<T>(Xv, XPowInt[k]) : VT::set1(T(0)));

            V YTerm = YSeq ? YSeqVal :
                      (YSmallInt ? powInt0to5Vec<T>(Yv, YPowInt[k]) : VT::set1(T(0)));

            V Term = VT::mul(VT::mul(XTerm, YTerm), VT::set1(CoefX[k]));
            Sum = VT::add(Sum, Term);

            if (XSeq) XSeqVal = VT::mul(XSeqVal, Xv);
            if (YSeq) YSeqVal = VT::mul(YSeqVal, Yv);
        }

        VT::storeu(Xd + i, Sum);
    }

    scalarKernelSeqR1<T>(X, Y, CoefX, X_Xpower, X_Ypower, XPowInt, YPowInt,
                         XSeq, XSeqStart, YSeq, YSeqStart, XSmallInt, YSmallInt,
                         i, I1, M, Xd);
}

template <typename T>
void avxKernelSeqScalarR(
    const T* X, const T* Y,
    const T* CoefEff,
    const T* X_Xpower, const T* X_Ypower,
    const int* XPowInt, const int* YPowInt,
    bool XSeq, int XSeqStart,
    bool YSeq, int YSeqStart,
    bool XSmallInt, bool YSmallInt,
    mwSize I0, mwSize I1, mwSize M,
    T* Xd)
{
    using VT = VecTraits<T>;
    using V  = typename VT::Vec;

    const mwSize W = VT::Width;
    mwSize i = I0;

    for (; i + W <= I1; i += W)
    {
        V Xv = VT::loadu(X + i);
        V Yv = VT::loadu(Y + i);
        V Sum = VT::set1(T(0));

        V XSeqVal = VT::set1(T(1));
        V YSeqVal = VT::set1(T(1));

        if (XSeq) XSeqVal = (XSeqStart == 0) ? VT::set1(T(1)) : Xv;
        if (YSeq) YSeqVal = (YSeqStart == 0) ? VT::set1(T(1)) : Yv;

        for (mwSize k = 0; k < M; ++k)
        {
            V XTerm = XSeq ? XSeqVal :
                      (XSmallInt ? powInt0to5Vec<T>(Xv, XPowInt[k]) : VT::set1(T(0)));

            V YTerm = YSeq ? YSeqVal :
                      (YSmallInt ? powInt0to5Vec<T>(Yv, YPowInt[k]) : VT::set1(T(0)));

            V Term = VT::mul(VT::mul(XTerm, YTerm), VT::set1(CoefEff[k]));
            Sum = VT::add(Sum, Term);

            if (XSeq) XSeqVal = VT::mul(XSeqVal, Xv);
            if (YSeq) YSeqVal = VT::mul(YSeqVal, Yv);
        }

        VT::storeu(Xd + i, Sum);
    }

    scalarKernelSeqScalarR<T>(X, Y, CoefEff, X_Xpower, X_Ypower, XPowInt, YPowInt,
                              XSeq, XSeqStart, YSeq, YSeqStart, XSmallInt, YSmallInt,
                              i, I1, M, Xd);
}

template <typename T>
void avxKernelSeqRArray(
    const T* X, const T* Y, const T* R,
    const T* CoefX,
    const T* X_Xpower, const T* X_Ypower, const T* X_Rpower,
    const int* XPowInt, const int* YPowInt, const int* RPowInt,
    bool XSeq, int XSeqStart,
    bool YSeq, int YSeqStart,
    bool RSeq, int RSeqStart,
    bool XSmallInt, bool YSmallInt, bool RSmallInt,
    mwSize I0, mwSize I1, mwSize M,
    T* Xd)
{
    using VT = VecTraits<T>;
    using V  = typename VT::Vec;

    const mwSize W = VT::Width;
    mwSize i = I0;

    for (; i + W <= I1; i += W)
    {
        V Xv = VT::loadu(X + i);
        V Yv = VT::loadu(Y + i);
        V Rv = VT::loadu(R + i);
        V Sum = VT::set1(T(0));

        V XSeqVal = VT::set1(T(1));
        V YSeqVal = VT::set1(T(1));
        V RSeqVal = VT::set1(T(1));

        if (XSeq) XSeqVal = (XSeqStart == 0) ? VT::set1(T(1)) : Xv;
        if (YSeq) YSeqVal = (YSeqStart == 0) ? VT::set1(T(1)) : Yv;
        if (RSeq) RSeqVal = (RSeqStart == 0) ? VT::set1(T(1)) : Rv;

        for (mwSize k = 0; k < M; ++k)
        {
            V XTerm = XSeq ? XSeqVal :
                      (XSmallInt ? powInt0to5Vec<T>(Xv, XPowInt[k]) : VT::set1(T(0)));

            V YTerm = YSeq ? YSeqVal :
                      (YSmallInt ? powInt0to5Vec<T>(Yv, YPowInt[k]) : VT::set1(T(0)));

            V RTerm = RSeq ? RSeqVal :
                      (RSmallInt ? powInt0to5Vec<T>(Rv, RPowInt[k]) : VT::set1(T(0)));

            V Term = VT::mul(VT::mul(XTerm, YTerm), RTerm);
            Term = VT::mul(Term, VT::set1(CoefX[k]));
            Sum = VT::add(Sum, Term);

            if (XSeq) XSeqVal = VT::mul(XSeqVal, Xv);
            if (YSeq) YSeqVal = VT::mul(YSeqVal, Yv);
            if (RSeq) RSeqVal = VT::mul(RSeqVal, Rv);
        }

        VT::storeu(Xd + i, Sum);
    }

    scalarKernelSeqRArray<T>(X, Y, R, CoefX, X_Xpower, X_Ypower, X_Rpower,
                             XPowInt, YPowInt, RPowInt,
                             XSeq, XSeqStart, YSeq, YSeqStart, RSeq, RSeqStart,
                             XSmallInt, YSmallInt, RSmallInt,
                             i, I1, M, Xd);
}

#endif

// ============================================================================
// Main computation
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

    if (UseSeqBranch)
    {
        if (RScalarIsOne)
        {
#if defined(__AVX2__)
            if ((XSmallInt || XSeq) && (YSmallInt || YSeq))
            {
                avxKernelSeqR1<T>(X, Y, CoefX, X_Xpower, X_Ypower, XPowInt, YPowInt,
                                  XSeq, XSeqStart, YSeq, YSeqStart, XSmallInt, YSmallInt,
                                  0, N, M, Xd);
            }
            else
#endif
            {
                scalarKernelSeqR1<T>(X, Y, CoefX, X_Xpower, X_Ypower, XPowInt, YPowInt,
                                     XSeq, XSeqStart, YSeq, YSeqStart, XSmallInt, YSmallInt,
                                     0, N, M, Xd);
            }

            mxFree(XPowInt); mxFree(YPowInt); mxFree(RPowInt);
            return;
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
                    CoefEff[k] = CoefX[k] * powInt0to5Scalar(RScalar, RPowInt[k]);
                }
            }
            else
            {
                for (mwSize k = 0; k < M; ++k)
                {
                    CoefEff[k] = CoefX[k] * fastPowScalar(RScalar, X_Rpower[k]);
                }
            }

#if defined(__AVX2__)
            if ((XSmallInt || XSeq) && (YSmallInt || YSeq))
            {
                avxKernelSeqScalarR<T>(X, Y, CoefEff, X_Xpower, X_Ypower, XPowInt, YPowInt,
                                       XSeq, XSeqStart, YSeq, YSeqStart, XSmallInt, YSmallInt,
                                       0, N, M, Xd);
            }
            else
#endif
            {
                scalarKernelSeqScalarR<T>(X, Y, CoefEff, X_Xpower, X_Ypower, XPowInt, YPowInt,
                                          XSeq, XSeqStart, YSeq, YSeqStart, XSmallInt, YSmallInt,
                                          0, N, M, Xd);
            }

            mxFree(CoefEff);
            mxFree(XPowInt); mxFree(YPowInt); mxFree(RPowInt);
            return;
        }
        else
        {
#if defined(__AVX2__)
            if ((XSmallInt || XSeq) && (YSmallInt || YSeq) && (RSmallInt || RSeq))
            {
                avxKernelSeqRArray<T>(X, Y, R, CoefX, X_Xpower, X_Ypower, X_Rpower,
                                      XPowInt, YPowInt, RPowInt,
                                      XSeq, XSeqStart, YSeq, YSeqStart, RSeq, RSeqStart,
                                      XSmallInt, YSmallInt, RSmallInt,
                                      0, N, M, Xd);
            }
            else
#endif
            {
                scalarKernelSeqRArray<T>(X, Y, R, CoefX, X_Xpower, X_Ypower, X_Rpower,
                                         XPowInt, YPowInt, RPowInt,
                                         XSeq, XSeqStart, YSeq, YSeqStart, RSeq, RSeqStart,
                                         XSmallInt, YSmallInt, RSmallInt,
                                         0, N, M, Xd);
            }

            mxFree(XPowInt); mxFree(YPowInt); mxFree(RPowInt);
            return;
        }
    }

    if (XSmallInt && YSmallInt && RSmallInt)
    {
        if (RScalarIsOne)
        {
#if defined(__AVX2__)
            avxKernelSmallR1<T>(X, Y, CoefX, XPowInt, YPowInt, 0, N, M, Xd);
#else
            scalarKernelSmallR1<T>(X, Y, CoefX, XPowInt, YPowInt, 0, N, M, Xd);
#endif
        }
        else if (RIsScalar)
        {
            T* CoefEff = static_cast<T*>(mxMalloc(M * sizeof(T)));
            for (mwSize k = 0; k < M; ++k)
            {
                CoefEff[k] = CoefX[k] * powInt0to5Scalar(RScalar, RPowInt[k]);
            }

#if defined(__AVX2__)
            avxKernelSmallScalarR<T>(X, Y, CoefEff, XPowInt, YPowInt, 0, N, M, Xd);
#else
            scalarKernelSmallScalarR<T>(X, Y, CoefEff, XPowInt, YPowInt, 0, N, M, Xd);
#endif

            mxFree(CoefEff);
        }
        else
        {
#if defined(__AVX2__)
            avxKernelSmallRArray<T>(X, Y, R, CoefX, XPowInt, YPowInt, RPowInt, 0, N, M, Xd);
#else
            scalarKernelSmallRArray<T>(X, Y, R, CoefX, XPowInt, YPowInt, RPowInt, 0, N, M, Xd);
#endif
        }

        mxFree(XPowInt); mxFree(YPowInt); mxFree(RPowInt);
        return;
    }

    if (RScalarIsOne)
    {
        scalarKernelGenericR1<T>(X, Y, CoefX, X_Xpower, X_Ypower, 0, N, M, Xd);
    }
    else if (RIsScalar)
    {
        T* CoefEff = static_cast<T*>(mxMalloc(M * sizeof(T)));
        for (mwSize k = 0; k < M; ++k)
        {
            CoefEff[k] = CoefX[k] * fastPowScalar(RScalar, X_Rpower[k]);
        }

        scalarKernelGenericScalarR<T>(X, Y, CoefEff, X_Xpower, X_Ypower, 0, N, M, Xd);
        mxFree(CoefEff);
    }
    else
    {
        scalarKernelGenericRArray<T>(X, Y, R, CoefX, X_Xpower, X_Ypower, X_Rpower, 0, N, M, Xd);
    }

    mxFree(XPowInt);
    mxFree(YPowInt);
    mxFree(RPowInt);
}

// ============================================================================
// Validation
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

    const mwSize* DimsX = mxGetDimensions(X);
    const mwSize NdimsX = mxGetNumberOfDimensions(X);
    plhs[0] = mxCreateNumericArray(NdimsX, DimsX, mxGetClassID(X), mxREAL);

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

        computePolyRadialDistortion<double>(Xp, Yp, Rp, RIsScalar, Cp, PXp, PYp, PRp, N, M, Xdp);
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

        computePolyRadialDistortion<float>(Xp, Yp, Rp, RIsScalar, Cp, PXp, PYp, PRp, N, M, Xdp);
    }
}
