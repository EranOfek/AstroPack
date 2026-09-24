#include "mex.h"
#include <cmath>
#include <algorithm>
#include <cstring>

template <typename T>
inline T ScaleVal(const T& x, double w)
{
    return static_cast<T>(x * static_cast<double>(w));
}

template <>
inline mxComplexDouble ScaleVal(const mxComplexDouble& x, double w)
{
    mxComplexDouble y;
    y.real = x.real * w;
    y.imag = x.imag * w;
    return y;
}

template <>
inline mxComplexSingle ScaleVal(const mxComplexSingle& x, double w)
{
    mxComplexSingle y;
    y.real = static_cast<float>(x.real * w);
    y.imag = static_cast<float>(x.imag * w);
    return y;
}

inline double CornerWeightFromR2(double R2, double Rin2, double Rout2, double Rin, double Rout, double InvDelta)
{
    if (R2 <= Rin2) {
        return 1.0;
    }
    if (R2 >= Rout2) {
        return 0.0;
    }

    const double R = std::sqrt(R2);
    const double T = (R - Rin) * InvDelta;   // 0..1
    return std::cos(0.5 * M_PI * T);
}

inline double PixelWeight(mwSize I, mwSize J, mwSize NI, mwSize NJ,
                          double Rin2, double Rout2, double Rin, double Rout, double InvDelta)
{
    const double I0 = static_cast<double>(I);
    const double J0 = static_cast<double>(J);
    const double I1 = static_cast<double>(NI - 1 - I);
    const double J1 = static_cast<double>(NJ - 1 - J);

    const double R2_UL = I0*I0 + J0*J0;
    const double R2_UR = I0*I0 + J1*J1;
    const double R2_LL = I1*I1 + J0*J0;
    const double R2_LR = I1*I1 + J1*J1;

    if (R2_UL <= Rin2 || R2_UR <= Rin2 || R2_LL <= Rin2 || R2_LR <= Rin2) {
        return 1.0;
    }

    if (R2_UL >= Rout2 && R2_UR >= Rout2 && R2_LL >= Rout2 && R2_LR >= Rout2) {
        return 0.0;
    }

    double W = 0.0;

    if (R2_UL < Rout2) {
        W = std::max(W, CornerWeightFromR2(R2_UL, Rin2, Rout2, Rin, Rout, InvDelta));
    }
    if (R2_UR < Rout2) {
        W = std::max(W, CornerWeightFromR2(R2_UR, Rin2, Rout2, Rin, Rout, InvDelta));
    }
    if (R2_LL < Rout2) {
        W = std::max(W, CornerWeightFromR2(R2_LL, Rin2, Rout2, Rin, Rout, InvDelta));
    }
    if (R2_LR < Rout2) {
        W = std::max(W, CornerWeightFromR2(R2_LR, Rin2, Rout2, Rin, Rout, InvDelta));
    }

    return W;
}

template <typename T>
void RunTyped(const mxArray* In, const double Rin, const double Rout, mxArray*& Out)
{
    const mwSize Nd = mxGetNumberOfDimensions(In);
    const mwSize* Sz = mxGetDimensions(In);

    const mwSize NI = Sz[0];
    const mwSize NJ = Sz[1];
    const mwSize NK = (Nd >= 3) ? Sz[2] : 1;
    const mwSize PageSize = NI * NJ;

    Out = mxCreateNumericArray(Nd, Sz, mxGetClassID(In), mxIsComplex(In) ? mxCOMPLEX : mxREAL);

    const T* Src = reinterpret_cast<const T*>(mxGetData(In));
    T* Dst       = reinterpret_cast<T*>(mxGetData(Out));

    const double Rin2 = Rin * Rin;
    const double Rout2 = Rout * Rout;

    if (Rout < Rin) {
        mexErrMsgIdAndTxt("cosbellCorners:InvalidRadii", "AnnulusRadii(2) must be >= AnnulusRadii(1).");
    }

    if (Rout == Rin) {
        for (mwSize J = 0; J < NJ; ++J) {
            for (mwSize I = 0; I < NI; ++I) {
                const double I0 = static_cast<double>(I);
                const double J0 = static_cast<double>(J);
                const double I1 = static_cast<double>(NI - 1 - I);
                const double J1 = static_cast<double>(NJ - 1 - J);

                const double R2_UL = I0*I0 + J0*J0;
                const double R2_UR = I0*I0 + J1*J1;
                const double R2_LL = I1*I1 + J0*J0;
                const double R2_LR = I1*I1 + J1*J1;

                const double W = (R2_UL <= Rin2 || R2_UR <= Rin2 || R2_LL <= Rin2 || R2_LR <= Rin2) ? 1.0 : 0.0;
                const mwSize Idx2 = I + J * NI;

                if (W == 0.0) {
                    for (mwSize K = 0; K < NK; ++K) {
                        Dst[Idx2 + K * PageSize] = ScaleVal(Src[Idx2 + K * PageSize], 0.0);
                    }
                } else {
                    for (mwSize K = 0; K < NK; ++K) {
                        Dst[Idx2 + K * PageSize] = Src[Idx2 + K * PageSize];
                    }
                }
            }
        }
        return;
    }

    const double InvDelta = 1.0 / (Rout - Rin);

    for (mwSize J = 0; J < NJ; ++J) {
        for (mwSize I = 0; I < NI; ++I) {
            const double W = PixelWeight(I, J, NI, NJ, Rin2, Rout2, Rin, Rout, InvDelta);
            const mwSize Idx2 = I + J * NI;

            if (W == 0.0) {
                for (mwSize K = 0; K < NK; ++K) {
                    Dst[Idx2 + K * PageSize] = ScaleVal(Src[Idx2 + K * PageSize], 0.0);
                }
            } else if (W == 1.0) {
                for (mwSize K = 0; K < NK; ++K) {
                    Dst[Idx2 + K * PageSize] = Src[Idx2 + K * PageSize];
                }
            } else {
                for (mwSize K = 0; K < NK; ++K) {
                    Dst[Idx2 + K * PageSize] = ScaleVal(Src[Idx2 + K * PageSize], W);
                }
            }
        }
    }
}

void mexFunction(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[])
{
    if (nrhs != 2) {
        mexErrMsgIdAndTxt("cosbellCorners:NumInputs", "Usage: NewFullCube = cosbellCorners(FullCube, [Rin Rout])");
    }
    if (nlhs > 1) {
        mexErrMsgIdAndTxt("cosbellCorners:NumOutputs", "One output only.");
    }

    const mxArray* FullCube = prhs[0];
    const mxArray* AnnulusRadii = prhs[1];

    if (!mxIsSingle(FullCube) && !mxIsDouble(FullCube)) {
        mexErrMsgIdAndTxt("cosbellCorners:TypeError", "FullCube must be single or double.");
    }
    if (mxIsSparse(FullCube)) {
        mexErrMsgIdAndTxt("cosbellCorners:SparseError", "Sparse arrays are not supported.");
    }

    const mwSize Nd = mxGetNumberOfDimensions(FullCube);
    if (Nd != 2 && Nd != 3) {
        mexErrMsgIdAndTxt("cosbellCorners:DimError", "FullCube must be 2-D or 3-D.");
    }

    if (!mxIsDouble(AnnulusRadii) || mxIsComplex(AnnulusRadii) || mxGetNumberOfElements(AnnulusRadii) != 2) {
        mexErrMsgIdAndTxt("cosbellCorners:RadiiError", "AnnulusRadii must be a real double vector [Rin Rout].");
    }

    const double* R = mxGetPr(AnnulusRadii);
    const double Rin = R[0];
    const double Rout = R[1];

    if (Rin < 0.0 || Rout < 0.0) {
        mexErrMsgIdAndTxt("cosbellCorners:RadiiError", "Radii must be non-negative.");
    }

    if (mxIsComplex(FullCube)) {
        if (mxGetClassID(FullCube) == mxDOUBLE_CLASS) {
            RunTyped<mxComplexDouble>(FullCube, Rin, Rout, plhs[0]);
        } else {
            RunTyped<mxComplexSingle>(FullCube, Rin, Rout, plhs[0]);
        }
    } else {
        if (mxGetClassID(FullCube) == mxDOUBLE_CLASS) {
            RunTyped<double>(FullCube, Rin, Rout, plhs[0]);
        } else {
            RunTyped<float>(FullCube, Rin, Rout, plhs[0]);
        }
    }
}
