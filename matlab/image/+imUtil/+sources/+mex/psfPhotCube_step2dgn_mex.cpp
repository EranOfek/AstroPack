// mex -R2018a CXXFLAGS='$CXXFLAGS -O3 -fopenmp' LDFLAGS='$LDFLAGS -fopenmp' psfPhotCube_step2dgn_mex.cpp
#include "mex.h"
#include <cmath>
#include <cstdint>
#include <limits>
#include <algorithm>

#if defined(_OPENMP)
#include <omp.h>
#endif

enum class StdMode {
    SCALAR,
    VECTOR_NIM,
    CUBE
};

struct NumericReader {
    const void* Data;
    mxClassID ClassID;

    NumericReader() : Data(nullptr), ClassID(mxUNKNOWN_CLASS) {}

    NumericReader(const mxArray* Arr)
        : Data(mxGetData(Arr)), ClassID(mxGetClassID(Arr)) {}

    inline double get(mwIndex I) const {
        if (ClassID == mxDOUBLE_CLASS) {
            return static_cast<const double*>(Data)[I];
        } else {
            return static_cast<double>(static_cast<const float*>(Data)[I]);
        }
    }
};

template <typename T>
inline double ToDouble(T x) {
    return static_cast<double>(x);
}

inline double Rcond2x2(double A11, double A12, double A22)
{
    const double Det = A11 * A22 - A12 * A12;
    if (!std::isfinite(Det) || std::abs(Det) == 0.0) {
        return 0.0;
    }

    const double NormA1 = std::max(std::abs(A11) + std::abs(A12),
                                   std::abs(A12) + std::abs(A22));
    if (!(NormA1 > 0.0) || !std::isfinite(NormA1)) {
        return 0.0;
    }

    const double NormInvA1 = std::max(std::abs(A22) + std::abs(A12),
                                      std::abs(A12) + std::abs(A11)) / std::abs(Det);
    if (!(NormInvA1 > 0.0) || !std::isfinite(NormInvA1)) {
        return 0.0;
    }

    return 1.0 / (NormA1 * NormInvA1);
}

void ValidateRealSingleOrDouble(const mxArray* Arr, const char* Name)
{
    const mxClassID ClassID = mxGetClassID(Arr);

    if (!(ClassID == mxDOUBLE_CLASS || ClassID == mxSINGLE_CLASS)) {
        mexErrMsgIdAndTxt("psfPhotCube_step2dgn_mex:Class",
                          "%s must be single or double.", Name);
    }

    if (mxIsComplex(Arr)) {
        mexErrMsgIdAndTxt("psfPhotCube_step2dgn_mex:Complex",
                          "%s must be real.", Name);
    }
}

void ValidateVectorLength(const mxArray* Arr, mwSize Expected, const char* Name)
{
    if (mxGetNumberOfElements(Arr) != Expected) {
        mexErrMsgIdAndTxt("psfPhotCube_step2dgn_mex:Size",
                          "%s must contain exactly the expected number of elements.", Name);
    }
}

void ValidateSameImageStackSize(const mxArray* Arr, mwSize Ny, mwSize Nx, mwSize Nim, const char* Name)
{
    const mwSize Ndim = mxGetNumberOfDimensions(Arr);
    const mwSize* Dims = mxGetDimensions(Arr);

    if (Ndim == 2) {
        if (!(Nim == 1 && Dims[0] == Ny && Dims[1] == Nx)) {
            mexErrMsgIdAndTxt("psfPhotCube_step2dgn_mex:Size",
                              "%s must have the same size as Cube.", Name);
        }
    } else if (Ndim == 3) {
        if (!(Dims[0] == Ny && Dims[1] == Nx && Dims[2] == Nim)) {
            mexErrMsgIdAndTxt("psfPhotCube_step2dgn_mex:Size",
                              "%s must have the same size as Cube.", Name);
        }
    } else {
        mexErrMsgIdAndTxt("psfPhotCube_step2dgn_mex:Size",
                          "%s must be NyxNx or NyxNxxNim.", Name);
    }
}

template <typename T, StdMode MODE, bool USE_RADIUS>
void ComputeStepsKernel(
    const T* Cube,
    NumericReader Std,
    NumericReader ShiftedPSF,
    NumericReader PSF_Xp,
    NumericReader PSF_Xm,
    NumericReader PSF_Yp,
    NumericReader PSF_Ym,
    NumericReader SX,
    NumericReader SY,
    NumericReader Flux,
    NumericReader DX,
    NumericReader DY,
    NumericReader VecXrel,
    NumericReader VecYrel,
    mwSize Ny,
    mwSize Nx,
    mwSize Nim,
    double FitRadius2,
    double MaxStep,
    T* StepX,
    T* StepY)
{
    const mwSize Npix = Ny * Nx;
    const double Radius = USE_RADIUS ? std::sqrt(FitRadius2) : 0.0;

    #if defined(_OPENMP)
    #pragma omp parallel for
    #endif
    for (mwIndex Iim = 0; Iim < Nim; ++Iim) {

        const T* CubeI = Cube + Iim * Npix;

        const double FluxI = Flux.get(Iim);
        const double DXI   = DX.get(Iim);
        const double DYI   = DY.get(Iim);
        const double SXI   = SX.get(Iim);
        const double SYI   = SY.get(Iim);

        double SxOut = 0.0;
        double SyOut = 0.0;

        if (!(std::isfinite(FluxI) && std::isfinite(DXI) && std::isfinite(DYI) &&
              std::isfinite(SXI) && std::isfinite(SYI) && SXI != 0.0 && SYI != 0.0)) {
            StepX[Iim] = static_cast<T>(0);
            StepY[Iim] = static_cast<T>(0);
            continue;
        }

        const double Inv2SX = 1.0 / (2.0 * SXI);
        const double Inv2SY = 1.0 / (2.0 * SYI);

        double A11 = 0.0;
        double A12 = 0.0;
        double A22 = 0.0;
        double B1  = 0.0;
        double B2  = 0.0;
        bool AnyGood = false;

        mwIndex IxStart = 0;
        mwIndex IxEnd   = Nx - 1;
        mwIndex IyStart = 0;
        mwIndex IyEnd   = Ny - 1;

        if constexpr (USE_RADIUS) {
            while (IxStart < Nx && std::abs(VecXrel.get(IxStart) - DXI) >= Radius) {
                ++IxStart;
            }
            while (IxEnd > IxStart && std::abs(VecXrel.get(IxEnd) - DXI) >= Radius) {
                --IxEnd;
            }
            while (IyStart < Ny && std::abs(VecYrel.get(IyStart) - DYI) >= Radius) {
                ++IyStart;
            }
            while (IyEnd > IyStart && std::abs(VecYrel.get(IyEnd) - DYI) >= Radius) {
                --IyEnd;
            }

            if (IxStart >= Nx || IyStart >= Ny || IxStart > IxEnd || IyStart > IyEnd) {
                StepX[Iim] = static_cast<T>(0);
                StepY[Iim] = static_cast<T>(0);
                continue;
            }
        }

        for (mwIndex Ix = IxStart; Ix <= IxEnd; ++Ix) {
            const double Xr = VecXrel.get(Ix) - DXI;
            const double Xr2 = Xr * Xr;

            for (mwIndex Iy = IyStart; Iy <= IyEnd; ++Iy) {
                if constexpr (USE_RADIUS) {
                    const double Yr = VecYrel.get(Iy) - DYI;
                    const double R2 = Xr2 + Yr * Yr;
                    if (!(R2 < FitRadius2)) {
                        continue;
                    }
                }

                const mwIndex Ip = Iy + Ix * Ny;
                const mwIndex Ip3 = Iim * Npix + Ip;

                double StdVal;
                if constexpr (MODE == StdMode::SCALAR) {
                    StdVal = Std.get(0);
                } else if constexpr (MODE == StdMode::VECTOR_NIM) {
                    StdVal = Std.get(Iim);
                } else {
                    StdVal = Std.get(Ip3);
                }

                // Fixed Std issue:
                // invalid, zero, or negative Std values are skipped.
                if (!(std::isfinite(StdVal)) || StdVal <= 0.0) {
                    continue;
                }

                const double W = 1.0 / (StdVal * StdVal);

                const double Resid = ToDouble(CubeI[Ip]) - ShiftedPSF.get(Ip3) * FluxI;
                const double dPdx  = (PSF_Xp.get(Ip3) - PSF_Xm.get(Ip3)) * Inv2SX;
                const double dPdy  = (PSF_Yp.get(Ip3) - PSF_Ym.get(Ip3)) * Inv2SY;

                const double Jx = FluxI * dPdx;
                const double Jy = FluxI * dPdy;

                if (!(std::isfinite(W) && std::isfinite(Resid) &&
                      std::isfinite(Jx) && std::isfinite(Jy))) {
                    continue;
                }

                AnyGood = true;

                A11 += W * Jx * Jx;
                A12 += W * Jx * Jy;
                A22 += W * Jy * Jy;
                B1  += W * Jx * Resid;
                B2  += W * Jy * Resid;
            }
        }

        if (AnyGood) {
            const bool FiniteA = std::isfinite(A11) && std::isfinite(A12) && std::isfinite(A22);
            const bool FiniteB = std::isfinite(B1)  && std::isfinite(B2);

            if (FiniteA && FiniteB && Rcond2x2(A11, A12, A22) > 1e-10) {
                const double Det = A11 * A22 - A12 * A12;
                SxOut = ( A22 * B1 - A12 * B2 ) / Det;
                SyOut = ( -A12 * B1 + A11 * B2 ) / Det;
            } else {
                SxOut = 0.0;
                SyOut = 0.0;
                if (std::isfinite(A11) && A11 > 0.0) {
                    SxOut = B1 / A11;
                }
                if (std::isfinite(A22) && A22 > 0.0) {
                    SyOut = B2 / A22;
                }
            }
        }

        if (std::isfinite(SxOut)) {
            SxOut = ((SxOut > 0.0) ? 1.0 : ((SxOut < 0.0) ? -1.0 : 0.0)) *
                    std::min(std::abs(SxOut), MaxStep);
        } else {
            SxOut = 0.0;
        }

        if (std::isfinite(SyOut)) {
            SyOut = ((SyOut > 0.0) ? 1.0 : ((SyOut < 0.0) ? -1.0 : 0.0)) *
                    std::min(std::abs(SyOut), MaxStep);
        } else {
            SyOut = 0.0;
        }

        if (!std::isfinite(SxOut)) { SxOut = 0.0; }
        if (!std::isfinite(SyOut)) { SyOut = 0.0; }

        StepX[Iim] = static_cast<T>(SxOut);
        StepY[Iim] = static_cast<T>(SyOut);
    }
}

template <typename T>
void DispatchStdMode(
    StdMode Mode,
    bool UseFitRadius,
    const T* Cube,
    NumericReader Std,
    NumericReader ShiftedPSF,
    NumericReader PSF_Xp,
    NumericReader PSF_Xm,
    NumericReader PSF_Yp,
    NumericReader PSF_Ym,
    NumericReader SX,
    NumericReader SY,
    NumericReader Flux,
    NumericReader DX,
    NumericReader DY,
    NumericReader VecXrel,
    NumericReader VecYrel,
    mwSize Ny,
    mwSize Nx,
    mwSize Nim,
    double FitRadius2,
    double MaxStep,
    T* StepX,
    T* StepY)
{
    if (UseFitRadius) {
        switch (Mode) {
            case StdMode::SCALAR:
                ComputeStepsKernel<T, StdMode::SCALAR, true>(
                    Cube, Std, ShiftedPSF, PSF_Xp, PSF_Xm, PSF_Yp, PSF_Ym,
                    SX, SY, Flux, DX, DY, VecXrel, VecYrel,
                    Ny, Nx, Nim, FitRadius2, MaxStep, StepX, StepY);
                break;

            case StdMode::VECTOR_NIM:
                ComputeStepsKernel<T, StdMode::VECTOR_NIM, true>(
                    Cube, Std, ShiftedPSF, PSF_Xp, PSF_Xm, PSF_Yp, PSF_Ym,
                    SX, SY, Flux, DX, DY, VecXrel, VecYrel,
                    Ny, Nx, Nim, FitRadius2, MaxStep, StepX, StepY);
                break;

            case StdMode::CUBE:
            default:
                ComputeStepsKernel<T, StdMode::CUBE, true>(
                    Cube, Std, ShiftedPSF, PSF_Xp, PSF_Xm, PSF_Yp, PSF_Ym,
                    SX, SY, Flux, DX, DY, VecXrel, VecYrel,
                    Ny, Nx, Nim, FitRadius2, MaxStep, StepX, StepY);
                break;
        }
    } else {
        switch (Mode) {
            case StdMode::SCALAR:
                ComputeStepsKernel<T, StdMode::SCALAR, false>(
                    Cube, Std, ShiftedPSF, PSF_Xp, PSF_Xm, PSF_Yp, PSF_Ym,
                    SX, SY, Flux, DX, DY, VecXrel, VecYrel,
                    Ny, Nx, Nim, FitRadius2, MaxStep, StepX, StepY);
                break;

            case StdMode::VECTOR_NIM:
                ComputeStepsKernel<T, StdMode::VECTOR_NIM, false>(
                    Cube, Std, ShiftedPSF, PSF_Xp, PSF_Xm, PSF_Yp, PSF_Ym,
                    SX, SY, Flux, DX, DY, VecXrel, VecYrel,
                    Ny, Nx, Nim, FitRadius2, MaxStep, StepX, StepY);
                break;

            case StdMode::CUBE:
            default:
                ComputeStepsKernel<T, StdMode::CUBE, false>(
                    Cube, Std, ShiftedPSF, PSF_Xp, PSF_Xm, PSF_Yp, PSF_Ym,
                    SX, SY, Flux, DX, DY, VecXrel, VecYrel,
                    Ny, Nx, Nim, FitRadius2, MaxStep, StepX, StepY);
                break;
        }
    }
}

void mexFunction(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[])
{
    if (nrhs != 16) {
        mexErrMsgIdAndTxt("psfPhotCube_step2dgn_mex:Input",
            "Expected 16 inputs: Cube, Std, ShiftedPSF, PSF_Xp, PSF_Xm, PSF_Yp, PSF_Ym, SX, SY, Flux, DX, DY, VecXrel, VecYrel, FitRadius2, MaxStep.");
    }

    if (nlhs != 2) {
        mexErrMsgIdAndTxt("psfPhotCube_step2dgn_mex:Output",
            "Expected 2 outputs: StepX, StepY.");
    }

    const mxArray* CubeArr       = prhs[0];
    const mxArray* StdArr        = prhs[1];
    const mxArray* ShiftedPSFArr = prhs[2];
    const mxArray* PSF_XpArr     = prhs[3];
    const mxArray* PSF_XmArr     = prhs[4];
    const mxArray* PSF_YpArr     = prhs[5];
    const mxArray* PSF_YmArr     = prhs[6];
    const mxArray* SXArr         = prhs[7];
    const mxArray* SYArr         = prhs[8];
    const mxArray* FluxArr       = prhs[9];
    const mxArray* DXArr         = prhs[10];
    const mxArray* DYArr         = prhs[11];
    const mxArray* VecXrelArr    = prhs[12];
    const mxArray* VecYrelArr    = prhs[13];
    const mxArray* FitRadiusArr  = prhs[14];
    const mxArray* MaxStepArr    = prhs[15];

    ValidateRealSingleOrDouble(CubeArr,       "Cube");
    ValidateRealSingleOrDouble(StdArr,        "Std");
    ValidateRealSingleOrDouble(ShiftedPSFArr, "ShiftedPSF");
    ValidateRealSingleOrDouble(PSF_XpArr,     "PSF_Xp");
    ValidateRealSingleOrDouble(PSF_XmArr,     "PSF_Xm");
    ValidateRealSingleOrDouble(PSF_YpArr,     "PSF_Yp");
    ValidateRealSingleOrDouble(PSF_YmArr,     "PSF_Ym");
    ValidateRealSingleOrDouble(SXArr,         "SX");
    ValidateRealSingleOrDouble(SYArr,         "SY");
    ValidateRealSingleOrDouble(FluxArr,       "Flux");
    ValidateRealSingleOrDouble(DXArr,         "DX");
    ValidateRealSingleOrDouble(DYArr,         "DY");
    ValidateRealSingleOrDouble(VecXrelArr,    "VecXrel");
    ValidateRealSingleOrDouble(VecYrelArr,    "VecYrel");

    const mxClassID ClassID = mxGetClassID(CubeArr);

    const mwSize CubeNdim = mxGetNumberOfDimensions(CubeArr);
    const mwSize* CubeDims = mxGetDimensions(CubeArr);

    if (!(CubeNdim == 2 || CubeNdim == 3)) {
        mexErrMsgIdAndTxt("psfPhotCube_step2dgn_mex:Size",
                          "Cube must be NyxNx or NyxNxxNim.");
    }

    const mwSize Ny  = CubeDims[0];
    const mwSize Nx  = CubeDims[1];
    const mwSize Nim = (CubeNdim == 3) ? CubeDims[2] : 1;

    ValidateSameImageStackSize(ShiftedPSFArr, Ny, Nx, Nim, "ShiftedPSF");
    ValidateSameImageStackSize(PSF_XpArr,     Ny, Nx, Nim, "PSF_Xp");
    ValidateSameImageStackSize(PSF_XmArr,     Ny, Nx, Nim, "PSF_Xm");
    ValidateSameImageStackSize(PSF_YpArr,     Ny, Nx, Nim, "PSF_Yp");
    ValidateSameImageStackSize(PSF_YmArr,     Ny, Nx, Nim, "PSF_Ym");

    ValidateVectorLength(SXArr, Nim, "SX");
    ValidateVectorLength(SYArr, Nim, "SY");
    ValidateVectorLength(FluxArr, Nim, "Flux");
    ValidateVectorLength(DXArr, Nim, "DX");
    ValidateVectorLength(DYArr, Nim, "DY");
    ValidateVectorLength(VecXrelArr, Nx, "VecXrel");
    ValidateVectorLength(VecYrelArr, Ny, "VecYrel");

    StdMode ThisStdMode;
    {
        const mwSize StdN = mxGetNumberOfElements(StdArr);
        const mwSize StdNdim = mxGetNumberOfDimensions(StdArr);
        const mwSize* StdDims = mxGetDimensions(StdArr);

        if (StdN == 1) {
            ThisStdMode = StdMode::SCALAR;
        } else if (StdN == Nim &&
                   ((StdNdim == 2 && (StdDims[0] == Nim || StdDims[1] == Nim)) ||
                    (StdNdim == 3 && StdDims[0] == 1 && StdDims[1] == 1 && StdDims[2] == Nim))) {
            ThisStdMode = StdMode::VECTOR_NIM;
        } else if (StdNdim == 3 && StdDims[0] == Ny && StdDims[1] == Nx && StdDims[2] == Nim) {
            ThisStdMode = StdMode::CUBE;
        } else if (Nim == 1 && StdNdim == 2 && StdDims[0] == Ny && StdDims[1] == Nx) {
            ThisStdMode = StdMode::CUBE;
        } else {
            mexErrMsgIdAndTxt("psfPhotCube_step2dgn_mex:Size",
                              "Std must be scalar, vector of length Nim, 1x1xNim, or NyxNxxNim.");
        }
    }

    bool UseFitRadius = false;
    double FitRadius2 = 0.0;

    if (!mxIsEmpty(FitRadiusArr)) {
        if (mxGetNumberOfElements(FitRadiusArr) != 1) {
            mexErrMsgIdAndTxt("psfPhotCube_step2dgn_mex:Size",
                              "FitRadius2 must be empty or scalar.");
        }

        FitRadius2 = mxGetScalar(FitRadiusArr);
        UseFitRadius = true;
    }

    if (mxGetNumberOfElements(MaxStepArr) != 1) {
        mexErrMsgIdAndTxt("psfPhotCube_step2dgn_mex:Size", "MaxStep must be scalar.");
    }

    const double MaxStep = mxGetScalar(MaxStepArr);

    // Kept as in your original function: row vectors 1 x Nim.
    mwSize OutDims[2] = {1, Nim};
    plhs[0] = mxCreateNumericArray(2, OutDims, ClassID, mxREAL);
    plhs[1] = mxCreateNumericArray(2, OutDims, ClassID, mxREAL);

    NumericReader StdReader(StdArr);
    NumericReader ShiftedPSFReader(ShiftedPSFArr);
    NumericReader PSF_XpReader(PSF_XpArr);
    NumericReader PSF_XmReader(PSF_XmArr);
    NumericReader PSF_YpReader(PSF_YpArr);
    NumericReader PSF_YmReader(PSF_YmArr);
    NumericReader SXReader(SXArr);
    NumericReader SYReader(SYArr);
    NumericReader FluxReader(FluxArr);
    NumericReader DXReader(DXArr);
    NumericReader DYReader(DYArr);
    NumericReader VecXrelReader(VecXrelArr);
    NumericReader VecYrelReader(VecYrelArr);

    if (ClassID == mxDOUBLE_CLASS) {
        DispatchStdMode<double>(
            ThisStdMode, UseFitRadius,
            static_cast<const double*>(mxGetData(CubeArr)),
            StdReader,
            ShiftedPSFReader,
            PSF_XpReader,
            PSF_XmReader,
            PSF_YpReader,
            PSF_YmReader,
            SXReader,
            SYReader,
            FluxReader,
            DXReader,
            DYReader,
            VecXrelReader,
            VecYrelReader,
            Ny, Nx, Nim, FitRadius2, MaxStep,
            static_cast<double*>(mxGetData(plhs[0])),
            static_cast<double*>(mxGetData(plhs[1]))
        );
    } else {
        DispatchStdMode<float>(
            ThisStdMode, UseFitRadius,
            static_cast<const float*>(mxGetData(CubeArr)),
            StdReader,
            ShiftedPSFReader,
            PSF_XpReader,
            PSF_XmReader,
            PSF_YpReader,
            PSF_YmReader,
            SXReader,
            SYReader,
            FluxReader,
            DXReader,
            DYReader,
            VecXrelReader,
            VecYrelReader,
            Ny, Nx, Nim, FitRadius2, MaxStep,
            static_cast<float*>(mxGetData(plhs[0])),
            static_cast<float*>(mxGetData(plhs[1]))
        );
    }
}
