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

void ValidateRealSingleOrDouble(const mxArray* Arr, const char* Name)
{
    const mxClassID ClassID = mxGetClassID(Arr);
    if (!(ClassID == mxDOUBLE_CLASS || ClassID == mxSINGLE_CLASS)) {
        mexErrMsgIdAndTxt("psfPhotCube_chi2flux_mex:Class",
                          "%s must be single or double.", Name);
    }

    if (mxIsComplex(Arr)) {
        mexErrMsgIdAndTxt("psfPhotCube_chi2flux_mex:Complex",
                          "%s must be real.", Name);
    }
}

void ValidateVectorLength(const mxArray* Arr, mwSize Expected, const char* Name)
{
    if (mxGetNumberOfElements(Arr) != Expected) {
        mexErrMsgIdAndTxt("psfPhotCube_chi2flux_mex:Size",
                          "%s must contain exactly the expected number of elements.", Name);
    }
}

void ValidateSameImageStackSize(const mxArray* Arr, mwSize Ny, mwSize Nx, mwSize Nim, const char* Name)
{
    const mwSize Ndim = mxGetNumberOfDimensions(Arr);
    const mwSize* Dims = mxGetDimensions(Arr);

    if (Ndim == 2) {
        if (!(Nim == 1 && Dims[0] == Ny && Dims[1] == Nx)) {
            mexErrMsgIdAndTxt("psfPhotCube_chi2flux_mex:Size",
                              "%s must have the same size as Cube.", Name);
        }
    } else if (Ndim == 3) {
        if (!(Dims[0] == Ny && Dims[1] == Nx && Dims[2] == Nim)) {
            mexErrMsgIdAndTxt("psfPhotCube_chi2flux_mex:Size",
                              "%s must have the same size as Cube.", Name);
        }
    } else {
        mexErrMsgIdAndTxt("psfPhotCube_chi2flux_mex:Size",
                          "%s must be NyxNx or NyxNxxNim.", Name);
    }
}

template <typename T, StdMode MODE, bool USE_RADIUS>
void ComputeChi2FluxKernel(
    const T* Cube,
    NumericReader Std,
    NumericReader ShiftedPSF,
    NumericReader DX,
    NumericReader DY,
    NumericReader VecXrel,
    NumericReader VecYrel,
    mwSize Ny,
    mwSize Nx,
    mwSize Nim,
    double FitRadius2,
    T* Chi2,
    T* Flux,
    T* Dof,
    T* FluxErr)
{
    const mwSize Npix = Ny * Nx;
    const double Radius = USE_RADIUS ? std::sqrt(FitRadius2) : 0.0;

    #if defined(_OPENMP)
    #pragma omp parallel for
    #endif
    for (mwIndex Iim = 0; Iim < Nim; ++Iim) {
        const T* CubeI = Cube + Iim * Npix;

        const double DXI = DX.get(Iim);
        const double DYI = DY.get(Iim);

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
        }

        double Num = 0.0;
        double Den = 0.0;
        double SumD2 = 0.0;
        double DofCount = 0.0;

        if (!(IxStart >= Nx || IyStart >= Ny || IxStart > IxEnd || IyStart > IyEnd)) {
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

                    double StdVal;
                    if constexpr (MODE == StdMode::SCALAR) {
                        StdVal = Std.get(0);
                    } else if constexpr (MODE == StdMode::VECTOR_NIM) {
                        StdVal = Std.get(Iim);
                    } else {
                        StdVal = Std.get(Iim * Npix + Ip);
                    }

                    const double CubeVal = ToDouble(CubeI[Ip]);
                    const double PVal    = ShiftedPSF.get(Iim * Npix + Ip);

                    if (!(std::isfinite(StdVal) &&
                          std::isfinite(CubeVal) &&
                          std::isfinite(PVal))) {
                        continue;
                    }

                    // Requested fix:
                    // Std <= 0 is invalid. Do not give it enormous weight.
                    if (StdVal <= 0.0) {
                        continue;
                    }

                    const double W = 1.0 / (StdVal * StdVal);

                    const double WP  = W * PVal;
                    const double WDP = WP * CubeVal;
                    const double WPP = WP * PVal;
                    const double WDD = W * CubeVal * CubeVal;

                    if (!(std::isfinite(WDP) &&
                          std::isfinite(WPP) &&
                          std::isfinite(WDD))) {
                        continue;
                    }

                    Num   += WDP;
                    Den   += WPP;
                    SumD2 += WDD;

                    // DofCount now counts only pixels that actually enter the fit.
                    DofCount += 1.0;
                }
            }
        }

        double FluxI;
        double FluxErrI;
        double Chi2I;

        if (Den > 0.0 && std::isfinite(Den)) {
            FluxI = Num / Den;
            FluxErrI = std::sqrt(1.0 / Den);

            // One-pass chi2 formula:
            // chi2 = sum(w D^2) - (sum(w D P))^2 / sum(w P^2)
            Chi2I = SumD2 - (Num * Num) / Den;

            // Protect against tiny negative values from floating-point roundoff.
            if (Chi2I < 0.0) {
                const double Scale = std::max(std::abs(SumD2), std::abs((Num * Num) / Den));
                if (Chi2I > -1e-12 * Scale) {
                    Chi2I = 0.0;
                }
            }
        } else {
            FluxI    = std::numeric_limits<double>::quiet_NaN();
            FluxErrI = std::numeric_limits<double>::infinity();
            Chi2I    = std::numeric_limits<double>::quiet_NaN();
        }

        // Requested: keep 3 fitted parameters.
        double DofI = DofCount - 3.0;

        if (!std::isfinite(DofI)) {
            DofI = 0.0;
        }

        Chi2[Iim]    = static_cast<T>(Chi2I);
        Flux[Iim]    = static_cast<T>(FluxI);
        Dof[Iim]     = static_cast<T>(DofI);
        FluxErr[Iim] = static_cast<T>(FluxErrI);
    }
}

template <typename T>
void DispatchStdMode(
    StdMode Mode,
    bool UseFitRadius,
    const T* Cube,
    NumericReader Std,
    NumericReader ShiftedPSF,
    NumericReader DX,
    NumericReader DY,
    NumericReader VecXrel,
    NumericReader VecYrel,
    mwSize Ny,
    mwSize Nx,
    mwSize Nim,
    double FitRadius2,
    T* Chi2,
    T* Flux,
    T* Dof,
    T* FluxErr)
{
    if (UseFitRadius) {
        switch (Mode) {
            case StdMode::SCALAR:
                ComputeChi2FluxKernel<T, StdMode::SCALAR, true>(
                    Cube, Std, ShiftedPSF, DX, DY, VecXrel, VecYrel,
                    Ny, Nx, Nim, FitRadius2, Chi2, Flux, Dof, FluxErr);
                break;

            case StdMode::VECTOR_NIM:
                ComputeChi2FluxKernel<T, StdMode::VECTOR_NIM, true>(
                    Cube, Std, ShiftedPSF, DX, DY, VecXrel, VecYrel,
                    Ny, Nx, Nim, FitRadius2, Chi2, Flux, Dof, FluxErr);
                break;

            case StdMode::CUBE:
            default:
                ComputeChi2FluxKernel<T, StdMode::CUBE, true>(
                    Cube, Std, ShiftedPSF, DX, DY, VecXrel, VecYrel,
                    Ny, Nx, Nim, FitRadius2, Chi2, Flux, Dof, FluxErr);
                break;
        }
    } else {
        switch (Mode) {
            case StdMode::SCALAR:
                ComputeChi2FluxKernel<T, StdMode::SCALAR, false>(
                    Cube, Std, ShiftedPSF, DX, DY, VecXrel, VecYrel,
                    Ny, Nx, Nim, FitRadius2, Chi2, Flux, Dof, FluxErr);
                break;

            case StdMode::VECTOR_NIM:
                ComputeChi2FluxKernel<T, StdMode::VECTOR_NIM, false>(
                    Cube, Std, ShiftedPSF, DX, DY, VecXrel, VecYrel,
                    Ny, Nx, Nim, FitRadius2, Chi2, Flux, Dof, FluxErr);
                break;

            case StdMode::CUBE:
            default:
                ComputeChi2FluxKernel<T, StdMode::CUBE, false>(
                    Cube, Std, ShiftedPSF, DX, DY, VecXrel, VecYrel,
                    Ny, Nx, Nim, FitRadius2, Chi2, Flux, Dof, FluxErr);
                break;
        }
    }
}

void mexFunction(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[])
{
    if (nrhs != 8) {
        mexErrMsgIdAndTxt("psfPhotCube_chi2flux_mex:Input",
            "Expected 8 inputs: Cube, Std, ShiftedPSF, DX, DY, VecXrel, VecYrel, FitRadius2.");
    }

    if (nlhs != 4) {
        mexErrMsgIdAndTxt("psfPhotCube_chi2flux_mex:Output",
            "Expected 4 outputs: Chi2, Flux, Dof, FluxErr.");
    }

    const mxArray* CubeArr       = prhs[0];
    const mxArray* StdArr        = prhs[1];
    const mxArray* ShiftedPSFArr = prhs[2];
    const mxArray* DXArr         = prhs[3];
    const mxArray* DYArr         = prhs[4];
    const mxArray* VecXrelArr    = prhs[5];
    const mxArray* VecYrelArr    = prhs[6];
    const mxArray* FitRadiusArr  = prhs[7];

    ValidateRealSingleOrDouble(CubeArr,       "Cube");
    ValidateRealSingleOrDouble(StdArr,        "Std");
    ValidateRealSingleOrDouble(ShiftedPSFArr, "ShiftedPSF");
    ValidateRealSingleOrDouble(DXArr,         "DX");
    ValidateRealSingleOrDouble(DYArr,         "DY");
    ValidateRealSingleOrDouble(VecXrelArr,    "VecXrel");
    ValidateRealSingleOrDouble(VecYrelArr,    "VecYrel");

    const mxClassID CubeClassID = mxGetClassID(CubeArr);

    const mwSize CubeNdim = mxGetNumberOfDimensions(CubeArr);
    const mwSize* CubeDims = mxGetDimensions(CubeArr);

    if (!(CubeNdim == 2 || CubeNdim == 3)) {
        mexErrMsgIdAndTxt("psfPhotCube_chi2flux_mex:Size",
                          "Cube must be NyxNx or NyxNxxNim.");
    }

    const mwSize Ny  = CubeDims[0];
    const mwSize Nx  = CubeDims[1];
    const mwSize Nim = (CubeNdim == 3) ? CubeDims[2] : 1;

    if (Ny == 0 || Nx == 0 || Nim == 0) {
        mexErrMsgIdAndTxt("psfPhotCube_chi2flux_mex:Size",
                          "Cube dimensions must be non-zero.");
    }

    ValidateSameImageStackSize(ShiftedPSFArr, Ny, Nx, Nim, "ShiftedPSF");
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
            mexErrMsgIdAndTxt("psfPhotCube_chi2flux_mex:Size",
                              "Std must be scalar, vector of length Nim, 1x1xNim, or NyxNxxNim.");
        }
    }

    bool UseFitRadius = false;
    double FitRadius2 = 0.0;

    if (!mxIsEmpty(FitRadiusArr)) {
        if (mxGetNumberOfElements(FitRadiusArr) != 1) {
            mexErrMsgIdAndTxt("psfPhotCube_chi2flux_mex:Size",
                              "FitRadius2 must be empty or scalar.");
        }

        FitRadius2 = mxGetScalar(FitRadiusArr);

        if (!(std::isfinite(FitRadius2)) || FitRadius2 <= 0.0) {
            mexErrMsgIdAndTxt("psfPhotCube_chi2flux_mex:Value",
                              "FitRadius2 must be positive and finite.");
        }

        UseFitRadius = true;
    }

    mwSize OutDims[2] = {Nim, 1};

    plhs[0] = mxCreateNumericArray(2, OutDims, CubeClassID, mxREAL); // Chi2
    plhs[1] = mxCreateNumericArray(2, OutDims, CubeClassID, mxREAL); // Flux
    plhs[2] = mxCreateNumericArray(2, OutDims, CubeClassID, mxREAL); // Dof
    plhs[3] = mxCreateNumericArray(2, OutDims, CubeClassID, mxREAL); // FluxErr

    NumericReader StdReader(StdArr);
    NumericReader ShiftedPSFReader(ShiftedPSFArr);
    NumericReader DXReader(DXArr);
    NumericReader DYReader(DYArr);
    NumericReader VecXrelReader(VecXrelArr);
    NumericReader VecYrelReader(VecYrelArr);

    if (CubeClassID == mxDOUBLE_CLASS) {
        DispatchStdMode<double>(
            ThisStdMode, UseFitRadius,
            static_cast<const double*>(mxGetData(CubeArr)),
            StdReader,
            ShiftedPSFReader,
            DXReader,
            DYReader,
            VecXrelReader,
            VecYrelReader,
            Ny, Nx, Nim, FitRadius2,
            static_cast<double*>(mxGetData(plhs[0])),
            static_cast<double*>(mxGetData(plhs[1])),
            static_cast<double*>(mxGetData(plhs[2])),
            static_cast<double*>(mxGetData(plhs[3]))
        );
    } else {
        DispatchStdMode<float>(
            ThisStdMode, UseFitRadius,
            static_cast<const float*>(mxGetData(CubeArr)),
            StdReader,
            ShiftedPSFReader,
            DXReader,
            DYReader,
            VecXrelReader,
            VecYrelReader,
            Ny, Nx, Nim, FitRadius2,
            static_cast<float*>(mxGetData(plhs[0])),
            static_cast<float*>(mxGetData(plhs[1])),
            static_cast<float*>(mxGetData(plhs[2])),
            static_cast<float*>(mxGetData(plhs[3]))
        );
    }
}
