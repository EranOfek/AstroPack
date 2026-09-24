#include "mex.h"
#include <cmath>
#include <cstdint>
#include <limits>
#include <algorithm>
#include <cstring>

#if defined(_OPENMP)
#include <omp.h>
#endif

#if defined(__AVX2__) || defined(__AVX512F__)
#include <immintrin.h>
#endif

enum class StdMode {
    SCALAR,
    VECTOR_NIM,
    CUBE
};

template <typename T>
inline double ToDouble(T x) {
    return static_cast<double>(x);
}

void ValidateVectorLength(const mxArray* Arr, mwSize Expected, const char* Name)
{
    if (mxGetNumberOfElements(Arr) != Expected) {
        mexErrMsgIdAndTxt("psfPhotCube_chi2flux_mex:Size",
                          "%s must contain exactly the expected number of elements.", Name);
    }
}

void ValidateSameCubeSize(const mxArray* Arr, mwSize Ny, mwSize Nx, mwSize Nim, const char* Name)
{
    const mwSize Ndim = mxGetNumberOfDimensions(Arr);
    const mwSize* Dims = mxGetDimensions(Arr);
    if (Ndim != 3 || Dims[0] != Ny || Dims[1] != Nx || Dims[2] != Nim) {
        mexErrMsgIdAndTxt("psfPhotCube_chi2flux_mex:Size",
                          "%s must have the same size as Cube.", Name);
    }
}

#if defined(__AVX512F__)
static inline double HSum512(__m512d v)
{
    alignas(64) double Tmp[8];
    _mm512_store_pd(Tmp, v);
    double S = 0.0;
    for (int i = 0; i < 8; ++i) { S += Tmp[i]; }
    return S;
}
#endif

#if defined(__AVX2__)
static inline double HSum256(__m256d v)
{
    alignas(32) double Tmp[4];
    _mm256_store_pd(Tmp, v);
    return Tmp[0] + Tmp[1] + Tmp[2] + Tmp[3];
}
#endif

template <typename T>
void ComputeChi2FluxScalar(
    const T* CubeI,
    const T* StdI,
    const T* ShiftedPSFI,
    mwSize Ny,
    mwSize Nx,
    double DXI,
    double DYI,
    const T* VecXrel,
    const T* VecYrel,
    bool UseRadius,
    double FitRadius2,
    StdMode Mode,
    double StdScalar,
    double& Num,
    double& Den,
    double& DofCount)
{
    const mwSize Npix = Ny * Nx;
    const double Eps = std::numeric_limits<double>::epsilon();
    (void)Npix;

    for (mwIndex Ix = 0; Ix < Nx; ++Ix) {
        const double Xr = ToDouble(VecXrel[Ix]) - DXI;
        const double Xr2 = Xr * Xr;

        for (mwIndex Iy = 0; Iy < Ny; ++Iy) {
            if (UseRadius) {
                const double Yr = ToDouble(VecYrel[Iy]) - DYI;
                const double R2 = Xr2 + Yr * Yr;
                if (!(R2 < FitRadius2)) {
                    continue;
                }
            }

            const mwIndex Ip = Iy + Ix * Ny;

            double StdVal;
            if (Mode == StdMode::SCALAR || Mode == StdMode::VECTOR_NIM) {
                StdVal = StdScalar;
            } else {
                StdVal = ToDouble(StdI[Ip]);
            }

            double Var = StdVal * StdVal;
            if (!std::isfinite(Var)) {
                continue;
            }
            if (Var < Eps) {
                Var = Eps;
            }
            const double W = 1.0 / Var;

            const double CubeVal = ToDouble(CubeI[Ip]);
            const double PVal    = ToDouble(ShiftedPSFI[Ip]);

            const double TermNum = W * CubeVal * PVal;
            const double TermDen = W * PVal * PVal;

            if (std::isfinite(TermNum) && std::isfinite(TermDen)) {
                Num += TermNum;
                Den += TermDen;
            }

            DofCount += 1.0;
        }
    }
}

template <typename T>
void ComputeChi2Scalar(
    const T* CubeI,
    const T* StdI,
    const T* ShiftedPSFI,
    mwSize Ny,
    mwSize Nx,
    double DXI,
    double DYI,
    const T* VecXrel,
    const T* VecYrel,
    bool UseRadius,
    double FitRadius2,
    StdMode Mode,
    double StdScalar,
    double FluxI,
    double& Chi2I)
{
    for (mwIndex Ix = 0; Ix < Nx; ++Ix) {
        const double Xr = ToDouble(VecXrel[Ix]) - DXI;
        const double Xr2 = Xr * Xr;

        for (mwIndex Iy = 0; Iy < Ny; ++Iy) {
            if (UseRadius) {
                const double Yr = ToDouble(VecYrel[Iy]) - DYI;
                const double R2 = Xr2 + Yr * Yr;
                if (!(R2 < FitRadius2)) {
                    continue;
                }
            }

            const mwIndex Ip = Iy + Ix * Ny;

            double StdVal;
            if (Mode == StdMode::SCALAR || Mode == StdMode::VECTOR_NIM) {
                StdVal = StdScalar;
            } else {
                StdVal = ToDouble(StdI[Ip]);
            }

            const double CubeVal = ToDouble(CubeI[Ip]);
            const double PVal    = ToDouble(ShiftedPSFI[Ip]);

            if (!(std::isfinite(StdVal) && std::isfinite(CubeVal) && std::isfinite(PVal))) {
                continue;
            }
            if (StdVal == 0.0) {
                continue;
            }

            const double Resid = CubeVal - PVal * FluxI;
            const double ResidStd = Resid / StdVal;
            const double Term = ResidStd * ResidStd;

            if (std::isfinite(Term)) {
                Chi2I += Term;
            }
        }
    }
}

#if defined(__AVX512F__)
static void ComputeChi2FluxAVX512_Double_ScalarStd_NoRadius(
    const double* CubeI,
    const double* ShiftedPSFI,
    mwSize Npix,
    double StdScalar,
    double& Num,
    double& Den,
    double& DofCount)
{
    const double Eps = std::numeric_limits<double>::epsilon();
    double Var = StdScalar * StdScalar;
    if (!std::isfinite(Var)) {
        return;
    }
    if (Var < Eps) {
        Var = Eps;
    }
    const double W = 1.0 / Var;
    const __m512d VW = _mm512_set1_pd(W);

    __m512d VNum = _mm512_setzero_pd();
    __m512d VDen = _mm512_setzero_pd();

    mwIndex I = 0;
    for (; I + 7 < Npix; I += 8) {
        __m512d VC = _mm512_loadu_pd(CubeI + I);
        __m512d VP = _mm512_loadu_pd(ShiftedPSFI + I);

        __m512d TNum = _mm512_mul_pd(VW, _mm512_mul_pd(VC, VP));
        __m512d TDen = _mm512_mul_pd(VW, _mm512_mul_pd(VP, VP));

        VNum = _mm512_add_pd(VNum, TNum);
        VDen = _mm512_add_pd(VDen, TDen);
    }

    Num += HSum512(VNum);
    Den += HSum512(VDen);
    DofCount += static_cast<double>(Npix);

    for (; I < Npix; ++I) {
        const double TermNum = W * CubeI[I] * ShiftedPSFI[I];
        const double TermDen = W * ShiftedPSFI[I] * ShiftedPSFI[I];
        if (std::isfinite(TermNum) && std::isfinite(TermDen)) {
            Num += TermNum;
            Den += TermDen;
        }
    }
}

static void ComputeChi2AVX512_Double_ScalarStd_NoRadius(
    const double* CubeI,
    const double* ShiftedPSFI,
    mwSize Npix,
    double StdScalar,
    double FluxI,
    double& Chi2I)
{
    const double InvStd = 1.0 / StdScalar;
    const __m512d VFlux = _mm512_set1_pd(FluxI);
    const __m512d VInvS = _mm512_set1_pd(InvStd);

    __m512d VChi2 = _mm512_setzero_pd();

    mwIndex I = 0;
    for (; I + 7 < Npix; I += 8) {
        __m512d VC = _mm512_loadu_pd(CubeI + I);
        __m512d VP = _mm512_loadu_pd(ShiftedPSFI + I);

        __m512d VR = _mm512_sub_pd(VC, _mm512_mul_pd(VP, VFlux));
        __m512d VRS = _mm512_mul_pd(VR, VInvS);
        __m512d VT = _mm512_mul_pd(VRS, VRS);

        VChi2 = _mm512_add_pd(VChi2, VT);
    }

    Chi2I += HSum512(VChi2);

    for (; I < Npix; ++I) {
        const double Resid = CubeI[I] - ShiftedPSFI[I] * FluxI;
        const double ResidStd = Resid / StdScalar;
        const double Term = ResidStd * ResidStd;
        if (std::isfinite(Term)) {
            Chi2I += Term;
        }
    }
}
#endif

#if defined(__AVX2__)
static void ComputeChi2FluxAVX2_Double_ScalarStd_NoRadius(
    const double* CubeI,
    const double* ShiftedPSFI,
    mwSize Npix,
    double StdScalar,
    double& Num,
    double& Den,
    double& DofCount)
{
    const double Eps = std::numeric_limits<double>::epsilon();
    double Var = StdScalar * StdScalar;
    if (!std::isfinite(Var)) {
        return;
    }
    if (Var < Eps) {
        Var = Eps;
    }
    const double W = 1.0 / Var;
    const __m256d VW = _mm256_set1_pd(W);

    __m256d VNum = _mm256_setzero_pd();
    __m256d VDen = _mm256_setzero_pd();

    mwIndex I = 0;
    for (; I + 3 < Npix; I += 4) {
        __m256d VC = _mm256_loadu_pd(CubeI + I);
        __m256d VP = _mm256_loadu_pd(ShiftedPSFI + I);

        __m256d TNum = _mm256_mul_pd(VW, _mm256_mul_pd(VC, VP));
        __m256d TDen = _mm256_mul_pd(VW, _mm256_mul_pd(VP, VP));

        VNum = _mm256_add_pd(VNum, TNum);
        VDen = _mm256_add_pd(VDen, TDen);
    }

    Num += HSum256(VNum);
    Den += HSum256(VDen);
    DofCount += static_cast<double>(Npix);

    for (; I < Npix; ++I) {
        const double TermNum = W * CubeI[I] * ShiftedPSFI[I];
        const double TermDen = W * ShiftedPSFI[I] * ShiftedPSFI[I];
        if (std::isfinite(TermNum) && std::isfinite(TermDen)) {
            Num += TermNum;
            Den += TermDen;
        }
    }
}

static void ComputeChi2AVX2_Double_ScalarStd_NoRadius(
    const double* CubeI,
    const double* ShiftedPSFI,
    mwSize Npix,
    double StdScalar,
    double FluxI,
    double& Chi2I)
{
    const double InvStd = 1.0 / StdScalar;
    const __m256d VFlux = _mm256_set1_pd(FluxI);
    const __m256d VInvS = _mm256_set1_pd(InvStd);

    __m256d VChi2 = _mm256_setzero_pd();

    mwIndex I = 0;
    for (; I + 3 < Npix; I += 4) {
        __m256d VC = _mm256_loadu_pd(CubeI + I);
        __m256d VP = _mm256_loadu_pd(ShiftedPSFI + I);

        __m256d VR = _mm256_sub_pd(VC, _mm256_mul_pd(VP, VFlux));
        __m256d VRS = _mm256_mul_pd(VR, VInvS);
        __m256d VT = _mm256_mul_pd(VRS, VRS);

        VChi2 = _mm256_add_pd(VChi2, VT);
    }

    Chi2I += HSum256(VChi2);

    for (; I < Npix; ++I) {
        const double Resid = CubeI[I] - ShiftedPSFI[I] * FluxI;
        const double ResidStd = Resid / StdScalar;
        const double Term = ResidStd * ResidStd;
        if (std::isfinite(Term)) {
            Chi2I += Term;
        }
    }
}
#endif

template <typename T, StdMode MODE, bool USE_RADIUS>
void ComputeChi2FluxKernel(
    const T* Cube,
    const T* Std,
    const T* ShiftedPSF,
    const T* DX,
    const T* DY,
    const T* VecXrel,
    const T* VecYrel,
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
    const double Eps = std::numeric_limits<double>::epsilon();
    const double Radius = USE_RADIUS ? std::sqrt(FitRadius2) : 0.0;

    #if defined(_OPENMP)
    #pragma omp parallel for
    #endif
    for (mwIndex Iim = 0; Iim < Nim; ++Iim) {
        const T* CubeI       = Cube       + Iim * Npix;
        const T* ShiftedPSFI = ShiftedPSF + Iim * Npix;

        const double DXI = ToDouble(DX[Iim]);
        const double DYI = ToDouble(DY[Iim]);

        mwIndex IxStart = 0;
        mwIndex IxEnd   = Nx - 1;
        mwIndex IyStart = 0;
        mwIndex IyEnd   = Ny - 1;

        if constexpr (USE_RADIUS) {
            while (IxStart < Nx && std::abs(ToDouble(VecXrel[IxStart]) - DXI) >= Radius) {
                ++IxStart;
            }
            while (IxEnd > IxStart && std::abs(ToDouble(VecXrel[IxEnd]) - DXI) >= Radius) {
                --IxEnd;
            }
            while (IyStart < Ny && std::abs(ToDouble(VecYrel[IyStart]) - DYI) >= Radius) {
                ++IyStart;
            }
            while (IyEnd > IyStart && std::abs(ToDouble(VecYrel[IyEnd]) - DYI) >= Radius) {
                --IyEnd;
            }
        }

        double Num = 0.0;
        double Den = 0.0;
        double DofCount = 0.0;

        const bool DenseLoop = !USE_RADIUS;

        if (!(IxStart >= Nx || IyStart >= Ny || IxStart > IxEnd || IyStart > IyEnd)) {

            bool UsedSIMD = false;

            if constexpr (!USE_RADIUS) {
                if constexpr (std::is_same<T,double>::value) {
                    double StdScalar = 0.0;
                    bool CanSIMD = false;

                    if constexpr (MODE == StdMode::SCALAR) {
                        StdScalar = ToDouble(Std[0]);
                        CanSIMD = true;
                    } else if constexpr (MODE == StdMode::VECTOR_NIM) {
                        StdScalar = ToDouble(Std[Iim]);
                        CanSIMD = true;
                    }

                    if (CanSIMD && std::isfinite(StdScalar) && StdScalar != 0.0) {
                        #if defined(__AVX512F__)
                        ComputeChi2FluxAVX512_Double_ScalarStd_NoRadius(
                            reinterpret_cast<const double*>(CubeI),
                            reinterpret_cast<const double*>(ShiftedPSFI),
                            Npix, StdScalar, Num, Den, DofCount);
                        UsedSIMD = true;
                        #elif defined(__AVX2__)
                        ComputeChi2FluxAVX2_Double_ScalarStd_NoRadius(
                            reinterpret_cast<const double*>(CubeI),
                            reinterpret_cast<const double*>(ShiftedPSFI),
                            Npix, StdScalar, Num, Den, DofCount);
                        UsedSIMD = true;
                        #endif
                    }
                }
            }

            if (!UsedSIMD) {
                const T* StdI = (MODE == StdMode::CUBE) ? (Std + Iim * Npix) : Std;
                double StdScalar = 0.0;
                if constexpr (MODE == StdMode::SCALAR) {
                    StdScalar = ToDouble(Std[0]);
                } else if constexpr (MODE == StdMode::VECTOR_NIM) {
                    StdScalar = ToDouble(Std[Iim]);
                }

                ComputeChi2FluxScalar(
                    CubeI, StdI, ShiftedPSFI, Ny, Nx, DXI, DYI,
                    VecXrel, VecYrel, USE_RADIUS, FitRadius2, MODE, StdScalar,
                    Num, Den, DofCount);
            }
        }

        if (Den < Eps || !std::isfinite(Den)) {
            Den = Eps;
        }

        const double FluxI = Num / Den;
        const double FluxErrI = std::sqrt(1.0 / Den);

        double Chi2I = 0.0;

        if (!(IxStart >= Nx || IyStart >= Ny || IxStart > IxEnd || IyStart > IyEnd)) {
            bool UsedSIMD = false;

            if constexpr (!USE_RADIUS) {
                if constexpr (std::is_same<T,double>::value) {
                    double StdScalar = 0.0;
                    bool CanSIMD = false;

                    if constexpr (MODE == StdMode::SCALAR) {
                        StdScalar = ToDouble(Std[0]);
                        CanSIMD = true;
                    } else if constexpr (MODE == StdMode::VECTOR_NIM) {
                        StdScalar = ToDouble(Std[Iim]);
                        CanSIMD = true;
                    }

                    if (CanSIMD && std::isfinite(StdScalar) && StdScalar != 0.0) {
                        #if defined(__AVX512F__)
                        ComputeChi2AVX512_Double_ScalarStd_NoRadius(
                            reinterpret_cast<const double*>(CubeI),
                            reinterpret_cast<const double*>(ShiftedPSFI),
                            Npix, StdScalar, FluxI, Chi2I);
                        UsedSIMD = true;
                        #elif defined(__AVX2__)
                        ComputeChi2AVX2_Double_ScalarStd_NoRadius(
                            reinterpret_cast<const double*>(CubeI),
                            reinterpret_cast<const double*>(ShiftedPSFI),
                            Npix, StdScalar, FluxI, Chi2I);
                        UsedSIMD = true;
                        #endif
                    }
                }
            }

            if (!UsedSIMD) {
                const T* StdI = (MODE == StdMode::CUBE) ? (Std + Iim * Npix) : Std;
                double StdScalar = 0.0;
                if constexpr (MODE == StdMode::SCALAR) {
                    StdScalar = ToDouble(Std[0]);
                } else if constexpr (MODE == StdMode::VECTOR_NIM) {
                    StdScalar = ToDouble(Std[Iim]);
                }

                ComputeChi2Scalar(
                    CubeI, StdI, ShiftedPSFI, Ny, Nx, DXI, DYI,
                    VecXrel, VecYrel, USE_RADIUS, FitRadius2, MODE, StdScalar,
                    FluxI, Chi2I);
            }
        }

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
    const T* Std,
    const T* ShiftedPSF,
    const T* DX,
    const T* DY,
    const T* VecXrel,
    const T* VecYrel,
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

    const mxClassID ClassID = mxGetClassID(CubeArr);
    if (!(ClassID == mxDOUBLE_CLASS || ClassID == mxSINGLE_CLASS)) {
        mexErrMsgIdAndTxt("psfPhotCube_chi2flux_mex:Class",
                          "Cube must be single or double.");
    }
    if (mxIsComplex(CubeArr)) {
        mexErrMsgIdAndTxt("psfPhotCube_chi2flux_mex:Complex", "Cube must be real.");
    }

    const mxArray* SameClass[] = {
        StdArr, ShiftedPSFArr, DXArr, DYArr, VecXrelArr, VecYrelArr
    };
    for (mwIndex I = 0; I < sizeof(SameClass)/sizeof(SameClass[0]); ++I) {
        if (mxGetClassID(SameClass[I]) != ClassID) {
            mexErrMsgIdAndTxt("psfPhotCube_chi2flux_mex:Class",
                              "All numeric inputs must have the same class as Cube.");
        }
        if (mxIsComplex(SameClass[I])) {
            mexErrMsgIdAndTxt("psfPhotCube_chi2flux_mex:Complex",
                              "All numeric inputs must be real.");
        }
    }

    const mwSize CubeNdim = mxGetNumberOfDimensions(CubeArr);
    const mwSize* CubeDims = mxGetDimensions(CubeArr);
    if (CubeNdim != 3) {
        mexErrMsgIdAndTxt("psfPhotCube_chi2flux_mex:Size", "Cube must be a 3D array.");
    }

    const mwSize Ny  = CubeDims[0];
    const mwSize Nx  = CubeDims[1];
    const mwSize Nim = CubeDims[2];

    ValidateSameCubeSize(ShiftedPSFArr, Ny, Nx, Nim, "ShiftedPSF");
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
        UseFitRadius = true;
    }

    mwSize OutDims[2] = {Nim, 1};
    plhs[0] = mxCreateNumericArray(2, OutDims, ClassID, mxREAL);
    plhs[1] = mxCreateNumericArray(2, OutDims, ClassID, mxREAL);
    plhs[2] = mxCreateNumericArray(2, OutDims, ClassID, mxREAL);
    plhs[3] = mxCreateNumericArray(2, OutDims, ClassID, mxREAL);

    if (ClassID == mxDOUBLE_CLASS) {
        DispatchStdMode<double>(
            ThisStdMode, UseFitRadius,
            static_cast<const double*>(mxGetData(CubeArr)),
            static_cast<const double*>(mxGetData(StdArr)),
            static_cast<const double*>(mxGetData(ShiftedPSFArr)),
            static_cast<const double*>(mxGetData(DXArr)),
            static_cast<const double*>(mxGetData(DYArr)),
            static_cast<const double*>(mxGetData(VecXrelArr)),
            static_cast<const double*>(mxGetData(VecYrelArr)),
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
            static_cast<const float*>(mxGetData(StdArr)),
            static_cast<const float*>(mxGetData(ShiftedPSFArr)),
            static_cast<const float*>(mxGetData(DXArr)),
            static_cast<const float*>(mxGetData(DYArr)),
            static_cast<const float*>(mxGetData(VecXrelArr)),
            static_cast<const float*>(mxGetData(VecYrelArr)),
            Ny, Nx, Nim, FitRadius2,
            static_cast<float*>(mxGetData(plhs[0])),
            static_cast<float*>(mxGetData(plhs[1])),
            static_cast<float*>(mxGetData(plhs[2])),
            static_cast<float*>(mxGetData(plhs[3]))
        );
    }
}
