#include "mex.h"
#include <cmath>
#include <algorithm>
#include <limits>

#if defined(_OPENMP)
#include <omp.h>
#endif

template <typename T>
inline double ToDouble(T x) {
    return static_cast<double>(x);
}

void ValidateVectorLength(const mxArray* Arr, mwSize Expected, const char* Name)
{
    if (mxGetNumberOfElements(Arr) != Expected) {
        mexErrMsgIdAndTxt("psfPhotCube_step2d_mex:Size",
                          "%s must contain exactly Nim elements.", Name);
    }
}

template <typename T>
void ComputeSteps(
    const T* F0,
    const T* Fxp,
    const T* Fxm,
    const T* Fyp,
    const T* Fym,
    const T* Fpp,
    const T* Fpm,
    const T* Fmp,
    const T* Fmm,
    const T* sx,
    const T* sy,
    mwSize Nim,
    double MaxStep,
    T* StepX,
    T* StepY)
{
    const double Tiny = std::numeric_limits<double>::epsilon();

    #if defined(_OPENMP)
    #pragma omp parallel for
    #endif
    for (mwIndex Iim = 0; Iim < Nim; ++Iim) {

        const double F0i  = ToDouble(F0[Iim]);
        const double Fxpi = ToDouble(Fxp[Iim]);
        const double Fxmi = ToDouble(Fxm[Iim]);
        const double Fypi = ToDouble(Fyp[Iim]);
        const double Fymi = ToDouble(Fym[Iim]);
        const double Fppi = ToDouble(Fpp[Iim]);
        const double Fpmi = ToDouble(Fpm[Iim]);
        const double Fmpi = ToDouble(Fmp[Iim]);
        const double Fmmi = ToDouble(Fmm[Iim]);
        const double sxi  = ToDouble(sx[Iim]);
        const double syi  = ToDouble(sy[Iim]);

        double OutX = 0.0;
        double OutY = 0.0;

        // If sx or sy are invalid, mimic MATLAB behavior by resulting in non-finite
        // intermediates and then falling back to zeros after cleanup.
        const bool GoodStep = std::isfinite(sxi) && std::isfinite(syi) &&
                              (sxi != 0.0) && (syi != 0.0);

        double Gx, Gy, Hxx, Hyy, Hxy;
        if (GoodStep) {
            const double Inv2sx = 1.0 / (2.0 * sxi);
            const double Inv2sy = 1.0 / (2.0 * syi);
            const double InvSx2 = 1.0 / (sxi * sxi);
            const double InvSy2 = 1.0 / (syi * syi);
            const double Inv4sxsy = 1.0 / (4.0 * sxi * syi);

            Gx  = (Fxpi - Fxmi) * Inv2sx;
            Gy  = (Fypi - Fymi) * Inv2sy;

            Hxx = (Fxpi - 2.0 * F0i + Fxmi) * InvSx2;
            Hyy = (Fypi - 2.0 * F0i + Fymi) * InvSy2;
            Hxy = (Fppi - Fpmi - Fmpi + Fmmi) * Inv4sxsy;
        } else {
            Gx = Gy = Hxx = Hyy = Hxy = std::numeric_limits<double>::quiet_NaN();
        }

        const bool FiniteH = std::isfinite(Hxx) && std::isfinite(Hxy) && std::isfinite(Hyy);
        const bool FiniteG = std::isfinite(Gx)  && std::isfinite(Gy);

        if (FiniteH && FiniteG) {
            const double Det = Hxx * Hyy - Hxy * Hxy;

            // MATLAB:
            // if det(Hmat) > 0 && Hxx > 0 && Hyy > 0
            if (std::isfinite(Det) && (Det > 0.0) && (Hxx > 0.0) && (Hyy > 0.0)) {
                // Step = -Hmat \ Gvec;
                // For H = [Hxx Hxy; Hxy Hyy], inv(H)*G =
                // [ Hyy*Gx - Hxy*Gy ; -Hxy*Gx + Hxx*Gy ] / Det
                OutX = -( Hyy * Gx - Hxy * Gy ) / Det;
                OutY = -( -Hxy * Gx + Hxx * Gy ) / Det;
            } else {
                // MATLAB fallback:
                // Step = zeros(2,1);
                // if isfinite(Hxx) && Hxx ~= 0
                //     Step(1) = -Gx ./ Hxx;
                // end
                // if isfinite(Hyy) && Hyy ~= 0
                //     Step(2) = -Gy ./ Hyy;
                // end
                OutX = 0.0;
                OutY = 0.0;

                if (std::isfinite(Hxx) && (Hxx != 0.0)) {
                    OutX = -Gx / Hxx;
                }
                if (std::isfinite(Hyy) && (Hyy != 0.0)) {
                    OutY = -Gy / Hyy;
                }
            }
        } else {
            OutX = 0.0;
            OutY = 0.0;
        }

        // MATLAB:
        // Step(1) = sign(Step(1)) .* min(abs(Step(1)), MaxStep);
        // Step(2) = sign(Step(2)) .* min(abs(Step(2)), MaxStep);
        if (std::isfinite(OutX)) {
            OutX = ((OutX > 0.0) ? 1.0 : ((OutX < 0.0) ? -1.0 : 0.0)) *
                   std::min(std::abs(OutX), MaxStep);
        } else {
            OutX = 0.0;
        }

        if (std::isfinite(OutY)) {
            OutY = ((OutY > 0.0) ? 1.0 : ((OutY < 0.0) ? -1.0 : 0.0)) *
                   std::min(std::abs(OutY), MaxStep);
        } else {
            OutY = 0.0;
        }

        // MATLAB final cleanup:
        if (!std::isfinite(OutX)) { OutX = 0.0; }
        if (!std::isfinite(OutY)) { OutY = 0.0; }

        StepX[Iim] = static_cast<T>(OutX);
        StepY[Iim] = static_cast<T>(OutY);
    }
}

void mexFunction(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[])
{
    if (nrhs != 12) {
        mexErrMsgIdAndTxt("psfPhotCube_step2d_mex:Input",
            "Expected 12 inputs: F0, Fxp, Fxm, Fyp, Fym, Fpp, Fpm, Fmp, Fmm, sx, sy, MaxStep.");
    }
    if (nlhs != 2) {
        mexErrMsgIdAndTxt("psfPhotCube_step2d_mex:Output",
            "Expected 2 outputs: StepX, StepY.");
    }

    const mxArray* F0Arr  = prhs[0];
    const mxArray* FxpArr = prhs[1];
    const mxArray* FxmArr = prhs[2];
    const mxArray* FypArr = prhs[3];
    const mxArray* FymArr = prhs[4];
    const mxArray* FppArr = prhs[5];
    const mxArray* FpmArr = prhs[6];
    const mxArray* FmpArr = prhs[7];
    const mxArray* FmmArr = prhs[8];
    const mxArray* sxArr  = prhs[9];
    const mxArray* syArr  = prhs[10];
    const mxArray* MaxStepArr = prhs[11];

    const mxClassID ClassID = mxGetClassID(F0Arr);
    if (!(ClassID == mxDOUBLE_CLASS || ClassID == mxSINGLE_CLASS)) {
        mexErrMsgIdAndTxt("psfPhotCube_step2d_mex:Class",
                          "Inputs must be single or double.");
    }

    const mxArray* SameClass[] = {
        FxpArr, FxmArr, FypArr, FymArr, FppArr, FpmArr, FmpArr, FmmArr, sxArr, syArr
    };

    for (mwIndex I = 0; I < sizeof(SameClass)/sizeof(SameClass[0]); ++I) {
        if (mxGetClassID(SameClass[I]) != ClassID) {
            mexErrMsgIdAndTxt("psfPhotCube_step2d_mex:Class",
                              "All numeric inputs must have the same class.");
        }
        if (mxIsComplex(SameClass[I])) {
            mexErrMsgIdAndTxt("psfPhotCube_step2d_mex:Complex",
                              "All numeric inputs must be real.");
        }
    }
    if (mxIsComplex(F0Arr)) {
        mexErrMsgIdAndTxt("psfPhotCube_step2d_mex:Complex",
                          "Inputs must be real.");
    }

    const mwSize Nim = mxGetNumberOfElements(F0Arr);

    ValidateVectorLength(FxpArr, Nim, "Fxp");
    ValidateVectorLength(FxmArr, Nim, "Fxm");
    ValidateVectorLength(FypArr, Nim, "Fyp");
    ValidateVectorLength(FymArr, Nim, "Fym");
    ValidateVectorLength(FppArr, Nim, "Fpp");
    ValidateVectorLength(FpmArr, Nim, "Fpm");
    ValidateVectorLength(FmpArr, Nim, "Fmp");
    ValidateVectorLength(FmmArr, Nim, "Fmm");
    ValidateVectorLength(sxArr,  Nim, "sx");
    ValidateVectorLength(syArr,  Nim, "sy");

    if (mxGetNumberOfElements(MaxStepArr) != 1) {
        mexErrMsgIdAndTxt("psfPhotCube_step2d_mex:Size",
                          "MaxStep must be scalar.");
    }
    const double MaxStep = mxGetScalar(MaxStepArr);

    mwSize OutDims[2] = {1, Nim};
    plhs[0] = mxCreateNumericArray(2, OutDims, ClassID, mxREAL);
    plhs[1] = mxCreateNumericArray(2, OutDims, ClassID, mxREAL);

    if (ClassID == mxDOUBLE_CLASS) {
        ComputeSteps<double>(
            static_cast<const double*>(mxGetData(F0Arr)),
            static_cast<const double*>(mxGetData(FxpArr)),
            static_cast<const double*>(mxGetData(FxmArr)),
            static_cast<const double*>(mxGetData(FypArr)),
            static_cast<const double*>(mxGetData(FymArr)),
            static_cast<const double*>(mxGetData(FppArr)),
            static_cast<const double*>(mxGetData(FpmArr)),
            static_cast<const double*>(mxGetData(FmpArr)),
            static_cast<const double*>(mxGetData(FmmArr)),
            static_cast<const double*>(mxGetData(sxArr)),
            static_cast<const double*>(mxGetData(syArr)),
            Nim,
            MaxStep,
            static_cast<double*>(mxGetData(plhs[0])),
            static_cast<double*>(mxGetData(plhs[1]))
        );
    } else {
        ComputeSteps<float>(
            static_cast<const float*>(mxGetData(F0Arr)),
            static_cast<const float*>(mxGetData(FxpArr)),
            static_cast<const float*>(mxGetData(FxmArr)),
            static_cast<const float*>(mxGetData(FypArr)),
            static_cast<const float*>(mxGetData(FymArr)),
            static_cast<const float*>(mxGetData(FppArr)),
            static_cast<const float*>(mxGetData(FpmArr)),
            static_cast<const float*>(mxGetData(FmpArr)),
            static_cast<const float*>(mxGetData(FmmArr)),
            static_cast<const float*>(mxGetData(sxArr)),
            static_cast<const float*>(mxGetData(syArr)),
            Nim,
            MaxStep,
            static_cast<float*>(mxGetData(plhs[0])),
            static_cast<float*>(mxGetData(plhs[1]))
        );
    }
}
