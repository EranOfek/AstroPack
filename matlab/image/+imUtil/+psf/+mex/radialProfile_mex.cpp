#include "mex.h"
#include <vector>
#include <cmath>
#include <limits>
#include <algorithm>
#include <cstdint>
#include <cstring>

#ifdef _OPENMP
#include <omp.h>
#endif

struct Params {
    double X0;
    double Y0;
    double MaxR;
    double Step;
    bool IgnoreNaN;
};

template <typename Tin>
inline bool IsNaNValue(Tin x)
{
    return std::isnan(static_cast<double>(x));
}

template <typename Tin, typename Tgeom, bool IGNORE_NAN>
void RunTyped(const mxArray* Cube,
              const Params& P,
              const int nlhs,
              mxArray*& OutR,
              mxArray*& OutMean,
              mxArray*& OutStd,
              mxArray*& OutMin,
              mxArray*& OutMax)
{
    const mwSize Nd = mxGetNumberOfDimensions(Cube);
    const mwSize* Sz = mxGetDimensions(Cube);

    const mwSize NI = Sz[0];
    const mwSize NJ = Sz[1];
    const mwSize NK = (Nd >= 3) ? Sz[2] : 1;
    const mwSize PageSize = NI * NJ;

    if (P.Step <= 0.0) {
        mexErrMsgIdAndTxt("radialProfile_mex:Step", "Step must be positive.");
    }
    if (P.MaxR <= 0.0) {
        mexErrMsgIdAndTxt("radialProfile_mex:MaxR", "MaxR must be positive.");
    }

    const bool NeedR    = (nlhs >= 1);
    const bool NeedMean = (nlhs >= 2);
    const bool NeedStd  = (nlhs >= 3);
    const bool NeedMin  = (nlhs >= 4);
    const bool NeedMax  = (nlhs >= 5);

    const Tin* Data = static_cast<const Tin*>(mxGetData(Cube));

    const Tgeom X0g   = static_cast<Tgeom>(P.X0);
    const Tgeom Y0g   = static_cast<Tgeom>(P.Y0);
    const Tgeom MaxRg = static_cast<Tgeom>(P.MaxR);
    const Tgeom Stepg = static_cast<Tgeom>(P.Step);

    const double MaxRd = static_cast<double>(MaxRg);
    const double Stepd = static_cast<double>(Stepg);

    const bool StepIsOne = (Stepd == 1.0);
    const Tgeom InvStepg = StepIsOne ? static_cast<Tgeom>(1) : static_cast<Tgeom>(1.0 / Stepd);

    const mwSize Nbin = static_cast<mwSize>(std::floor(MaxRd / Stepd + 0.5));
    if (Nbin == 0) {
        mexErrMsgIdAndTxt("radialProfile_mex:NoBins",
                          "No bins available for the requested MaxR and Step.");
    }

    if (NeedR) {
        OutR = mxCreateDoubleMatrix(Nbin, 1, mxREAL);
        double* Rptr = mxGetPr(OutR);
        for (mwSize K = 0; K < Nbin; ++K) {
            Rptr[K] = (static_cast<double>(K) + 0.5) * Stepd;
        }
    } else {
        OutR = nullptr;
    }

    if (!NeedMean && !NeedStd && !NeedMin && !NeedMax) {
        return;
    }

    OutMean = NeedMean ? mxCreateNumericMatrix(Nbin, NK, mxGetClassID(Cube), mxREAL) : nullptr;
    OutStd  = NeedStd  ? mxCreateNumericMatrix(Nbin, NK, mxGetClassID(Cube), mxREAL) : nullptr;
    OutMin  = NeedMin  ? mxCreateNumericMatrix(Nbin, NK, mxGetClassID(Cube), mxREAL) : nullptr;
    OutMax  = NeedMax  ? mxCreateNumericMatrix(Nbin, NK, mxGetClassID(Cube), mxREAL) : nullptr;

    Tin* MeanPtr = NeedMean ? static_cast<Tin*>(mxGetData(OutMean)) : nullptr;
    Tin* StdPtr  = NeedStd  ? static_cast<Tin*>(mxGetData(OutStd))  : nullptr;
    Tin* MinPtr  = NeedMin  ? static_cast<Tin*>(mxGetData(OutMin))  : nullptr;
    Tin* MaxPtr  = NeedMax  ? static_cast<Tin*>(mxGetData(OutMax))  : nullptr;

    const Tgeom MaxR2g = MaxRg * MaxRg;

    mwSignedIndex Jmin = static_cast<mwSignedIndex>(std::ceil(static_cast<double>(X0g - MaxRg - static_cast<Tgeom>(1))));
    mwSignedIndex Jmax = static_cast<mwSignedIndex>(std::floor(static_cast<double>(X0g + MaxRg - static_cast<Tgeom>(1))));
    mwSignedIndex Imin = static_cast<mwSignedIndex>(std::ceil(static_cast<double>(Y0g - MaxRg - static_cast<Tgeom>(1))));
    mwSignedIndex Imax = static_cast<mwSignedIndex>(std::floor(static_cast<double>(Y0g + MaxRg - static_cast<Tgeom>(1))));

    Jmin = std::max<mwSignedIndex>(0, Jmin);
    Imin = std::max<mwSignedIndex>(0, Imin);
    Jmax = std::min<mwSignedIndex>(static_cast<mwSignedIndex>(NJ) - 1, Jmax);
    Imax = std::min<mwSignedIndex>(static_cast<mwSignedIndex>(NI) - 1, Imax);

    if (Jmin > Jmax || Imin > Imax) {
        const Tin NaNVal = static_cast<Tin>(mxGetNaN());
        const mwSize Tot = Nbin * NK;
        if (NeedMean) { for (mwSize i = 0; i < Tot; ++i) MeanPtr[i] = NaNVal; }
        if (NeedStd)  { for (mwSize i = 0; i < Tot; ++i) StdPtr[i]  = NaNVal; }
        if (NeedMin)  { for (mwSize i = 0; i < Tot; ++i) MinPtr[i]  = NaNVal; }
        if (NeedMax)  { for (mwSize i = 0; i < Tot; ++i) MaxPtr[i]  = NaNVal; }
        return;
    }

    const mwSize NJv = static_cast<mwSize>(Jmax - Jmin + 1);
    const mwSize NIv = static_cast<mwSize>(Imax - Imin + 1);

    std::vector<Tgeom> DX2(NJv);
    std::vector<Tgeom> DY2(NIv);

    for (mwSize jj = 0; jj < NJv; ++jj) {
        const mwSize J = static_cast<mwSize>(Jmin) + jj;
        const Tgeom DX = static_cast<Tgeom>(J + 1) - X0g;
        DX2[jj] = DX * DX;
    }
    for (mwSize ii = 0; ii < NIv; ++ii) {
        const mwSize I = static_cast<mwSize>(Imin) + ii;
        const Tgeom DY = static_cast<Tgeom>(I + 1) - Y0g;
        DY2[ii] = DY * DY;
    }

    int Nthreads = 1;
#ifdef _OPENMP
    Nthreads = omp_get_max_threads();
#endif

    const bool ParallelOverSlices = (NK > 1 && static_cast<int>(NK) >= Nthreads);

    if (ParallelOverSlices) {

        #pragma omp parallel for if(NK > 1)
        for (mwSignedIndex Kimg = 0; Kimg < static_cast<mwSignedIndex>(NK); ++Kimg) {

            const Tin* Slice = Data + static_cast<mwSize>(Kimg) * PageSize;

            std::vector<double>   Sum(NeedMean || NeedStd ? Nbin : 0, 0.0);
            std::vector<double>   Sum2(NeedStd ? Nbin : 0, 0.0);
            std::vector<double>   MinV(NeedMin ? Nbin : 0,  std::numeric_limits<double>::infinity());
            std::vector<double>   MaxV(NeedMax ? Nbin : 0, -std::numeric_limits<double>::infinity());
            std::vector<uint32_t> Count((NeedMean || NeedStd || NeedMin || NeedMax) ? Nbin : 0, 0u);

            for (mwSize jj = 0; jj < NJv; ++jj) {
                const Tgeom DX2j = DX2[jj];
                const mwSize J = static_cast<mwSize>(Jmin) + jj;
                const mwSize ColOffset = J * NI;

                for (mwSize ii = 0; ii < NIv; ++ii) {
                    const Tgeom R2 = DX2j + DY2[ii];
                    if (R2 >= MaxR2g) {
                        continue;
                    }

                    mwSize Kbin;
                    if (StepIsOne) {
                        Kbin = static_cast<mwSize>(std::sqrt(R2));
                    } else {
                        Kbin = static_cast<mwSize>(std::sqrt(R2) * InvStepg);
                    }

                    if (Kbin >= Nbin) {
                        continue;
                    }

                    const mwSize I = static_cast<mwSize>(Imin) + ii;
                    const Tin Vraw = Slice[ColOffset + I];

                    if constexpr (IGNORE_NAN) {
                        if (IsNaNValue(Vraw)) {
                            continue;
                        }
                    }

                    const double V = static_cast<double>(Vraw);

                    Count[Kbin] += 1u;
                    if (NeedMean || NeedStd) { Sum[Kbin] += V; }
                    if (NeedStd)             { Sum2[Kbin] += V * V; }
                    if (NeedMin)             { MinV[Kbin] = std::min(MinV[Kbin], V); }
                    if (NeedMax)             { MaxV[Kbin] = std::max(MaxV[Kbin], V); }
                }
            }

            const mwSize BaseOut = static_cast<mwSize>(Kimg) * Nbin;
            const Tin NaNVal = static_cast<Tin>(mxGetNaN());

            for (mwSize Kbin = 0; Kbin < Nbin; ++Kbin) {
                const uint32_t N = Count[Kbin];
                const mwSize OutIdx = BaseOut + Kbin;

                if (N == 0u) {
                    if (NeedMean) MeanPtr[OutIdx] = NaNVal;
                    if (NeedStd)  StdPtr[OutIdx]  = NaNVal;
                    if (NeedMin)  MinPtr[OutIdx]  = NaNVal;
                    if (NeedMax)  MaxPtr[OutIdx]  = NaNVal;
                    continue;
                }

                double Mean = 0.0;
                if (NeedMean || NeedStd) {
                    Mean = Sum[Kbin] / static_cast<double>(N);
                }

                if (NeedMean) {
                    MeanPtr[OutIdx] = static_cast<Tin>(Mean);
                }

                if (NeedStd) {
                    double Std = mxGetNaN();
                    if (N >= 2u) {
                        double Var = (Sum2[Kbin] - (Sum[Kbin] * Sum[Kbin]) / static_cast<double>(N)) /
                                     static_cast<double>(N - 1u);
                        if (Var < 0.0) {
                            Var = 0.0;
                        }
                        Std = std::sqrt(Var);
                    }
                    StdPtr[OutIdx] = static_cast<Tin>(Std);
                }

                if (NeedMin) {
                    MinPtr[OutIdx] = static_cast<Tin>(MinV[Kbin]);
                }
                if (NeedMax) {
                    MaxPtr[OutIdx] = static_cast<Tin>(MaxV[Kbin]);
                }
            }
        }

    } else {

        const mwSize Tot = Nbin * NK;

        std::vector<double>   GSum(NeedMean || NeedStd ? Tot : 0, 0.0);
        std::vector<double>   GSum2(NeedStd ? Tot : 0, 0.0);
        std::vector<double>   GMin(NeedMin ? Tot : 0,  std::numeric_limits<double>::infinity());
        std::vector<double>   GMax(NeedMax ? Tot : 0, -std::numeric_limits<double>::infinity());
        std::vector<uint32_t> GCount((NeedMean || NeedStd || NeedMin || NeedMax) ? Tot : 0, 0u);

        #pragma omp parallel
        {
            std::vector<double>   LSum(NeedMean || NeedStd ? Tot : 0, 0.0);
            std::vector<double>   LSum2(NeedStd ? Tot : 0, 0.0);
            std::vector<double>   LMin(NeedMin ? Tot : 0,  std::numeric_limits<double>::infinity());
            std::vector<double>   LMax(NeedMax ? Tot : 0, -std::numeric_limits<double>::infinity());
            std::vector<uint32_t> LCount((NeedMean || NeedStd || NeedMin || NeedMax) ? Tot : 0, 0u);

            #pragma omp for schedule(static)
            for (mwSignedIndex jjSigned = 0; jjSigned < static_cast<mwSignedIndex>(NJv); ++jjSigned) {
                const mwSize jj = static_cast<mwSize>(jjSigned);
                const Tgeom DX2j = DX2[jj];
                const mwSize J = static_cast<mwSize>(Jmin) + jj;
                const mwSize ColOffset = J * NI;

                for (mwSize ii = 0; ii < NIv; ++ii) {
                    const Tgeom R2 = DX2j + DY2[ii];
                    if (R2 >= MaxR2g) {
                        continue;
                    }

                    mwSize Kbin;
                    if (StepIsOne) {
                        Kbin = static_cast<mwSize>(std::sqrt(R2));
                    } else {
                        Kbin = static_cast<mwSize>(std::sqrt(R2) * InvStepg);
                    }

                    if (Kbin >= Nbin) {
                        continue;
                    }

                    const mwSize I = static_cast<mwSize>(Imin) + ii;
                    const mwSize PixIdx = ColOffset + I;

                    for (mwSize Kimg = 0; Kimg < NK; ++Kimg) {
                        const mwSize OutIdx = Kbin + Kimg * Nbin;
                        const Tin Vraw = Data[PixIdx + Kimg * PageSize];

                        if constexpr (IGNORE_NAN) {
                            if (IsNaNValue(Vraw)) {
                                continue;
                            }
                        }

                        const double V = static_cast<double>(Vraw);

                        LCount[OutIdx] += 1u;
                        if (NeedMean || NeedStd) { LSum[OutIdx] += V; }
                        if (NeedStd)             { LSum2[OutIdx] += V * V; }
                        if (NeedMin)             { LMin[OutIdx] = std::min(LMin[OutIdx], V); }
                        if (NeedMax)             { LMax[OutIdx] = std::max(LMax[OutIdx], V); }
                    }
                }
            }

            #pragma omp critical
            {
                for (mwSize Idx = 0; Idx < Tot; ++Idx) {
                    GCount[Idx] += LCount[Idx];
                    if (NeedMean || NeedStd) { GSum[Idx] += LSum[Idx]; }
                    if (NeedStd)             { GSum2[Idx] += LSum2[Idx]; }
                    if (NeedMin && LCount[Idx] > 0u) { GMin[Idx] = std::min(GMin[Idx], LMin[Idx]); }
                    if (NeedMax && LCount[Idx] > 0u) { GMax[Idx] = std::max(GMax[Idx], LMax[Idx]); }
                }
            }
        }

        const Tin NaNVal = static_cast<Tin>(mxGetNaN());

        for (mwSize Idx = 0; Idx < Tot; ++Idx) {
            const uint32_t N = GCount[Idx];

            if (N == 0u) {
                if (NeedMean) MeanPtr[Idx] = NaNVal;
                if (NeedStd)  StdPtr[Idx]  = NaNVal;
                if (NeedMin)  MinPtr[Idx]  = NaNVal;
                if (NeedMax)  MaxPtr[Idx]  = NaNVal;
                continue;
            }

            double Mean = 0.0;
            if (NeedMean || NeedStd) {
                Mean = GSum[Idx] / static_cast<double>(N);
            }

            if (NeedMean) {
                MeanPtr[Idx] = static_cast<Tin>(Mean);
            }

            if (NeedStd) {
                double Std = mxGetNaN();
                if (N >= 2u) {
                    double Var = (GSum2[Idx] - (GSum[Idx] * GSum[Idx]) / static_cast<double>(N)) /
                                 static_cast<double>(N - 1u);
                    if (Var < 0.0) {
                        Var = 0.0;
                    }
                    Std = std::sqrt(Var);
                }
                StdPtr[Idx] = static_cast<Tin>(Std);
            }

            if (NeedMin) {
                MinPtr[Idx] = static_cast<Tin>(GMin[Idx]);
            }
            if (NeedMax) {
                MaxPtr[Idx] = static_cast<Tin>(GMax[Idx]);
            }
        }
    }
}

void mexFunction(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[])
{
    if (nrhs < 1 || nrhs > 6) {
        mexErrMsgIdAndTxt("radialProfile_mex:NumInputs",
                          "Usage: [R,Mean,Std,Min,Max]=radialProfile_mex(Cube,X0,Y0,MaxR,Step,IgnoreNaN)");
    }
    if (nlhs > 5) {
        mexErrMsgIdAndTxt("radialProfile_mex:NumOutputs",
                          "Too many output arguments.");
    }

    const mxArray* Cube = prhs[0];

    if (!mxIsSingle(Cube) && !mxIsDouble(Cube)) {
        mexErrMsgIdAndTxt("radialProfile_mex:Type",
                          "Cube must be single or double.");
    }
    if (mxIsComplex(Cube)) {
        mexErrMsgIdAndTxt("radialProfile_mex:Complex",
                          "Complex input is not supported.");
    }
    if (mxIsSparse(Cube)) {
        mexErrMsgIdAndTxt("radialProfile_mex:Sparse",
                          "Sparse input is not supported.");
    }

    const mwSize Nd = mxGetNumberOfDimensions(Cube);
    if (Nd != 2 && Nd != 3) {
        mexErrMsgIdAndTxt("radialProfile_mex:Dim",
                          "Cube must be a 2-D image or a 3-D cube.");
    }

    const mwSize* Sz = mxGetDimensions(Cube);

    Params P;
    P.X0        = (static_cast<double>(Sz[1]) + 1.0) * 0.5;
    P.Y0        = (static_cast<double>(Sz[0]) + 1.0) * 0.5;
    P.MaxR      = 100.0;
    P.Step      = 1.0;
    P.IgnoreNaN = false;

    if (nrhs >= 2) {
        if (!mxIsNumeric(prhs[1]) || mxIsComplex(prhs[1]) || mxGetNumberOfElements(prhs[1]) != 1) {
            mexErrMsgIdAndTxt("radialProfile_mex:X0", "X0 must be a real scalar.");
        }
        P.X0 = mxGetScalar(prhs[1]);
    }
    if (nrhs >= 3) {
        if (!mxIsNumeric(prhs[2]) || mxIsComplex(prhs[2]) || mxGetNumberOfElements(prhs[2]) != 1) {
            mexErrMsgIdAndTxt("radialProfile_mex:Y0", "Y0 must be a real scalar.");
        }
        P.Y0 = mxGetScalar(prhs[2]);
    }
    if (nrhs >= 4) {
        if (!mxIsNumeric(prhs[3]) || mxIsComplex(prhs[3]) || mxGetNumberOfElements(prhs[3]) != 1) {
            mexErrMsgIdAndTxt("radialProfile_mex:MaxR", "MaxR must be a real scalar.");
        }
        P.MaxR = mxGetScalar(prhs[3]);
    }
    if (nrhs >= 5) {
        if (!mxIsNumeric(prhs[4]) || mxIsComplex(prhs[4]) || mxGetNumberOfElements(prhs[4]) != 1) {
            mexErrMsgIdAndTxt("radialProfile_mex:Step", "Step must be a real scalar.");
        }
        P.Step = mxGetScalar(prhs[4]);
    }
    if (nrhs >= 6) {
        if (!mxIsNumeric(prhs[5]) || mxIsComplex(prhs[5]) || mxGetNumberOfElements(prhs[5]) != 1) {
            mexErrMsgIdAndTxt("radialProfile_mex:IgnoreNaN", "IgnoreNaN must be a real scalar.");
        }
        P.IgnoreNaN = (mxGetScalar(prhs[5]) != 0.0);
    }

    plhs[0] = nullptr;
    if (nlhs >= 2) plhs[1] = nullptr;
    if (nlhs >= 3) plhs[2] = nullptr;
    if (nlhs >= 4) plhs[3] = nullptr;
    if (nlhs >= 5) plhs[4] = nullptr;

    if (mxGetClassID(Cube) == mxDOUBLE_CLASS) {
        if (P.IgnoreNaN) {
            RunTyped<double,double,true >(Cube, P, nlhs, plhs[0], plhs[1], plhs[2], plhs[3], plhs[4]);
        } else {
            RunTyped<double,double,false>(Cube, P, nlhs, plhs[0], plhs[1], plhs[2], plhs[3], plhs[4]);
        }
    } else {
        if (P.IgnoreNaN) {
            RunTyped<float,float,true >(Cube, P, nlhs, plhs[0], plhs[1], plhs[2], plhs[3], plhs[4]);
        } else {
            RunTyped<float,float,false>(Cube, P, nlhs, plhs[0], plhs[1], plhs[2], plhs[3], plhs[4]);
        }
    }
}
