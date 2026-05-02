// addBrightSourceProfile.cpp

#include "mex.h"
#include <vector>
#include <cmath>
#include <algorithm>

#ifdef _OPENMP
#include <omp.h>
#endif

struct ActiveSource {
    mwSize Index;
    double Dx2;
    int RowMin;
    int RowMax;
};

static bool isSingleOrDouble(const mxArray* A) {
    return mxIsSingle(A) || mxIsDouble(A);
}

static double getValue(const mxArray* A, mwIndex I) {
    if (mxIsDouble(A)) {
        return static_cast<const double*>(mxGetData(A))[I];
    }
    return static_cast<double>(static_cast<const float*>(mxGetData(A))[I]);
}

static std::vector<double> copyToDoubleVector(const mxArray* A, const char* Name) {
    if (!isSingleOrDouble(A) || mxIsComplex(A)) {
        mexErrMsgIdAndTxt("addBrightSourceProfile:InputType",
                          "%s must be real single or double.", Name);
    }

    const mwSize N = mxGetNumberOfElements(A);
    std::vector<double> V(N);

    for (mwIndex i = 0; i < N; ++i) {
        V[i] = getValue(A, i);
    }

    return V;
}

static double getScalarDouble(const mxArray* A, const char* Name) {
    if (!isSingleOrDouble(A) || mxIsComplex(A) ||
        mxGetNumberOfElements(A) != 1) {
        mexErrMsgIdAndTxt("addBrightSourceProfile:ScalarInput",
                          "%s must be a real scalar single or double.", Name);
    }
    return getValue(A, 0);
}

static inline double interpProfileSqrt(
    const double R2,
    const std::vector<double>& Profile,
    const std::vector<double>& ProfileSlope
) {
    const double R = std::sqrt(R2);
    const mwSize I = static_cast<mwSize>(R);

    if (I >= Profile.size() - 1) {
        return Profile.back();
    }

    const double T = R - static_cast<double>(I);
    return Profile[I] + T * ProfileSlope[I];
}

template<typename ImageType>
static void addSourcesBranch(
    ImageType* Out,
    const mwSize Ny,
    const mwSize Nx,
    const std::vector<double>& X,
    const std::vector<double>& Y,
    const std::vector<double>& Flux,
    const std::vector<double>& MaxRadius,
    const std::vector<double>& MaxRadius2,
    const std::vector<double>& Profile,
    const std::vector<double>& ProfileSlope
) {
    const mwSize Ns = X.size();

    for (mwSize s = 0; s < Ns; ++s) {
        const double Xs = X[s];
        const double Ys = Y[s];
        const double Fs = Flux[s];
        const double Rmax = MaxRadius[s];
        const double Rmax2 = MaxRadius2[s];

        if (Rmax <= 0.0 || Fs == 0.0) {
            continue;
        }

        int ColMin = static_cast<int>(std::ceil(Xs - Rmax));
        int ColMax = static_cast<int>(std::floor(Xs + Rmax));

        if (ColMax < 1 || ColMin > static_cast<int>(Nx)) {
            continue;
        }

        ColMin = std::max(ColMin, 1);
        ColMax = std::min(ColMax, static_cast<int>(Nx));

        for (int Col = ColMin; Col <= ColMax; ++Col) {
            const double Dx = static_cast<double>(Col) - Xs;
            const double Dx2 = Dx * Dx;

            if (Dx2 > Rmax2) {
                continue;
            }

            const double DyMax = std::sqrt(Rmax2 - Dx2);

            int RowMin = static_cast<int>(std::ceil(Ys - DyMax));
            int RowMax = static_cast<int>(std::floor(Ys + DyMax));

            if (RowMax < 1 || RowMin > static_cast<int>(Ny)) {
                continue;
            }

            RowMin = std::max(RowMin, 1);
            RowMax = std::min(RowMax, static_cast<int>(Ny));

            ImageType* Ptr =
                Out +
                static_cast<mwIndex>(Col - 1) * Ny +
                static_cast<mwIndex>(RowMin - 1);

            for (int Row = RowMin; Row <= RowMax; ++Row, ++Ptr) {
                const double Dy = static_cast<double>(Row) - Ys;
                const double R2 = Dx2 + Dy * Dy;

                const double R = std::sqrt(R2);
                const mwSize I = static_cast<mwSize>(R);

                double ProfVal;
                if (I >= Profile.size() - 1) {
                    ProfVal = Profile.back();
                } else {
                    ProfVal = Profile[I] + (R - static_cast<double>(I)) * ProfileSlope[I];
                }

                *Ptr = static_cast<ImageType>(
                    static_cast<double>(*Ptr) + Fs * ProfVal
                );
            }
        }
    }
}


template<typename ImageType>
static void addPixelsBranch(
    ImageType* Out,
    const mwSize Ny,
    const mwSize Nx,
    const std::vector<double>& X,
    const std::vector<double>& Y,
    const std::vector<double>& Flux,
    const std::vector<double>& MaxRadius2,
    const std::vector<double>& Profile,
    const std::vector<double>& ProfileSlope
) {
    const mwSize Ns = X.size();

#ifdef _OPENMP
#pragma omp parallel
#endif
    {
        std::vector<ActiveSource> Active;
        Active.reserve(Ns);

#ifdef _OPENMP
#pragma omp for schedule(static)
#endif
        for (mwIndex Col0 = 0; Col0 < static_cast<mwIndex>(Nx); ++Col0) {
            const double Col = static_cast<double>(Col0 + 1);

            Active.clear();

            int RowMinGlobal = static_cast<int>(Ny) + 1;
            int RowMaxGlobal = 0;

            for (mwSize s = 0; s < Ns; ++s) {
                const double Dx = Col - X[s];
                const double Dx2 = Dx * Dx;
                const double Rmax2 = MaxRadius2[s];

                if (Dx2 > Rmax2) {
                    continue;
                }

                const double DyMax = std::sqrt(Rmax2 - Dx2);

                int RowMin = static_cast<int>(std::ceil(Y[s] - DyMax));
                int RowMax = static_cast<int>(std::floor(Y[s] + DyMax));

                if (RowMax < 1 || RowMin > static_cast<int>(Ny)) {
                    continue;
                }

                RowMin = std::max(RowMin, 1);
                RowMax = std::min(RowMax, static_cast<int>(Ny));

                Active.push_back({s, Dx2, RowMin, RowMax});

                if (RowMin < RowMinGlobal) RowMinGlobal = RowMin;
                if (RowMax > RowMaxGlobal) RowMaxGlobal = RowMax;
            }

            if (Active.empty()) {
                continue;
            }

            ImageType* Ptr =
                Out +
                static_cast<mwIndex>(Col0) * Ny +
                static_cast<mwIndex>(RowMinGlobal - 1);

            for (int Row = RowMinGlobal; Row <= RowMaxGlobal; ++Row, ++Ptr) {
                const double RowD = static_cast<double>(Row);
                double Acc = static_cast<double>(*Ptr);

                for (mwSize a = 0; a < Active.size(); ++a) {
                    const ActiveSource& A = Active[a];

                    if (Row < A.RowMin || Row > A.RowMax) {
                        continue;
                    }

                    const mwSize s = A.Index;
                    const double Dy = RowD - Y[s];
                    const double R2 = A.Dx2 + Dy * Dy;

                    if (R2 <= MaxRadius2[s]) {
                        Acc += Flux[s] * interpProfileSqrt(R2, Profile, ProfileSlope);
                    }
                }

                *Ptr = static_cast<ImageType>(Acc);
            }
        }
    }
}

void mexFunction(
    int nlhs,
    mxArray* plhs[],
    int nrhs,
    const mxArray* prhs[]
) {
    if (nrhs < 6 || nrhs > 8) {
        mexErrMsgIdAndTxt("addBrightSourceProfile:NumInputs",
            "Usage: NewImage = addBrightSourceProfile(Image, X, Y, Flux, MaxRadius, RadialProfile, Threshold, InPlace)");
    }

    if (nlhs > 1) {
        mexErrMsgIdAndTxt("addBrightSourceProfile:NumOutputs",
            "Only one output is supported.");
    }

    const mxArray* Image = prhs[0];

    if (!isSingleOrDouble(Image) || mxIsComplex(Image) || mxIsSparse(Image)) {
        mexErrMsgIdAndTxt("addBrightSourceProfile:ImageType",
            "Image must be a real full single or double 2D matrix.");
    }

    if (mxGetNumberOfDimensions(Image) != 2) {
        mexErrMsgIdAndTxt("addBrightSourceProfile:ImageDim",
            "Image must be 2D.");
    }

    const mwSize Ny = mxGetM(Image);
    const mwSize Nx = mxGetN(Image);

    std::vector<double> X0         = copyToDoubleVector(prhs[1], "X");
    std::vector<double> Y0         = copyToDoubleVector(prhs[2], "Y");
    std::vector<double> Flux0      = copyToDoubleVector(prhs[3], "Flux");
    std::vector<double> MaxRadius0 = copyToDoubleVector(prhs[4], "MaxRadius");

    const mwSize Ns0 = X0.size();

    if (Y0.size() != Ns0 ||
        Flux0.size() != Ns0 ||
        MaxRadius0.size() != Ns0) {
        mexErrMsgIdAndTxt("addBrightSourceProfile:SourceSize",
            "X, Y, Flux, and MaxRadius must have the same number of elements.");
    }

    const mxArray* RadialProfile = prhs[5];

    if (!isSingleOrDouble(RadialProfile) || mxIsComplex(RadialProfile)) {
        mexErrMsgIdAndTxt("addBrightSourceProfile:ProfileType",
            "RadialProfile must be a real single or double vector.");
    }

    if (mxGetNumberOfElements(RadialProfile) < 2) {
        mexErrMsgIdAndTxt("addBrightSourceProfile:ProfileLength",
            "RadialProfile must contain at least two elements.");
    }

    std::vector<double> Profile = copyToDoubleVector(RadialProfile, "RadialProfile");

    for (mwIndex i = 0; i < Profile.size(); ++i) {
        if (!std::isfinite(Profile[i])) {
            mexErrMsgIdAndTxt("addBrightSourceProfile:ProfileValues",
                "RadialProfile must contain finite values.");
        }
    }

    std::vector<double> ProfileSlope(Profile.size(), 0.0);
    for (mwSize i = 0; i + 1 < Profile.size(); ++i) {
        ProfileSlope[i] = Profile[i + 1] - Profile[i];
    }

    double Threshold = 0.1;
    if (nrhs >= 7) {
        Threshold = getScalarDouble(prhs[6], "Threshold");

        if (!std::isfinite(Threshold) || Threshold <= 0.0) {
            mexErrMsgIdAndTxt("addBrightSourceProfile:Threshold",
                "Threshold must be positive and finite.");
        }
    }

    bool InPlace = false;
    if (nrhs >= 8) {
        InPlace = mxIsLogicalScalarTrue(prhs[7]);
    }

    (void)InPlace;

    const double ProfileRmax = static_cast<double>(Profile.size() - 1);

    std::vector<double> X;
    std::vector<double> Y;
    std::vector<double> Flux;
    std::vector<double> EffectiveRadius;
    std::vector<double> EffectiveRadius2;

    X.reserve(Ns0);
    Y.reserve(Ns0);
    Flux.reserve(Ns0);
    EffectiveRadius.reserve(Ns0);
    EffectiveRadius2.reserve(Ns0);

    double MaxEffectiveRadius = 0.0;

    for (mwSize s = 0; s < Ns0; ++s) {
        if (!std::isfinite(X0[s]) ||
            !std::isfinite(Y0[s]) ||
            !std::isfinite(Flux0[s]) ||
            !std::isfinite(MaxRadius0[s]) ||
            Flux0[s] == 0.0 ||
            MaxRadius0[s] <= 0.0) {
            continue;
        }

        const double R = std::min(MaxRadius0[s], ProfileRmax);

        if (R <= 0.0) {
            continue;
        }

        X.push_back(X0[s]);
        Y.push_back(Y0[s]);
        Flux.push_back(Flux0[s]);
        EffectiveRadius.push_back(R);
        EffectiveRadius2.push_back(R * R);

        if (R > MaxEffectiveRadius) {
            MaxEffectiveRadius = R;
        }
    }

    plhs[0] = mxDuplicateArray(Image);

    if (X.empty()) {
        return;
    }

    const double ImageSize =
        std::sqrt(static_cast<double>(Nx) * static_cast<double>(Ny));

    const double Ratio =
        ImageSize > 0.0 ? MaxEffectiveRadius / ImageSize : 0.0;

    const bool UseSourceBranch = (Ratio < Threshold);

    if (mxIsDouble(Image)) {
        double* Out = static_cast<double*>(mxGetData(plhs[0]));

        if (UseSourceBranch) {
            addSourcesBranch<double>(
                Out, Ny, Nx, X, Y, Flux,
                EffectiveRadius, EffectiveRadius2,
                Profile, ProfileSlope
            );
        } else {
            addPixelsBranch<double>(
                Out, Ny, Nx, X, Y, Flux,
                EffectiveRadius2,
                Profile, ProfileSlope
            );
        }
    } else {
        float* Out = static_cast<float*>(mxGetData(plhs[0]));

        if (UseSourceBranch) {
            addSourcesBranch<float>(
                Out, Ny, Nx, X, Y, Flux,
                EffectiveRadius, EffectiveRadius2,
                Profile, ProfileSlope
            );
        } else {
            addPixelsBranch<float>(
                Out, Ny, Nx, X, Y, Flux,
                EffectiveRadius2,
                Profile, ProfileSlope
            );
        }
    }
}
