#include "mex.h"
#include <vector>
#include <algorithm>
#include <cmath>
#include <limits>
#include <cstddef>
#include <cstring>
#include <utility>

#ifdef _OPENMP
#include <omp.h>
#endif

namespace {

constexpr double PI      = 3.141592653589793238462643383279502884;
constexpr double TWO_PI  = 2.0 * PI;
constexpr double HALF_PI = 0.5 * PI;

inline bool IsFinite(double X) {
    return std::isfinite(X);
}

inline double Clamp(double X, double A, double B) {
    return (X < A) ? A : ((X > B) ? B : X);
}

inline double NormalizeRA(double RA) {
    double R = std::fmod(RA, TWO_PI);
    if (R < 0.0) {
        R += TWO_PI;
    }
    return R;
}

inline void NormalizeRaDec(double& RA, double& Dec) {
    while (Dec > HALF_PI || Dec < -HALF_PI) {
        if (Dec > HALF_PI) {
            Dec = PI - Dec;
            RA += PI;
        } else {
            Dec = -PI - Dec;
            RA += PI;
        }
    }
    RA = NormalizeRA(RA);
}

inline double WrapDiffRA(double DRA) {
    if (DRA > PI) {
        DRA -= TWO_PI;
    } else if (DRA < -PI) {
        DRA += TWO_PI;
    }
    return DRA;
}

inline bool IsMaskEmpty(const mxArray* Arr) {
    return (Arr == nullptr || mxIsEmpty(Arr));
}

inline bool GetScalarBoolDefault(const mxArray* Arr, bool DefaultVal) {
    if (Arr == nullptr) {
        return DefaultVal;
    }
    if (mxIsLogical(Arr)) {
        return mxIsLogicalScalarTrue(Arr);
    }
    if (!mxIsDouble(Arr) || mxIsComplex(Arr) || mxGetNumberOfElements(Arr) != 1) {
        mexErrMsgIdAndTxt("matchCatalogs:InvalidScalarBool",
                          "Scalar logical/numeric input expected.");
    }
    return (mxGetScalar(Arr) != 0.0);
}

inline bool GetMaskValue(const mxArray* Mask, mwSize I) {
    if (IsMaskEmpty(Mask)) {
        return true;
    }

    if (mxIsLogical(Mask)) {
        const mxLogical* P = mxGetLogicals(Mask);
        return (P[I] != 0);
    }

    if (mxIsDouble(Mask) && !mxIsComplex(Mask)) {
        const double* P = mxGetPr(Mask);
        return (P[I] != 0.0);
    }

    mexErrMsgIdAndTxt("matchCatalogs:InvalidMask",
                      "Use1/Use2 must be logical, real double, or empty.");
    return false;
}

inline void CheckRealDoubleVector(const mxArray* Arr, const char* Name) {
    if (!mxIsDouble(Arr) || mxIsComplex(Arr)) {
        mexErrMsgIdAndTxt("matchCatalogs:InvalidInput",
                          "%s must be a real double array.", Name);
    }
}

struct CatalogCompact {
    std::vector<double> RA;
    std::vector<double> Dec;
    std::vector<double> SinDec;
    std::vector<double> CosDec;
    std::vector<mwSize> OrigInd;  // original 0-based index
};

CatalogCompact BuildCompactCatalog(const mxArray* RA_Arr,
                                   const mxArray* Dec_Arr,
                                   const mxArray* Use_Arr,
                                   double AngScale,
                                   bool CheckSortedDec,
                                   bool& HasSortedError,
                                   double* Ind,
                                   double* Dist,
                                   double* Nmatch) {
    const mwSize N = mxGetNumberOfElements(RA_Arr);
    const double* RA_P  = mxGetPr(RA_Arr);
    const double* Dec_P = mxGetPr(Dec_Arr);

    CatalogCompact C;
    C.RA.reserve(N);
    C.Dec.reserve(N);
    C.SinDec.reserve(N);
    C.CosDec.reserve(N);
    C.OrigInd.reserve(N);

    const double NaN = mxGetNaN();

    double PrevDec = -std::numeric_limits<double>::infinity();
    HasSortedError = false;

    for (mwSize I = 0; I < N; ++I) {
        if (Ind != nullptr) {
            Ind[I] = NaN;
        }
        if (Dist != nullptr) {
            Dist[I] = NaN;
        }
        if (Nmatch != nullptr) {
            Nmatch[I] = 0.0;
        }

        if (!GetMaskValue(Use_Arr, I)) {
            continue;
        }

        const double RA0  = RA_P[I];
        const double Dec0 = Dec_P[I];

        if (!IsFinite(RA0) || !IsFinite(Dec0)) {
            continue;
        }

        double RA_Rad  = RA0 * AngScale;
        double Dec_Rad = Dec0 * AngScale;

        NormalizeRaDec(RA_Rad, Dec_Rad);

        if (CheckSortedDec) {
            if (Dec_Rad < PrevDec) {
                HasSortedError = true;
            }
            PrevDec = Dec_Rad;
        }

        C.RA.push_back(RA_Rad);
        C.Dec.push_back(Dec_Rad);
        C.SinDec.push_back(std::sin(Dec_Rad));
        C.CosDec.push_back(std::cos(Dec_Rad));
        C.OrigInd.push_back(I);
    }

    return C;
}

struct MatchResult {
    mwSize BestOrigInd;
    double BestCosD;
    mwSize Count;
    bool   Found;
};

inline bool UseRaPrefilter(double CosDecQ, double SinR, double& DeltaRaMax) {
    const double AbsCosDecQ = std::fabs(CosDecQ);
    if (AbsCosDecQ <= 1e-12) {
        return false;
    }

    const double Ratio = SinR / AbsCosDecQ;
    if (Ratio >= 1.0) {
        return false;
    }

    DeltaRaMax = std::asin(Ratio);
    return (DeltaRaMax > 0.0 && DeltaRaMax < PI);
}

MatchResult FindBestInSortedDecSlab(double RA_Q,
                                    double Dec_Q,
                                    double SinDec_Q,
                                    double CosDec_Q,
                                    const CatalogCompact& Target,
                                    double MatchDistRad,
                                    double CosLimit,
                                    double SinR) {
    MatchResult R;
    R.BestOrigInd = 0;
    R.BestCosD    = -2.0;
    R.Count       = 0;
    R.Found       = false;

    const double DecMin = std::max(Dec_Q - MatchDistRad, -HALF_PI);
    const double DecMax = std::min(Dec_Q + MatchDistRad,  HALF_PI);

    auto ItLo = std::lower_bound(Target.Dec.begin(), Target.Dec.end(), DecMin);
    auto ItHi = std::upper_bound(Target.Dec.begin(), Target.Dec.end(), DecMax);

    const std::size_t ILo = static_cast<std::size_t>(ItLo - Target.Dec.begin());
    const std::size_t IHi = static_cast<std::size_t>(ItHi - Target.Dec.begin());

    bool DoRaPrefilter = false;
    double DeltaRaMax = 0.0;
    DoRaPrefilter = UseRaPrefilter(CosDec_Q, SinR, DeltaRaMax);

    for (std::size_t K = ILo; K < IHi; ++K) {
        const double DRA = WrapDiffRA(Target.RA[K] - RA_Q);
        if (DoRaPrefilter && std::fabs(DRA) > DeltaRaMax) {
            continue;
        }

        const double CosD =
            SinDec_Q * Target.SinDec[K] +
            CosDec_Q * Target.CosDec[K] * std::cos(DRA);

        if (CosD >= CosLimit) {
            ++R.Count;
            if (!R.Found || CosD > R.BestCosD ||
                (CosD == R.BestCosD && Target.OrigInd[K] < R.BestOrigInd)) {
                R.Found = true;
                R.BestCosD = CosD;
                R.BestOrigInd = Target.OrigInd[K];
            }
        }
    }

    return R;
}

inline double DistFromCosine(double CosD) {
    return std::acos(Clamp(CosD, -1.0, 1.0));
}

inline bool ShouldUseOpenMP(std::size_t M1, std::size_t M2, bool NeedReverse) {
#ifdef _OPENMP
    if (M1 < 512 || M2 < 128) {
        return false;
    }
    if (NeedReverse) {
        return (M1 + M2 >= 1024);
    }
    return (M1 >= 256);
#else
    (void)M1; (void)M2; (void)NeedReverse;
    return false;
#endif
}

} // anonymous namespace


void mexFunction(int Nlhs, mxArray* Plhs[], int Nrhs, const mxArray* Prhs[]) {
    if (Nrhs < 5 || Nrhs > 9) {
        mexErrMsgIdAndTxt("matchCatalogs:InvalidNumInputs",
                          "Expected 5 to 9 input arguments.");
    }
    if (Nlhs < 1 || Nlhs > 6) {
        mexErrMsgIdAndTxt("matchCatalogs:InvalidNumOutputs",
                          "Number of outputs must be between 1 and 6.");
    }

    const mxArray* RA1_Arr = Prhs[0];
    const mxArray* Dec1_Arr = Prhs[1];
    const mxArray* RA2_Arr = Prhs[2];
    const mxArray* Dec2_Arr = Prhs[3];
    const mxArray* MatchDist_Arr = Prhs[4];

    const mxArray* IsDeg_Arr        = (Nrhs >= 6) ? Prhs[5] : nullptr;
    const mxArray* Use1_Arr         = (Nrhs >= 7) ? Prhs[6] : nullptr;
    const mxArray* Use2_Arr         = (Nrhs >= 8) ? Prhs[7] : nullptr;
    const mxArray* CheckSorted2_Arr = (Nrhs >= 9) ? Prhs[8] : nullptr;

    CheckRealDoubleVector(RA1_Arr, "RA1");
    CheckRealDoubleVector(Dec1_Arr, "Dec1");
    CheckRealDoubleVector(RA2_Arr, "RA2");
    CheckRealDoubleVector(Dec2_Arr, "Dec2");

    if (!mxIsDouble(MatchDist_Arr) || mxIsComplex(MatchDist_Arr) ||
        mxGetNumberOfElements(MatchDist_Arr) != 1) {
        mexErrMsgIdAndTxt("matchCatalogs:InvalidMatchDist",
                          "MatchDist must be a real double scalar.");
    }

    const mwSize N1 = mxGetNumberOfElements(RA1_Arr);
    const mwSize N2 = mxGetNumberOfElements(RA2_Arr);

    if (mxGetNumberOfElements(Dec1_Arr) != N1) {
        mexErrMsgIdAndTxt("matchCatalogs:SizeMismatch",
                          "RA1 and Dec1 must have the same number of elements.");
    }
    if (mxGetNumberOfElements(Dec2_Arr) != N2) {
        mexErrMsgIdAndTxt("matchCatalogs:SizeMismatch",
                          "RA2 and Dec2 must have the same number of elements.");
    }

    if (!IsMaskEmpty(Use1_Arr) && mxGetNumberOfElements(Use1_Arr) != N1) {
        mexErrMsgIdAndTxt("matchCatalogs:SizeMismatch",
                          "Use1 must have the same number of elements as RA1/Dec1, or be empty.");
    }
    if (!IsMaskEmpty(Use2_Arr) && mxGetNumberOfElements(Use2_Arr) != N2) {
        mexErrMsgIdAndTxt("matchCatalogs:SizeMismatch",
                          "Use2 must have the same number of elements as RA2/Dec2, or be empty.");
    }

    const bool IsDeg = GetScalarBoolDefault(IsDeg_Arr, true);
    const bool CheckSorted2 = GetScalarBoolDefault(CheckSorted2_Arr, false);
    const double MatchDistIn = mxGetScalar(MatchDist_Arr);

    if (!IsFinite(MatchDistIn) || MatchDistIn < 0.0) {
        mexErrMsgIdAndTxt("matchCatalogs:InvalidMatchDist",
                          "MatchDist must be finite and >= 0.");
    }

    const double AngScale = IsDeg ? (PI / 180.0) : 1.0;
    const double MatchDistRad = std::min(MatchDistIn * AngScale, PI);
    const double CosLimit = std::cos(MatchDistRad);
    const double SinR = std::sin(MatchDistRad);

    // Output order:
    // 1: Ind1
    // 2: Dist1
    // 3: Nmatch1
    // 4: Ind2
    // 5: Dist2
    // 6: Nmatch2

    const bool NeedDist1   = (Nlhs >= 2);
    const bool NeedNmatch1 = (Nlhs >= 3);
    const bool NeedReverse = (Nlhs >= 4);
    const bool NeedDist2   = (Nlhs >= 5);
    const bool NeedNmatch2 = (Nlhs >= 6);

    // Allocate outputs now, but do not write to them in parallel.
    Plhs[0] = mxCreateDoubleMatrix(N1, 1, mxREAL); // Ind1
    double* Ind1 = mxGetPr(Plhs[0]);

    double* Dist1 = nullptr;
    double* Nmatch1 = nullptr;
    double* Ind2 = nullptr;
    double* Dist2 = nullptr;
    double* Nmatch2 = nullptr;

    if (NeedDist1) {
        Plhs[1] = mxCreateDoubleMatrix(N1, 1, mxREAL);
        Dist1 = mxGetPr(Plhs[1]);
    }
    if (NeedNmatch1) {
        Plhs[2] = mxCreateDoubleMatrix(N1, 1, mxREAL);
        Nmatch1 = mxGetPr(Plhs[2]);
    }
    if (NeedReverse) {
        Plhs[3] = mxCreateDoubleMatrix(N2, 1, mxREAL);
        Ind2 = mxGetPr(Plhs[3]);
    }
    if (NeedDist2) {
        Plhs[4] = mxCreateDoubleMatrix(N2, 1, mxREAL);
        Dist2 = mxGetPr(Plhs[4]);
    }
    if (NeedNmatch2) {
        Plhs[5] = mxCreateDoubleMatrix(N2, 1, mxREAL);
        Nmatch2 = mxGetPr(Plhs[5]);
    }

    // Temporary output buffers
    const double NaN = mxGetNaN();

    std::vector<double> TmpInd1(N1, NaN);
    std::vector<double> TmpDist1;
    std::vector<double> TmpNmatch1;
    if (NeedDist1)   TmpDist1.assign(N1, NaN);
    if (NeedNmatch1) TmpNmatch1.assign(N1, 0.0);

    std::vector<double> TmpInd2;
    std::vector<double> TmpDist2;
    std::vector<double> TmpNmatch2;
    if (NeedReverse) TmpInd2.assign(N2, NaN);
    if (NeedDist2)   TmpDist2.assign(N2, NaN);
    if (NeedNmatch2) TmpNmatch2.assign(N2, 0.0);

    bool HasSortedError2 = false;
    bool DummySortedError = false;

    CatalogCompact C1 = BuildCompactCatalog(RA1_Arr, Dec1_Arr, Use1_Arr,
                                            AngScale, false, DummySortedError,
                                            TmpInd1.data(),
                                            NeedDist1   ? TmpDist1.data()   : nullptr,
                                            NeedNmatch1 ? TmpNmatch1.data() : nullptr);

    CatalogCompact C2 = BuildCompactCatalog(RA2_Arr, Dec2_Arr, Use2_Arr,
                                            AngScale, CheckSorted2, HasSortedError2,
                                            NeedReverse ? TmpInd2.data() : nullptr,
                                            NeedDist2   ? TmpDist2.data()   : nullptr,
                                            NeedNmatch2 ? TmpNmatch2.data() : nullptr);

    if (CheckSorted2 && HasSortedError2) {
        mexErrMsgIdAndTxt("matchCatalogs:Dec2NotSorted",
                          "Valid/used Dec2 entries must be sorted in ascending order after declination correction.");
    }

    const std::size_t M1 = C1.Dec.size();
    const std::size_t M2 = C2.Dec.size();
    const bool UseOMP = ShouldUseOpenMP(M1, M2, NeedReverse);

    // Direction 1: Catalog 1 -> Catalog 2 (binary search in sorted Dec2)
#ifdef _OPENMP
#pragma omp parallel for if(UseOMP) schedule(guided, 32)
#endif
    for (long long Kll = 0; Kll < static_cast<long long>(M1); ++Kll) {
        const std::size_t K = static_cast<std::size_t>(Kll);
        const mwSize I1 = C1.OrigInd[K];

        MatchResult R = FindBestInSortedDecSlab(
            C1.RA[K],
            C1.Dec[K],
            C1.SinDec[K],
            C1.CosDec[K],
            C2,
            MatchDistRad,
            CosLimit,
            SinR
        );

        if (NeedNmatch1) {
            TmpNmatch1[I1] = static_cast<double>(R.Count);
        }

        if (R.Found) {
            TmpInd1[I1] = static_cast<double>(R.BestOrigInd + 1);
            if (NeedDist1) {
                TmpDist1[I1] = DistFromCosine(R.BestCosD) / AngScale;
            }
        }
    }

    // Direction 2: Catalog 2 -> Catalog 1 (also slab search now)
    if (NeedReverse) {
#ifdef _OPENMP
#pragma omp parallel for if(UseOMP) schedule(guided, 32)
#endif
        for (long long Kll = 0; Kll < static_cast<long long>(M2); ++Kll) {
            const std::size_t K = static_cast<std::size_t>(Kll);
            const mwSize I2 = C2.OrigInd[K];

            MatchResult R = FindBestInSortedDecSlab(
                C2.RA[K],
                C2.Dec[K],
                C2.SinDec[K],
                C2.CosDec[K],
                C1,
                MatchDistRad,
                CosLimit,
                SinR
            );

            if (NeedNmatch2) {
                TmpNmatch2[I2] = static_cast<double>(R.Count);
            }

            if (R.Found) {
                TmpInd2[I2] = static_cast<double>(R.BestOrigInd + 1);
                if (NeedDist2) {
                    TmpDist2[I2] = DistFromCosine(R.BestCosD) / AngScale;
                }
            }
        }
    }

    // Copy serially into MATLAB outputs
    std::memcpy(Ind1, TmpInd1.data(), N1 * sizeof(double));
    if (NeedDist1) {
        std::memcpy(Dist1, TmpDist1.data(), N1 * sizeof(double));
    }
    if (NeedNmatch1) {
        std::memcpy(Nmatch1, TmpNmatch1.data(), N1 * sizeof(double));
    }

    if (NeedReverse) {
        std::memcpy(Ind2, TmpInd2.data(), N2 * sizeof(double));
    }
    if (NeedDist2) {
        std::memcpy(Dist2, TmpDist2.data(), N2 * sizeof(double));
    }
    if (NeedNmatch2) {
        std::memcpy(Nmatch2, TmpNmatch2.data(), N2 * sizeof(double));
    }
}
