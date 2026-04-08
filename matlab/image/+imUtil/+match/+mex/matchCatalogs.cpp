#include "mex.h"
#include <vector>
#include <algorithm>
#include <cmath>
#include <limits>
#include <cstddef>

#ifdef _OPENMP
#include <omp.h>
#endif

// matchCatalogs.cpp
//
// [Ind1,Dist1,Nmatch1,Ind2,Dist2,Nmatch2] = matchCatalogs( ...
//      RA1, Dec1, RA2, Dec2, MatchDist, IsDeg, Use1, Use2, CheckIsSorted2, PoleDecThresh)
//
// Notes:
// - Catalog 2 is assumed sorted by Dec ascending.
// - Matching is non-symmetric.
// - Outputs refer to original indices (1-based in MATLAB).
// - NaNs in RA/Dec are ignored.
// - Use1 / Use2 are logical or numeric masks.
// - If Use1 or Use2 are empty ([]), they are treated as all true.
// - Distances are returned in the same units as the input angular units.
// - Internally all calculations are in radians.
// - If Dec is outside [-pi/2,+pi/2], then it is corrected by pole reflection
//   and RA is shifted by pi accordingly.
// - PoleDecThresh is accepted for interface compatibility; with the exact
//   haversine-kernel implementation below it does not affect correctness.
// - If fewer than 4 outputs are requested, then only the forward
//   1->2 matching is calculated, and the reverse 2->1 matching is skipped.
//
// Compile example on Linux:
//   mex -R2018a CXXFLAGS="$CXXFLAGS -O3 -std=c++11" matchCatalogs.cpp
//
// With OpenMP (Linux):
//   mex -R2018a CXXFLAGS="$CXXFLAGS -O3 -std=c++11 -fopenmp" LDFLAGS="$LDFLAGS -fopenmp" matchCatalogs.cpp

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

inline double GetScalarDoubleDefault(const mxArray* Arr, double DefaultVal) {
    if (Arr == nullptr) {
        return DefaultVal;
    }
    if (!mxIsDouble(Arr) || mxIsComplex(Arr) || mxGetNumberOfElements(Arr) != 1) {
        mexErrMsgIdAndTxt("matchCatalogs:InvalidScalarDouble",
                          "Scalar double input expected.");
    }
    return mxGetScalar(Arr);
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

struct CatalogCompact {
    std::vector<double> RA;
    std::vector<double> Dec;
    std::vector<double> CosDec;
    std::vector<mwSize> OrigInd;  // original 0-based index
};

inline void CheckRealDoubleVector(const mxArray* Arr, const char* Name) {
    if (!mxIsDouble(Arr) || mxIsComplex(Arr)) {
        mexErrMsgIdAndTxt("matchCatalogs:InvalidInput",
                          "%s must be a real double array.", Name);
    }
}

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
        C.CosDec.push_back(std::cos(Dec_Rad));
        C.OrigInd.push_back(I);
    }

    return C;
}

inline double HaversineA(double DecQ, double CosDecQ,
                         double DecT, double CosDecT,
                         double DRA) {
    const double HalfDDec = 0.5 * (DecT - DecQ);
    const double HalfDRA  = 0.5 * DRA;
    const double S1 = std::sin(HalfDDec);
    const double S2 = std::sin(HalfDRA);
    return S1*S1 + CosDecQ * CosDecT * S2*S2;
}

inline double DistFromHaversineA(double A) {
    const double Ac = Clamp(A, 0.0, 1.0);
    return 2.0 * std::asin(std::sqrt(Ac));
}

struct MatchResult {
    mwSize BestOrigInd;
    double BestA;
    mwSize Count;
    bool   Found;
};

MatchResult FindBestInSortedDecSlab(double RA_Q,
                                    double Dec_Q,
                                    double CosDec_Q,
                                    const CatalogCompact& Target,
                                    double MatchDistRad,
                                    double A_Limit) {
    MatchResult R;
    R.BestOrigInd = 0;
    R.BestA       = std::numeric_limits<double>::infinity();
    R.Count       = 0;
    R.Found       = false;

    const double DecMin = std::max(Dec_Q - MatchDistRad, -HALF_PI);
    const double DecMax = std::min(Dec_Q + MatchDistRad,  HALF_PI);

    auto ItLo = std::lower_bound(Target.Dec.begin(), Target.Dec.end(), DecMin);
    auto ItHi = std::upper_bound(Target.Dec.begin(), Target.Dec.end(), DecMax);

    const std::size_t ILo = static_cast<std::size_t>(ItLo - Target.Dec.begin());
    const std::size_t IHi = static_cast<std::size_t>(ItHi - Target.Dec.begin());

    for (std::size_t K = ILo; K < IHi; ++K) {
        const double DRA = WrapDiffRA(Target.RA[K] - RA_Q);
        const double A = HaversineA(Dec_Q, CosDec_Q,
                                    Target.Dec[K], Target.CosDec[K],
                                    DRA);

        if (A <= A_Limit) {
            ++R.Count;
            if (!R.Found || A < R.BestA ||
                (A == R.BestA && Target.OrigInd[K] < R.BestOrigInd)) {
                R.Found = true;
                R.BestA = A;
                R.BestOrigInd = Target.OrigInd[K];
            }
        }
    }

    return R;
}

MatchResult FindBestByFullScan(double RA_Q,
                               double Dec_Q,
                               double CosDec_Q,
                               const CatalogCompact& Target,
                               double MatchDistRad,
                               double A_Limit) {
    MatchResult R;
    R.BestOrigInd = 0;
    R.BestA       = std::numeric_limits<double>::infinity();
    R.Count       = 0;
    R.Found       = false;

    const double DecMin = Dec_Q - MatchDistRad;
    const double DecMax = Dec_Q + MatchDistRad;

    const std::size_t N = Target.Dec.size();
    for (std::size_t K = 0; K < N; ++K) {
        const double DecT = Target.Dec[K];
        if (DecT < DecMin || DecT > DecMax) {
            continue;
        }

        const double DRA = WrapDiffRA(Target.RA[K] - RA_Q);
        const double A = HaversineA(Dec_Q, CosDec_Q,
                                    DecT, Target.CosDec[K],
                                    DRA);

        if (A <= A_Limit) {
            ++R.Count;
            if (!R.Found || A < R.BestA ||
                (A == R.BestA && Target.OrigInd[K] < R.BestOrigInd)) {
                R.Found = true;
                R.BestA = A;
                R.BestOrigInd = Target.OrigInd[K];
            }
        }
    }

    return R;
}

} // anonymous namespace


void mexFunction(int Nlhs, mxArray* Plhs[], int Nrhs, const mxArray* Prhs[]) {
    if (Nrhs < 5 || Nrhs > 10) {
        mexErrMsgIdAndTxt("matchCatalogs:InvalidNumInputs",
                          "Expected 5 to 10 input arguments.");
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

    const mxArray* IsDeg_Arr         = (Nrhs >= 6)  ? Prhs[5] : nullptr;
    const mxArray* Use1_Arr          = (Nrhs >= 7)  ? Prhs[6] : nullptr;
    const mxArray* Use2_Arr          = (Nrhs >= 8)  ? Prhs[7] : nullptr;
    const mxArray* CheckSorted2_Arr  = (Nrhs >= 9)  ? Prhs[8] : nullptr;
    const mxArray* PoleDecThresh_Arr = (Nrhs >= 10) ? Prhs[9] : nullptr;

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
    const double PoleThreshIn = GetScalarDoubleDefault(PoleDecThresh_Arr, 87.0);

    if (!IsFinite(MatchDistIn) || MatchDistIn < 0.0) {
        mexErrMsgIdAndTxt("matchCatalogs:InvalidMatchDist",
                          "MatchDist must be finite and >= 0.");
    }

    const double AngScale = IsDeg ? (PI / 180.0) : 1.0;
    const double MatchDistRad = std::min(MatchDistIn * AngScale, PI);
    const double PoleThreshRad = PoleThreshIn * AngScale;
    (void)PoleThreshRad; // kept for interface compatibility

    const double A_Limit = std::pow(std::sin(0.5 * MatchDistRad), 2.0);

    // Output order:
    // 1: Ind1
    // 2: Dist1
    // 3: Nmatch1
    // 4: Ind2
    // 5: Dist2
    // 6: Nmatch2

    Plhs[0] = mxCreateDoubleMatrix(N1, 1, mxREAL); // Ind1
    double* Ind1 = mxGetPr(Plhs[0]);

    double* Dist1 = nullptr;
    double* Nmatch1 = nullptr;
    double* Ind2 = nullptr;
    double* Dist2 = nullptr;
    double* Nmatch2 = nullptr;

    if (Nlhs >= 2) {
        Plhs[1] = mxCreateDoubleMatrix(N1, 1, mxREAL); // Dist1
        Dist1 = mxGetPr(Plhs[1]);
    }
    if (Nlhs >= 3) {
        Plhs[2] = mxCreateDoubleMatrix(N1, 1, mxREAL); // Nmatch1
        Nmatch1 = mxGetPr(Plhs[2]);
    }
    if (Nlhs >= 4) {
        Plhs[3] = mxCreateDoubleMatrix(N2, 1, mxREAL); // Ind2
        Ind2 = mxGetPr(Plhs[3]);
    }
    if (Nlhs >= 5) {
        Plhs[4] = mxCreateDoubleMatrix(N2, 1, mxREAL); // Dist2
        Dist2 = mxGetPr(Plhs[4]);
    }
    if (Nlhs >= 6) {
        Plhs[5] = mxCreateDoubleMatrix(N2, 1, mxREAL); // Nmatch2
        Nmatch2 = mxGetPr(Plhs[5]);
    }

    bool HasSortedError2 = false;
    bool DummySortedError = false;

    CatalogCompact C1 = BuildCompactCatalog(RA1_Arr, Dec1_Arr, Use1_Arr,
                                            AngScale, false, DummySortedError,
                                            Ind1, Dist1, Nmatch1);

    CatalogCompact C2 = BuildCompactCatalog(RA2_Arr, Dec2_Arr, Use2_Arr,
                                            AngScale, CheckSorted2, HasSortedError2,
                                            Ind2, Dist2, Nmatch2);

    if (CheckSorted2 && HasSortedError2) {
        mexErrMsgIdAndTxt("matchCatalogs:Dec2NotSorted",
                          "Valid/used Dec2 entries must be sorted in ascending order after declination correction.");
    }

    const std::size_t M1 = C1.Dec.size();
    const std::size_t M2 = C2.Dec.size();

    // Direction 1: Catalog 1 -> Catalog 2 (binary search in sorted Dec2)
    #ifdef _OPENMP
    #pragma omp parallel for schedule(static)
    #endif
    for (std::ptrdiff_t K = 0; K < static_cast<std::ptrdiff_t>(M1); ++K) {
        const std::size_t KK = static_cast<std::size_t>(K);
        const mwSize I1 = C1.OrigInd[KK];

        MatchResult R = FindBestInSortedDecSlab(
            C1.RA[KK],
            C1.Dec[KK],
            C1.CosDec[KK],
            C2,
            MatchDistRad,
            A_Limit
        );

        if (Nmatch1 != nullptr) {
            Nmatch1[I1] = static_cast<double>(R.Count);
        }

        if (R.Found) {
            Ind1[I1] = static_cast<double>(R.BestOrigInd + 1);
            if (Dist1 != nullptr) {
                Dist1[I1] = DistFromHaversineA(R.BestA) / AngScale;
            }
        }
    }

    // If fewer than 4 outputs are requested, skip the expensive reverse search.
    if (Nlhs < 4) {
        return;
    }

    // Direction 2: Catalog 2 -> Catalog 1 (full scan over compact valid list 1)
    #ifdef _OPENMP
    #pragma omp parallel for schedule(static)
    #endif
    for (std::ptrdiff_t K = 0; K < static_cast<std::ptrdiff_t>(M2); ++K) {
        const std::size_t KK = static_cast<std::size_t>(K);
        const mwSize I2 = C2.OrigInd[KK];

        MatchResult R = FindBestByFullScan(
            C2.RA[KK],
            C2.Dec[KK],
            C2.CosDec[KK],
            C1,
            MatchDistRad,
            A_Limit
        );

        if (Nmatch2 != nullptr) {
            Nmatch2[I2] = static_cast<double>(R.Count);
        }

        if (R.Found) {
            Ind2[I2] = static_cast<double>(R.BestOrigInd + 1);
            if (Dist2 != nullptr) {
                Dist2[I2] = DistFromHaversineA(R.BestA) / AngScale;
            }
        }
    }
}
