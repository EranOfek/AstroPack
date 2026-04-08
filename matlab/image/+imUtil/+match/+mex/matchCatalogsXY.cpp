#include "mex.h"
#include <vector>
#include <algorithm>
#include <cmath>
#include <limits>
#include <cstddef>

#ifdef _OPENMP
#include <omp.h>
#endif

// matchCatalogsXY.cpp
//
// [Ind1,Dist1,Nmatch1,Ind2,Dist2,Nmatch2] = matchCatalogsXY( ...
//      X1, Y1, X2, Y2, MatchDist, IsDeg, Use1, Use2, CheckSorted2, Dummy)
//
// Notes:
// - Planar geometry in X,Y.
// - Matching is non-symmetric.
// - Outputs refer to original indices (1-based in MATLAB).
// - NaNs in X/Y are ignored.
// - Use1 / Use2 are logical or numeric masks.
// - If Use1 or Use2 are empty ([]), they are treated as all true.
// - Distances are Euclidean distances.
// - Input arguments are kept compatible with matchCatalogs. IsDeg and the
//   10th argument are accepted for interface compatibility but ignored.
// - Catalog 2 is assumed sorted by Y ascending.
// - If CheckSorted2=true, valid/used entries in Y2 are checked for sorting.
// - If fewer than 4 outputs are requested, then only the forward
//   1->2 matching is calculated, and the reverse 2->1 matching is skipped.
//
// Compile example on Linux:
//   mex -R2018a CXXFLAGS="$CXXFLAGS -O3 -std=c++11" matchCatalogsXY.cpp
//
// With OpenMP (Linux):
//   mex -R2018a CXXFLAGS="$CXXFLAGS -O3 -std=c++11 -fopenmp" LDFLAGS="$LDFLAGS -fopenmp" matchCatalogsXY.cpp

namespace {

inline bool IsFinite(double X) {
    return std::isfinite(X);
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
        mexErrMsgIdAndTxt("matchCatalogsXY:InvalidScalarBool",
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

    mexErrMsgIdAndTxt("matchCatalogsXY:InvalidMask",
                      "Use1/Use2 must be logical, real double, or empty.");
    return false;
}

inline void CheckRealDoubleVector(const mxArray* Arr, const char* Name) {
    if (!mxIsDouble(Arr) || mxIsComplex(Arr)) {
        mexErrMsgIdAndTxt("matchCatalogsXY:InvalidInput",
                          "%s must be a real double array.", Name);
    }
}

struct CatalogCompact {
    std::vector<double> X;
    std::vector<double> Y;
    std::vector<mwSize> OrigInd;  // original 0-based index
};

CatalogCompact BuildCompactCatalog(const mxArray* X_Arr,
                                   const mxArray* Y_Arr,
                                   const mxArray* Use_Arr,
                                   bool CheckSortedY,
                                   bool& HasSortedError,
                                   double* Ind,
                                   double* Dist,
                                   double* Nmatch) {
    const mwSize N = mxGetNumberOfElements(X_Arr);
    const double* X_P = mxGetPr(X_Arr);
    const double* Y_P = mxGetPr(Y_Arr);

    CatalogCompact C;
    C.X.reserve(N);
    C.Y.reserve(N);
    C.OrigInd.reserve(N);

    const double NaN = mxGetNaN();

    double PrevY = -std::numeric_limits<double>::infinity();
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

        const double X0 = X_P[I];
        const double Y0 = Y_P[I];

        if (!IsFinite(X0) || !IsFinite(Y0)) {
            continue;
        }

        if (CheckSortedY) {
            if (Y0 < PrevY) {
                HasSortedError = true;
            }
            PrevY = Y0;
        }

        C.X.push_back(X0);
        C.Y.push_back(Y0);
        C.OrigInd.push_back(I);
    }

    return C;
}

struct MatchResult {
    mwSize BestOrigInd;
    double BestDist2;
    mwSize Count;
    bool   Found;
};

MatchResult FindBestInSortedYSlab(double X_Q,
                                  double Y_Q,
                                  const CatalogCompact& Target,
                                  double MatchDist,
                                  double MatchDist2) {
    MatchResult R;
    R.BestOrigInd = 0;
    R.BestDist2   = std::numeric_limits<double>::infinity();
    R.Count       = 0;
    R.Found       = false;

    const double YMin = Y_Q - MatchDist;
    const double YMax = Y_Q + MatchDist;

    auto ItLo = std::lower_bound(Target.Y.begin(), Target.Y.end(), YMin);
    auto ItHi = std::upper_bound(Target.Y.begin(), Target.Y.end(), YMax);

    const std::size_t ILo = static_cast<std::size_t>(ItLo - Target.Y.begin());
    const std::size_t IHi = static_cast<std::size_t>(ItHi - Target.Y.begin());

    for (std::size_t K = ILo; K < IHi; ++K) {
        const double dY = Target.Y[K] - Y_Q;
        const double dX = Target.X[K] - X_Q;
        const double D2 = dX*dX + dY*dY;

        if (D2 <= MatchDist2) {
            ++R.Count;
            if (!R.Found || D2 < R.BestDist2 ||
                (D2 == R.BestDist2 && Target.OrigInd[K] < R.BestOrigInd)) {
                R.Found = true;
                R.BestDist2 = D2;
                R.BestOrigInd = Target.OrigInd[K];
            }
        }
    }

    return R;
}

MatchResult FindBestByFullScan(double X_Q,
                               double Y_Q,
                               const CatalogCompact& Target,
                               double MatchDist,
                               double MatchDist2) {
    MatchResult R;
    R.BestOrigInd = 0;
    R.BestDist2   = std::numeric_limits<double>::infinity();
    R.Count       = 0;
    R.Found       = false;

    const double YMin = Y_Q - MatchDist;
    const double YMax = Y_Q + MatchDist;

    const std::size_t N = Target.Y.size();
    for (std::size_t K = 0; K < N; ++K) {
        const double YT = Target.Y[K];
        if (YT < YMin || YT > YMax) {
            continue;
        }

        const double dY = YT - Y_Q;
        const double dX = Target.X[K] - X_Q;
        const double D2 = dX*dX + dY*dY;

        if (D2 <= MatchDist2) {
            ++R.Count;
            if (!R.Found || D2 < R.BestDist2 ||
                (D2 == R.BestDist2 && Target.OrigInd[K] < R.BestOrigInd)) {
                R.Found = true;
                R.BestDist2 = D2;
                R.BestOrigInd = Target.OrigInd[K];
            }
        }
    }

    return R;
}

} // anonymous namespace


void mexFunction(int Nlhs, mxArray* Plhs[], int Nrhs, const mxArray* Prhs[]) {
    if (Nrhs < 5 || Nrhs > 10) {
        mexErrMsgIdAndTxt("matchCatalogsXY:InvalidNumInputs",
                          "Expected 5 to 10 input arguments.");
    }
    if (Nlhs < 1 || Nlhs > 6) {
        mexErrMsgIdAndTxt("matchCatalogsXY:InvalidNumOutputs",
                          "Number of outputs must be between 1 and 6.");
    }

    const mxArray* X1_Arr = Prhs[0];
    const mxArray* Y1_Arr = Prhs[1];
    const mxArray* X2_Arr = Prhs[2];
    const mxArray* Y2_Arr = Prhs[3];
    const mxArray* MatchDist_Arr = Prhs[4];

    const mxArray* IsDeg_Arr        = (Nrhs >= 6)  ? Prhs[5] : nullptr; // ignored
    const mxArray* Use1_Arr         = (Nrhs >= 7)  ? Prhs[6] : nullptr;
    const mxArray* Use2_Arr         = (Nrhs >= 8)  ? Prhs[7] : nullptr;
    const mxArray* CheckSorted2_Arr = (Nrhs >= 9)  ? Prhs[8] : nullptr;
    const mxArray* Dummy_Arr        = (Nrhs >= 10) ? Prhs[9] : nullptr; // ignored

    (void)IsDeg_Arr;
    (void)Dummy_Arr;

    CheckRealDoubleVector(X1_Arr, "X1");
    CheckRealDoubleVector(Y1_Arr, "Y1");
    CheckRealDoubleVector(X2_Arr, "X2");
    CheckRealDoubleVector(Y2_Arr, "Y2");

    if (!mxIsDouble(MatchDist_Arr) || mxIsComplex(MatchDist_Arr) ||
        mxGetNumberOfElements(MatchDist_Arr) != 1) {
        mexErrMsgIdAndTxt("matchCatalogsXY:InvalidMatchDist",
                          "MatchDist must be a real double scalar.");
    }

    const mwSize N1 = mxGetNumberOfElements(X1_Arr);
    const mwSize N2 = mxGetNumberOfElements(X2_Arr);

    if (mxGetNumberOfElements(Y1_Arr) != N1) {
        mexErrMsgIdAndTxt("matchCatalogsXY:SizeMismatch",
                          "X1 and Y1 must have the same number of elements.");
    }
    if (mxGetNumberOfElements(Y2_Arr) != N2) {
        mexErrMsgIdAndTxt("matchCatalogsXY:SizeMismatch",
                          "X2 and Y2 must have the same number of elements.");
    }

    if (!IsMaskEmpty(Use1_Arr) && mxGetNumberOfElements(Use1_Arr) != N1) {
        mexErrMsgIdAndTxt("matchCatalogsXY:SizeMismatch",
                          "Use1 must have the same number of elements as X1/Y1, or be empty.");
    }
    if (!IsMaskEmpty(Use2_Arr) && mxGetNumberOfElements(Use2_Arr) != N2) {
        mexErrMsgIdAndTxt("matchCatalogsXY:SizeMismatch",
                          "Use2 must have the same number of elements as X2/Y2, or be empty.");
    }

    const bool CheckSorted2 = GetScalarBoolDefault(CheckSorted2_Arr, false);
    const double MatchDist = mxGetScalar(MatchDist_Arr);

    if (!IsFinite(MatchDist) || MatchDist < 0.0) {
        mexErrMsgIdAndTxt("matchCatalogsXY:InvalidMatchDist",
                          "MatchDist must be finite and >= 0.");
    }

    const double MatchDist2 = MatchDist * MatchDist;

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

    CatalogCompact C1 = BuildCompactCatalog(X1_Arr, Y1_Arr, Use1_Arr,
                                            false, DummySortedError,
                                            Ind1, Dist1, Nmatch1);

    CatalogCompact C2 = BuildCompactCatalog(X2_Arr, Y2_Arr, Use2_Arr,
                                            CheckSorted2, HasSortedError2,
                                            Ind2, Dist2, Nmatch2);

    if (CheckSorted2 && HasSortedError2) {
        mexErrMsgIdAndTxt("matchCatalogsXY:Y2NotSorted",
                          "Valid/used Y2 entries must be sorted in ascending order.");
    }

    const std::size_t M1 = C1.Y.size();
    const std::size_t M2 = C2.Y.size();

    // Direction 1: Catalog 1 -> Catalog 2 (binary search in sorted Y2)
    #ifdef _OPENMP
    #pragma omp parallel for schedule(static)
    #endif
    for (std::ptrdiff_t K = 0; K < static_cast<std::ptrdiff_t>(M1); ++K) {
        const std::size_t KK = static_cast<std::size_t>(K);
        const mwSize I1 = C1.OrigInd[KK];

        MatchResult R = FindBestInSortedYSlab(
            C1.X[KK],
            C1.Y[KK],
            C2,
            MatchDist,
            MatchDist2
        );

        if (Nmatch1 != nullptr) {
            Nmatch1[I1] = static_cast<double>(R.Count);
        }

        if (R.Found) {
            Ind1[I1] = static_cast<double>(R.BestOrigInd + 1);
            if (Dist1 != nullptr) {
                Dist1[I1] = std::sqrt(R.BestDist2);
            }
        }
    }

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
            C2.X[KK],
            C2.Y[KK],
            C1,
            MatchDist,
            MatchDist2
        );

        if (Nmatch2 != nullptr) {
            Nmatch2[I2] = static_cast<double>(R.Count);
        }

        if (R.Found) {
            Ind2[I2] = static_cast<double>(R.BestOrigInd + 1);
            if (Dist2 != nullptr) {
                Dist2[I2] = std::sqrt(R.BestDist2);
            }
        }
    }
}
