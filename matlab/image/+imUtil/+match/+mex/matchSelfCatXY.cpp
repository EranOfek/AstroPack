#include "mex.h"
#include <cmath>
#include <vector>
#include <algorithm>
#include <limits>
#include <cstring>
#include <type_traits>
#include <utility>

#ifdef _OPENMP
#include <omp.h>
#endif

template <typename T>
inline bool isFiniteT(T x) {
    return std::isfinite(static_cast<double>(x));
}

inline bool isScalarLogicalTrue(const mxArray* A, bool defaultValue=false) {
    if (A == nullptr) {
        return defaultValue;
    }

    if (mxIsLogical(A)) {
        if (mxGetNumberOfElements(A) != 1) {
            mexErrMsgIdAndTxt("matchSelfCatXY:Input", "Logical scalar expected.");
        }
        return mxIsLogicalScalarTrue(A);
    }

    if (!mxIsNumeric(A) || mxIsComplex(A) || mxGetNumberOfElements(A) != 1) {
        mexErrMsgIdAndTxt("matchSelfCatXY:Input", "Scalar logical/numeric expected.");
    }

    return mxGetScalar(A) != 0.0;
}

template <typename T>
void validateVectorReal(const mxArray* A, const char* nameA) {
    if (!mxIsNumeric(A) || mxIsComplex(A) || mxIsSparse(A)) {
        mexErrMsgIdAndTxt("matchSelfCatXY:Input", "%s must be a real full numeric array.", nameA);
    }
}

template <typename T>
mxArray* createNumericVector(mwSize N, mxClassID classId) {
    return mxCreateNumericMatrix(N, 1, classId, mxREAL);
}

inline bool shouldUseOpenMP(mwSize N1, bool needIndAll) {
#ifdef _OPENMP
    if (N1 < 512) {
        return false;
    }
    if (needIndAll) {
        return (N1 >= 1024);
    } else {
        return (N1 >= 512);
    }
#else
    (void)N1; (void)needIndAll;
    return false;
#endif
}

template <typename T>
struct MatchRecord {
    mwIndex OrigIdx1;   // MATLAB 1-based index
    T Dist;
};

template <typename T>
void runKernel(
    int nlhs, mxArray* plhs[],
    const mxArray* X1_in, const mxArray* Y1_in,
    double searchRadiusInput,
    bool checkList1sorted,
    bool sortIndAll,
    bool removeDuplicates
) {
    const mwSize N1full = mxGetNumberOfElements(X1_in);

    const T* X1ptr = static_cast<const T*>(mxGetData(X1_in));
    const T* Y1ptr = static_cast<const T*>(mxGetData(Y1_in));

    const T searchRadius = static_cast<T>(searchRadiusInput);
    if (!(isFiniteT(searchRadius)) || searchRadius < static_cast<T>(0)) {
        mexErrMsgIdAndTxt("matchSelfCatXY:Input", "SearchRadius must be finite and non-negative.");
    }
    const T searchRadius2 = searchRadius * searchRadius;

    // ---------------------------------------------------------------------
    // Preprocess: compact valid catalog
    // ---------------------------------------------------------------------
    std::vector<T> X1v;
    std::vector<T> Y1v;
    std::vector<mwIndex> OrigIdx1v;   // original MATLAB index, 1-based

    X1v.reserve(N1full);
    Y1v.reserve(N1full);
    OrigIdx1v.reserve(N1full);

    // Original row -> compact valid row. invalid => -1
    std::vector<mwIndex> OrigToCompact(N1full, static_cast<mwIndex>(-1));

    bool firstValid = true;
    T prevY = static_cast<T>(0);

    for (mwIndex i = 0; i < N1full; ++i) {
        const T x = X1ptr[i];
        const T y = Y1ptr[i];

        if (!isFiniteT(x) || !isFiniteT(y)) {
            continue;
        }

        if (checkList1sorted) {
            if (firstValid) {
                prevY = y;
                firstValid = false;
            } else {
                if (y < prevY) {
                    mexErrMsgIdAndTxt("matchSelfCatXY:Sorted",
                                      "X1/Y1 is not sorted by Y1.");
                }
                prevY = y;
            }
        }

        OrigToCompact[i] = static_cast<mwIndex>(X1v.size());

        X1v.push_back(x);
        Y1v.push_back(y);
        OrigIdx1v.push_back(i + 1);
    }

    const mwSize Nvalid = static_cast<mwSize>(X1v.size());

    const bool needDist   = (nlhs >= 2);
    const bool needNmatch = (nlhs >= 3);
    const bool needIndAll = (nlhs >= 4);

    // ---------------------------------------------------------------------
    // Temporary C++ result buffers only
    // ---------------------------------------------------------------------
    std::vector<double> TmpIndNearest(N1full, mxGetNaN());

    std::vector<T> TmpDistNearest;
    if (needDist) {
        TmpDistNearest.assign(N1full, std::numeric_limits<T>::quiet_NaN());
    }

    std::vector<uint32_T> TmpNmatch;
    if (needNmatch) {
        TmpNmatch.assign(N1full, 0);
    }

    std::vector< std::vector< MatchRecord<T> > > TmpAllMatches;
    if (needIndAll) {
        TmpAllMatches.resize(N1full);
    }

    if (Nvalid > 0) {
        const bool useOMP = shouldUseOpenMP(Nvalid, needIndAll);

#ifdef _OPENMP
#pragma omp parallel for if(useOMP) schedule(guided, 32)
#endif
        for (long long iOrigLL = 0; iOrigLL < static_cast<long long>(N1full); ++iOrigLL) {
            const mwIndex iOrig = static_cast<mwIndex>(iOrigLL);

            if (OrigToCompact[iOrig] == static_cast<mwIndex>(-1)) {
                continue;
            }

            const mwIndex iSelf = OrigToCompact[iOrig];

            const T x2 = X1v[iSelf];
            const T y2 = Y1v[iSelf];
            const mwIndex origSelfIdx = OrigIdx1v[iSelf];  // 1-based original index

            const T yLo = y2 - searchRadius;
            const T yHi = y2 + searchRadius;

            auto lowIt  = std::lower_bound(Y1v.begin(), Y1v.end(), yLo);
            auto highIt = std::upper_bound(Y1v.begin(), Y1v.end(), yHi);

            const mwIndex iLo = static_cast<mwIndex>(lowIt  - Y1v.begin());
            const mwIndex iHi = static_cast<mwIndex>(highIt - Y1v.begin());

            uint32_T count = 0;
            bool foundAny = false;

            T bestDist2 = std::numeric_limits<T>::infinity();
            mwIndex bestOrigIdx = 0;
            T bestDist = std::numeric_limits<T>::quiet_NaN();

            std::vector< MatchRecord<T> > localMatches;
            if (needIndAll) {
                localMatches.reserve(16);
            }

            for (mwIndex j = iLo; j < iHi; ++j) {
                // Ignore self
                if (j == iSelf) {
                    continue;
                }

                const mwIndex candOrigIdx = OrigIdx1v[j];

                // Remove symmetric duplicate links by original index order
                if (removeDuplicates && candOrigIdx <= origSelfIdx) {
                    continue;
                }

                const T dx = X1v[j] - x2;

                // X prefilter
                if (std::fabs(dx) > searchRadius) {
                    continue;
                }

                const T dy = Y1v[j] - y2;
                const T dist2 = dx*dx + dy*dy;

                if (dist2 <= searchRadius2) {
                    ++count;
                    foundAny = true;

                    const bool isBetter = (dist2 < bestDist2);

                    T dist = static_cast<T>(0);
                    if (needIndAll || isBetter) {
                        dist = std::sqrt(dist2);
                    }

                    if (isBetter) {
                        bestDist2 = dist2;
                        bestOrigIdx = candOrigIdx;
                        bestDist = dist;
                    }

                    if (needIndAll) {
                        MatchRecord<T> rec;
                        rec.OrigIdx1 = candOrigIdx;
                        rec.Dist = dist;
                        localMatches.push_back(rec);
                    }
                }
            }

            // Write only to temporary C++ buffers
            if (needNmatch) {
                TmpNmatch[iOrig] = count;
            }

            if (foundAny) {
                TmpIndNearest[iOrig] = static_cast<double>(bestOrigIdx);

                if (needDist) {
                    TmpDistNearest[iOrig] = bestDist;
                }
            }

            if (needIndAll) {
                if (sortIndAll && localMatches.size() > 1) {
                    std::sort(localMatches.begin(), localMatches.end(),
                              [](const MatchRecord<T>& a, const MatchRecord<T>& b) {
                                  return a.Dist < b.Dist;
                              });
                } else if (!sortIndAll && localMatches.size() > 1) {
                    // nearest first, rest unchanged
                    size_t bestPos = 0;
                    T minDist = localMatches[0].Dist;
                    for (size_t k = 1; k < localMatches.size(); ++k) {
                        if (localMatches[k].Dist < minDist) {
                            minDist = localMatches[k].Dist;
                            bestPos = k;
                        }
                    }
                    if (bestPos != 0) {
                        MatchRecord<T> bestRec = localMatches[bestPos];
                        for (size_t k = bestPos; k > 0; --k) {
                            localMatches[k] = localMatches[k - 1];
                        }
                        localMatches[0] = bestRec;
                    }
                }

                TmpAllMatches[iOrig] = std::move(localMatches);
            }
        }
    }

    // ---------------------------------------------------------------------
    // Create MATLAB outputs serially
    // ---------------------------------------------------------------------
    plhs[0] = mxCreateDoubleMatrix(N1full, 1, mxREAL);
    double* IndNearest = mxGetPr(plhs[0]);
    std::memcpy(IndNearest, TmpIndNearest.data(), N1full * sizeof(double));

    if (needDist) {
        mxClassID outClass = std::is_same<T,double>::value ? mxDOUBLE_CLASS : mxSINGLE_CLASS;
        plhs[1] = createNumericVector<T>(N1full, outClass);
        T* DistNearest = static_cast<T*>(mxGetData(plhs[1]));
        std::memcpy(DistNearest, TmpDistNearest.data(), N1full * sizeof(T));
    }

    if (needNmatch) {
        plhs[2] = mxCreateNumericMatrix(N1full, 1, mxUINT32_CLASS, mxREAL);
        uint32_T* NmatchPtr = static_cast<uint32_T*>(mxGetData(plhs[2]));
        std::memcpy(NmatchPtr, TmpNmatch.data(), N1full * sizeof(uint32_T));
    }

    if (needIndAll) {
        const char* fieldNames[] = {"Ind", "Dist"};
        plhs[3] = mxCreateStructMatrix(N1full, 1, 2, fieldNames);

        const mxClassID outClass = std::is_same<T,double>::value ? mxDOUBLE_CLASS : mxSINGLE_CLASS;

        for (mwIndex i = 0; i < N1full; ++i) {
            const mwSize Nm = static_cast<mwSize>(TmpAllMatches[i].size());

            mxArray* IndCell = mxCreateDoubleMatrix(Nm, 1, mxREAL);
            double* IndPtr = mxGetPr(IndCell);

            mxArray* DistCell = mxCreateNumericMatrix(Nm, 1, outClass, mxREAL);
            T* DistPtr = static_cast<T*>(mxGetData(DistCell));

            for (mwIndex k = 0; k < Nm; ++k) {
                IndPtr[k] = static_cast<double>(TmpAllMatches[i][k].OrigIdx1);
                DistPtr[k] = TmpAllMatches[i][k].Dist;
            }

            mxSetField(plhs[3], i, "Ind", IndCell);
            mxSetField(plhs[3], i, "Dist", DistCell);
        }
    }
}

void mexFunction(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[]) {
    if (nrhs < 3 || nrhs > 7) {
        mexErrMsgIdAndTxt(
            "matchSelfCatXY:Input",
            "Usage: [IndNearest2to1, DistNearest, Nmatch, IndAll] = matchSelfCatXY(X1, Y1, SearchRadius, IsUnitsDeg, CheckList1sorted, SortIndAll, RemoveDuplicates)"
        );
    }

    if (nlhs > 4) {
        mexErrMsgIdAndTxt("matchSelfCatXY:Output", "Too many output arguments.");
    }

    const mxArray* X1 = prhs[0];
    const mxArray* Y1 = prhs[1];
    const mxArray* SearchRadius = prhs[2];

    validateVectorReal<double>(X1, "X1");
    validateVectorReal<double>(Y1, "Y1");

    if (mxGetClassID(X1) != mxGetClassID(Y1)) {
        mexErrMsgIdAndTxt("matchSelfCatXY:Input",
                          "X1 and Y1 must have the same numeric class.");
    }

    if (mxGetNumberOfElements(X1) != mxGetNumberOfElements(Y1)) {
        mexErrMsgIdAndTxt("matchSelfCatXY:Input",
                          "X1 and Y1 must have the same number of elements.");
    }

    if (!mxIsNumeric(SearchRadius) || mxIsComplex(SearchRadius) || mxGetNumberOfElements(SearchRadius) != 1) {
        mexErrMsgIdAndTxt("matchSelfCatXY:Input",
                          "SearchRadius must be a real numeric scalar.");
    }

    // IsUnitsDeg is accepted for API compatibility, but ignored
    const bool checkList1sorted = (nrhs >= 5) ? isScalarLogicalTrue(prhs[4], false) : false;
    const bool sortIndAll       = (nrhs >= 6) ? isScalarLogicalTrue(prhs[5], false) : false;
    const bool removeDuplicates = (nrhs >= 7) ? isScalarLogicalTrue(prhs[6], false) : false;

    const double searchRadiusInput = mxGetScalar(SearchRadius);

    const mxClassID cid = mxGetClassID(X1);
    switch (cid) {
        case mxDOUBLE_CLASS:
            runKernel<double>(nlhs, plhs, X1, Y1,
                              searchRadiusInput, checkList1sorted, sortIndAll, removeDuplicates);
            break;

        case mxSINGLE_CLASS:
            runKernel<float>(nlhs, plhs, X1, Y1,
                             searchRadiusInput, checkList1sorted, sortIndAll, removeDuplicates);
            break;

        default:
            mexErrMsgIdAndTxt("matchSelfCatXY:Input",
                              "X/Y inputs must be single or double.");
    }
}
