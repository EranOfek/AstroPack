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
            mexErrMsgIdAndTxt("matchTwoCatsXY:Input", "Logical scalar expected.");
        }
        return mxIsLogicalScalarTrue(A);
    }

    if (!mxIsNumeric(A) || mxIsComplex(A) || mxGetNumberOfElements(A) != 1) {
        mexErrMsgIdAndTxt("matchTwoCatsXY:Input", "Scalar logical/numeric expected.");
    }

    return mxGetScalar(A) != 0.0;
}

template <typename T>
void validateVectorRealSameType(const mxArray* A, const mxArray* B, const char* nameA, const char* nameB) {
    if (!mxIsNumeric(A) || mxIsComplex(A) || mxIsSparse(A)) {
        mexErrMsgIdAndTxt("matchTwoCatsXY:Input", "%s must be a real full numeric array.", nameA);
    }
    if (!mxIsNumeric(B) || mxIsComplex(B) || mxIsSparse(B)) {
        mexErrMsgIdAndTxt("matchTwoCatsXY:Input", "%s must be a real full numeric array.", nameB);
    }
    if (mxGetClassID(A) != mxGetClassID(B)) {
        mexErrMsgIdAndTxt("matchTwoCatsXY:Input", "%s and %s must have the same class.", nameA, nameB);
    }
    if (mxGetNumberOfElements(A) != mxGetNumberOfElements(B)) {
        mexErrMsgIdAndTxt("matchTwoCatsXY:Input", "%s and %s must have the same number of elements.", nameA, nameB);
    }
}

template <typename T>
mxArray* createNumericVector(mwSize N, mxClassID classId) {
    return mxCreateNumericMatrix(N, 1, classId, mxREAL);
}

inline bool shouldUseOpenMP(mwSize N1, mwSize N2, bool needIndAll) {
#ifdef _OPENMP
    if (N1 < 512 || N2 < 128) {
        return false;
    }
    if (needIndAll) {
        return (N2 >= 1024);
    } else {
        return (N2 >= 256);
    }
#else
    (void)N1; (void)N2; (void)needIndAll;
    return false;
#endif
}

template <typename T>
struct MatchRecord {
    mwIndex OrigIdx1;   // MATLAB 1-based
    T Dist;
};

template <typename T>
void runKernel(
    int nlhs, mxArray* plhs[],
    const mxArray* X1_in, const mxArray* Y1_in,
    const mxArray* X2_in, const mxArray* Y2_in,
    double searchRadiusInput,
    bool checkList1sorted,
    bool sortIndAll
) {
    const mwSize N1full = mxGetNumberOfElements(X1_in);
    const mwSize N2full = mxGetNumberOfElements(X2_in);

    const T* X1ptr = static_cast<const T*>(mxGetData(X1_in));
    const T* Y1ptr = static_cast<const T*>(mxGetData(Y1_in));
    const T* X2ptr = static_cast<const T*>(mxGetData(X2_in));
    const T* Y2ptr = static_cast<const T*>(mxGetData(Y2_in));

    const T searchRadius = static_cast<T>(searchRadiusInput);
    if (!(isFiniteT(searchRadius)) || searchRadius < static_cast<T>(0)) {
        mexErrMsgIdAndTxt("matchTwoCatsXY:Input", "SearchRadius must be finite and non-negative.");
    }
    const T searchRadius2 = searchRadius * searchRadius;

    // -----------------------------------------------------------------
    // Preprocess catalog 1
    // -----------------------------------------------------------------
    std::vector<T> X1v;
    std::vector<T> Y1v;
    std::vector<mwIndex> OrigIdx1v;

    X1v.reserve(N1full);
    Y1v.reserve(N1full);
    OrigIdx1v.reserve(N1full);

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
                    mexErrMsgIdAndTxt("matchTwoCatsXY:Sorted",
                                      "X1/Y1 is not sorted by Y1.");
                }
                prevY = y;
            }
        }

        X1v.push_back(x);
        Y1v.push_back(y);
        OrigIdx1v.push_back(i + 1);  // MATLAB 1-based
    }

    const mwSize N1 = static_cast<mwSize>(X1v.size());

    const bool needDist   = (nlhs >= 2);
    const bool needNmatch = (nlhs >= 3);
    const bool needIndAll = (nlhs >= 4);

    // -----------------------------------------------------------------
    // Temporary C++ buffers only
    // -----------------------------------------------------------------
    std::vector<double> TmpIndNearest(N2full, mxGetNaN());

    std::vector<T> TmpDistNearest;
    if (needDist) {
        TmpDistNearest.assign(N2full, std::numeric_limits<T>::quiet_NaN());
    }

    std::vector<uint32_T> TmpNmatch;
    if (needNmatch) {
        TmpNmatch.assign(N2full, 0);
    }

    std::vector< std::vector< MatchRecord<T> > > TmpAllMatches;
    if (needIndAll) {
        TmpAllMatches.resize(N2full);
    }

    if (N1 > 0) {
        const bool useOMP = shouldUseOpenMP(N1, N2full, needIndAll);

#ifdef _OPENMP
#pragma omp parallel for if(useOMP) schedule(guided, 32)
#endif
        for (long long i2ll = 0; i2ll < static_cast<long long>(N2full); ++i2ll) {
            const mwIndex i2 = static_cast<mwIndex>(i2ll);

            const T x2 = X2ptr[i2];
            const T y2 = Y2ptr[i2];

            if (!isFiniteT(x2) || !isFiniteT(y2)) {
                continue;
            }

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
                const T dx = X1v[j] - x2;

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
                        bestOrigIdx = OrigIdx1v[j];
                        bestDist = dist;
                    }

                    if (needIndAll) {
                        MatchRecord<T> rec;
                        rec.OrigIdx1 = OrigIdx1v[j];
                        rec.Dist = dist;
                        localMatches.push_back(rec);
                    }
                }
            }

            if (needNmatch) {
                TmpNmatch[i2] = count;
            }

            if (foundAny) {
                TmpIndNearest[i2] = static_cast<double>(bestOrigIdx);
                if (needDist) {
                    TmpDistNearest[i2] = bestDist;
                }
            }

            if (needIndAll) {
                if (sortIndAll && localMatches.size() > 1) {
                    std::sort(localMatches.begin(), localMatches.end(),
                              [](const MatchRecord<T>& a, const MatchRecord<T>& b) {
                                  return a.Dist < b.Dist;
                              });
                } else if (!sortIndAll && localMatches.size() > 1) {
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

                TmpAllMatches[i2] = std::move(localMatches);
            }
        }
    }

    // -----------------------------------------------------------------
    // Create MATLAB outputs serially
    // -----------------------------------------------------------------
    plhs[0] = mxCreateDoubleMatrix(N2full, 1, mxREAL);
    double* IndNearest = mxGetPr(plhs[0]);
    std::memcpy(IndNearest, TmpIndNearest.data(), N2full * sizeof(double));

    if (needDist) {
        const mxClassID outClass = std::is_same<T,double>::value ? mxDOUBLE_CLASS : mxSINGLE_CLASS;
        plhs[1] = createNumericVector<T>(N2full, outClass);
        T* DistNearest = static_cast<T*>(mxGetData(plhs[1]));
        std::memcpy(DistNearest, TmpDistNearest.data(), N2full * sizeof(T));
    }

    if (needNmatch) {
        plhs[2] = mxCreateNumericMatrix(N2full, 1, mxUINT32_CLASS, mxREAL);
        uint32_T* NmatchPtr = static_cast<uint32_T*>(mxGetData(plhs[2]));
        std::memcpy(NmatchPtr, TmpNmatch.data(), N2full * sizeof(uint32_T));
    }

    if (needIndAll) {
        const char* fieldNames[] = {"Ind", "Dist"};
        plhs[3] = mxCreateStructMatrix(N2full, 1, 2, fieldNames);

        const mxClassID outClass = std::is_same<T,double>::value ? mxDOUBLE_CLASS : mxSINGLE_CLASS;

        for (mwIndex i2 = 0; i2 < N2full; ++i2) {
            const mwSize Nm = static_cast<mwSize>(TmpAllMatches[i2].size());

            mxArray* IndCell = mxCreateDoubleMatrix(Nm, 1, mxREAL);
            double* IndPtr = mxGetPr(IndCell);

            mxArray* DistCell = mxCreateNumericMatrix(Nm, 1, outClass, mxREAL);
            T* DistPtr = static_cast<T*>(mxGetData(DistCell));

            for (mwIndex k = 0; k < Nm; ++k) {
                IndPtr[k] = static_cast<double>(TmpAllMatches[i2][k].OrigIdx1);
                DistPtr[k] = TmpAllMatches[i2][k].Dist;
            }

            mxSetField(plhs[3], i2, "Ind", IndCell);
            mxSetField(plhs[3], i2, "Dist", DistCell);
        }
    }
}

void mexFunction(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[]) {
    if (nrhs < 5 || nrhs > 8) {
        mexErrMsgIdAndTxt(
            "matchTwoCatsXY:Input",
            "Usage: [IndNearest2to1, DistNearest, Nmatch, IndAll] = matchTwoCatsXY(X1, Y1, X2, Y2, SearchRadius, IsUnitsDeg, CheckList1sorted, SortIndAll)"
        );
    }

    if (nlhs > 4) {
        mexErrMsgIdAndTxt("matchTwoCatsXY:Output", "Too many output arguments.");
    }

    const mxArray* X1 = prhs[0];
    const mxArray* Y1 = prhs[1];
    const mxArray* X2 = prhs[2];
    const mxArray* Y2 = prhs[3];
    const mxArray* SearchRadius = prhs[4];

    validateVectorRealSameType<double>(X1, Y1, "X1", "Y1");
    validateVectorRealSameType<double>(X2, Y2, "X2", "Y2");

    if (mxGetClassID(X1) != mxGetClassID(X2)) {
        mexErrMsgIdAndTxt("matchTwoCatsXY:Input",
                          "X1/Y1 and X2/Y2 must all have the same numeric class.");
    }

    if (!mxIsNumeric(SearchRadius) || mxIsComplex(SearchRadius) || mxGetNumberOfElements(SearchRadius) != 1) {
        mexErrMsgIdAndTxt("matchTwoCatsXY:Input",
                          "SearchRadius must be a real numeric scalar.");
    }

    // IsUnitsDeg is accepted for API compatibility, but ignored
    const bool checkList1sorted = (nrhs >= 7) ? isScalarLogicalTrue(prhs[6], false) : false;
    const bool sortIndAll       = (nrhs >= 8) ? isScalarLogicalTrue(prhs[7], false) : false;

    const double searchRadiusInput = mxGetScalar(SearchRadius);

    const mxClassID cid = mxGetClassID(X1);
    switch (cid) {
        case mxDOUBLE_CLASS:
            runKernel<double>(nlhs, plhs, X1, Y1, X2, Y2,
                              searchRadiusInput, checkList1sorted, sortIndAll);
            break;

        case mxSINGLE_CLASS:
            runKernel<float>(nlhs, plhs, X1, Y1, X2, Y2,
                             searchRadiusInput, checkList1sorted, sortIndAll);
            break;

        default:
            mexErrMsgIdAndTxt("matchTwoCatsXY:Input",
                              "X/Y inputs must be single or double.");
    }
}
