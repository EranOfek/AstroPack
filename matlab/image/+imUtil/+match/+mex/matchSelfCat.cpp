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

#ifndef M_PI
#define M_PI 3.141592653589793238462643383279502884
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
            mexErrMsgIdAndTxt("matchSelfCat:Input", "Logical scalar expected.");
        }
        return mxIsLogicalScalarTrue(A);
    }

    if (!mxIsNumeric(A) || mxIsComplex(A) || mxGetNumberOfElements(A) != 1) {
        mexErrMsgIdAndTxt("matchSelfCat:Input", "Scalar logical/numeric expected.");
    }

    return mxGetScalar(A) != 0.0;
}

template <typename T>
inline T clampCos(T x) {
    if (x > static_cast<T>(1))  return static_cast<T>(1);
    if (x < static_cast<T>(-1)) return static_cast<T>(-1);
    return x;
}

template <typename T>
inline T deg2radT(T x) {
    return x * static_cast<T>(M_PI / 180.0);
}

template <typename T>
inline T rad2degT(T x) {
    return x * static_cast<T>(180.0 / M_PI);
}

template <typename T>
inline T wrapModPositive(T x, T period) {
    T y = std::fmod(x, period);
    if (y < static_cast<T>(0)) {
        y += period;
    }
    return y;
}

template <typename T>
inline void normalizeRaDecRad(T& ra, T& dec) {
    const T TwoPi  = static_cast<T>(2.0 * M_PI);
    const T Pi     = static_cast<T>(M_PI);
    const T HalfPi = static_cast<T>(0.5 * M_PI);

    dec = wrapModPositive(dec + Pi, TwoPi) - Pi;

    if (dec > HalfPi) {
        dec = Pi - dec;
        ra += Pi;
    } else if (dec < -HalfPi) {
        dec = -Pi - dec;
        ra += Pi;
    }

    ra = wrapModPositive(ra, TwoPi);
}

template <typename T>
inline void normalizeRaDecDeg(T& ra, T& dec) {
    const T Full    = static_cast<T>(360);
    const T Half    = static_cast<T>(180);
    const T Quarter = static_cast<T>(90);

    dec = wrapModPositive(dec + Half, Full) - Half;

    if (dec > Quarter) {
        dec = Half - dec;
        ra += Half;
    } else if (dec < -Quarter) {
        dec = -Half - dec;
        ra += Half;
    }

    ra = wrapModPositive(ra, Full);
}

template <typename T>
inline void normalizeRaDec(T& ra, T& dec, bool isUnitsDeg) {
    if (isUnitsDeg) {
        normalizeRaDecDeg(ra, dec);
    } else {
        normalizeRaDecRad(ra, dec);
    }
}

template <typename T>
inline T deltaRaAbsRad(T a, T b) {
    T d = std::fabs(a - b);
    const T TwoPi = static_cast<T>(2.0 * M_PI);
    const T Pi    = static_cast<T>(M_PI);
    if (d > Pi) {
        d = TwoPi - d;
    }
    return d;
}

template <typename T>
struct MatchRecord {
    mwIndex OrigIdx1;   // MATLAB 1-based index
    T Dist;
};

template <typename T>
void validateVectorReal(const mxArray* A, const char* nameA) {
    if (!mxIsNumeric(A) || mxIsComplex(A) || mxIsSparse(A)) {
        mexErrMsgIdAndTxt("matchSelfCat:Input", "%s must be a real full numeric array.", nameA);
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
void runKernel(
    int nlhs, mxArray* plhs[],
    const mxArray* RA1_in, const mxArray* Dec1_in,
    double searchRadiusInput,
    bool isUnitsDeg,
    bool checkList1sorted,
    bool sortIndAll,
    bool removeDuplicates
) {
    const mwSize N1full = mxGetNumberOfElements(RA1_in);

    const T* RA1ptr  = static_cast<const T*>(mxGetData(RA1_in));
    const T* Dec1ptr = static_cast<const T*>(mxGetData(Dec1_in));

    T searchRadius = static_cast<T>(searchRadiusInput);
    if (isUnitsDeg) {
        searchRadius = deg2radT(searchRadius);
    }

    if (!(isFiniteT(searchRadius)) || searchRadius < static_cast<T>(0)) {
        mexErrMsgIdAndTxt("matchSelfCat:Input", "SearchRadius must be finite and non-negative.");
    }

    // ---------------------------------------------------------------------
    // Preprocess: compact valid normalized catalog
    // ---------------------------------------------------------------------
    std::vector<T> RA1v;
    std::vector<T> Dec1v;
    std::vector<T> SinDec1v;
    std::vector<T> CosDec1v;
    std::vector<mwIndex> OrigIdx1v;   // original MATLAB index, 1-based

    RA1v.reserve(N1full);
    Dec1v.reserve(N1full);
    SinDec1v.reserve(N1full);
    CosDec1v.reserve(N1full);
    OrigIdx1v.reserve(N1full);

    // Original row -> compact valid row. invalid => -1
    std::vector<mwIndex> OrigToCompact(N1full, static_cast<mwIndex>(-1));

    bool firstValid = true;
    T prevDec = static_cast<T>(0);

    for (mwIndex i = 0; i < N1full; ++i) {
        T ra  = RA1ptr[i];
        T dec = Dec1ptr[i];

        if (!isFiniteT(ra) || !isFiniteT(dec)) {
            continue;
        }

        normalizeRaDec(ra, dec, isUnitsDeg);

        if (isUnitsDeg) {
            ra  = deg2radT(ra);
            dec = deg2radT(dec);
        }

        if (checkList1sorted) {
            if (firstValid) {
                prevDec = dec;
                firstValid = false;
            } else {
                if (dec < prevDec) {
                    mexErrMsgIdAndTxt("matchSelfCat:Sorted",
                                      "RA1/Dec1 is not sorted by Dec after normalization.");
                }
                prevDec = dec;
            }
        }

        OrigToCompact[i] = static_cast<mwIndex>(RA1v.size());

        RA1v.push_back(ra);
        Dec1v.push_back(dec);
        SinDec1v.push_back(std::sin(dec));
        CosDec1v.push_back(std::cos(dec));
        OrigIdx1v.push_back(i + 1);
    }

    const mwSize Nvalid = static_cast<mwSize>(RA1v.size());

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
        const T cosR = std::cos(searchRadius);
        const T sinR = std::sin(searchRadius);
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

            const T ra2  = RA1v[iSelf];
            const T dec2 = Dec1v[iSelf];

            const T sinDec2 = SinDec1v[iSelf];
            const T cosDec2 = CosDec1v[iSelf];
            const mwIndex origSelfIdx = OrigIdx1v[iSelf];  // 1-based original index

            const T decLo = dec2 - searchRadius;
            const T decHi = dec2 + searchRadius;

            auto lowIt  = std::lower_bound(Dec1v.begin(), Dec1v.end(), decLo);
            auto highIt = std::upper_bound(Dec1v.begin(), Dec1v.end(), decHi);

            const mwIndex iLo = static_cast<mwIndex>(lowIt  - Dec1v.begin());
            const mwIndex iHi = static_cast<mwIndex>(highIt - Dec1v.begin());

            uint32_T count = 0;
            bool foundAny = false;

            T bestCosd = static_cast<T>(-2);
            mwIndex bestOrigIdx = 0;
            T bestDist = std::numeric_limits<T>::quiet_NaN();

            std::vector< MatchRecord<T> > localMatches;
            if (needIndAll) {
                localMatches.reserve(16);
            }

            // Conservative RA prefilter
            bool useRaPrefilter = false;
            T deltaRaMax = static_cast<T>(0);

            const T absCosDec2 = std::fabs(cosDec2);
            if (absCosDec2 > static_cast<T>(1e-12)) {
                T ratio = sinR / absCosDec2;
                if (ratio < static_cast<T>(1)) {
                    deltaRaMax = std::asin(ratio);
                    if (deltaRaMax > static_cast<T>(0) && deltaRaMax < static_cast<T>(M_PI)) {
                        useRaPrefilter = true;
                    }
                }
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

                const T dRA = deltaRaAbsRad(RA1v[j], ra2);

                if (useRaPrefilter && dRA > deltaRaMax) {
                    continue;
                }

                T cosd = SinDec1v[j] * sinDec2 + CosDec1v[j] * cosDec2 * std::cos(dRA);

                if (cosd >= cosR) {
                    ++count;
                    foundAny = true;
                    cosd = clampCos(cosd);

                    const bool isBetter = (cosd > bestCosd);

                    T dist = static_cast<T>(0);
                    if (needIndAll || isBetter) {
                        dist = std::acos(cosd);
                    }

                    if (isBetter) {
                        bestCosd = cosd;
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
                    T outDist = bestDist;
                    if (isUnitsDeg) {
                        outDist = rad2degT(outDist);
                    }
                    TmpDistNearest[iOrig] = outDist;
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

                T outDist = TmpAllMatches[i][k].Dist;
                if (isUnitsDeg) {
                    outDist = rad2degT(outDist);
                }
                DistPtr[k] = outDist;
            }

            mxSetField(plhs[3], i, "Ind", IndCell);
            mxSetField(plhs[3], i, "Dist", DistCell);
        }
    }
}

void mexFunction(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[]) {
    if (nrhs < 3 || nrhs > 7) {
        mexErrMsgIdAndTxt(
            "matchSelfCat:Input",
            "Usage: [IndNearest2to1, DistNearest, Nmatch, IndAll] = matchSelfCat(RA1, Dec1, SearchRadius, IsUnitsDeg, CheckList1sorted, SortIndAll, RemoveDuplicates)"
        );
    }

    if (nlhs > 4) {
        mexErrMsgIdAndTxt("matchSelfCat:Output", "Too many output arguments.");
    }

    const mxArray* RA1 = prhs[0];
    const mxArray* Dec1 = prhs[1];
    const mxArray* SearchRadius = prhs[2];

    validateVectorReal<double>(RA1, "RA1");
    validateVectorReal<double>(Dec1, "Dec1");

    if (mxGetClassID(RA1) != mxGetClassID(Dec1)) {
        mexErrMsgIdAndTxt("matchSelfCat:Input",
                          "RA1 and Dec1 must have the same numeric class.");
    }

    if (mxGetNumberOfElements(RA1) != mxGetNumberOfElements(Dec1)) {
        mexErrMsgIdAndTxt("matchSelfCat:Input",
                          "RA1 and Dec1 must have the same number of elements.");
    }

    if (!mxIsNumeric(SearchRadius) || mxIsComplex(SearchRadius) || mxGetNumberOfElements(SearchRadius) != 1) {
        mexErrMsgIdAndTxt("matchSelfCat:Input",
                          "SearchRadius must be a real numeric scalar.");
    }

    const bool isUnitsDeg       = (nrhs >= 4) ? isScalarLogicalTrue(prhs[3], false) : false;
    const bool checkList1sorted = (nrhs >= 5) ? isScalarLogicalTrue(prhs[4], false) : false;
    const bool sortIndAll       = (nrhs >= 6) ? isScalarLogicalTrue(prhs[5], false) : false;
    const bool removeDuplicates = (nrhs >= 7) ? isScalarLogicalTrue(prhs[6], false) : false;

    const double searchRadiusInput = mxGetScalar(SearchRadius);

    const mxClassID cid = mxGetClassID(RA1);
    switch (cid) {
        case mxDOUBLE_CLASS:
            runKernel<double>(nlhs, plhs, RA1, Dec1,
                              searchRadiusInput, isUnitsDeg, checkList1sorted, sortIndAll, removeDuplicates);
            break;

        case mxSINGLE_CLASS:
            runKernel<float>(nlhs, plhs, RA1, Dec1,
                             searchRadiusInput, isUnitsDeg, checkList1sorted, sortIndAll, removeDuplicates);
            break;

        default:
            mexErrMsgIdAndTxt("matchSelfCat:Input",
                              "RA/Dec inputs must be single or double.");
    }
}
