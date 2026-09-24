// interp2_nearest_any_mex.cpp
//
// Nearest-neighbor 2D interpolation (like interp2(...,'nearest')) for ANY real MATLAB numeric type.
//
// Usage:
//   Vq = interp2_nearest_any_mex(Xin, Yin, Z, Xout, Yout)
//
// Conventions (same as your previous MEXes):
// - Xin/Yin can be vectors OR meshgrid matrices (rectilinear: Xin first row, Yin first col).
// - If Xout is 1xM and Yout is Nx1 -> output is N x M.
// - Else Xout and Yout must be the same size -> output that size.
// - No extrapolation required; BUT we guard: if query is out-of-bounds -> output 0 (or NaN for float).
//   (You can change this behavior easily.)
//
// Types:
// - Z can be any real numeric class: double, single, int8/16/32/64, uint8/16/32/64.
// - Output has the SAME class as Z.
// - Xin, Yin, Xout, Yout must be real float (single/double). (Like MATLAB, coordinates are floating.)
//   If you really need integer coordinate arrays too, tell me and I’ll generalize.
//
// Performance:
// - Algorithmic: precompute X and Y nearest indices in meshgrid-output mode.
// - OpenMP: parallel over columns (meshgrid-output) or linear index (general mode).
// - No SIMD (not needed; this is memory-bound and already very fast).

#include "mex.h"
#include <cmath>
#include <cstdint>
#include <algorithm>
#include <limits>
#include <type_traits>

#ifdef _OPENMP
  #include <omp.h>
#endif

// ------------------------ helpers ------------------------
static inline bool isVector(const mxArray* A) { return (mxGetM(A) == 1 || mxGetN(A) == 1); }
static inline bool isRowVector(const mxArray* A) { return (mxGetM(A) == 1 && mxGetN(A) >= 1); }
static inline bool isColVector(const mxArray* A) { return (mxGetN(A) == 1 && mxGetM(A) >= 1); }

template<typename T>
struct AxisView {
    const T* p = nullptr;
    mwSize n = 0;
    mwSize stride = 1;
    inline T at(mwIndex i) const { return p[(mwSize)i * stride]; }
};

template<typename T>
static inline bool isStrictIncreasing(const AxisView<T>& a) {
    if (a.n < 2) return true;
    T prev = a.at(0);
    for (mwIndex i = 1; i < (mwIndex)a.n; ++i) { T v = a.at(i); if (!(v > prev)) return false; prev = v; }
    return true;
}
template<typename T>
static inline bool isStrictDecreasing(const AxisView<T>& a) {
    if (a.n < 2) return true;
    T prev = a.at(0);
    for (mwIndex i = 1; i < (mwIndex)a.n; ++i) { T v = a.at(i); if (!(v < prev)) return false; prev = v; }
    return true;
}

// Binary interval: i s.t. axis[i] <= x <= axis[i+1] (inc) or axis[i] >= x >= axis[i+1] (dec)
template<typename T>
static inline mwIndex findIntervalBinary(const AxisView<T>& a, bool inc, T x) {
    const T a0 = a.at(0);
    const T aN = a.at((mwIndex)a.n - 1);
    if (inc) { if (x < a0 || x > aN) return (mwIndex)-1; }
    else     { if (x > a0 || x < aN) return (mwIndex)-1; }

    mwIndex lo = 0, hi = (mwIndex)a.n - 1;
    while (hi - lo > 1) {
        mwIndex mid = lo + (hi - lo) / 2;
        T am = a.at(mid);
        if (inc) { if (x >= am) lo = mid; else hi = mid; }
        else     { if (x <= am) lo = mid; else hi = mid; }
    }
    if (lo >= (mwIndex)a.n - 1) lo = (mwIndex)a.n - 2;
    return lo;
}

template<typename T>
static inline mwIndex nearestIndex(const AxisView<T>& a, bool inc, T x, bool& oob) {
    mwIndex i = findIntervalBinary(a, inc, x);
    if (i == (mwIndex)-1) { oob = true; return 0; }
    oob = false;

    T v0 = a.at(i);
    T v1 = a.at(i + 1);

    // choose nearer; ties go "up" (matches typical nearest behavior)
    T d0 = (T)std::abs((double)(x - v0));
    T d1 = (T)std::abs((double)(v1 - x));
    return (d1 <= d0) ? (i + 1) : i;
}

static inline mwIndex clampIndex(mwIndex v, mwIndex lo, mwIndex hi) {
    return (v < lo) ? lo : (v > hi ? hi : v);
}

// Return fill value for OOB
template<typename T>
static inline T oobFill() { return (T)0; }
template<>
inline float oobFill<float>() { return std::numeric_limits<float>::quiet_NaN(); }
template<>
inline double oobFill<double>() { return std::numeric_limits<double>::quiet_NaN(); }

// Copy element of Z at (row=iy, col=ix) -> Out[k], for arbitrary numeric types
template<typename Tz>
static inline void copyNearest(const Tz* Z, mwIndex Ny, mwIndex ix, mwIndex iy, Tz* Out, mwIndex outIndex) {
    Out[outIndex] = Z[iy + ix * Ny];
}

template<typename Tx> // Tx = float/double axis/query type
static void runNearestAnyType(
    const mxArray* mxXin, const mxArray* mxYin, const mxArray* mxZ,
    const mxArray* mxXq,  const mxArray* mxYq,
    mxArray* mxOut
) {
    // Build axis views in Tx
    AxisView<Tx> Xaxis, Yaxis;

    const mwSize Zm = mxGetM(mxZ);
    const mwSize Zn = mxGetN(mxZ);

    if (isVector(mxXin) && isVector(mxYin)) {
        Xaxis.p = (const Tx*)mxGetData(mxXin); Xaxis.n = mxGetNumberOfElements(mxXin); Xaxis.stride = 1;
        Yaxis.p = (const Tx*)mxGetData(mxYin); Yaxis.n = mxGetNumberOfElements(mxYin); Yaxis.stride = 1;
        if (Zm != Yaxis.n || Zn != Xaxis.n) {
            mexErrMsgIdAndTxt("interp2_nearest_any_mex:dim",
                              "For vector axes: Z must be size [numel(Yin) x numel(Xin)].");
        }
    } else {
        mwSize Xm = mxGetM(mxXin), Xn = mxGetN(mxXin);
        mwSize Ym = mxGetM(mxYin), Yn = mxGetN(mxYin);
        if (!(Xm == Zm && Xn == Zn && Ym == Zm && Yn == Zn)) {
            mexErrMsgIdAndTxt("interp2_nearest_any_mex:grid",
                              "Xin/Yin must be vectors OR matrices the same size as Z (meshgrid form).");
        }
        const Tx* XinGrid = (const Tx*)mxGetData(mxXin);
        const Tx* YinGrid = (const Tx*)mxGetData(mxYin);

        Xaxis.p = XinGrid; Xaxis.n = Zn; Xaxis.stride = Zm; // first row
        Yaxis.p = YinGrid; Yaxis.n = Zm; Yaxis.stride = 1;  // first col
    }

    if (Xaxis.n < 2 || Yaxis.n < 2) {
        mexErrMsgIdAndTxt("interp2_nearest_any_mex:grid", "Grid must have at least 2 points in each dimension.");
    }

    const bool xInc = isStrictIncreasing(Xaxis);
    const bool xDec = isStrictDecreasing(Xaxis);
    const bool yInc = isStrictIncreasing(Yaxis);
    const bool yDec = isStrictDecreasing(Yaxis);
    if (!xInc && !xDec) mexErrMsgIdAndTxt("interp2_nearest_any_mex:grid", "Xin must be strictly monotonic.");
    if (!yInc && !yDec) mexErrMsgIdAndTxt("interp2_nearest_any_mex:grid", "Yin must be strictly monotonic.");

    const bool xIncreasing = xInc;
    const bool yIncreasing = yInc;

    const Tx* Xq = (const Tx*)mxGetData(mxXq);
    const Tx* Yq = (const Tx*)mxGetData(mxYq);

    const bool meshgridOut = isRowVector(mxXq) && isColVector(mxYq);
    const mwSize outM = mxGetM(mxOut);
    const mwSize outN = mxGetN(mxOut);

    // Dispatch by Z type for output copy
    mxClassID zcls = mxGetClassID(mxZ);

#define DISPATCH_ZTYPE(TZ) do { \
    const TZ* Z = (const TZ*)mxGetData(mxZ); \
    TZ* Out = (TZ*)mxGetData(mxOut); \
    const mwIndex Ny = (mwIndex)Zm; \
    if (meshgridOut) { \
        const mwSize M = outN; \
        const mwSize N = outM; \
        /* precompute ix for each Xout, iy for each Yout */ \
        mwIndex* Ix = (mwIndex*)mxMalloc(sizeof(mwIndex)*M); \
        mwIndex* Iy = (mwIndex*)mxMalloc(sizeof(mwIndex)*N); \
        uint8_t* xOOB = (uint8_t*)mxMalloc(sizeof(uint8_t)*M); \
        uint8_t* yOOB = (uint8_t*)mxMalloc(sizeof(uint8_t)*N); \
        for (mwSize j=0;j<M;++j){ bool oob=false; Ix[j]=nearestIndex(Xaxis, xIncreasing, Xq[j], oob); xOOB[j]=(uint8_t)oob; } \
        for (mwSize i=0;i<N;++i){ bool oob=false; Iy[i]=nearestIndex(Yaxis, yIncreasing, Yq[i], oob); yOOB[i]=(uint8_t)oob; } \
        /* parallel over columns */ \
        _Pragma("omp parallel for schedule(static)") \
        for (mwIndex jj=0;jj<(mwIndex)M;++jj){ \
            const mwIndex base = jj*(mwIndex)outM; \
            if (xOOB[jj]) { \
                for (mwIndex ii=0;ii<(mwIndex)N;++ii) Out[base+ii] = (TZ)0; \
                continue; \
            } \
            mwIndex ix = Ix[jj]; \
            for (mwIndex ii=0;ii<(mwIndex)N;++ii){ \
                if (yOOB[ii]) Out[base+ii] = (TZ)0; \
                else { mwIndex iy = Iy[ii]; Out[base+ii] = Z[iy + ix*Ny]; } \
            } \
        } \
        mxFree(Ix); mxFree(Iy); mxFree(xOOB); mxFree(yOOB); \
    } else { \
        const mwSize Ne = outM*outN; \
        _Pragma("omp parallel for schedule(static)") \
        for (mwIndex k=0;k<(mwIndex)Ne;++k){ \
            bool xoob=false, yoob=false; \
            mwIndex ix = nearestIndex(Xaxis, xIncreasing, Xq[k], xoob); \
            mwIndex iy = nearestIndex(Yaxis, yIncreasing, Yq[k], yoob); \
            if (xoob || yoob) Out[k] = (TZ)0; \
            else Out[k] = Z[iy + ix*Ny]; \
        } \
    } \
} while(0)

    // Note: OpenMP pragmas above are active only if built with -fopenmp / /openmp
    // If not, they are ignored (fine).

    switch (zcls) {
        case mxDOUBLE_CLASS: DISPATCH_ZTYPE(double); break;
        case mxSINGLE_CLASS: DISPATCH_ZTYPE(float); break;
        case mxINT8_CLASS:   DISPATCH_ZTYPE(int8_t); break;
        case mxUINT8_CLASS:  DISPATCH_ZTYPE(uint8_t); break;
        case mxINT16_CLASS:  DISPATCH_ZTYPE(int16_t); break;
        case mxUINT16_CLASS: DISPATCH_ZTYPE(uint16_t); break;
        case mxINT32_CLASS:  DISPATCH_ZTYPE(int32_t); break;
        case mxUINT32_CLASS: DISPATCH_ZTYPE(uint32_t); break;
        case mxINT64_CLASS:  DISPATCH_ZTYPE(int64_t); break;
        case mxUINT64_CLASS: DISPATCH_ZTYPE(uint64_t); break;
        default:
            mexErrMsgIdAndTxt("interp2_nearest_any_mex:type", "Unsupported Z class.");
    }

#undef DISPATCH_ZTYPE
}

// ------------------------ mex entry ------------------------
void mexFunction(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[]) {
    if (nrhs != 5) {
        mexErrMsgIdAndTxt("interp2_nearest_any_mex:nrhs",
                          "Usage: Vq = interp2_nearest_any_mex(Xin, Yin, Z, Xout, Yout)");
    }
    if (nlhs > 1) mexErrMsgIdAndTxt("interp2_nearest_any_mex:nlhs", "One output only.");

    const mxArray* mxXin = prhs[0];
    const mxArray* mxYin = prhs[1];
    const mxArray* mxZ   = prhs[2];
    const mxArray* mxXq  = prhs[3];
    const mxArray* mxYq  = prhs[4];

    // Z: any real numeric
    if (!mxIsNumeric(mxZ) || mxIsComplex(mxZ)) {
        mexErrMsgIdAndTxt("interp2_nearest_any_mex:type", "Z must be real numeric.");
    }

    // Coordinates must be real single/double
    auto isRealFloat = [](const mxArray* A)->bool {
        return mxIsNumeric(A) && !mxIsComplex(A) && (mxGetClassID(A) == mxSINGLE_CLASS || mxGetClassID(A) == mxDOUBLE_CLASS);
    };
    if (!isRealFloat(mxXin) || !isRealFloat(mxYin) || !isRealFloat(mxXq) || !isRealFloat(mxYq)) {
        mexErrMsgIdAndTxt("interp2_nearest_any_mex:type",
                          "Xin, Yin, Xout, Yout must be real single or double.");
    }

    // Z must be 2D
    if (mxGetNumberOfDimensions(mxZ) != 2) {
        mexErrMsgIdAndTxt("interp2_nearest_any_mex:dim", "Z must be a 2D matrix.");
    }

    // Output sizing
    const bool meshgridOut = isRowVector(mxXq) && isColVector(mxYq);

    mwSize outM = 0, outN = 0;
    if (meshgridOut) {
        outM = mxGetM(mxYq);
        outN = mxGetN(mxXq);
    } else {
        if (mxGetM(mxXq) != mxGetM(mxYq) || mxGetN(mxXq) != mxGetN(mxYq)) {
            mexErrMsgIdAndTxt("interp2_nearest_any_mex:querydim",
                              "Xout and Yout must be same size, unless Xout is 1xM and Yout is Nx1.");
        }
        outM = mxGetM(mxXq);
        outN = mxGetN(mxXq);
    }

    // Output class = Z class
    plhs[0] = mxCreateNumericMatrix(outM, outN, mxGetClassID(mxZ), mxREAL);

    // Dispatch coordinate type (single/double)
    if (mxGetClassID(mxXin) != mxGetClassID(mxYin) ||
        mxGetClassID(mxXin) != mxGetClassID(mxXq)  ||
        mxGetClassID(mxXin) != mxGetClassID(mxYq)) {
        mexErrMsgIdAndTxt("interp2_nearest_any_mex:class",
                          "Xin, Yin, Xout, Yout must have the same class (single or double).");
    }

    if (mxGetClassID(mxXin) == mxDOUBLE_CLASS) {
        runNearestAnyType<double>(mxXin, mxYin, mxZ, mxXq, mxYq, plhs[0]);
    } else {
        runNearestAnyType<float>(mxXin, mxYin, mxZ, mxXq, mxYq, plhs[0]);
    }
}
