// hist2d_reg_mex.c
//
// [H, Xedges, Yedges, Xcenters, Ycenters] = hist2d_reg_mex( ...
//        X, Y, [Xmin Xmax], [Ymin Ymax], NbinsX, NbinsY, Step=1, IgnoreNaN=true)
//
// Fast regular-grid 2D histogram over linearized X(1:Step:end), Y(1:Step:end).
//
// Inputs:
//   X, Y      : single/double, full, real, same number of elements (any dims allowed)
//   Xrange    : [Xmin Xmax], finite, Xmax > Xmin
//   Yrange    : [Ymin Ymax], finite, Ymax > Ymin
//   NbinsX    : positive integer
//   NbinsY    : positive integer
//   Step      : positive integer, default 1 (linear index step)
//   IgnoreNaN : logical/0/1, default true
//              - if true: explicitly rejects NaNs using (x!=x)||(y!=y) (cheap)
//              - if false: no explicit NaN test; uses NaN-safe range test:
//                    if (!(x>=xmin && x<=xmax)) continue;   (same for y)
//                This rejects NaN and +/-Inf automatically.
//
// Outputs (optional after the first):
//   H         : NbinsX x NbinsY double matrix (column-major, like MATLAB)  [required]
//   Xedges    : (NbinsX+1) x 1 double                                     [optional]
//   Yedges    : (NbinsY+1) x 1 double                                     [optional]
//   Xcenters  : NbinsX x 1 double                                         [optional]
//   Ycenters  : NbinsY x 1 double                                         [optional]
//
// Binning convention (histcounts-like uniform edges):
//   X bin k counts x in [edge(k), edge(k+1)) for k=1..NbinsX-1
//   last X bin includes right edge: x == Xmax goes to bin NbinsX
//   similarly for Y.
//
// Performance features:
//   - OpenMP + per-thread privatization (no atomics in hot loop) when memory allows
//   - Optional small-total-bins branch using per-thread stack cache + merge
//   - uint32 local counts, uint64 reduction, output double
//
// Build (Linux):
//   mex -R2018a CFLAGS="\$CFLAGS -O3 -march=native -DNDEBUG -fopenmp" \
//              LDFLAGS="\$LDFLAGS -fopenmp" hist2d_reg_mex.c
//

#include "mex.h"
#include <stdint.h>
#include <string.h>
#include <math.h>

#ifdef _OPENMP
#include <omp.h>
#endif

static void die(const char* msg) { mexErrMsgIdAndTxt("hist2d_reg_mex:err", "%s", msg); }

static mwSize parsePosIntScalar(const mxArray* a, const char* name) {
    if (!mxIsNumeric(a) || mxIsComplex(a) || mxGetNumberOfElements(a) != 1)
        mexErrMsgIdAndTxt("hist2d_reg_mex:err", "%s must be a real numeric scalar.", name);
    double v = mxGetScalar(a);
    if (!mxIsFinite(v) || v < 1.0)
        mexErrMsgIdAndTxt("hist2d_reg_mex:err", "%s must be >= 1.", name);
    mwSize iv = (mwSize)v;
    if ((double)iv != v)
        mexErrMsgIdAndTxt("hist2d_reg_mex:err", "%s must be an integer.", name);
    return iv;
}

static int parseBoolDefaultTrue(const mxArray* a, const char* name) {
    if (a == NULL) return 1;
    if (!(mxIsLogical(a) || mxIsNumeric(a)) || mxIsComplex(a) || mxGetNumberOfElements(a) != 1)
        mexErrMsgIdAndTxt("hist2d_reg_mex:err", "%s must be a logical/numeric scalar.", name);
    return (mxGetScalar(a) != 0.0);
}

static inline int inRange_nanSafe(double x, double lo, double hi) { return (x >= lo && x <= hi); }
static inline int inRange_fast(double x, double lo, double hi)    { return !(x < lo || x > hi); }

static inline mwSize bin_from_t(double x, double lo, double hi, double invW, mwSize nBins) {
    double t = (x - lo) * invW;   // x assumed in [lo,hi]
    mwSize b = (mwSize)t;         // trunc == floor since t>=0
    if (b >= nBins) {
        if (x == hi) return nBins - 1;
        return (mwSize)(-1);
    }
    return b;
}

#ifndef PRIV_HIST_MAX_MB
#define PRIV_HIST_MAX_MB 256u
#endif

#ifndef STACK_CELLS
#define STACK_CELLS 4096u
#endif

void mexFunction(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[])
{
    if (nlhs < 1 || nlhs > 5) die("Outputs: H required; Xedges/Yedges/Xcenters/Ycenters optional.");
    if (!(nrhs == 6 || nrhs == 7 || nrhs == 8)) {
        die("Usage: [H,...] = hist2d_reg_mex(X,Y,[Xmin Xmax],[Ymin Ymax],NbinsX,NbinsY,Step=1,IgnoreNaN=true)");
    }

    const mxArray* X  = prhs[0];
    const mxArray* Y  = prhs[1];
    const mxArray* XR = prhs[2];
    const mxArray* YR = prhs[3];
    const mxArray* NX = prhs[4];
    const mxArray* NY = prhs[5];
    const mxArray* ST = (nrhs >= 7) ? prhs[6] : NULL;
    const mxArray* IN = (nrhs >= 8) ? prhs[7] : NULL;

    const int ignoreNaN = parseBoolDefaultTrue(IN, "IgnoreNaN");

    if (mxIsSparse(X) || mxIsComplex(X) || mxIsSparse(Y) || mxIsComplex(Y))
        die("X and Y must be full and real.");
    mxClassID cX = mxGetClassID(X);
    mxClassID cY = mxGetClassID(Y);
    if (!((cX == mxSINGLE_CLASS || cX == mxDOUBLE_CLASS) && (cY == mxSINGLE_CLASS || cY == mxDOUBLE_CLASS)))
        die("X and Y must be single or double.");
    if (cX != cY) die("X and Y must have the same class (single or double).");

    const mwSize nElX = (mwSize)mxGetNumberOfElements(X);
    const mwSize nElY = (mwSize)mxGetNumberOfElements(Y);
    if (nElX != nElY) die("X and Y must have the same number of elements.");

    if (!mxIsNumeric(XR) || mxIsComplex(XR) || mxGetNumberOfElements(XR) != 2)
        die("[Xmin Xmax] must be a real numeric vector with 2 elements.");
    if (!mxIsNumeric(YR) || mxIsComplex(YR) || mxGetNumberOfElements(YR) != 2)
        die("[Ymin Ymax] must be a real numeric vector with 2 elements.");

    const double* xrp = mxGetPr(XR);
    const double* yrp = mxGetPr(YR);
    const double xmin = xrp[0], xmax = xrp[1];
    const double ymin = yrp[0], ymax = yrp[1];
    if (!mxIsFinite(xmin) || !mxIsFinite(xmax) || !(xmax > xmin)) die("Require finite Xmin < Xmax.");
    if (!mxIsFinite(ymin) || !mxIsFinite(ymax) || !(ymax > ymin)) die("Require finite Ymin < Ymax.");

    const mwSize nBinsX = parsePosIntScalar(NX, "NbinsX");
    const mwSize nBinsY = parsePosIntScalar(NY, "NbinsY");
    const mwSize step   = (ST ? parsePosIntScalar(ST, "Step") : (mwSize)1);

    const mwSize nCells = nBinsX * nBinsY;
    const double invWx  = (double)nBinsX / (xmax - xmin);
    const double invWy  = (double)nBinsY / (ymax - ymin);

    // Output H always
    plhs[0] = mxCreateDoubleMatrix(nBinsX, nBinsY, mxREAL);
    double* H = mxGetPr(plhs[0]);
    memset(H, 0, (size_t)nCells * sizeof(double));

    // Threads
    int maxThreads = 1;
#ifdef _OPENMP
    maxThreads = omp_get_max_threads();
#endif

    const int useStack = (nCells <= (mwSize)STACK_CELLS);
    const size_t bytesPerHist = (size_t)nCells * sizeof(uint32_t);
    const size_t totalBytes   = bytesPerHist * (size_t)maxThreads;
    const size_t limitBytes   = (size_t)PRIV_HIST_MAX_MB * (size_t)(1u << 20);
    const int usePrivHeap = (!useStack && maxThreads > 1 && totalBytes <= limitBytes);

    // Accumulate
    if (useStack && maxThreads > 1) {
        uint64_t* global = (uint64_t*)mxMalloc((size_t)nCells * sizeof(uint64_t));
        memset(global, 0, (size_t)nCells * sizeof(uint64_t));

        if (cX == mxDOUBLE_CLASS) {
            const double* xp = (const double*)mxGetData(X);
            const double* yp = (const double*)mxGetData(Y);
#ifdef _OPENMP
            #pragma omp parallel
#endif
            {
                uint32_t loc[STACK_CELLS];
                memset(loc, 0, (size_t)nCells * sizeof(uint32_t));

                if (ignoreNaN) {
#ifdef _OPENMP
                    #pragma omp for schedule(static)
#endif
                    for (ptrdiff_t i = 0; i < (ptrdiff_t)nElX; i += (ptrdiff_t)step) {
                        double x = xp[i], y = yp[i];
                        if ((x != x) || (y != y)) continue;
                        if (!inRange_fast(x, xmin, xmax) || !inRange_fast(y, ymin, ymax)) continue;
                        mwSize bx = bin_from_t(x, xmin, xmax, invWx, nBinsX);
                        mwSize by = bin_from_t(y, ymin, ymax, invWy, nBinsY);
                        if (bx == (mwSize)(-1) || by == (mwSize)(-1)) continue;
                        loc[bx + by * nBinsX] += 1u;
                    }
                } else {
#ifdef _OPENMP
                    #pragma omp for schedule(static)
#endif
                    for (ptrdiff_t i = 0; i < (ptrdiff_t)nElX; i += (ptrdiff_t)step) {
                        double x = xp[i], y = yp[i];
                        if (!inRange_nanSafe(x, xmin, xmax) || !inRange_nanSafe(y, ymin, ymax)) continue;
                        mwSize bx = bin_from_t(x, xmin, xmax, invWx, nBinsX);
                        mwSize by = bin_from_t(y, ymin, ymax, invWy, nBinsY);
                        if (bx == (mwSize)(-1) || by == (mwSize)(-1)) continue;
                        loc[bx + by * nBinsX] += 1u;
                    }
                }

#ifdef _OPENMP
                #pragma omp critical
#endif
                {
                    for (mwSize k = 0; k < nCells; ++k) global[k] += (uint64_t)loc[k];
                }
            }
        } else {
            const float* xp = (const float*)mxGetData(X);
            const float* yp = (const float*)mxGetData(Y);
#ifdef _OPENMP
            #pragma omp parallel
#endif
            {
                uint32_t loc[STACK_CELLS];
                memset(loc, 0, (size_t)nCells * sizeof(uint32_t));

                if (ignoreNaN) {
#ifdef _OPENMP
                    #pragma omp for schedule(static)
#endif
                    for (ptrdiff_t i = 0; i < (ptrdiff_t)nElX; i += (ptrdiff_t)step) {
                        double x = (double)xp[i], y = (double)yp[i];
                        if ((x != x) || (y != y)) continue;
                        if (!inRange_fast(x, xmin, xmax) || !inRange_fast(y, ymin, ymax)) continue;
                        mwSize bx = bin_from_t(x, xmin, xmax, invWx, nBinsX);
                        mwSize by = bin_from_t(y, ymin, ymax, invWy, nBinsY);
                        if (bx == (mwSize)(-1) || by == (mwSize)(-1)) continue;
                        loc[bx + by * nBinsX] += 1u;
                    }
                } else {
#ifdef _OPENMP
                    #pragma omp for schedule(static)
#endif
                    for (ptrdiff_t i = 0; i < (ptrdiff_t)nElX; i += (ptrdiff_t)step) {
                        double x = (double)xp[i], y = (double)yp[i];
                        if (!inRange_nanSafe(x, xmin, xmax) || !inRange_nanSafe(y, ymin, ymax)) continue;
                        mwSize bx = bin_from_t(x, xmin, xmax, invWx, nBinsX);
                        mwSize by = bin_from_t(y, ymin, ymax, invWy, nBinsY);
                        if (bx == (mwSize)(-1) || by == (mwSize)(-1)) continue;
                        loc[bx + by * nBinsX] += 1u;
                    }
                }

#ifdef _OPENMP
                #pragma omp critical
#endif
                {
                    for (mwSize k = 0; k < nCells; ++k) global[k] += (uint64_t)loc[k];
                }
            }
        }

        for (mwSize k = 0; k < nCells; ++k) H[k] = (double)global[k];
        mxFree(global);
    }
    else if (usePrivHeap) {
        uint32_t* all = (uint32_t*)mxMalloc((size_t)maxThreads * (size_t)nCells * sizeof(uint32_t));
        memset(all, 0, (size_t)maxThreads * (size_t)nCells * sizeof(uint32_t));

        if (cX == mxDOUBLE_CLASS) {
            const double* xp = (const double*)mxGetData(X);
            const double* yp = (const double*)mxGetData(Y);
#ifdef _OPENMP
            #pragma omp parallel
#endif
            {
#ifdef _OPENMP
                int tid = omp_get_thread_num();
#else
                int tid = 0;
#endif
                uint32_t* loc = all + (size_t)tid * (size_t)nCells;

                if (ignoreNaN) {
#ifdef _OPENMP
                    #pragma omp for schedule(static)
#endif
                    for (ptrdiff_t i = 0; i < (ptrdiff_t)nElX; i += (ptrdiff_t)step) {
                        double x = xp[i], y = yp[i];
                        if ((x != x) || (y != y)) continue;
                        if (!inRange_fast(x, xmin, xmax) || !inRange_fast(y, ymin, ymax)) continue;
                        mwSize bx = bin_from_t(x, xmin, xmax, invWx, nBinsX);
                        mwSize by = bin_from_t(y, ymin, ymax, invWy, nBinsY);
                        if (bx == (mwSize)(-1) || by == (mwSize)(-1)) continue;
                        loc[bx + by * nBinsX] += 1u;
                    }
                } else {
#ifdef _OPENMP
                    #pragma omp for schedule(static)
#endif
                    for (ptrdiff_t i = 0; i < (ptrdiff_t)nElX; i += (ptrdiff_t)step) {
                        double x = xp[i], y = yp[i];
                        if (!inRange_nanSafe(x, xmin, xmax) || !inRange_nanSafe(y, ymin, ymax)) continue;
                        mwSize bx = bin_from_t(x, xmin, xmax, invWx, nBinsX);
                        mwSize by = bin_from_t(y, ymin, ymax, invWy, nBinsY);
                        if (bx == (mwSize)(-1) || by == (mwSize)(-1)) continue;
                        loc[bx + by * nBinsX] += 1u;
                    }
                }
            }
        } else {
            const float* xp = (const float*)mxGetData(X);
            const float* yp = (const float*)mxGetData(Y);
#ifdef _OPENMP
            #pragma omp parallel
#endif
            {
#ifdef _OPENMP
                int tid = omp_get_thread_num();
#else
                int tid = 0;
#endif
                uint32_t* loc = all + (size_t)tid * (size_t)nCells;

                if (ignoreNaN) {
#ifdef _OPENMP
                    #pragma omp for schedule(static)
#endif
                    for (ptrdiff_t i = 0; i < (ptrdiff_t)nElX; i += (ptrdiff_t)step) {
                        double x = (double)xp[i], y = (double)yp[i];
                        if ((x != x) || (y != y)) continue;
                        if (!inRange_fast(x, xmin, xmax) || !inRange_fast(y, ymin, ymax)) continue;
                        mwSize bx = bin_from_t(x, xmin, xmax, invWx, nBinsX);
                        mwSize by = bin_from_t(y, ymin, ymax, invWy, nBinsY);
                        if (bx == (mwSize)(-1) || by == (mwSize)(-1)) continue;
                        loc[bx + by * nBinsX] += 1u;
                    }
                } else {
#ifdef _OPENMP
                    #pragma omp for schedule(static)
#endif
                    for (ptrdiff_t i = 0; i < (ptrdiff_t)nElX; i += (ptrdiff_t)step) {
                        double x = (double)xp[i], y = (double)yp[i];
                        if (!inRange_nanSafe(x, xmin, xmax) || !inRange_nanSafe(y, ymin, ymax)) continue;
                        mwSize bx = bin_from_t(x, xmin, xmax, invWx, nBinsX);
                        mwSize by = bin_from_t(y, ymin, ymax, invWy, nBinsY);
                        if (bx == (mwSize)(-1) || by == (mwSize)(-1)) continue;
                        loc[bx + by * nBinsX] += 1u;
                    }
                }
            }
        }

        for (mwSize k = 0; k < nCells; ++k) {
            uint64_t s = 0;
            for (int t = 0; t < maxThreads; ++t) s += (uint64_t)all[(size_t)t * (size_t)nCells + (size_t)k];
            H[k] = (double)s;
        }
        mxFree(all);
    }
    else {
        uint32_t* counts = (uint32_t*)mxMalloc((size_t)nCells * sizeof(uint32_t));
        memset(counts, 0, (size_t)nCells * sizeof(uint32_t));

        if (cX == mxDOUBLE_CLASS) {
            const double* xp = (const double*)mxGetData(X);
            const double* yp = (const double*)mxGetData(Y);

            if (ignoreNaN) {
                for (mwSize i = 0; i < nElX; i += step) {
                    double x = xp[i], y = yp[i];
                    if ((x != x) || (y != y)) continue;
                    if (!inRange_fast(x, xmin, xmax) || !inRange_fast(y, ymin, ymax)) continue;
                    mwSize bx = bin_from_t(x, xmin, xmax, invWx, nBinsX);
                    mwSize by = bin_from_t(y, ymin, ymax, invWy, nBinsY);
                    if (bx == (mwSize)(-1) || by == (mwSize)(-1)) continue;
                    counts[bx + by * nBinsX] += 1u;
                }
            } else {
                for (mwSize i = 0; i < nElX; i += step) {
                    double x = xp[i], y = yp[i];
                    if (!inRange_nanSafe(x, xmin, xmax) || !inRange_nanSafe(y, ymin, ymax)) continue;
                    mwSize bx = bin_from_t(x, xmin, xmax, invWx, nBinsX);
                    mwSize by = bin_from_t(y, ymin, ymax, invWy, nBinsY);
                    if (bx == (mwSize)(-1) || by == (mwSize)(-1)) continue;
                    counts[bx + by * nBinsX] += 1u;
                }
            }
        } else {
            const float* xp = (const float*)mxGetData(X);
            const float* yp = (const float*)mxGetData(Y);

            if (ignoreNaN) {
                for (mwSize i = 0; i < nElX; i += step) {
                    double x = (double)xp[i], y = (double)yp[i];
                    if ((x != x) || (y != y)) continue;
                    if (!inRange_fast(x, xmin, xmax) || !inRange_fast(y, ymin, ymax)) continue;
                    mwSize bx = bin_from_t(x, xmin, xmax, invWx, nBinsX);
                    mwSize by = bin_from_t(y, ymin, ymax, invWy, nBinsY);
                    if (bx == (mwSize)(-1) || by == (mwSize)(-1)) continue;
                    counts[bx + by * nBinsX] += 1u;
                }
            } else {
                for (mwSize i = 0; i < nElX; i += step) {
                    double x = (double)xp[i], y = (double)yp[i];
                    if (!inRange_nanSafe(x, xmin, xmax) || !inRange_nanSafe(y, ymin, ymax)) continue;
                    mwSize bx = bin_from_t(x, xmin, xmax, invWx, nBinsX);
                    mwSize by = bin_from_t(y, ymin, ymax, invWy, nBinsY);
                    if (bx == (mwSize)(-1) || by == (mwSize)(-1)) continue;
                    counts[bx + by * nBinsX] += 1u;
                }
            }
        }

        for (mwSize k = 0; k < nCells; ++k) H[k] = (double)counts[k];
        mxFree(counts);
    }

    // Optional outputs
    if (nlhs >= 2) {
        plhs[1] = mxCreateDoubleMatrix(nBinsX + 1, 1, mxREAL);
        double* Xedges = mxGetPr(plhs[1]);
        const double wx = (xmax - xmin) / (double)nBinsX;
        for (mwSize k = 0; k <= nBinsX; ++k) Xedges[k] = xmin + (double)k * wx;
    }
    if (nlhs >= 3) {
        plhs[2] = mxCreateDoubleMatrix(nBinsY + 1, 1, mxREAL);
        double* Yedges = mxGetPr(plhs[2]);
        const double wy = (ymax - ymin) / (double)nBinsY;
        for (mwSize k = 0; k <= nBinsY; ++k) Yedges[k] = ymin + (double)k * wy;
    }
    if (nlhs >= 4) {
        plhs[3] = mxCreateDoubleMatrix(nBinsX, 1, mxREAL);
        double* Xcen = mxGetPr(plhs[3]);
        const double wx = (xmax - xmin) / (double)nBinsX;
        const double base = xmin + 0.5 * wx;
        for (mwSize k = 0; k < nBinsX; ++k) Xcen[k] = base + (double)k * wx;
    }
    if (nlhs >= 5) {
        plhs[4] = mxCreateDoubleMatrix(nBinsY, 1, mxREAL);
        double* Ycen = mxGetPr(plhs[4]);
        const double wy = (ymax - ymin) / (double)nBinsY;
        const double base = ymin + 0.5 * wy;
        for (mwSize k = 0; k < nBinsY; ++k) Ycen[k] = base + (double)k * wy;
    }
}
