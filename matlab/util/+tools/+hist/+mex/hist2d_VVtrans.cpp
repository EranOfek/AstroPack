// hist2d_VVtrans_mex.c
//
// [Hist2D, EdgesX, EdgesY, BinCenterX, BinCenterY] = hist2d_VVtrans_mex( ...
//     Xcat, Ycat, Xref, Yref, FlipX, FlipY, RangeX, StepX, RangeY, StepY)
//
// Computes 2D histogram of pairwise translated differences without materializing Dx/Dy:
//   dx = Xcat(i) - FlipX * Xref(j)
//   dy = Ycat(i) - FlipY * Yref(j)
// and bins all (i,j) pairs into regular bins defined by
//   EdgesX = RangeX(1):StepX:RangeX(2)
//   EdgesY = RangeY(1):StepY:RangeY(2)
// matching histcounts2 inclusion rules: [left,right) except last includes right edge.
//
// Inputs:
//   Xcat,Ycat : column vectors (single or double), same length Nc
//   Xref,Yref : column vectors (single or double), same length Nr
//   FlipX,FlipY : scalar numeric/logical (typically +1 or -1)
//   RangeX : [xmin xmax] (2 elements, finite, xmax>xmin)
//   StepX  : positive scalar
//   RangeY : [ymin ymax] (2 elements, finite, ymax>ymin)
//   StepY  : positive scalar
//
// Outputs:
//   Hist2D     : NbinsX x NbinsY double
//   EdgesX     : (NbinsX+1) x 1 double  (optional)
//   EdgesY     : (NbinsY+1) x 1 double  (optional)
//   BinCenterX : NbinsX x 1 double      (optional)
//   BinCenterY : NbinsY x 1 double      (optional)
//
// Notes:
// - NaNs are ignored (like histcounts2) via NaN-safe comparisons.
// - No intermediate Dx/Dy allocation; exactly one nested loop over (i,j).
//
// Build (Linux):
//   mex -R2018a CFLAGS="\$CFLAGS -O3 -march=native -DNDEBUG -fopenmp" \
//              LDFLAGS="\$LDFLAGS -fopenmp" hist2d_VVtrans_mex.c
//

#include "mex.h"
#include <stdint.h>
#include <string.h>
#include <math.h>

#ifdef _OPENMP
#include <omp.h>
#endif

static void die(const char* msg) { mexErrMsgIdAndTxt("hist2d_VVtrans_mex:err", "%s", msg); }

static mwSize parsePosIntFromDouble(double v, const char* name) {
    if (!mxIsFinite(v) || v <= 0.0) mexErrMsgIdAndTxt("hist2d_VVtrans_mex:err", "%s must be > 0.", name);
    mwSize iv = (mwSize)v;
    if ((double)iv != v) mexErrMsgIdAndTxt("hist2d_VVtrans_mex:err", "%s must be an integer.", name);
    return iv;
}

static double parseScalarDouble(const mxArray* a, const char* name) {
    if (!mxIsNumeric(a) || mxIsComplex(a) || mxGetNumberOfElements(a) != 1)
        mexErrMsgIdAndTxt("hist2d_VVtrans_mex:err", "%s must be a real numeric scalar.", name);
    double v = mxGetScalar(a);
    if (!mxIsFinite(v)) mexErrMsgIdAndTxt("hist2d_VVtrans_mex:err", "%s must be finite.", name);
    return v;
}

static void parseRange2(const mxArray* a, const char* name, double* lo, double* hi) {
    if (!mxIsNumeric(a) || mxIsComplex(a) || mxGetNumberOfElements(a) != 2)
        mexErrMsgIdAndTxt("hist2d_VVtrans_mex:err", "%s must be a real numeric vector with 2 elements.", name);
    const double* p = mxGetPr(a);
    *lo = p[0];
    *hi = p[1];
    if (!mxIsFinite(*lo) || !mxIsFinite(*hi) || !(*hi > *lo))
        mexErrMsgIdAndTxt("hist2d_VVtrans_mex:err", "%s must satisfy finite lo < hi.", name);
}

static mwSize computeNbins(double lo, double hi, double step, const char* name) {
    if (!(step > 0.0) || !mxIsFinite(step))
        mexErrMsgIdAndTxt("hist2d_VVtrans_mex:err", "%s must be finite and > 0.", name);

    // Expect regular grid: hi ~= lo + n*step
    double nb = (hi - lo) / step;
    if (!(nb > 0.0) || !mxIsFinite(nb))
        mexErrMsgIdAndTxt("hist2d_VVtrans_mex:err", "%s invalid with given range.", name);

    // round-to-nearest with tolerance
    double nb_r = floor(nb + 0.5);
    if (fabs(nb - nb_r) > 1e-9 * (1.0 + fabs(nb_r))) {
        // still allow floor behavior, but warn-ish via error (safer correctness)
        mexErrMsgIdAndTxt("hist2d_VVtrans_mex:err",
                          "%s does not evenly divide range (need (hi-lo)/step integer).", name);
    }
    mwSize nBins = (mwSize)nb_r;
    if (nBins < 1) mexErrMsgIdAndTxt("hist2d_VVtrans_mex:err", "Computed %s bins < 1.", name);
    return nBins;
}

static inline int bin1_nanSafe(double v, double lo, double hi, double invStep, mwSize nBins) {
    // NaN-safe range test: comparisons are false for NaN -> reject.
    if (!(v >= lo && v <= hi)) return -1;

    double t = (v - lo) * invStep;      // ideally [0..nBins]
    mwSize b = (mwSize)t;               // trunc
    if (b >= nBins) {
        // only allow exact right edge into last bin
        if (v == hi) return (int)(nBins - 1);
        return -1;
    }
    return (int)b;
}

// --- Tunables (similar spirit to your 1D code) ---
#ifndef STACK_BINS_2D
#define STACK_BINS_2D 4096u   // per-thread stack hist when total bins <= this
#endif

#ifndef PRIV_HIST_MAX_MB_2D
#define PRIV_HIST_MAX_MB_2D 256u // per-thread heap privatization if total <= this
#endif

#define IDX(bx, by, nBinsX) ((size_t)(bx) + (size_t)(by) * (size_t)(nBinsX))

template <typename T>
static void hist2d_core(const T* xcat, const T* ycat, mwSize Nc,
                        const T* xref, const T* yref, mwSize Nr,
                        double flipX, double flipY,
                        double x0, double x1, double invStepX, mwSize nBinsX,
                        double y0, double y1, double invStepY, mwSize nBinsY,
                        double* outD)
{
    const size_t nTot = (size_t)nBinsX * (size_t)nBinsY;

    int maxThreads = 1;
#ifdef _OPENMP
    maxThreads = omp_get_max_threads();
#endif

    const int useStack = (nTot <= (size_t)STACK_BINS_2D);
    const size_t bytesPerHist = nTot * sizeof(uint32_t);
    const size_t totalBytes = bytesPerHist * (size_t)maxThreads;
    const size_t limitBytes = (size_t)PRIV_HIST_MAX_MB_2D * (size_t)(1u << 20);
    const int usePrivHeap = (!useStack && maxThreads > 1 && totalBytes <= limitBytes);

    // ----------------------------
    // (D2) stack per-thread hist
    // ----------------------------
    if (useStack && maxThreads > 1) {
        uint64_t* global = (uint64_t*)mxMalloc(nTot * sizeof(uint64_t));
        memset(global, 0, nTot * sizeof(uint64_t));

#ifdef _OPENMP
        #pragma omp parallel
#endif
        {
            uint32_t loc[STACK_BINS_2D];
            memset(loc, 0, nTot * sizeof(uint32_t));

#ifdef _OPENMP
            #pragma omp for schedule(static)
#endif
            for (ptrdiff_t i = 0; i < (ptrdiff_t)Nc; ++i) {
                double xc = (double)xcat[i];
                double yc = (double)ycat[i];

                // NaN-safe reject: if either is NaN, skip i entirely
                if (!(xc == xc) || !(yc == yc)) continue;

                for (mwSize j = 0; j < Nr; ++j) {
                    double dx = xc - flipX * (double)xref[j];
                    double dy = yc - flipY * (double)yref[j];

                    int bx = bin1_nanSafe(dx, x0, x1, invStepX, nBinsX);
                    if (bx < 0) continue;
                    int by = bin1_nanSafe(dy, y0, y1, invStepY, nBinsY);
                    if (by < 0) continue;

                    loc[IDX(bx, by, nBinsX)] += 1u;
                }
            }

#ifdef _OPENMP
            #pragma omp critical
#endif
            {
                for (size_t k = 0; k < nTot; ++k) global[k] += (uint64_t)loc[k];
            }
        }

        for (size_t k = 0; k < nTot; ++k) outD[k] = (double)global[k];
        mxFree(global);
        return;
    }

    // ----------------------------------------
    // (A2,B2,C2) heap per-thread privatization
    // ----------------------------------------
    if (usePrivHeap) {
        uint32_t* all = (uint32_t*)mxMalloc((size_t)maxThreads * nTot * sizeof(uint32_t));
        memset(all, 0, (size_t)maxThreads * nTot * sizeof(uint32_t));

#ifdef _OPENMP
        #pragma omp parallel
#endif
        {
#ifdef _OPENMP
            int tid = omp_get_thread_num();
#else
            int tid = 0;
#endif
            uint32_t* loc = all + (size_t)tid * nTot;

#ifdef _OPENMP
            #pragma omp for schedule(static)
#endif
            for (ptrdiff_t i = 0; i < (ptrdiff_t)Nc; ++i) {
                double xc = (double)xcat[i];
                double yc = (double)ycat[i];
                if (!(xc == xc) || !(yc == yc)) continue;

                for (mwSize j = 0; j < Nr; ++j) {
                    double dx = xc - flipX * (double)xref[j];
                    double dy = yc - flipY * (double)yref[j];

                    int bx = bin1_nanSafe(dx, x0, x1, invStepX, nBinsX);
                    if (bx < 0) continue;
                    int by = bin1_nanSafe(dy, y0, y1, invStepY, nBinsY);
                    if (by < 0) continue;

                    loc[IDX(bx, by, nBinsX)] += 1u;
                }
            }
        }

        // reduce
        for (size_t k = 0; k < nTot; ++k) {
            uint64_t s = 0;
            for (int t = 0; t < maxThreads; ++t) s += (uint64_t)all[(size_t)t * nTot + k];
            outD[k] = (double)s;
        }
        mxFree(all);
        return;
    }

    // ----------------------------
    // fallback: single-thread
    // ----------------------------
    uint32_t* counts = (uint32_t*)mxMalloc(nTot * sizeof(uint32_t));
    memset(counts, 0, nTot * sizeof(uint32_t));

    for (mwSize i = 0; i < Nc; ++i) {
        double xc = (double)xcat[i];
        double yc = (double)ycat[i];
        if (!(xc == xc) || !(yc == yc)) continue;

        for (mwSize j = 0; j < Nr; ++j) {
            double dx = xc - flipX * (double)xref[j];
            double dy = yc - flipY * (double)yref[j];

            int bx = bin1_nanSafe(dx, x0, x1, invStepX, nBinsX);
            if (bx < 0) continue;
            int by = bin1_nanSafe(dy, y0, y1, invStepY, nBinsY);
            if (by < 0) continue;

            counts[IDX(bx, by, nBinsX)] += 1u;
        }
    }

    for (size_t k = 0; k < nTot; ++k) outD[k] = (double)counts[k];
    mxFree(counts);
}

void mexFunction(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[])
{
    if (nrhs != 10)
        die("Usage: [H,Ex,Ey,Cx,Cy]=hist2d_VVtrans_mex(Xcat,Ycat,Xref,Yref,FlipX,FlipY,RangeX,StepX,RangeY,StepY)");
    if (nlhs < 1 || nlhs > 5)
        die("Outputs: H required; Ex/Ey/Cx/Cy optional.");

    const mxArray* Xcat = prhs[0];
    const mxArray* Ycat = prhs[1];
    const mxArray* Xref = prhs[2];
    const mxArray* Yref = prhs[3];

    if (mxIsSparse(Xcat) || mxIsSparse(Ycat) || mxIsSparse(Xref) || mxIsSparse(Yref)) die("Inputs must be full.");
    if (mxIsComplex(Xcat) || mxIsComplex(Ycat) || mxIsComplex(Xref) || mxIsComplex(Yref)) die("Inputs must be real.");
    if (mxGetNumberOfElements(Xcat) != mxGetNumberOfElements(Ycat)) die("Xcat and Ycat must have same length.");
    if (mxGetNumberOfElements(Xref) != mxGetNumberOfElements(Yref)) die("Xref and Yref must have same length.");

    mxClassID cid = mxGetClassID(Xcat);
    if (!(cid == mxSINGLE_CLASS || cid == mxDOUBLE_CLASS)) die("Xcat/Ycat must be single or double.");
    if (mxGetClassID(Ycat) != cid || mxGetClassID(Xref) != cid || mxGetClassID(Yref) != cid)
        die("All coordinate inputs must have the same class (all single or all double).");

    const mwSize Nc = (mwSize)mxGetNumberOfElements(Xcat);
    const mwSize Nr = (mwSize)mxGetNumberOfElements(Xref);

    // Flips
    double flipX = parseScalarDouble(prhs[4], "FlipX");
    double flipY = parseScalarDouble(prhs[5], "FlipY");

    // Ranges and steps
    double x0, x1, y0, y1;
    parseRange2(prhs[6], "RangeX", &x0, &x1);
    double stepX = parseScalarDouble(prhs[7], "StepX");
    parseRange2(prhs[8], "RangeY", &y0, &y1);
    double stepY = parseScalarDouble(prhs[9], "StepY");

    const mwSize nBinsX = computeNbins(x0, x1, stepX, "NbinsX");
    const mwSize nBinsY = computeNbins(y0, y1, stepY, "NbinsY");

    const double invStepX = 1.0 / stepX;
    const double invStepY = 1.0 / stepY;

    // Output histogram: NbinsX x NbinsY (like histcounts2)
    plhs[0] = mxCreateDoubleMatrix(nBinsX, nBinsY, mxREAL);
    double* H = mxGetPr(plhs[0]);
    memset(H, 0, (size_t)nBinsX * (size_t)nBinsY * sizeof(double));

    if (cid == mxDOUBLE_CLASS) {
        const double* xcat = (const double*)mxGetData(Xcat);
        const double* ycat = (const double*)mxGetData(Ycat);
        const double* xref = (const double*)mxGetData(Xref);
        const double* yref = (const double*)mxGetData(Yref);
        hist2d_core<double>(xcat, ycat, Nc, xref, yref, Nr,
                            flipX, flipY,
                            x0, x1, invStepX, nBinsX,
                            y0, y1, invStepY, nBinsY,
                            H);
    } else {
        const float* xcat = (const float*)mxGetData(Xcat);
        const float* ycat = (const float*)mxGetData(Ycat);
        const float* xref = (const float*)mxGetData(Xref);
        const float* yref = (const float*)mxGetData(Yref);
        hist2d_core<float>(xcat, ycat, Nc, xref, yref, Nr,
                           flipX, flipY,
                           x0, x1, invStepX, nBinsX,
                           y0, y1, invStepY, nBinsY,
                           H);
    }

    // Optional outputs: edges and centers (column vectors)
    if (nlhs >= 2) {
        plhs[1] = mxCreateDoubleMatrix(nBinsX + 1, 1, mxREAL);
        double* ex = mxGetPr(plhs[1]);
        for (mwSize k = 0; k <= nBinsX; ++k) ex[k] = x0 + (double)k * stepX;
    }
    if (nlhs >= 3) {
        plhs[2] = mxCreateDoubleMatrix(nBinsY + 1, 1, mxREAL);
        double* ey = mxGetPr(plhs[2]);
        for (mwSize k = 0; k <= nBinsY; ++k) ey[k] = y0 + (double)k * stepY;
    }
    if (nlhs >= 4) {
        plhs[3] = mxCreateDoubleMatrix(nBinsX, 1, mxREAL);
        double* cx = mxGetPr(plhs[3]);
        double base = x0 + 0.5 * stepX;
        for (mwSize k = 0; k < nBinsX; ++k) cx[k] = base + (double)k * stepX;
    }
    if (nlhs >= 5) {
        plhs[4] = mxCreateDoubleMatrix(nBinsY, 1, mxREAL);
        double* cy = mxGetPr(plhs[4]);
        double base = y0 + 0.5 * stepY;
        for (mwSize k = 0; k < nBinsY; ++k) cy[k] = base + (double)k * stepY;
    }
}
