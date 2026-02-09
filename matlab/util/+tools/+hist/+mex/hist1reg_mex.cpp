// hist1d_reg_mex.c
//
// [N,Edges,Centers] = hist1d_reg_mex(X, [Xstart Xend], Nbins, Step=1, CheckNaN=true)
//
// Fast regular-grid 1D histogram over linearized X(1:Step:end).
// X: single/double, full, real
// Range: [Xstart Xend], finite, Xend>Xstart
// Nbins: positive integer
// Step: positive integer, default 1
// CheckNaN: logical/0/1, default true.
//   - If true: reject NaNs explicitly using (x!=x) in the hot loop (cheap).
//   - If false: NO explicit NaN check, but we use a NaN-safe range test:
//         if (!(x >= x0 && x <= x1)) continue;
//     This rejects NaN (comparisons are false) and rejects +/-Inf automatically.
//
// Outputs:
//   N       : 1 x Nbins double counts  (row vector; like MATLAB)
//   Edges   : (Nbins+1) x 1 double (optional)
//   Centers : Nbins x 1 double (optional)
//
// Performance features:
//   (A) OpenMP + per-thread privatization (no atomics in hot loop)
//   (B) uint32 local counts, uint64 reduction, output double
//   (C) mxMalloc + memset (no mxCalloc)
//   (D) small-Nbins branch: per-thread stack histogram cache + merge
//
// Build (Linux):
//   mex -R2018a CFLAGS="\$CFLAGS -O3 -march=native -DNDEBUG -fopenmp" \
//              LDFLAGS="\$LDFLAGS -fopenmp" hist1d_reg_mex.c
//

#include "mex.h"
#include <stdint.h>
#include <string.h>
#include <math.h>

#ifdef _OPENMP
#include <omp.h>
#endif

static void die(const char* msg) { mexErrMsgIdAndTxt("hist1d_reg_mex:err", "%s", msg); }

static mwSize parsePosIntScalar(const mxArray* a, const char* name) {
    if (!mxIsNumeric(a) || mxIsComplex(a) || mxGetNumberOfElements(a) != 1)
        mexErrMsgIdAndTxt("hist1d_reg_mex:err", "%s must be a real numeric scalar.", name);
    double v = mxGetScalar(a);
    if (!mxIsFinite(v) || v < 1.0)
        mexErrMsgIdAndTxt("hist1d_reg_mex:err", "%s must be >= 1.", name);
    mwSize iv = (mwSize)v;
    if ((double)iv != v)
        mexErrMsgIdAndTxt("hist1d_reg_mex:err", "%s must be an integer.", name);
    return iv;
}

static int parseBoolScalarDefaultTrue(const mxArray* a, const char* name) {
    if (a == NULL) return 1;
    if (!(mxIsLogical(a) || mxIsNumeric(a)) || mxIsComplex(a) || mxGetNumberOfElements(a) != 1)
        mexErrMsgIdAndTxt("hist1d_reg_mex:err", "%s must be a logical/numeric scalar.", name);
    double v = mxGetScalar(a);
    return (v != 0.0);
}

static inline mwSize binOf_nanSafeRange(double x, double x0, double x1, double invW, mwSize nBins) {
    // NaN-safe range test: rejects NaN because comparisons are false.
    if (!(x >= x0 && x <= x1)) return (mwSize)(-1);

    double t = (x - x0) * invW;      // ideally [0..nBins]
    mwSize b = (mwSize)t;            // trunc == floor since t>=0 (since x>=x0)
    if (b >= nBins) {
        // only allow exact right edge into last bin
        if (x == x1) return nBins - 1;
        return (mwSize)(-1);
    }
    return b;
}

static inline mwSize binOf_fastRange(double x, double x0, double x1, double invW, mwSize nBins) {
    // Faster range test (NOT NaN-safe): caller must reject NaN before calling.
    if (x < x0 || x > x1) return (mwSize)(-1);

    double t = (x - x0) * invW;      // ideally [0..nBins]
    mwSize b = (mwSize)t;            // trunc == floor since t>=0
    if (b >= nBins) {
        if (x == x1) return nBins - 1;
        return (mwSize)(-1);
    }
    return b;
}

// --- Tunables for branch (D) ---
#ifndef STACK_BINS
#define STACK_BINS 4096u   // per-thread stack cache used when Nbins <= STACK_BINS
#endif

#ifndef PRIV_HIST_MAX_MB
#define PRIV_HIST_MAX_MB 256u // per-thread heap privatization used if total <= this
#endif

void mexFunction(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[])
{
    if (!(nrhs == 3 || nrhs == 4 || nrhs == 5)) {
        die("Usage: [N,Edges,Centers] = hist1d_reg_mex(X,[Xstart Xend],Nbins,Step=1,CheckNaN=true)");
    }
    if (nlhs < 1 || nlhs > 3) die("Outputs: N required; Edges/Centers optional.");

    const mxArray* X  = prhs[0];
    const mxArray* R  = prhs[1];
    const mxArray* NB = prhs[2];
    const mxArray* ST = (nrhs >= 4) ? prhs[3] : NULL;
    const mxArray* CN = (nrhs >= 5) ? prhs[4] : NULL;

    const int checkNaN = parseBoolScalarDefaultTrue(CN, "CheckNaN");

    if (mxIsSparse(X) || mxIsComplex(X)) die("X must be full and real.");
    mxClassID cid = mxGetClassID(X);
    if (!(cid == mxSINGLE_CLASS || cid == mxDOUBLE_CLASS)) die("X must be single or double.");

    if (!mxIsNumeric(R) || mxIsComplex(R) || mxGetNumberOfElements(R) != 2)
        die("[Xstart Xend] must be a real numeric vector with 2 elements.");

    const double* rp = mxGetPr(R);
    const double x0 = rp[0];
    const double x1 = rp[1];
    if (!mxIsFinite(x0) || !mxIsFinite(x1) || !(x1 > x0)) die("Require finite Xstart < Xend.");

    const mwSize nBins = parsePosIntScalar(NB, "Nbins");
    const mwSize step  = (ST ? parsePosIntScalar(ST, "Step") : (mwSize)1);

    const mwSize nEl = (mwSize)mxGetNumberOfElements(X);
    const double invW = (double)nBins / (x1 - x0);

    // Output N as double ROW (1 x Nbins)
    plhs[0] = mxCreateDoubleMatrix(1, nBins, mxREAL);
    double* outD = mxGetPr(plhs[0]);
    memset(outD, 0, (size_t)nBins * sizeof(double));

    // Determine threads
    int maxThreads = 1;
#ifdef _OPENMP
    maxThreads = omp_get_max_threads();
#endif

    // Branch selection
    const int useStack = (nBins <= (mwSize)STACK_BINS);

    const size_t bytesPerHist = (size_t)nBins * sizeof(uint32_t);
    const size_t totalBytes = bytesPerHist * (size_t)maxThreads;
    const size_t limitBytes = (size_t)PRIV_HIST_MAX_MB * (size_t)(1u << 20);

    const int usePrivHeap = (!useStack && maxThreads > 1 && totalBytes <= limitBytes);

    // ------------------------------------------------------------
    // (D) small-Nbins: stack local hist per thread, then merge
    // ------------------------------------------------------------
    if (useStack && maxThreads > 1) {
        uint64_t* global = (uint64_t*)mxMalloc((size_t)nBins * sizeof(uint64_t));
        memset(global, 0, (size_t)nBins * sizeof(uint64_t));

        if (cid == mxDOUBLE_CLASS) {
            const double* xp = (const double*)mxGetData(X);
#ifdef _OPENMP
            #pragma omp parallel
#endif
            {
                uint32_t loc[STACK_BINS];
                memset(loc, 0, (size_t)nBins * sizeof(uint32_t));

                if (checkNaN) {
#ifdef _OPENMP
                    #pragma omp for schedule(static)
#endif
                    for (ptrdiff_t i = 0; i < (ptrdiff_t)nEl; i += (ptrdiff_t)step) {
                        double x = xp[i];
                        if (x != x) continue; // reject NaN
                        mwSize b = binOf_fastRange(x, x0, x1, invW, nBins);
                        if (b != (mwSize)(-1)) loc[b] += 1u;
                    }
                } else {
#ifdef _OPENMP
                    #pragma omp for schedule(static)
#endif
                    for (ptrdiff_t i = 0; i < (ptrdiff_t)nEl; i += (ptrdiff_t)step) {
                        double x = xp[i];
                        mwSize b = binOf_nanSafeRange(x, x0, x1, invW, nBins);
                        if (b != (mwSize)(-1)) loc[b] += 1u;
                    }
                }

#ifdef _OPENMP
                #pragma omp critical
#endif
                {
                    for (mwSize b = 0; b < nBins; ++b) global[b] += (uint64_t)loc[b];
                }
            }
        } else {
            const float* xp = (const float*)mxGetData(X);
#ifdef _OPENMP
            #pragma omp parallel
#endif
            {
                uint32_t loc[STACK_BINS];
                memset(loc, 0, (size_t)nBins * sizeof(uint32_t));

                if (checkNaN) {
#ifdef _OPENMP
                    #pragma omp for schedule(static)
#endif
                    for (ptrdiff_t i = 0; i < (ptrdiff_t)nEl; i += (ptrdiff_t)step) {
                        double x = (double)xp[i];
                        if (x != x) continue; // reject NaN
                        mwSize b = binOf_fastRange(x, x0, x1, invW, nBins);
                        if (b != (mwSize)(-1)) loc[b] += 1u;
                    }
                } else {
#ifdef _OPENMP
                    #pragma omp for schedule(static)
#endif
                    for (ptrdiff_t i = 0; i < (ptrdiff_t)nEl; i += (ptrdiff_t)step) {
                        double x = (double)xp[i];
                        mwSize b = binOf_nanSafeRange(x, x0, x1, invW, nBins);
                        if (b != (mwSize)(-1)) loc[b] += 1u;
                    }
                }

#ifdef _OPENMP
                #pragma omp critical
#endif
                {
                    for (mwSize b = 0; b < nBins; ++b) global[b] += (uint64_t)loc[b];
                }
            }
        }

        for (mwSize b = 0; b < nBins; ++b) outD[b] = (double)global[b];
        mxFree(global);
    }
    // ------------------------------------------------------------
    // (A,B,C) heap privatization for moderate/large nBins
    // ------------------------------------------------------------
    else if (usePrivHeap) {
        uint32_t* all = (uint32_t*)mxMalloc((size_t)maxThreads * (size_t)nBins * sizeof(uint32_t));
        memset(all, 0, (size_t)maxThreads * (size_t)nBins * sizeof(uint32_t));

        if (cid == mxDOUBLE_CLASS) {
            const double* xp = (const double*)mxGetData(X);
#ifdef _OPENMP
            #pragma omp parallel
#endif
            {
#ifdef _OPENMP
                int tid = omp_get_thread_num();
#else
                int tid = 0;
#endif
                uint32_t* loc = all + (size_t)tid * (size_t)nBins;

                if (checkNaN) {
#ifdef _OPENMP
                    #pragma omp for schedule(static)
#endif
                    for (ptrdiff_t i = 0; i < (ptrdiff_t)nEl; i += (ptrdiff_t)step) {
                        double x = xp[i];
                        if (x != x) continue;
                        mwSize b = binOf_fastRange(x, x0, x1, invW, nBins);
                        if (b != (mwSize)(-1)) loc[b] += 1u;
                    }
                } else {
#ifdef _OPENMP
                    #pragma omp for schedule(static)
#endif
                    for (ptrdiff_t i = 0; i < (ptrdiff_t)nEl; i += (ptrdiff_t)step) {
                        double x = xp[i];
                        mwSize b = binOf_nanSafeRange(x, x0, x1, invW, nBins);
                        if (b != (mwSize)(-1)) loc[b] += 1u;
                    }
                }
            }
        } else {
            const float* xp = (const float*)mxGetData(X);
#ifdef _OPENMP
            #pragma omp parallel
#endif
            {
#ifdef _OPENMP
                int tid = omp_get_thread_num();
#else
                int tid = 0;
#endif
                uint32_t* loc = all + (size_t)tid * (size_t)nBins;

                if (checkNaN) {
#ifdef _OPENMP
                    #pragma omp for schedule(static)
#endif
                    for (ptrdiff_t i = 0; i < (ptrdiff_t)nEl; i += (ptrdiff_t)step) {
                        double x = (double)xp[i];
                        if (x != x) continue;
                        mwSize b = binOf_fastRange(x, x0, x1, invW, nBins);
                        if (b != (mwSize)(-1)) loc[b] += 1u;
                    }
                } else {
#ifdef _OPENMP
                    #pragma omp for schedule(static)
#endif
                    for (ptrdiff_t i = 0; i < (ptrdiff_t)nEl; i += (ptrdiff_t)step) {
                        double x = (double)xp[i];
                        mwSize b = binOf_nanSafeRange(x, x0, x1, invW, nBins);
                        if (b != (mwSize)(-1)) loc[b] += 1u;
                    }
                }
            }
        }

        for (mwSize b = 0; b < nBins; ++b) {
            uint64_t s = 0;
            for (int t = 0; t < maxThreads; ++t) {
                s += (uint64_t)all[(size_t)t * (size_t)nBins + (size_t)b];
            }
            outD[b] = (double)s;
        }

        mxFree(all);
    }
    // ------------------------------------------------------------
    // Fallback: single-thread
    // ------------------------------------------------------------
    else {
        uint32_t* counts = (uint32_t*)mxMalloc((size_t)nBins * sizeof(uint32_t));
        memset(counts, 0, (size_t)nBins * sizeof(uint32_t));

        if (cid == mxDOUBLE_CLASS) {
            const double* xp = (const double*)mxGetData(X);

            if (checkNaN) {
                for (mwSize i = 0; i < nEl; i += step) {
                    double x = xp[i];
                    if (x != x) continue;
                    mwSize b = binOf_fastRange(x, x0, x1, invW, nBins);
                    if (b != (mwSize)(-1)) counts[b] += 1u;
                }
            } else {
                for (mwSize i = 0; i < nEl; i += step) {
                    double x = xp[i];
                    mwSize b = binOf_nanSafeRange(x, x0, x1, invW, nBins);
                    if (b != (mwSize)(-1)) counts[b] += 1u;
                }
            }
        } else {
            const float* xp = (const float*)mxGetData(X);

            if (checkNaN) {
                for (mwSize i = 0; i < nEl; i += step) {
                    double x = (double)xp[i];
                    if (x != x) continue;
                    mwSize b = binOf_fastRange(x, x0, x1, invW, nBins);
                    if (b != (mwSize)(-1)) counts[b] += 1u;
                }
            } else {
                for (mwSize i = 0; i < nEl; i += step) {
                    double x = (double)xp[i];
                    mwSize b = binOf_nanSafeRange(x, x0, x1, invW, nBins);
                    if (b != (mwSize)(-1)) counts[b] += 1u;
                }
            }
        }

        for (mwSize b = 0; b < nBins; ++b) outD[b] = (double)counts[b];
        mxFree(counts);
    }

    // Optional edges/centers (keep as column vectors)
    if (nlhs >= 2) {
        plhs[1] = mxCreateDoubleMatrix(nBins + 1, 1, mxREAL);
        double* e = mxGetPr(plhs[1]);
        const double w = (x1 - x0) / (double)nBins;
        for (mwSize k = 0; k <= nBins; ++k) e[k] = x0 + (double)k * w;
    }
    if (nlhs >= 3) {
        plhs[2] = mxCreateDoubleMatrix(nBins, 1, mxREAL);
        double* c = mxGetPr(plhs[2]);
        const double w = (x1 - x0) / (double)nBins;
        const double base = x0 + 0.5 * w;
        for (mwSize k = 0; k < nBins; ++k) c[k] = base + (double)k * w;
    }
}
