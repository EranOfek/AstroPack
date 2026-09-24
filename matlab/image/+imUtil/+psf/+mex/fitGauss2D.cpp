/*==========================================================================
 * fitGauss2D.cpp  ─  MEX: fit a centred 2-D Gaussian to an image stamp
 *
 * Syntax (MATLAB):
 *   [Status, TotalFlux, SigmaX, SigmaY, Rho, RMS] = fitGauss2D(Stamp)
 *   [Status, TotalFlux, SigmaX, SigmaY, Rho, RMS] = fitGauss2D(Cube)
 *
 * Input:
 *   Stamp  –  2-D single or double array  [nrows × ncols],
 *             background-subtracted; peak assumed at stamp centre.
 *   Cube   –  3-D single or double array  [nrows × ncols × nslices].
 *             Each slice (:,:,k) is an independent stamp; the same
 *             centred-Gaussian model is fitted to every slice.
 *
 * Output (scalars for 2-D input; [nslices × 1] vectors for 3-D input):
 *   Status     – logical: true if fit succeeded for that slice
 *   TotalFlux  – A · 2π · σx · σy · √(1−ρ²)   [same units as input]
 *   SigmaX     – Gaussian σ along columns  [pixels]
 *   SigmaY     – Gaussian σ along rows     [pixels]
 *   Rho        – correlation coefficient ρ ∈ (−1, 1)
 *   RMS        – √(Σ residual² / N)  over all pixels of each slice
 *
 * ── Model ────────────────────────────────────────────────────────────────
 *
 *   I(x,y) = A · exp{ −[(x/σx)² − 2ρ(x/σx)(y/σy) + (y/σy)²] / [2(1−ρ²)] }
 *
 *   Origin:  x = col − (ncols−1)/2,   y = row − (nrows−1)/2
 *   TotalFlux = ∫∫ I dx dy = A · 2π · σx · σy · √(1−ρ²)
 *
 * ── Algorithm ────────────────────────────────────────────────────────────
 *
 *   PRIMARY  – log-linear regression (single pass, no iteration):
 *     Taking the logarithm:   ln I = θ₀ + θ₁x² + θ₂xy + θ₃y²
 *       θ₀ = ln A
 *       θ₁ = −b,    b = 1 / [2σx²(1−ρ²)] > 0
 *       θ₂ =  d,    d = ρ / [σxσy(1−ρ²)]
 *       θ₃ = −c,    c = 1 / [2σy²(1−ρ²)] > 0
 *
 *     Solve the 4×4 weighted normal equations  (AᵀWA)θ = AᵀWz
 *       w = I  (intensity weighting, optimal for Poisson/photon noise)
 *       z = ln I
 *     Pixels with I < 0.1 % of peak are excluded (can't take log).
 *
 *     Recover shape parameters:
 *       det = 4bc − d²  (> 0 for valid ρ)
 *       σx = √(2c / det),  σy = √(2b / det),  ρ = d / (2√(bc))
 *
 *   FALLBACK – weighted image moments (if regression fails):
 *     Robust when too few pixels exceed the log-threshold.
 *     Estimates σx², σy², ρ from  <x²>_w, <y²>_w, <xy>_w  (w = I, I > 0).
 *
 * ── Efficiency tricks ────────────────────────────────────────────────────
 *   · Linear problem → single-pass accumulation, no outer iteration.
 *   · 4×4 Gauss–Jordan solve  (constant cost independent of stamp size).
 *   · Column-major inner loops match MATLAB memory layout.
 *   · Column-invariant sub-expressions hoisted out of row loop.
 *   · Template instantiation for float/double; double-precision accumulation.
 *   · RMS pass reuses precomputed per-column exponential prefix.
 *
 * ── Compile ──────────────────────────────────────────────────────────────
 *   mex -O CXXFLAGS='$CXXFLAGS -O3 -march=native -ffast-math' fitGauss2D.cpp
 *
 *   On Windows (MSVC):
 *   mex -O COMPFLAGS='$COMPFLAGS /O2 /arch:AVX2' fitGauss2D.cpp
 *
 * ── Quick self-test (MATLAB) ─────────────────────────────────────────────
 *   % 2-D case
 *   [X,Y] = meshgrid(-10:10,-10:10);
 *   stamp = 1000 * exp(-0.5*(X.^2/9 + Y.^2/4));   % σx=3, σy=2, ρ=0
 *   [ok,F,sx,sy,rh,rms] = fitGauss2D(stamp);
 *   fprintf('sx=%.4f sy=%.4f rho=%.4f flux=%.1f rms=%.2e\n',sx,sy,rh,F,rms)
 *   % Expected: sx≈3, sy≈2, rho≈0, flux≈37699
 *
 *   % 3-D cube case (10 slices, varying flux)
 *   cube = zeros(21,21,10);
 *   for k=1:10, cube(:,:,k)=k*100*exp(-0.5*(X.^2/9+Y.^2/4)); end
 *   [ok,F,sx,sy,rh,rms] = fitGauss2D(cube);
 *   % ok, sx, sy, … are [10×1] vectors
 *
 * Author: LAST pipeline  –  2024
 *=========================================================================*/

#include "mex.h"
#include "matrix.h"
#include <cmath>
#include <algorithm>
#include <limits>
#include <cstring>

/* ── compile-time constants ─────────────────────────────────────────────── */
static constexpr double PI2       = 6.283185307179586476925;
static constexpr double MIN_SIG   = 0.1;       /* minimum allowed σ  [pixels]  */
static constexpr double MAX_RHO   = 0.99;      /* |ρ| clamping limit            */
static constexpr double PIVOT_TOL = 1.0e-14;   /* Gauss–Jordan singularity guard*/
static constexpr int    MIN_VALID = 5;         /* min pixels for a 4-param fit  */
static constexpr double EPS_DEF   = 1.0e-3;    /* default threshold fraction    */


/* ═══════════════════════════════════════════════════════════════════════════
 *  Solve  4×4 linear system  A x = b
 *  via Gauss–Jordan elimination with partial pivoting.
 *  A is overwritten; returns false if matrix is (near-)singular.
 * ═══════════════════════════════════════════════════════════════════════════*/
static bool solve4(double A[4][4], double b[4], double x[4])
{
    /* build augmented matrix [A | b] */
    double M[4][5];
    for (int i = 0; i < 4; ++i) {
        M[i][0]=A[i][0]; M[i][1]=A[i][1]; M[i][2]=A[i][2]; M[i][3]=A[i][3];
        M[i][4]=b[i];
    }

    for (int p = 0; p < 4; ++p) {
        /* find largest pivot in column p */
        int    bestRow = p;
        double bestVal = std::abs(M[p][p]);
        for (int i = p+1; i < 4; ++i) {
            double v = std::abs(M[i][p]);
            if (v > bestVal) { bestVal = v; bestRow = i; }
        }
        if (bestVal < PIVOT_TOL) return false;

        /* swap rows */
        if (bestRow != p)
            for (int j = 0; j <= 4; ++j) std::swap(M[p][j], M[bestRow][j]);

        /* eliminate column p in all other rows */
        const double inv = 1.0 / M[p][p];
        for (int i = 0; i < 4; ++i) {
            if (i == p) continue;
            const double f = M[i][p] * inv;
            for (int j = p; j <= 4; ++j) M[i][j] -= f * M[p][j];
        }
    }
    x[0] = M[0][4]/M[0][0];  x[1] = M[1][4]/M[1][1];
    x[2] = M[2][4]/M[2][2];  x[3] = M[3][4]/M[3][3];
    return true;
}


/* ═══════════════════════════════════════════════════════════════════════════
 *  Core fitting routine – templated on pixel type (float or double).
 *  All internal arithmetic is double-precision.
 * ═══════════════════════════════════════════════════════════════════════════*/
template <typename T>
static void fitCore(const T* __restrict__ img,
                    const mwSize nrows, const mwSize ncols,
                    const double eps,                  /* threshold = eps * Ipeak */
                    bool&   status,
                    double& totalFlux,
                    double& sigX, double& sigY,
                    double& rhoOut, double& rms)
{
    /* Initialise outputs to safe defaults */
    status = false;
    totalFlux = sigX = sigY = rhoOut = rms = 0.0;

    const double cx = 0.5 * (double)(ncols - 1);   /* column centre index */
    const double cy = 0.5 * (double)(nrows - 1);   /* row    centre index */
    const mwSize N  = nrows * ncols;

    /* ─── Pass 1: peak value (sets log-threshold) ───────────────────────── */
    double Ipeak = 0.0;
    for (mwSize k = 0; k < N; ++k) {
        double v = (double)img[k];
        if (v > Ipeak) Ipeak = v;
    }
    if (Ipeak <= 0.0) return;    /* blank or all-negative stamp */

    /* Absolute pixel threshold: only pixels with I > thr enter the fit.
     * This excludes background (zero / near-zero / negative) pixels from
     * both the regression and the RMS, preventing them from diluting the
     * result and ensures log() is never called on a non-positive value.  */
    const double thr = eps * Ipeak;


    /* ─── Pass 2: accumulate 4×4 weighted normal equations ─────────────────
     *
     *  Design vector:  φ = [1,  x²,  xy,  y²]  (row of design matrix Φ)
     *  Observation:    z = ln I
     *  Weight:         w = I    (optimal for Poisson noise)
     *
     *  We accumulate the lower triangle of ΦᵀWΦ and the vector ΦᵀWz.
     *  MATLAB stores matrices column-major:  img[r + nrows*c]
     *                                                                        */
    double AA[4][4] = {};   /* ΦᵀWΦ, symmetric                              */
    double Ab[4]    = {};   /* ΦᵀWz                                          */
    int    nValid   = 0;

    for (mwSize c = 0; c < ncols; ++c) {
        const double  xc   = (double)c - cx;
        const double  xc2  = xc * xc;             /* x²                    */
        const T*      pcol = img + c * nrows;      /* pointer to col c      */

        for (mwSize r = 0; r < nrows; ++r) {
            const double v = (double)pcol[r];
            if (v <= thr) continue;             /* below threshold        */

            const double yr   = (double)r - cy;
            const double yr2  = yr * yr;           /* y²                    */
            const double xy   = xc * yr;           /* xy                    */
            const double lv   = std::log(v);       /* ln I                  */
            const double w    = v;                 /* weight = I            */

            /* Precompute weighted basis products (used multiple times) */
            const double w1   = w;                 /* w·φ₀  = w             */
            const double wx2  = w * xc2;           /* w·φ₁  = w·x²         */
            const double wxy  = w * xy;            /* w·φ₂  = w·xy         */
            const double wy2  = w * yr2;           /* w·φ₃  = w·y²         */

            /* Lower-triangle of ΦᵀWΦ  (10 unique entries) */
            AA[0][0] += w1;
            AA[1][0] += wx2;     AA[1][1] += wx2 * xc2;
            AA[2][0] += wxy;     AA[2][1] += wxy * xc2;    AA[2][2] += wxy * xy;
            AA[3][0] += wy2;     AA[3][1] += wy2 * xc2;    AA[3][2] += wy2 * xy;
            AA[3][3] += wy2 * yr2;

            /* ΦᵀWz */
            Ab[0] += w1  * lv;
            Ab[1] += wx2 * lv;
            Ab[2] += wxy * lv;
            Ab[3] += wy2 * lv;
            ++nValid;
        }
    }

    /* Fill upper triangle (ΦᵀWΦ is symmetric) */
    AA[0][1]=AA[1][0];  AA[0][2]=AA[2][0];  AA[0][3]=AA[3][0];
                        AA[1][2]=AA[2][1];  AA[1][3]=AA[3][1];
                                            AA[2][3]=AA[3][2];

    /* ─── Primary path: solve log-linear regression ──────────────────────── */
    bool usedRegression = false;
    double sX = 0.0, sY = 0.0, rh = 0.0, Apeak = 0.0;

    if (nValid >= MIN_VALID) {
        double theta[4];
        if (solve4(AA, Ab, theta)) {
            /* θ = [ln A,  −b,  d,  −c]
             *
             *  b = −θ₁ = 1/[2σx²(1−ρ²)]  > 0
             *  c = −θ₃ = 1/[2σy²(1−ρ²)]  > 0
             *  d =  θ₂ = ρ/[σxσy(1−ρ²)]
             *
             *  Recovery (from precision matrix Σ⁻¹ = [[2b,−d],[−d,2c]]):
             *    det_prec = 4bc − d²  > 0
             *    σx = √(2c / det_prec)
             *    σy = √(2b / det_prec)
             *    ρ  = d / (2√(bc))
             */
            const double b   = -theta[1];
            const double d   =  theta[2];
            const double cc  = -theta[3];   /* 'cc' avoids shadowing loop var */

            if (b > 0.0 && cc > 0.0) {
                const double det = 4.0*b*cc - d*d;
                if (det > 0.0) {
                    sX = std::sqrt(2.0*cc / det);
                    sY = std::sqrt(2.0*b  / det);
                    rh = std::max(-MAX_RHO,
                              std::min( MAX_RHO, d / (2.0*std::sqrt(b*cc)) ));

                    Apeak = std::exp(theta[0]);
                    if (sX >= MIN_SIG && sY >= MIN_SIG
                            && std::isfinite(Apeak) && Apeak > 0.0) {
                        usedRegression = true;
                    }
                }
            }
        }
    }

    /* ─── Fallback: weighted image moments ───────────────────────────────────
     *
     *  Used when the log-regression is degenerate (very faint object, almost
     *  all pixels below threshold, ill-conditioned normal matrix, …).
     *  Computes σ from intensity-weighted second moments of the stamp.
     *  Less accurate for noisy data but always defined.
     *                                                                        */
    if (!usedRegression) {
        double S0=0, Sxx=0, Syy=0, Sxy=0;

        for (mwSize c = 0; c < ncols; ++c) {
            const double xc  = (double)c - cx;
            const double xc2 = xc * xc;
            const T*     col = img + c * nrows;

            for (mwSize r = 0; r < nrows; ++r) {
                const double v = (double)col[r];
                if (v <= thr) continue;   /* same threshold as regression */
                const double yr = (double)r - cy;
                S0  += v;
                Sxx += v * xc2;
                Syy += v * yr  * yr;
                Sxy += v * xc  * yr;
            }
        }

        if (S0 <= 0.0) return;   /* entirely non-positive stamp: give up */

        const double vxx = Sxx / S0;
        const double vyy = Syy / S0;
        const double vxy = Sxy / S0;

        sX = std::sqrt(std::max(vxx, MIN_SIG*MIN_SIG));
        sY = std::sqrt(std::max(vyy, MIN_SIG*MIN_SIG));
        rh = std::max(-MAX_RHO,
                  std::min( MAX_RHO, vxy / (sX * sY) ));

        /* Peak amplitude from total flux and fitted shape */
        Apeak = S0 / (PI2 * sX * sY * std::sqrt(1.0 - rh*rh));
        /* NB: moment-method total flux = sum of pixels, NOT the Gaussian integral */
    }

    sigX   = sX;
    sigY   = sY;
    rhoOut = rh;
    totalFlux = Apeak * PI2 * sX * sY * std::sqrt(1.0 - rh*rh);

    /* ─── Pass 3: RMS over signal pixels only (I > thr) ──────────────────────
     *
     *  Using only pixels above the threshold gives a meaningful fit-quality
     *  metric: the many near-zero background pixels would otherwise dominate
     *  the sum and mask poor Gaussian fits at the peak.
     *
     *  I_model(x,y) = Apeak · exp(−Q),
     *    Q = inv2 · (x²/σx² − 2ρxy/(σxσy) + y²/σy²)
     *  Column-hoisted prefactors keep the inner loop cheap:
     *    eAx = Apeak · exp(−inv2·x²/σx²)   ← computed once per column
     *    per-row: multiply by exp(+Bx·y − inv2·y²/σy²)
     *                                                                        */
    const double rh2         = rh * rh;
    const double inv2        = 0.5  / (1.0 - rh2);
    const double isx2        = 1.0  / (sX * sX);
    const double isy2        = 1.0  / (sY * sY);
    const double twoRhiSxSy  = 2.0 * rh / (sX * sY);

    double ssq  = 0.0;
    mwSize nRMS = 0;

    for (mwSize c = 0; c < ncols; ++c) {
        const double xc   = (double)c - cx;
        const double Ax   = inv2 * xc * xc * isx2;
        const double Bx   = inv2 * xc * twoRhiSxSy;
        const double eAx  = Apeak * std::exp(-Ax);
        const T*     pcol = img + c * nrows;

        for (mwSize r = 0; r < nrows; ++r) {
            const double v = (double)pcol[r];
            if (v <= thr) continue;              /* skip background pixels */
            const double yr  = (double)r - cy;
            const double arg = Bx*yr - inv2*yr*yr*isy2;
            const double res = v - eAx * std::exp(arg);
            ssq  += res * res;
            ++nRMS;
        }
    }

    rms    = (nRMS > 0) ? std::sqrt(ssq / (double)nRMS) : 0.0;
    status = true;
}


/* ═══════════════════════════════════════════════════════════════════════════
 *  MEX gateway
 *
 *  Accepts a 2-D stamp  (nrows × ncols)          → scalar outputs
 *      or a 3-D cube    (nrows × ncols × nslices) → [nslices × 1] outputs
 *
 *  MATLAB memory layout for a 3-D array A(r,c,k):
 *    address = data + k * nrows*ncols + c * nrows + r      (column-major)
 *  So slice k starts at  data + k * nrows * ncols.
 * ═══════════════════════════════════════════════════════════════════════════*/
void mexFunction(int nlhs, mxArray* plhs[],
                 int nrhs, const mxArray* prhs[])
{
    /* ── validate inputs ── */
    if (nrhs < 1 || nrhs > 2)
        mexErrMsgIdAndTxt("fitGauss2D:nrhs",
            "One or two input arguments required: fitGauss2D(Stamp [, InputEps]).");
    if (nlhs > 6)
        mexErrMsgIdAndTxt("fitGauss2D:nlhs",
            "At most 6 output arguments supported.");
    if (mxIsSparse(prhs[0]) || mxIsComplex(prhs[0]))
        mexErrMsgIdAndTxt("fitGauss2D:type",
            "Input must be a full real-valued array.");
    if (!mxIsDouble(prhs[0]) && !mxIsSingle(prhs[0]))
        mexErrMsgIdAndTxt("fitGauss2D:type",
            "Input must be single or double.");

    const mwSize  ndims   = mxGetNumberOfDimensions(prhs[0]);
    if (ndims < 2 || ndims > 3)
        mexErrMsgIdAndTxt("fitGauss2D:dims",
            "Input must be a 2-D matrix or a 3-D cube (nrows x ncols x nslices).");

    const mwSize* dims    = mxGetDimensions(prhs[0]);
    const mwSize  nrows   = dims[0];
    const mwSize  ncols   = dims[1];
    const mwSize  nslices = (ndims == 3) ? dims[2] : 1;

    if (nrows < 3 || ncols < 3)
        mexErrMsgIdAndTxt("fitGauss2D:size",
            "Each stamp slice must be at least 3x3 pixels.");

    /* ── parse InputEps (optional, default EPS_DEF) ────────────────────────
     * eps is the minimum pixel value expressed as a fraction of the peak.
     * Only pixels with  I > eps * Ipeak  enter the fit and the RMS.       */
    double eps = EPS_DEF;
    if (nrhs >= 2) {
        if (!mxIsScalar(prhs[1]) || mxIsComplex(prhs[1]) ||
            (!mxIsDouble(prhs[1]) && !mxIsSingle(prhs[1])))
            mexErrMsgIdAndTxt("fitGauss2D:eps",
                "InputEps must be a real scalar (single or double).");
        eps = mxGetScalar(prhs[1]);
        if (eps <= 0.0 || eps >= 1.0)
            mexErrMsgIdAndTxt("fitGauss2D:eps",
                "InputEps must satisfy 0 < InputEps < 1.");
    }

    const mwSize slicePix = nrows * ncols;   /* pixels per slice */

    /* ── allocate output arrays  [nslices × 1] ──────────────────────────────
     * For a 2-D input nslices == 1, so these are 1×1 scalars — fully
     * backward-compatible with code that expects scalar outputs.           */
    mxArray* pStatus = mxCreateLogicalMatrix(nslices, 1);
    mxArray* pFlux   = mxCreateDoubleMatrix(nslices, 1, mxREAL);
    mxArray* pSigX   = mxCreateDoubleMatrix(nslices, 1, mxREAL);
    mxArray* pSigY   = mxCreateDoubleMatrix(nslices, 1, mxREAL);
    mxArray* pRho    = mxCreateDoubleMatrix(nslices, 1, mxREAL);
    mxArray* pRMS    = mxCreateDoubleMatrix(nslices, 1, mxREAL);

    mxLogical* statusPtr = mxGetLogicals(pStatus);
    double*    fluxPtr   = mxGetPr(pFlux);
    double*    sigXPtr   = mxGetPr(pSigX);
    double*    sigYPtr   = mxGetPr(pSigY);
    double*    rhoPtr    = mxGetPr(pRho);
    double*    rmsPtr    = mxGetPr(pRMS);

    /* ── fit each slice ──────────────────────────────────────────────────── */
    if (mxIsDouble(prhs[0])) {
        const double* img = static_cast<const double*>(mxGetData(prhs[0]));
        for (mwSize k = 0; k < nslices; ++k) {
            bool   st=false; double fl=0, sx=0, sy=0, rh=0, rm=0;
            fitCore<double>(img + k*slicePix, nrows, ncols, eps,
                            st, fl, sx, sy, rh, rm);
            statusPtr[k]=(mxLogical)st; fluxPtr[k]=fl;
            sigXPtr[k]=sx; sigYPtr[k]=sy; rhoPtr[k]=rh; rmsPtr[k]=rm;
        }
    } else {
        const float*  img = static_cast<const float*> (mxGetData(prhs[0]));
        for (mwSize k = 0; k < nslices; ++k) {
            bool   st=false; double fl=0, sx=0, sy=0, rh=0, rm=0;
            fitCore<float> (img + k*slicePix, nrows, ncols, eps,
                            st, fl, sx, sy, rh, rm);
            statusPtr[k]=(mxLogical)st; fluxPtr[k]=fl;
            sigXPtr[k]=sx; sigYPtr[k]=sy; rhoPtr[k]=rh; rmsPtr[k]=rm;
        }
    }

    /* ── populate requested outputs ── */
    if (nlhs >= 1) plhs[0] = pStatus;
    if (nlhs >= 2) plhs[1] = pFlux;
    if (nlhs >= 3) plhs[2] = pSigX;
    if (nlhs >= 4) plhs[3] = pSigY;
    if (nlhs >= 5) plhs[4] = pRho;
    if (nlhs >= 6) plhs[5] = pRMS;

    /* free any unrequested output arrays to avoid memory leaks */
    if (nlhs < 1) mxDestroyArray(pStatus);
    if (nlhs < 2) mxDestroyArray(pFlux);
    if (nlhs < 3) mxDestroyArray(pSigX);
    if (nlhs < 4) mxDestroyArray(pSigY);
    if (nlhs < 5) mxDestroyArray(pRho);
    if (nlhs < 6) mxDestroyArray(pRMS);
}
