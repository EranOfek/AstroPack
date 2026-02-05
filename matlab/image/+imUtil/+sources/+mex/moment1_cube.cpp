// wcenteroid_cube.cpp
// Poisson MLE-like Gaussian centroiding with per-slice background and one-time A estimate (iter #1)
//
// Compile (Linux/g++):
//   mex -O CXXFLAGS="\$CXXFLAGS -O3 -std=c++17 -march=native -fopenmp" LDFLAGS="\$LDFLAGS -fopenmp" wcenteroid_cube.cpp
//
// USAGE:
//   [X1,Y1,IterConv] = wcenteroid_cube(Cube, Back, SN, MaxIter=10, SigmaWidth=2, k=3, RelToCenter=true, MaxStep=1/(sqrt(2)*MaxIter), MaxStep1=MaxStep)
//
// INPUTS:
//   Cube  : MxK or MxKxN, real single/double.
//           IMPORTANT: Cube is assumed to INCLUDE the background.
//   Back  : scalar or length-N vector, background level per slice (>=0).
//   SN    : scalar or length-N vector, S/N per slice (>0). Used only for convergence tolerance.
//
// OPTIONAL INPUTS:
//   MaxIter     : positive integer (default 10).
//   SigmaWidth  : scalar or 2-element vector [Sigma1 Sigma2] (default 2).
//                Sigma1 is used in the 1st iteration; Sigma2 is used in all subsequent iterations
//                AND defines the support radius and convergence tolerance.
//   k           : scalar (default 3). Support radius factor.
//   RelToCenter : logical / 0/1 numeric (default true).
//                true  => X1,Y1 returned relative to stamp center (0,0 at center).
//                false => X1,Y1 returned in MATLAB 1-based coordinates.
//   MaxStep     : scalar >=0 (default 1/(sqrt(2)*MaxIter)). Step clamp for iter>=2.
//   MaxStep1    : scalar >=0 (default MaxStep). Step clamp for the 1st iteration.
//
// OUTPUTS:
//   X1,Y1     : Nx1 double vectors.
//   IterConv  : Nx1 double vector. 1-based iteration index where convergence declared,
//              NaN if not converged.
//
// ALGORITHM SUMMARY (per slice):
//   Model per pixel i:  lambda_i = B + A * g_i(x,y)
//     where g is a *normalized* 2D Gaussian with sigma = SigmaWidth(iter):
//       g = (1/(2*pi*sigma^2)) * exp(-(dx^2+dy^2)/(2*sigma^2))
//
//   A estimation (ONLY once, in iteration 1, around the initial guess = stamp center):
//     A = sum_{r <= 2*Sigma1} max(I - B, 0), ignoring NaNs.
//
//   Fixed circular support (for speed + robustness):
//     radius R = min(k*Sigma2, half_stamp_size) ; fixed center = stamp center (not updated).
//     Pixels outside this mask are ignored in all iterations.
//
//   MLE-like centroid update (each iteration):
//     w_i = I_i * g_i / (B + A*g_i)
//     x_hat = sum(w_i * x_i)/sum(w_i),  y_hat = sum(w_i * y_i)/sum(w_i)
//     Proposed step: dx = x_hat - x0, dy = y_hat - y0
//     Always clamp step magnitude separately in x,y:
//       dx = sign(dx)*min(|dx|, MaxStepIter), similarly for dy
//     Update: x0 += dx ; y0 += dy
//
//   Convergence:
//     tol = Sigma2 / SN
//     Require TWO successive iterations with |dx|<tol AND |dy|<tol.
//     Also, convergence is never allowed on the first iteration (always do >=2 iterations).
//
//   Final iteration with correct window:
//     After the loop (converged or not), perform one extra update using Sigma2 and MaxStep.
//     IterConv is not changed by this final polishing step.
//
// NOTES:
//   - NaN/Inf pixels in Cube are ignored (they do not contribute).
//   - If sum(w) <= 0 or not finite, slice returns current estimate and stops iterating.
//   - Background B may be 0, but if B=0 and A=0 then denom can be 0; such pixels are skipped.

#include "mex.h"
#include <cmath>
#include <cstdint>
#include <vector>
#include <algorithm>

#if defined(_OPENMP)
  #include <omp.h>
#endif

static void die(const char* msg) {
    mexErrMsgIdAndTxt("wcenteroid_cube:err", "%s", msg);
}

static inline bool isRealSingleOrDouble(const mxArray* A) {
    return (mxIsSingle(A) || mxIsDouble(A)) && !mxIsComplex(A);
}

static inline bool parseBoolDefault(const mxArray* A, bool def) {
    if (!A) return def;
    if (mxIsLogicalScalar(A)) return mxIsLogicalScalarTrue(A);
    if (!mxIsNumeric(A) || mxIsComplex(A) || mxGetNumberOfElements(A) != 1)
        die("RelToCenter must be logical scalar or numeric scalar.");
    return (mxGetScalar(A) != 0.0);
}

static inline int parseIntScalarDefault(const mxArray* A, int def) {
    if (!A) return def;
    if (!mxIsNumeric(A) || mxIsComplex(A) || mxGetNumberOfElements(A) != 1)
        die("MaxIter must be a numeric scalar.");
    double v = mxGetScalar(A);
    if (!(v >= 1.0) || v != std::floor(v)) die("MaxIter must be a positive integer.");
    if (v > 1000000.0) die("MaxIter too large.");
    return (int)v;
}

static inline double parseDoubleScalarDefault(const mxArray* A, double def, const char* name) {
    if (!A) return def;
    if (!mxIsNumeric(A) || mxIsComplex(A) || mxGetNumberOfElements(A) != 1)
        mexErrMsgIdAndTxt("wcenteroid_cube:err", "%s must be a numeric scalar.", name);
    double v = mxGetScalar(A);
    if (!mxIsFinite(v)) mexErrMsgIdAndTxt("wcenteroid_cube:err", "%s must be finite.", name);
    return v;
}

static void readVecToDouble(const mxArray* A, mwSize N, std::vector<double>& out, const char* name,
                            bool allowZero, bool requirePositive)
{
    if (!A) {
        mexErrMsgIdAndTxt("wcenteroid_cube:err", "%s input is required.", name);
    }
    if (!isRealSingleOrDouble(A))
        mexErrMsgIdAndTxt("wcenteroid_cube:err", "%s must be real single/double.", name);

    mwSize nEl = mxGetNumberOfElements(A);
    if (!(nEl == 1 || nEl == N))
        mexErrMsgIdAndTxt("wcenteroid_cube:err", "%s must be scalar or length N.", name);

    out.resize((size_t)N);
    if (mxIsDouble(A)) {
        const double* p = (const double*)mxGetData(A);
        if (nEl == 1) {
            const double v = p[0];
            for (mwSize i=0;i<N;++i) out[i] = v;
        } else {
            std::copy(p, p+N, out.begin());
        }
    } else {
        const float* p = (const float*)mxGetData(A);
        if (nEl == 1) {
            const double v = (double)p[0];
            for (mwSize i=0;i<N;++i) out[i] = v;
        } else {
            for (mwSize i=0;i<N;++i) out[i] = (double)p[i];
        }
    }

    for (mwSize i=0;i<N;++i) {
        const double v = out[i];
        if (!mxIsFinite(v)) mexErrMsgIdAndTxt("wcenteroid_cube:err", "%s must be finite.", name);
        if (requirePositive) {
            if (!(v > 0.0)) mexErrMsgIdAndTxt("wcenteroid_cube:err", "%s must be > 0.", name);
        } else if (!allowZero) {
            if (!(v != 0.0)) mexErrMsgIdAndTxt("wcenteroid_cube:err", "%s must be non-zero.", name);
        } else {
            if (v < 0.0) mexErrMsgIdAndTxt("wcenteroid_cube:err", "%s must be >= 0.", name);
        }
    }
}

// SigmaWidth can be scalar or 2-element vector [Sigma1 Sigma2]
static void parseSigmaWidth(const mxArray* A, double def, double& sigma1, double& sigma2) {
    sigma1 = def; sigma2 = def;
    if (!A) return;

    if (!mxIsNumeric(A) || mxIsComplex(A))
        die("SigmaWidth must be numeric (real).");

    mwSize nEl = mxGetNumberOfElements(A);
    if (!(nEl == 1 || nEl == 2))
        die("SigmaWidth must be a scalar or 2-element vector [Sigma1 Sigma2].");

    if (mxIsDouble(A)) {
        const double* p = (const double*)mxGetData(A);
        if (nEl == 1) { sigma1 = p[0]; sigma2 = p[0]; }
        else { sigma1 = p[0]; sigma2 = p[1]; }
    } else {
        const float* p = (const float*)mxGetData(A);
        if (nEl == 1) { sigma1 = (double)p[0]; sigma2 = (double)p[0]; }
        else { sigma1 = (double)p[0]; sigma2 = (double)p[1]; }
    }

    if (!(mxIsFinite(sigma1) && mxIsFinite(sigma2)) || sigma1 <= 0.0 || sigma2 <= 0.0)
        die("SigmaWidth values must be finite and > 0.");
}

static inline double clampStep(double d, double maxStep) {
    if (maxStep < 0.0) return d;
    if (d >  maxStep) return  maxStep;
    if (d < -maxStep) return -maxStep;
    return d;
}

template <typename T>
static void centroid_one_slice_mle(
    const T* img, mwSize M, mwSize K,
    double B,
    int maxIter,
    double sigma1, double sigma2,
    double tol,               // sigma2 / SN
    double kcut,              // support radius factor (applies to sigma2)
    bool relToCenter,
    double maxStep, double maxStep1,
    double* outX, double* outY, double* outIterConv
) {
    const double cx = 0.5 * ((double)K + 1.0);
    const double cy = 0.5 * ((double)M + 1.0);

    // fixed support: centered on stamp center, radius R = min(k*sigma2, half_stamp_size)
    const double halfSize = 0.5 * (double)std::min(M, K);
    double R = kcut * sigma2;
    if (R > halfSize) R = halfSize;
    if (R < 0.0) R = 0.0;
    const double R2 = R * R;

    // Precompute coordinate arrays for the fixed-support mask to avoid branching in inner loops.
    // We store linear indices and their x,y coords (1-based).
    std::vector<mwIndex> idx;
    std::vector<double>  xcoord;
    std::vector<double>  ycoord;
    idx.reserve((size_t)(M*K));

    for (mwSize x = 0; x < K; ++x) {
        const double dx0 = (double)(x + 1) - cx;
        const double dx02 = dx0*dx0;
        const mwIndex base = (mwIndex)(M * x);
        for (mwSize y = 0; y < M; ++y) {
            const double dy0 = (double)(y + 1) - cy;
            if (dx02 + dy0*dy0 <= R2) {
                idx.push_back(base + (mwIndex)y);
                xcoord.push_back((double)(x + 1));
                ycoord.push_back((double)(y + 1));
            }
        }
    }

    // Initial guess at stamp center
    double x0 = cx;
    double y0 = cy;

    // ----- Estimate amplitude A ONCE using Sigma1 and radius 2*Sigma1 around the initial center -----
    double A = 0.0;
    {
        const double RA2 = (2.0*sigma1) * (2.0*sigma1);
        // We can use the same fixed-support list, but also apply r<=2*sigma1 about (x0,y0)=center.
        for (size_t p = 0; p < idx.size(); ++p) {
            const double dx = xcoord[p] - x0;
            const double dy = ycoord[p] - y0;
            if (dx*dx + dy*dy > RA2) continue;
            const double I = (double)img[idx[p]];
            if (!std::isfinite(I)) continue;
            const double sub = I - B;
            if (sub > 0.0) A += sub;
        }
        if (!(A >= 0.0) || !mxIsFinite(A)) A = 0.0;
    }

    double iterConv = mxGetNaN();
    int smallCount = 0;

    // Helper lambda to do one MLE update with a specified sigma and maxStep clamp.
    auto do_one_update = [&](double sigma, double maxStepThis, double& dxOut, double& dyOut) -> bool {
        const double inv2s2 = 0.5 / (sigma * sigma);
        const double normG  = 1.0 / (2.0 * M_PI * sigma * sigma);

        double sumw = 0.0, sumwx = 0.0, sumwy = 0.0;

        for (size_t p = 0; p < idx.size(); ++p) {
            const double I = (double)img[idx[p]];
            if (!std::isfinite(I)) continue;

            const double dx = xcoord[p] - x0;
            const double dy = ycoord[p] - y0;
            const double r2 = dx*dx + dy*dy;

            const double g  = normG * std::exp(-r2 * inv2s2);
            const double denom = B + A * g;
            if (!(denom > 0.0) || !std::isfinite(denom) || !std::isfinite(g)) continue;

            const double w = (I * g) / denom;

            sumw  += w;
            sumwx += w * xcoord[p];
            sumwy += w * ycoord[p];
        }

        if (!(sumw > 0.0) || !std::isfinite(sumw)) return false;

        const double xhat = sumwx / sumw;
        const double yhat = sumwy / sumw;

        double dxp = xhat - x0;
        double dyp = yhat - y0;

        // Always clamp
        dxp = clampStep(dxp, maxStepThis);
        dyp = clampStep(dyp, maxStepThis);

        x0 += dxp;
        y0 += dyp;

        dxOut = dxp;
        dyOut = dyp;
        return true;
    };

    // ---- Iteration loop ----
    // Iteration 1 uses sigma1 and maxStep1.
    // Iterations >=2 use sigma2 and maxStep.
    // Convergence never allowed on iteration 1; require two successive small steps.
    for (int it = 0; it < maxIter; ++it) {
        double dx = 0.0, dy = 0.0;
        const bool ok = (it == 0)
            ? do_one_update(sigma1, maxStep1, dx, dy)
            : do_one_update(sigma2, maxStep,  dx, dy);

        if (!ok) break;

        if (it >= 1) { // only start checking from 2nd iteration
            if (std::abs(dx) < tol && std::abs(dy) < tol) {
                smallCount++;
            } else {
                smallCount = 0;
            }
            if (smallCount >= 2) {
                iterConv = (double)(it + 1); // 1-based iteration index (current)
                break;
            }
        }
    }

    // ---- Final polishing iteration with correct window (Sigma2) ----
    // Does not modify iterConv.
    {
        double dx = 0.0, dy = 0.0;
        (void)do_one_update(sigma2, maxStep, dx, dy);
    }

    if (relToCenter) {
        *outX = x0 - cx;
        *outY = y0 - cy;
    } else {
        *outX = x0;
        *outY = y0;
    }
    *outIterConv = iterConv;
}

template <typename T>
static void centroid_cube_mle(
    const T* cube, mwSize M, mwSize K, mwSize N,
    const double* Back,
    const double* SN,
    int maxIter,
    double sigma1, double sigma2,
    double kcut,
    bool relToCenter,
    double maxStep, double maxStep1,
    double* X, double* Y, double* IterConv
) {
    const mwSize stride = M * K;

#if defined(_OPENMP)
    #pragma omp parallel for schedule(static)
#endif
    for (mwSize n = 0; n < N; ++n) {
        const double tol = sigma2 / SN[n];
        double x, y, itc;
        centroid_one_slice_mle<T>(
            cube + n * stride, M, K,
            Back[n],
            maxIter,
            sigma1, sigma2,
            tol,
            kcut,
            relToCenter,
            maxStep, maxStep1,
            &x, &y, &itc
        );
        X[n] = x;
        Y[n] = y;
        IterConv[n] = itc;
    }
}

void mexFunction(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[]) {
    // Signature:
    //   [X1,Y1,IterConv] = wcenteroid_cube(Cube, Back, SN, MaxIter=10, SigmaWidth=2, k=3, RelToCenter=true, MaxStep=1/(sqrt(2)*MaxIter), MaxStep1=MaxStep)

    if (nrhs < 3 || nrhs > 9)
        die("Usage: [X1,Y1,IterConv]=wcenteroid_cube(Cube, Back, SN, MaxIter=10, SigmaWidth=2, k=3, RelToCenter=true, MaxStep=1/(sqrt(2)*MaxIter), MaxStep1=MaxStep)");
    if (nlhs != 3)
        die("Require three outputs: [X1, Y1, IterConv].");

    const mxArray* A    = prhs[0];
    const mxArray* BackA= prhs[1];
    const mxArray* SNA  = prhs[2];

    if (!isRealSingleOrDouble(A)) die("Cube must be real single or double.");

    const mwSize nd = mxGetNumberOfDimensions(A);
    if (nd != 2 && nd != 3) die("Cube must be 2-D (M x K) or 3-D (M x K x N).");

    const mwSize* dims = mxGetDimensions(A);
    const mwSize M = dims[0];
    const mwSize K = dims[1];
    const mwSize N = (nd == 3) ? dims[2] : 1;

    std::vector<double> Back, SN;
    readVecToDouble(BackA, N, Back, "Back", /*allowZero=*/true,  /*requirePositive=*/false);
    readVecToDouble(SNA,   N, SN,   "SN",   /*allowZero=*/false, /*requirePositive=*/true);

    const int maxIter = parseIntScalarDefault((nrhs >= 4) ? prhs[3] : nullptr, 10);

    double sigma1 = 2.0, sigma2 = 2.0;
    parseSigmaWidth((nrhs >= 5) ? prhs[4] : nullptr, 2.0, sigma1, sigma2);

    const double kcut = parseDoubleScalarDefault((nrhs >= 6) ? prhs[5] : nullptr, 3.0, "k");
    if (!(kcut >= 0.0) || !mxIsFinite(kcut)) die("k must be finite and >= 0.");

    const bool relToCenter = parseBoolDefault((nrhs >= 7) ? prhs[6] : nullptr, true);

    // Default MaxStep = 1/(sqrt(2)*MaxIter)
    const double defaultMaxStep = 1.0 / (std::sqrt(2.0) * (double)maxIter);
    double maxStep  = parseDoubleScalarDefault((nrhs >= 8) ? prhs[7] : nullptr, defaultMaxStep, "MaxStep");
    if (!(maxStep >= 0.0) || !mxIsFinite(maxStep)) die("MaxStep must be finite and >= 0.");

    double maxStep1 = parseDoubleScalarDefault((nrhs >= 9) ? prhs[8] : nullptr, maxStep, "MaxStep1");
    if (!(maxStep1 >= 0.0) || !mxIsFinite(maxStep1)) die("MaxStep1 must be finite and >= 0.");

    // Outputs
    plhs[0] = mxCreateDoubleMatrix((mwSize)N, 1, mxREAL);
    plhs[1] = mxCreateDoubleMatrix((mwSize)N, 1, mxREAL);
    plhs[2] = mxCreateDoubleMatrix((mwSize)N, 1, mxREAL);

    double* X   = (double*)mxGetData(plhs[0]);
    double* Y   = (double*)mxGetData(plhs[1]);
    double* ItC = (double*)mxGetData(plhs[2]);

    const mxClassID cid = mxGetClassID(A);
    if (cid == mxDOUBLE_CLASS) {
        const double* in = (const double*)mxGetData(A);
        centroid_cube_mle<double>(in, M, K, N, Back.data(), SN.data(), maxIter, sigma1, sigma2, kcut, relToCenter, maxStep, maxStep1, X, Y, ItC);
    } else {
        const float* in = (const float*)mxGetData(A);
        centroid_cube_mle<float>(in, M, K, N, Back.data(), SN.data(), maxIter, sigma1, sigma2, kcut, relToCenter, maxStep, maxStep1, X, Y, ItC);
    }
}
