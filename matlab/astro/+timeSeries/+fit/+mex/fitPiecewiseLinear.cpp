/*
 * fitPiecewiseLinear.cpp  —  MEX implementation
 *
 * Drop-in replacement for fitPiecewiseLinear.m with identical calling
 * convention and output layout.  Accepts single- or double-precision T, M,
 * and ErrorM; all internal accumulation is in double for numerical safety;
 * the cost matrix is stored as float (halved memory, matching the .m version).
 *
 * Algorithm
 * ---------
 *  For each source column of M:
 *  1. Build six prefix-sum arrays (O(n)) for O(1) per-segment weighted OLS.
 *  2. Fill an (N x N) float cost matrix: cost[j*N+i] = chi^2([i..j]).
 *  3. Dynamic programming (Ns layers, each O(n^2)) finds the globally
 *     optimal (Nseg-1) breakpoints.
 *  4. Backtrack, refit, emit output struct fields.
 *  Null hypothesis (constant fit) is computed from the full-range prefix
 *  sums at negligible cost.
 *
 * Compilation (see build_fitPiecewiseLinear.m for a one-click script)
 * -------------------------------------------------------------------
 *  Basic:
 *    mex -O CXXFLAGS='$CXXFLAGS -std=c++11' fitPiecewiseLinear.cpp
 *
 *  With OpenMP (parallelises the per-source loop on Linux/macOS):
 *    mex -O CXXFLAGS='$CXXFLAGS -std=c++11 -fopenmp' \
 *           LDFLAGS='$LDFLAGS -fopenmp' fitPiecewiseLinear.cpp
 *
 *  Windows (MSVC):
 *    mex -O COMPFLAGS='$COMPFLAGS /std:c++14 /O2 /openmp' fitPiecewiseLinear.cpp
 *
 * Usage (identical to the .m file)
 * ---------------------------------
 *  [Result, NullResult] = fitPiecewiseLinear(T, M, ErrorM, 'Nseg', 3, 'MinPts', 2)
 */

#include "mex.h"

#include <algorithm>
#include <cmath>
#include <cstring>
#include <limits>
#include <numeric>
#include <string>
#include <vector>

/* =========================================================================
 *  Internal helpers
 * ========================================================================= */

/* Read any real numeric mxArray as a flat double vector. */
static std::vector<double> toDouble(const mxArray* pa)
{
    int n = (int)mxGetNumberOfElements(pa);
    std::vector<double> v(n);
    if (mxIsSingle(pa)) {
        const float* p = static_cast<const float*>(mxGetData(pa));
        for (int i = 0; i < n; ++i) v[i] = static_cast<double>(p[i]);
    } else {
        const double* p = mxGetPr(pa);
        std::copy(p, p + n, v.begin());
    }
    return v;
}

/* Parse a scalar from trailing name-value pairs (search starts at prhs[3]). */
static double getNV(int nrhs, const mxArray* prhs[],
                    const char* name, double defVal)
{
    for (int i = 3; i + 1 < nrhs; i += 2) {
        if (!mxIsChar(prhs[i])) continue;
        char buf[64];
        mxGetString(prhs[i], buf, sizeof(buf));
        if (strcmp(buf, name) == 0) {
            if (!mxIsNumeric(prhs[i + 1]) ||
                mxGetNumberOfElements(prhs[i + 1]) != 1)
                mexErrMsgIdAndTxt("fitPiecewiseLinear:badArg",
                                  "Value of '%s' must be a numeric scalar.", name);
            return mxGetScalar(prhs[i + 1]);
        }
    }
    return defVal;
}

/* Expand ErrorM to a flat N*S double vector (MATLAB column-major order). */
static std::vector<double> expandErrors(const mxArray* mxErr, int N, int S)
{
    std::vector<double> E(N * S, 1.0);
    if (!mxErr || mxIsEmpty(mxErr)) return E;

    if (!mxIsNumeric(mxErr) || mxIsComplex(mxErr))
        mexErrMsgIdAndTxt("fitPiecewiseLinear:badErrorSize",
                          "ErrorM must be real numeric.");

    std::vector<double> raw = toDouble(mxErr);
    int ne = (int)raw.size();
    int nr = (int)mxGetM(mxErr);
    int nc = (int)mxGetN(mxErr);

    bool nonPos = false;
    auto clamp = [&](double v) -> double {
        if (v <= 0.0) { nonPos = true; return std::numeric_limits<double>::min(); }
        return v;
    };

    if (ne == 1) {
        /* scalar */
        std::fill(E.begin(), E.end(), clamp(raw[0]));
    } else if (ne == N && (nr == N || nc == N)) {
        /* N-vector: same error curve for all sources */
        for (int i = 0; i < N; ++i)
            for (int s = 0; s < S; ++s)
                E[s * N + i] = clamp(raw[i]);
    } else if (ne == S && (nr == S || nc == S)) {
        /* S-vector: per-source scalar */
        for (int s = 0; s < S; ++s)
            for (int i = 0; i < N; ++i)
                E[s * N + i] = clamp(raw[s]);
    } else if (nr == N && nc == S) {
        /* full N×S matrix */
        for (int k = 0; k < ne; ++k) E[k] = clamp(raw[k]);
    } else {
        mexErrMsgIdAndTxt("fitPiecewiseLinear:badErrorSize",
            "ErrorM [%d x %d] must be scalar, length-%d, length-%d, or [%d x %d].",
            nr, nc, N, S, N, S);
    }

    if (nonPos)
        mexWarnMsgIdAndTxt("fitPiecewiseLinear:nonPositiveError",
                           "Some ErrorM values are <= 0; replaced with eps.");
    return E;
}

/* =========================================================================
 *  Per-source computation
 *
 *  T   [N]      sorted time (double)
 *  Mv  [N]      data for this source (double)
 *  Ev  [N]      per-point errors (double, already clamped > 0)
 *  N, Ns, Mp    problem dimensions
 *  o*           output pointers into pre-allocated MATLAB arrays
 *  np..nn       null-hypothesis scalars (written by reference)
 * ========================================================================= */
static void processSource(
    const double* T, const double* Mv, const double* Ev,
    int N, int Ns, int Mp,
    double* oSeg,    double* oSegErr,
    double* oChi2,   double* oDof,  double* oNpt,
    double* oInd,    double* oTlim,
    double& np, double& npe, double& nc2, double& nd, double& nn)
{
    const double DINF = std::numeric_limits<double>::infinity();
    const float  FINF = std::numeric_limits<float>::infinity();

    /* ---- Six prefix-sum arrays (double for numerical stability) -------- */
    std::vector<double> Pw(N+1,0), PwT(N+1,0), PwT2(N+1,0);
    std::vector<double> PwM(N+1,0), PwTM(N+1,0), PwM2(N+1,0);

    for (int i = 0; i < N; ++i) {
        double w = 1.0 / (Ev[i] * Ev[i]);
        double t = T[i], m = Mv[i];
        Pw  [i+1] = Pw  [i] + w;
        PwT [i+1] = PwT [i] + w * t;
        PwT2[i+1] = PwT2[i] + w * t * t;
        PwM [i+1] = PwM [i] + w * m;
        PwTM[i+1] = PwTM[i] + w * t * m;
        PwM2[i+1] = PwM2[i] + w * m * m;
    }

    /* ---- Null hypothesis: constant fit m = c over all N points --------- */
    {
        double sw = Pw[N], swm = PwM[N], swm2 = PwM2[N];
        np  = swm / sw;
        npe = 1.0 / std::sqrt(sw);
        nc2 = std::max(0.0, swm2 - np * swm);
        nd  = static_cast<double>(N - 1);
        nn  = static_cast<double>(N);
    }

    /* ---- Cost matrix (float, column-major: cost[j*N + i] = chi2(i..j)) -
     *
     *  Only the lower triangle (j >= i+Mp-1) is written; the upper triangle
     *  stays at +inf.  The DP exploits this to avoid masking.
     * --------------------------------------------------------------------- */
    std::vector<float> Cost(static_cast<size_t>(N) * N, FINF);

    for (int i = 0; i < N - Mp + 1; ++i) {
        for (int j = i + Mp - 1; j < N; ++j) {
            double sw   = Pw  [j+1] - Pw  [i];
            double swT  = PwT [j+1] - PwT [i];
            double swT2 = PwT2[j+1] - PwT2[i];
            double swM  = PwM [j+1] - PwM [i];
            double swTM = PwTM[j+1] - PwTM[i];
            double swM2 = PwM2[j+1] - PwM2[i];

            double D = sw * swT2 - swT * swT;
            double b, a;
            if (D > 1e-14 * (sw * swT2 + 1.0)) {
                b = (sw * swTM - swT * swM) / D;
                a = (swM - b * swT) / sw;
            } else {
                b = 0.0;
                a = swM / sw;
            }
            Cost[static_cast<size_t>(j) * N + i] =
                static_cast<float>(std::max(0.0, swM2 - a * swM - b * swTM));
        }
    }

    /* ---- Dynamic programming ------------------------------------------
     *
     *  Storage: dp[j*Ns + k]  = Dp(k+1, j+1) in MATLAB 1-based notation.
     *           bp[j*Ns + k]  = 0-based start index of the (k+1)-th segment.
     *
     *  Base (k=0):
     *      dp[j*Ns+0] = cost[j*N+0]   for j in [Mp-1, N-1]
     *
     *  Recursion (k=1..Ns-1):
     *      dp[j*Ns+k] = min_{i in [iLo, min(j-Mp+1,iHi)]}
     *                       dp[(i-1)*Ns+(k-1)] + cost[j*N+i]
     *
     *  The upper triangle of cost is already +inf (segment < Mp points),
     *  so out-of-range i values are automatically excluded.
     * ------------------------------------------------------------------- */
    std::vector<double> dp(static_cast<size_t>(N) * Ns, DINF);
    std::vector<int>    bp(static_cast<size_t>(N) * Ns, 0);

    /* base */
    for (int j = Mp - 1; j < N; ++j) {
        dp[static_cast<size_t>(j) * Ns + 0] =
            static_cast<double>(Cost[static_cast<size_t>(j) * N + 0]);
        bp[static_cast<size_t>(j) * Ns + 0] = 0;
    }

    /* recursion */
    for (int k = 1; k < Ns; ++k) {
        int iLo   = k * Mp;
        int iHi   = N - Mp;
        int jStart= (k + 1) * Mp - 1;

        for (int j = jStart; j < N; ++j) {
            double best = DINF;
            int    bestI = iLo;
            int    iMax  = std::min(j - Mp + 1, iHi);

            for (int i = iLo; i <= iMax; ++i) {
                double val =
                    dp[static_cast<size_t>(i - 1) * Ns + (k - 1)] +
                    static_cast<double>(Cost[static_cast<size_t>(j) * N + i]);
                if (val < best) { best = val; bestI = i; }
            }
            dp[static_cast<size_t>(j) * Ns + k] = best;
            bp[static_cast<size_t>(j) * Ns + k] = bestI;
        }
    }

    /* ---- Backtrack to recover segment boundaries (0-based) ------------ */
    std::vector<int> segS(Ns), segE(Ns);
    segE[Ns - 1] = N - 1;
    for (int k = Ns - 1; k >= 1; --k) {
        segS[k]     = bp[static_cast<size_t>(segE[k]) * Ns + k];
        segE[k - 1] = segS[k] - 1;
    }
    segS[0] = 0;

    /* ---- Refit each segment and write output arrays -------------------- */
    for (int k = 0; k < Ns; ++k) {
        int i1  = segS[k];
        int i2  = segE[k];
        int npt = i2 - i1 + 1;

        double sw   = Pw  [i2+1] - Pw  [i1];
        double swT  = PwT [i2+1] - PwT [i1];
        double swT2 = PwT2[i2+1] - PwT2[i1];
        double swM  = PwM [i2+1] - PwM [i1];
        double swTM = PwTM[i2+1] - PwTM[i1];
        double swM2 = PwM2[i2+1] - PwM2[i1];

        double D = sw * swT2 - swT * swT;
        double slope, inter, eSlope, eInter;

        if (D > 1e-14 * (sw * swT2 + 1.0)) {
            slope  = (sw * swTM - swT * swM) / D;
            inter  = (swM - slope * swT) / sw;
            eSlope = std::sqrt(sw   / D);
            eInter = std::sqrt(swT2 / D);
        } else {                         /* degenerate: intercept-only */
            slope  = 0.0;
            inter  = swM / sw;
            eSlope = 0.0;
            eInter = std::sqrt(1.0 / sw);
        }

        double chi2 = std::max(0.0, swM2 - inter * swM - slope * swTM);
        int    dof  = std::max(1, npt - 2);

        /*
         *  MATLAB column-major layout for a [2 x Ns] matrix:
         *      element (row r, col k)  ->  flat index k*2 + r  (0-based)
         *  Row 0 = slope, Row 1 = intercept.
         */
        oSeg   [k * 2 + 0] = slope;   oSeg   [k * 2 + 1] = inter;
        oSegErr[k * 2 + 0] = eSlope;  oSegErr[k * 2 + 1] = eInter;
        oChi2[k] = chi2;
        oDof [k] = static_cast<double>(dof);
        oNpt [k] = static_cast<double>(npt);
        oInd [k * 2 + 0] = static_cast<double>(i1 + 1);   /* 1-based */
        oInd [k * 2 + 1] = static_cast<double>(i2 + 1);
        oTlim[k * 2 + 0] = T[i1];
        oTlim[k * 2 + 1] = T[i2];
    }
}

/* =========================================================================
 *  MEX gateway
 * ========================================================================= */
void mexFunction(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[])
{
    /* ---- Minimum argument check --------------------------------------- */
    if (nrhs < 2)
        mexErrMsgIdAndTxt("fitPiecewiseLinear:badInput",
            "Usage: [Result, NullResult] = "
            "fitPiecewiseLinear(T, M [,ErrorM] [,'Nseg',3] [,'MinPts',2])");

    /* ---- T ------------------------------------------------------------ */
    if (!mxIsNumeric(prhs[0]) || mxIsComplex(prhs[0]))
        mexErrMsgIdAndTxt("fitPiecewiseLinear:badInput",
                          "T must be a real numeric vector.");
    int N = (int)mxGetNumberOfElements(prhs[0]);

    /* ---- M ------------------------------------------------------------ */
    if (!mxIsNumeric(prhs[1]) || mxIsComplex(prhs[1]))
        mexErrMsgIdAndTxt("fitPiecewiseLinear:badInput",
                          "M must be a real numeric matrix.");
    if ((int)mxGetM(prhs[1]) != N)
        mexErrMsgIdAndTxt("fitPiecewiseLinear:dimMismatch",
            "T (length %d) and M (%d rows) must have the same number of rows.",
            N, (int)mxGetM(prhs[1]));
    int S = (int)mxGetN(prhs[1]);

    /* ---- Name-value arguments ----------------------------------------- */
    int Ns = std::max(1, (int)std::round(getNV(nrhs, prhs, "Nseg",   3.0)));
    int Mp = std::max(2, (int)std::round(getNV(nrhs, prhs, "MinPts", 2.0)));

    if (N < Mp * Ns)
        mexErrMsgIdAndTxt("fitPiecewiseLinear:tooFewPoints",
            "Need at least MinPts*Nseg = %d data points, got %d.", Mp * Ns, N);

    /* ---- ErrorM ------------------------------------------------------- */
    const mxArray* mxErr = (nrhs >= 3 && !mxIsEmpty(prhs[2])) ? prhs[2] : nullptr;
    std::vector<double> ErrFull = expandErrors(mxErr, N, S);

    /* ---- Read T and M as double (handles single or double input) ------- */
    std::vector<double> Td = toDouble(prhs[0]);
    std::vector<double> Md = toDouble(prhs[1]);   /* N*S, column-major */

    /* ---- Sort T (stable) and reorder M, ErrFull ----------------------- */
    std::vector<int> idx(N);
    std::iota(idx.begin(), idx.end(), 0);
    std::stable_sort(idx.begin(), idx.end(),
                     [&Td](int a, int b){ return Td[a] < Td[b]; });

    std::vector<double> Ts(N), Ms(N * S), Es(N * S);
    for (int i = 0; i < N; ++i) Ts[i] = Td[idx[i]];
    for (int s = 0; s < S; ++s)
        for (int i = 0; i < N; ++i) {
            Ms[s * N + i] = Md     [s * N + idx[i]];
            Es[s * N + i] = ErrFull[s * N + idx[i]];
        }

    /* ---- Create output struct arrays ---------------------------------- */
    const char* rFields[] = {"Seg","SegErr","Chi2","Dof","Npt","Ind","Tlim"};
    const char* nFields[] = {"Param","ParamErr","Chi2","Dof","Npt"};

    plhs[0] = mxCreateStructMatrix(1, S, 7, rFields);
    if (nlhs > 1)
        plhs[1] = mxCreateStructMatrix(1, S, 5, nFields);

    /* ---- Process each source ------------------------------------------ */
#ifdef _OPENMP
#pragma omp parallel for schedule(dynamic)
#endif
    for (int si = 0; si < S; ++si) {

        /* Per-source output MATLAB arrays */
        mxArray* mxSeg    = mxCreateDoubleMatrix(2, Ns, mxREAL);
        mxArray* mxSegErr = mxCreateDoubleMatrix(2, Ns, mxREAL);
        mxArray* mxChi2   = mxCreateDoubleMatrix(1, Ns, mxREAL);
        mxArray* mxDof    = mxCreateDoubleMatrix(1, Ns, mxREAL);
        mxArray* mxNpt    = mxCreateDoubleMatrix(1, Ns, mxREAL);
        mxArray* mxInd    = mxCreateDoubleMatrix(2, Ns, mxREAL);
        mxArray* mxTlim   = mxCreateDoubleMatrix(2, Ns, mxREAL);

        double np, npe, nc2, nd, nn;

        processSource(
            Ts.data(),
            Ms.data() + si * N,
            Es.data() + si * N,
            N, Ns, Mp,
            mxGetPr(mxSeg),    mxGetPr(mxSegErr),
            mxGetPr(mxChi2),   mxGetPr(mxDof),  mxGetPr(mxNpt),
            mxGetPr(mxInd),    mxGetPr(mxTlim),
            np, npe, nc2, nd, nn);

        mxSetField(plhs[0], si, "Seg",    mxSeg);
        mxSetField(plhs[0], si, "SegErr", mxSegErr);
        mxSetField(plhs[0], si, "Chi2",   mxChi2);
        mxSetField(plhs[0], si, "Dof",    mxDof);
        mxSetField(plhs[0], si, "Npt",    mxNpt);
        mxSetField(plhs[0], si, "Ind",    mxInd);
        mxSetField(plhs[0], si, "Tlim",   mxTlim);

        if (nlhs > 1) {
            mxSetField(plhs[1], si, "Param",    mxCreateDoubleScalar(np));
            mxSetField(plhs[1], si, "ParamErr", mxCreateDoubleScalar(npe));
            mxSetField(plhs[1], si, "Chi2",     mxCreateDoubleScalar(nc2));
            mxSetField(plhs[1], si, "Dof",      mxCreateDoubleScalar(nd));
            mxSetField(plhs[1], si, "Npt",      mxCreateDoubleScalar(nn));
        }
    }
}
