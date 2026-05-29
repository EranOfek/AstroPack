/*
 * sigmaClipCubeN.cpp  —  iterative sigma-clip stacker for 3-D pixel cubes
 *
 * --------------------------------------------------------------------------
 * Syntax
 * ------
 *   [Img]             = sigmaClipCubeN(Cube, [LowNs HighNs], Niter)
 *   [Img]             = sigmaClipCubeN(Cube, [LowNs HighNs], Niter, Weights)
 *   [Img, Var]        = sigmaClipCubeN(...)
 *   [Img, Var, Nused] = sigmaClipCubeN(...)
 *
 * Inputs
 * ------
 *   Cube            M x N x K  single or double  (K frames to stack)
 *   [LowNs HighNs]  1x2 real   lower / upper sigma-rejection thresholds (>= 0)
 *   Niter           scalar int  iteration count (>= 1):
 *                     1  ->  no clipping; mean & variance of all valid pixels
 *                     2  ->  one clip pass  (matches old sigma_clip_cube_fast)
 *                     N  ->  N-1 clip passes; exits early when converged
 *   Weights         K-element real vector  (optional 4th argument)
 *                     []  or omitted  ->  all slices have equal weight (unweighted)
 *                     Otherwise each element is the scalar weight applied to
 *                     every pixel in that slice.  Values must be finite and >= 0.
 *                     Zero-weight slices are skipped entirely.
 *
 * Outputs
 * -------
 *   Img    M x N  sigma-clipped (weighted) mean  (same class as Cube; NaN = no data)
 *   Var    M x N  variance of the final iteration  (same class; optional)
 *            Unweighted mode:  sample variance   M2 / (cnt-1)   (Bessel-corrected)
 *            Weighted mode:    biased weighted variance   M2w / W
 *                              where W = sum of admitted weights per pixel.
 *                              For equal unit weights reduces to M2/K (approx
 *                              M2/(K-1) for large K).
 *   Nused  M x N  integer number of FRAMES admitted to each pixel  (double; optional)
 *            Always the integer frame count, regardless of weighting.
 *
 * Weighted Welford algorithm  (West 1979)
 * ----------------------------------------
 *   For each slice k with weight w_k > 0 and pixel value x (not NaN, in window):
 *     W      += w_k                    (wsum accumulator)
 *     delta1  = x - mu_old
 *     mu     += (w_k / W) * delta1
 *     delta2  = x - mu_new
 *     M2w    += w_k * delta1 * delta2
 *     cnt    += 1                      (integer, for Nused and convergence)
 *
 *   Weighted mean  = mu
 *   Weighted var   = M2w / W           (biased; clip bounds + Var output)
 *   Unweighted var = M2w / (cnt-1)     (Bessel-corrected; when Weights omitted)
 *
 * Compile
 * -------
 *   mex CXXFLAGS='$CXXFLAGS -std=c++14 -O3 -mavx2 -mfma -fopenmp' ...
 *       LDFLAGS='$LDFLAGS -fopenmp' sigmaClipCubeN.cpp
 *
 *   Without OpenMP (single-threaded):
 *   mex CXXFLAGS='$CXXFLAGS -std=c++14 -O3 -mavx2 -mfma' sigmaClipCubeN.cpp
 *
 * Requirements: AVX2 (Intel Haswell 2013+, AMD Zen 2019+).
 *               OpenMP optional but strongly recommended for large cubes.
 * --------------------------------------------------------------------------
 */

#include "mex.h"
#include <immintrin.h>
#include <cmath>
#include <cstdarg>
#include <cstring>
#include <algorithm>
#include <limits>
#include <cstdio>
#include <vector>

#ifdef _OPENMP
#  include <omp.h>
#endif

#ifndef __AVX2__
#  error "AVX2 support required. Add -mavx2 (GCC/Clang) or /arch:AVX2 (MSVC)."
#endif


/* =========================================================================
 * 1. Utilities
 * ========================================================================= */

[[noreturn]] static void die(const char* fmt, ...)
{
    char buf[512];
    va_list ap;
    va_start(ap, fmt);
    std::vsnprintf(buf, sizeof(buf), fmt, ap);
    va_end(ap);
    mexErrMsgIdAndTxt("sigmaClipCubeN:error", "%s", buf);
    __builtin_unreachable();
}

static double* alloc_d(mwSize n)
{
    auto* p = static_cast<double*>(_mm_malloc(n * sizeof(double), 32));
    if (!p) die("Out of memory: cannot allocate %zu doubles.", static_cast<size_t>(n));
    return p;
}

/* Movable, non-copyable RAII wrapper for 32-byte-aligned double arrays.      */
struct AlignedBuf
{
    double* ptr = nullptr;

    AlignedBuf() = default;
    explicit AlignedBuf(mwSize n) : ptr(alloc_d(n)) {}
    ~AlignedBuf() noexcept { if (ptr) _mm_free(ptr); }

    AlignedBuf(const AlignedBuf&)            = delete;
    AlignedBuf& operator=(const AlignedBuf&) = delete;

    AlignedBuf(AlignedBuf&& o) noexcept : ptr(o.ptr) { o.ptr = nullptr; }
    AlignedBuf& operator=(AlignedBuf&& o) noexcept
    {
        if (ptr) _mm_free(ptr);
        ptr = o.ptr;
        o.ptr = nullptr;
        return *this;
    }

    operator       double*()       noexcept { return ptr; }
    operator const double*() const noexcept { return ptr; }
};


/* =========================================================================
 * 2. Typed load helpers  (always produce double; converts float on-the-fly)
 * ========================================================================= */

static inline __m256d load4(const double* p, mwSize i) { return _mm256_loadu_pd(p + i); }
static inline __m256d load4(const float*  p, mwSize i) { return _mm256_cvtps_pd(_mm_loadu_ps(p + i)); }

static inline double load1(const double* p, mwSize i) { return p[i]; }
static inline double load1(const float*  p, mwSize i) { return static_cast<double>(p[i]); }


/* =========================================================================
 * 3. In-register weighted Welford update  (4 pixels, AVX2)
 *
 * Implements West (1979) online weighted mean and M2 accumulation.
 * Passing vw = vone gives the standard (unweighted) Welford algorithm.
 *
 * vone   _mm256_set1_pd(1.0)         kept in a ymm register by the caller
 * vw     weight for current slice    broadcast scalar; pass vone if unweighted
 * m      inclusion mask              all-1 = include, all-0 = exclude
 * x      data values                 only included lanes are meaningful
 * cnt    integer frame count         += 1  per included lane
 * wsum   sum of admitted weights     += w_k per included lane
 * mu     running weighted mean
 * M2     running weighted M2  (= sum_i  w_i * d1_i * d2_i)
 *
 * For excluded lanes (m == 0): d1 = d2 = 0, so cnt/wsum/mu/M2 are unchanged.
 * vtiny = 1e-300 guards the division when wsum == 0 (lane never yet touched).
 * ========================================================================= */
static inline void welford4(const __m256d vone, const __m256d vw,
                             __m256d m, __m256d x,
                             __m256d& cnt, __m256d& wsum,
                             __m256d& mu, __m256d& M2) noexcept
{
    const __m256d vtiny = _mm256_set1_pd(1e-300);

    cnt  = _mm256_add_pd(cnt,  _mm256_and_pd(m, vone));
    wsum = _mm256_add_pd(wsum, _mm256_and_pd(m, vw));

    __m256d d1 = _mm256_and_pd(m, _mm256_sub_pd(x, mu));

    /* mu += (w_k * d1) / max(wsum, tiny)
       Excluded: d1 = 0  =>  numerator = 0  =>  mu unchanged regardless.    */
    mu = _mm256_add_pd(mu, _mm256_div_pd(
             _mm256_mul_pd(vw, d1),
             _mm256_max_pd(wsum, vtiny)));

    __m256d d2 = _mm256_and_pd(m, _mm256_sub_pd(x, mu));

    /* M2 += w_k * d1 * d2   (both d1 and d2 are 0 for excluded lanes)      */
#ifdef __FMA__
    M2 = _mm256_fmadd_pd(_mm256_mul_pd(vw, d1), d2, M2);
#else
    M2 = _mm256_add_pd(M2, _mm256_mul_pd(_mm256_mul_pd(vw, d1), d2));
#endif
}


/* =========================================================================
 * 4. Pass 0 — unclipped weighted Welford scan over all K frames
 *
 * weights  K-element weight array, or nullptr (unweighted, all w_k = 1).
 *          Slices with w_k <= 0 are skipped entirely.
 * wsum     output: sum of admitted weights per pixel.
 *          For unweighted (weights = nullptr): wsum == cnt on exit.
 *
 * Block size BLK: 4 arrays x 4096 x 8 B = 128 KB in L2.
 * ========================================================================= */
template<typename SrcT>
static void pass0(const SrcT*   cube,
                  const double* weights,
                  double*       mean,
                  double*       M2,
                  double*       cnt,
                  double*       wsum,
                  mwSize P, mwSize K) noexcept
{
    constexpr mwSize BLK = 4096;

    #pragma omp parallel for schedule(static)
    for (mwIndex ib = 0; ib < static_cast<mwIndex>(P); ib += static_cast<mwIndex>(BLK))
    {
        const mwSize p0 = static_cast<mwSize>(ib);
        const mwSize p1 = std::min(p0 + BLK, P);
        const mwSize bn = p1 - p0;

        std::memset(mean + p0, 0, bn * sizeof(double));
        std::memset(M2   + p0, 0, bn * sizeof(double));
        std::memset(cnt  + p0, 0, bn * sizeof(double));
        std::memset(wsum + p0, 0, bn * sizeof(double));

        const __m256d vone = _mm256_set1_pd(1.0);

        for (mwSize k = 0; k < K; ++k) {
            const double w_k = weights ? weights[k] : 1.0;
            if (w_k <= 0.0) continue;
            const __m256d vw = _mm256_set1_pd(w_k);

            const SrcT* sl = cube + k * P;
            mwSize p = p0;

            /* ---- SIMD: 4 doubles per step ---- */
            for (; p + 4 <= p1; p += 4) {
                __m256d x  = load4(sl, p);
                __m256d m  = _mm256_cmp_pd(x, x, _CMP_ORD_Q);

                __m256d c_ = _mm256_load_pd(cnt  + p);
                __m256d ws = _mm256_load_pd(wsum + p);
                __m256d u_ = _mm256_load_pd(mean + p);
                __m256d M_ = _mm256_load_pd(M2   + p);

                welford4(vone, vw, m, x, c_, ws, u_, M_);

                _mm256_store_pd(cnt  + p, c_);
                _mm256_store_pd(wsum + p, ws);
                _mm256_store_pd(mean + p, u_);
                _mm256_store_pd(M2   + p, M_);
            }

            /* ---- Scalar tail ---- */
            for (; p < p1; ++p) {
                double x = load1(sl, p);
                if (std::isnan(x)) continue;
                wsum[p] += w_k;
                double d1  = x - mean[p];
                mean[p]   += (w_k * d1) / wsum[p];
                M2[p]     += w_k * d1 * (x - mean[p]);
                cnt[p]    += 1.0;
            }
        }
    }
}


/* =========================================================================
 * 5. Standard deviation from Welford accumulators
 *
 * Unweighted (is_weighted = false):
 *   sd = sqrt(M2 / (cnt-1))   cnt > 1
 *      = 0                     cnt == 1  (single sample)
 *      = NaN                   cnt == 0  (dead pixel)
 *
 * Weighted (is_weighted = true):
 *   sd = sqrt(M2 / wsum)      cnt > 0   (biased weighted sd)
 *      = NaN                   cnt == 0  (dead pixel)
 *
 * In pass_clip, dead-pixel lanes produce NaN sd -> NaN clip bounds ->
 * GE_OQ/LE_OQ return false -> automatic rejection (second defence after
 * the explicit m_stat = (cnt > 0) gate).
 * ========================================================================= */
static void compute_sd(const double* M2,
                       const double* cnt,
                       const double* wsum,
                       double*       sd,
                       mwSize        P,
                       bool          is_weighted) noexcept
{
    constexpr double qNaN = std::numeric_limits<double>::quiet_NaN();

    #pragma omp parallel for schedule(static)
    for (mwIndex p = 0; p < static_cast<mwIndex>(P); ++p) {
        const double c = cnt[p];

        if (c < 1.0) {
            sd[p] = qNaN;                          /* dead pixel               */
        } else if (is_weighted) {
            const double ws = wsum[p];
            if (ws < 1e-300) { sd[p] = qNaN; }    /* degenerate weight sum    */
            else {
                const double v = M2[p] / ws;
                sd[p] = (v >= 0.0) ? std::sqrt(v) : 0.0;
            }
        } else {
            if (c < 2.0) { sd[p] = 0.0; }         /* single frame             */
            else {
                const double v = M2[p] / (c - 1.0);
                sd[p] = (v >= 0.0) ? std::sqrt(v) : 0.0;
            }
        }
    }
}


/* =========================================================================
 * 6. Clip pass — weighted Welford scan with per-pixel sigma rejection
 *
 * Admits only values in [mu - lowNs*sd,  mu + highNs*sd] where mu and sd
 * come from the PREVIOUS iteration (clip_mean, clip_sd).
 * clip_cnt is the previous-iteration integer count used for convergence.
 *
 * weights / clip_wsum / nxt_wsum:  nullptr / unused when unweighted.
 *
 * Dead-pixel rejection (dual defence):
 *   1. m_stat = (clip_cnt > 0) explicit gate.
 *   2. For dead pixels: clip_sd = NaN -> lo/hi = NaN ->
 *      _CMP_GE_OQ and _CMP_LE_OQ return false (quiet NaN semantics).
 *
 * Returns true if any pixel's INTEGER count changed (convergence indicator).
 * ========================================================================= */
template<typename SrcT>
static bool pass_clip(const SrcT*   cube,
                      const double* weights,
                      const double* clip_mean,
                      const double* clip_sd,
                      const double* clip_cnt,
                      const double* clip_wsum,
                      double        lowNs,
                      double        highNs,
                      double*       nxt_mean,
                      double*       nxt_M2,
                      double*       nxt_cnt,
                      double*       nxt_wsum,
                      mwSize P, mwSize K) noexcept
{
    constexpr mwSize BLK = 4096;
    int n_changed = 0;

    #pragma omp parallel for schedule(static) reduction(+:n_changed)
    for (mwIndex ib = 0; ib < static_cast<mwIndex>(P); ib += static_cast<mwIndex>(BLK))
    {
        const mwSize p0 = static_cast<mwSize>(ib);
        const mwSize p1 = std::min(p0 + BLK, P);
        const mwSize bn = p1 - p0;

        std::memset(nxt_mean + p0, 0, bn * sizeof(double));
        std::memset(nxt_M2   + p0, 0, bn * sizeof(double));
        std::memset(nxt_cnt  + p0, 0, bn * sizeof(double));
        std::memset(nxt_wsum + p0, 0, bn * sizeof(double));

        const __m256d vone  = _mm256_set1_pd(1.0);
        const __m256d vLow  = _mm256_set1_pd(lowNs);
        const __m256d vHigh = _mm256_set1_pd(highNs);
        const __m256d vzero = _mm256_setzero_pd();

        for (mwSize k = 0; k < K; ++k) {
            const double w_k = weights ? weights[k] : 1.0;
            if (w_k <= 0.0) continue;
            const __m256d vw = _mm256_set1_pd(w_k);

            const SrcT* sl = cube + k * P;
            mwSize p = p0;

            /* ---- SIMD ---- */
            for (; p + 4 <= p1; p += 4) {
                __m256d x  = load4(sl, p);
                __m256d mu = _mm256_load_pd(clip_mean + p);
                __m256d s  = _mm256_load_pd(clip_sd   + p);
                __m256d cc = _mm256_load_pd(clip_cnt  + p);

                __m256d m_x    = _mm256_cmp_pd(x, x,  _CMP_ORD_Q);
                __m256d m_stat = _mm256_cmp_pd(cc, vzero, _CMP_GT_OQ);

#ifdef __FMA__
                __m256d lo = _mm256_fnmadd_pd(vLow,  s, mu);
                __m256d hi = _mm256_fmadd_pd (vHigh, s, mu);
#else
                __m256d lo = _mm256_sub_pd(mu, _mm256_mul_pd(vLow,  s));
                __m256d hi = _mm256_add_pd(mu, _mm256_mul_pd(vHigh, s));
#endif
                __m256d m_lo = _mm256_cmp_pd(x, lo, _CMP_GE_OQ);
                __m256d m_hi = _mm256_cmp_pd(x, hi, _CMP_LE_OQ);

                __m256d m = _mm256_and_pd(m_x,
                            _mm256_and_pd(m_stat,
                            _mm256_and_pd(m_lo, m_hi)));

                __m256d nc = _mm256_load_pd(nxt_cnt  + p);
                __m256d nw = _mm256_load_pd(nxt_wsum + p);
                __m256d nm = _mm256_load_pd(nxt_mean + p);
                __m256d nM = _mm256_load_pd(nxt_M2   + p);

                welford4(vone, vw, m, x, nc, nw, nm, nM);

                _mm256_store_pd(nxt_cnt  + p, nc);
                _mm256_store_pd(nxt_wsum + p, nw);
                _mm256_store_pd(nxt_mean + p, nm);
                _mm256_store_pd(nxt_M2   + p, nM);
            }

            /* ---- Scalar tail ---- */
            for (; p < p1; ++p) {
                double x = load1(sl, p);
                if (std::isnan(x))     continue;
                if (clip_cnt[p] < 1.0) continue;

                double lo = clip_mean[p] - lowNs  * clip_sd[p];
                double hi = clip_mean[p] + highNs * clip_sd[p];

                if (x >= lo && x <= hi) {
                    nxt_wsum[p] += w_k;
                    double d1    = x - nxt_mean[p];
                    nxt_mean[p] += (w_k * d1) / nxt_wsum[p];
                    nxt_M2[p]   += w_k * d1 * (x - nxt_mean[p]);
                    nxt_cnt[p]  += 1.0;
                }
            }
        } /* k */

        /* Convergence check: integer count only — weights are constant.      */
        int blk_chg = 0;
        for (mwSize p = p0; p < p1 && !blk_chg; ++p)
            blk_chg = (nxt_cnt[p] != clip_cnt[p]) ? 1 : 0;
        n_changed += blk_chg;

    } /* ib */

    return n_changed > 0;
}


/* =========================================================================
 * 7. MEX entry point
 * ========================================================================= */
void mexFunction(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[])
{
    static const char* usage =
        "Usage: [Img, Var, Nused] = sigmaClipCubeN(Cube, [LowNs HighNs], Niter)\n"
        "       [Img, Var, Nused] = sigmaClipCubeN(Cube, [LowNs HighNs], Niter, Weights)\n"
        "  Weights: K-element real vector ([] or omit = unweighted)\n"
        "  Niter=1: no clipping; Niter=2: one pass; Niter=N: N-1 passes";

    if (nrhs < 3 || nrhs > 4)  die("%s", usage);
    if (nlhs < 1 || nlhs > 3)  die("Between 1 and 3 output arguments required.");

    const mxArray* CubeA = prhs[0];
    const mxArray* ThrA  = prhs[1];
    const mxArray* IterA = prhs[2];

    /* ── Cube ── */
    if (mxIsComplex(CubeA))
        die("Cube must be real (non-complex).");
    if (!(mxIsSingle(CubeA) || mxIsDouble(CubeA)))
        die("Cube must be single or double.");
    if (mxGetNumberOfDimensions(CubeA) != 3)
        die("Cube must be 3-D (M x N x K). Got %u dimensions.",
            static_cast<unsigned>(mxGetNumberOfDimensions(CubeA)));

    /* ── Thresholds ── */
    if (!(mxIsSingle(ThrA) || mxIsDouble(ThrA)) || mxGetNumberOfElements(ThrA) != 2)
        die("[LowNs HighNs] must be a 2-element single or double vector.");

    /* ── Niter ── */
    if (mxGetNumberOfElements(IterA) != 1 || mxIsComplex(IterA))
        die("Niter must be a real scalar.");

    const mwSize* dims = mxGetDimensions(CubeA);
    const mwSize  M = dims[0], N = dims[1], K = dims[2];
    const mwSize  P = M * N;

    if (K < 1) die("K (third dimension) must be >= 1, got %zu.", static_cast<size_t>(K));
    if (K < 3)
        mexWarnMsgIdAndTxt("sigmaClipCubeN:fewFrames",
            "K = %zu: sigma-clip statistics are unreliable with fewer than 3 frames.",
            static_cast<size_t>(K));

    /* ── Thresholds ── */
    double lowNs, highNs;
    if (mxIsDouble(ThrA)) {
        const double* t = static_cast<const double*>(mxGetData(ThrA));
        lowNs = t[0]; highNs = t[1];
    } else {
        const float* t = static_cast<const float*>(mxGetData(ThrA));
        lowNs = static_cast<double>(t[0]);
        highNs = static_cast<double>(t[1]);
    }
    if (!std::isfinite(lowNs) || !std::isfinite(highNs))
        die("LowNs and HighNs must be finite.");
    if (lowNs < 0.0 || highNs < 0.0)
        die("LowNs = %.6g and HighNs = %.6g must both be >= 0.", lowNs, highNs);

    /* ── Niter ── */
    const int Niter = static_cast<int>(mxGetScalar(IterA));
    if (Niter < 1) die("Niter must be >= 1 (got %d).", Niter);

    /* ── Weights (optional 4th argument) ────────────────────────────────────
       is_weighted = true only when a non-empty, valid weights vector is given.
       weights_vec stores K doubles; nullptr is passed to scan functions when
       unweighted so they broadcast vone and the overhead is zero.            */
    std::vector<double> weights_vec;
    bool is_weighted = false;

    if (nrhs >= 4 && !mxIsEmpty(prhs[3])) {
        const mxArray* WA = prhs[3];

        if (!(mxIsSingle(WA) || mxIsDouble(WA)) || mxIsComplex(WA))
            die("Weights must be a real single or double vector.");

        const mwSize nw = mxGetNumberOfElements(WA);
        if (nw != K)
            die("Weights must have K=%zu elements, got %zu.",
                static_cast<size_t>(K), static_cast<size_t>(nw));

        weights_vec.resize(K);
        const bool   wdbl = mxIsDouble(WA);
        const double* wd  = wdbl ? static_cast<const double*>(mxGetData(WA)) : nullptr;
        const float*  wf  = wdbl ? nullptr : static_cast<const float* >(mxGetData(WA));

        for (mwSize k = 0; k < K; ++k) {
            const double wv = wdbl ? wd[k] : static_cast<double>(wf[k]);
            if (!std::isfinite(wv) || wv < 0.0)
                die("Weights must be finite and >= 0 (element %zu = %.6g).",
                    static_cast<size_t>(k + 1), wv);
            weights_vec[k] = wv;
        }
        is_weighted = true;
    }

    const double* weights = is_weighted ? weights_vec.data() : nullptr;

    const bool want_var   = (nlhs >= 2);
    const bool want_nused = (nlhs >= 3);
    const bool is_float   = mxIsSingle(CubeA);
    const mxClassID cid   = mxGetClassID(CubeA);
    const void* cubeData  = mxGetData(CubeA);

    /* ── Allocate outputs ── */
    plhs[0] = mxCreateNumericMatrix(M, N, cid, mxREAL);
    if (want_var)   plhs[1] = mxCreateNumericMatrix(M, N, cid, mxREAL);
    if (want_nused) plhs[2] = mxCreateDoubleMatrix(M, N, mxREAL);

    if (P == 0) return;   /* empty image: outputs already correct             */

    /* ── Allocate working arrays (always double) ─────────────────────────────
       cur_*: statistics of the most recent completed iteration.
       nxt_*: statistics being built in the current clip pass.
       After each clip pass the two sets are pointer-swapped (no copy).
       wsum:  sum of admitted weights per pixel.
              Unweighted: equals cnt (each admitted frame contributes 1).
       AlignedBuf destructors free everything on any exit path.              */
    AlignedBuf mean_a(P), M2_a(P), cnt_a(P), wsum_a(P);
    AlignedBuf mean_b, M2_b, cnt_b, wsum_b, sd_buf;

    if (Niter > 1) {
        mean_b = AlignedBuf(P);
        M2_b   = AlignedBuf(P);
        cnt_b  = AlignedBuf(P);
        wsum_b = AlignedBuf(P);
        sd_buf = AlignedBuf(P);
    }

    double* cur_mean = mean_a, *cur_M2 = M2_a, *cur_cnt = cnt_a, *cur_wsum = wsum_a;
    double* nxt_mean = mean_b, *nxt_M2 = M2_b, *nxt_cnt = cnt_b, *nxt_wsum = wsum_b;
    double* sd       = sd_buf;

    /* ── Pass 0: full (un-clipped) Welford over all K frames ── */
    if (is_float)
        pass0(static_cast<const float* >(cubeData), weights,
              cur_mean, cur_M2, cur_cnt, cur_wsum, P, K);
    else
        pass0(static_cast<const double*>(cubeData), weights,
              cur_mean, cur_M2, cur_cnt, cur_wsum, P, K);

    /* ── Iterative sigma-clip ─────────────────────────────────────────────── */
    for (int iter = 1; iter < Niter; ++iter) {

        compute_sd(cur_M2, cur_cnt, cur_wsum, sd, P, is_weighted);

        bool changed;
        if (is_float)
            changed = pass_clip(static_cast<const float* >(cubeData),
                                weights,
                                cur_mean, sd, cur_cnt, cur_wsum,
                                lowNs, highNs,
                                nxt_mean, nxt_M2, nxt_cnt, nxt_wsum,
                                P, K);
        else
            changed = pass_clip(static_cast<const double*>(cubeData),
                                weights,
                                cur_mean, sd, cur_cnt, cur_wsum,
                                lowNs, highNs,
                                nxt_mean, nxt_M2, nxt_cnt, nxt_wsum,
                                P, K);

        std::swap(cur_mean, nxt_mean);
        std::swap(cur_M2,   nxt_M2);
        std::swap(cur_cnt,  nxt_cnt);
        std::swap(cur_wsum, nxt_wsum);

        if (!changed) break;
    }

    /* ── Write outputs ───────────────────────────────────────────────────────
       Img:   weighted mean (same class as input)
       Var:   unweighted -> M2/(cnt-1)   Bessel-corrected sample variance
              weighted   -> M2/wsum      biased weighted variance
       Nused: integer frame count (double)                                    */
    void*   out_img   = mxGetData(plhs[0]);
    void*   out_var   = want_var   ? mxGetData(plhs[1])   : nullptr;
    double* out_nused = want_nused ? static_cast<double*>(mxGetData(plhs[2])) : nullptr;

    const double qNaN_d = mxGetNaN();

    if (is_float) {
        float* img  = static_cast<float*>(out_img);
        float* var_ = static_cast<float*>(out_var);
        const float qNaN_f = static_cast<float>(qNaN_d);

        #pragma omp parallel for schedule(static)
        for (mwIndex p = 0; p < static_cast<mwIndex>(P); ++p) {
            const double c  = cur_cnt[p];
            const double ws = cur_wsum[p];
            if (out_nused) out_nused[p] = c;

            if (c < 1.0) {
                img[p] = qNaN_f;
                if (var_) var_[p] = qNaN_f;
            } else {
                img[p] = static_cast<float>(cur_mean[p]);
                if (var_) {
                    const double v = is_weighted
                        ? ((ws > 1e-300) ? cur_M2[p] / ws        : 0.0)
                        : ((c  > 1.0)   ? cur_M2[p] / (c - 1.0) : 0.0);
                    var_[p] = static_cast<float>(v >= 0.0 ? v : 0.0);
                }
            }
        }
    } else {
        double* img  = static_cast<double*>(out_img);
        double* var_ = static_cast<double*>(out_var);

        #pragma omp parallel for schedule(static)
        for (mwIndex p = 0; p < static_cast<mwIndex>(P); ++p) {
            const double c  = cur_cnt[p];
            const double ws = cur_wsum[p];
            if (out_nused) out_nused[p] = c;

            if (c < 1.0) {
                img[p] = qNaN_d;
                if (var_) var_[p] = qNaN_d;
            } else {
                img[p] = cur_mean[p];
                if (var_) {
                    const double v = is_weighted
                        ? ((ws > 1e-300) ? cur_M2[p] / ws        : 0.0)
                        : ((c  > 1.0)   ? cur_M2[p] / (c - 1.0) : 0.0);
                    var_[p] = (v >= 0.0) ? v : 0.0;
                }
            }
        }
    }

    /* AlignedBuf destructors free all temporary arrays.                      */
}
