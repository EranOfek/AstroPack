/*=============================================================================
 * modeVar_LeftHist_mex.cpp
 *
 * Fast MEX twin of modeVar_LeftHist.m: estimate the global image background
 * level (mode) and its noise variance from the left flank of the pixel
 * histogram, with the width fixed by Sigma0^2 = (B+RN2)/VarianceRatio so the
 * peak is orthogonal to the width (no peak/sigma covariance).
 *
 * Calling convention (identical to the .m):
 *   [Back, Var, Info] = modeVar_LeftHist_mex(Image, 'Name', Value, ...)
 *   Names: VarianceRatio RN2 BinFactor RangeLo RangeHi WinLo WinHi
 *          SmoothBins Niter MinBins FastMedian OS   (all numeric scalars)
 *
 *   RN2 (default 12): read-noise squared, in the same units as B (i.e. such
 *       that (B+RN2)/VarianceRatio is the noise variance in image-units^2). It
 *       adds a noise floor so the working scale stays well-defined as B -> 0.
 *   FastMedian (default 1): use the fast single-pass core (one fused O(N) pass,
 *       no data copy, no full-array selection; the median/seed come from a fine
 *       histogram). Set 0 for the exact core (nth_element median, bit-faithful
 *       to the .m) at ~5x the cost. OS (default 16) sets the fine-histogram
 *       oversampling relative to the working bin; larger OS -> closer to the
 *       exact binning (OS=16 keeps the smoothed-peak bin aligned well; very
 *       small OS can shift the argmax by a bin and perturb the fit). The fast
 *       core auto-falls back to the exact core if its working range ever runs
 *       off the fine grid.
 *
 * Supported image classes: double, single, uint16, int16, uint32, int32.
 *
 * Build (Linux/macOS, gcc/clang, OpenMP):
 *   mex -R2018a CXXFLAGS='$CXXFLAGS -O3 -march=native -fopenmp' ...
 *       LDFLAGS='$LDFLAGS -fopenmp' modeVar_LeftHist.cpp  %% leads to GLIBC incompatibilty 
 *   mex -O CXX=g++-9 CXXFLAGS='$CXXFLAGS -fopenmp -std=c++17' LDFLAGS='$LDFLAGS -fopenmp' modeVar_LeftHist.cpp
 * Build (Windows, MSVC):
 *   mex -R2018a COMPFLAGS='$COMPFLAGS /O2 /openmp' modeVar_LeftHist_mex.cpp
 *
 * Speed tricks used:
 *   - one O(N) pass to compact finite pixels + accumulate the mean;
 *   - exact median via std::nth_element (O(N)), not a full sort;
 *   - the working buffer is kept in the NATIVE pixel type (half the memory and
 *     cache traffic of a double copy for single/integer images);
 *   - the histogram is multi-threaded (OpenMP) with per-thread local bins and
 *     a tiny reduction; the bin index uses a precomputed reciprocal (multiply,
 *     no divide) and a branch-light range test;
 *   - all per-iteration model work (smoothing, peak, both fits) is over ~tens
 *     of bins, fully scalar and negligible; no log/divide touches the pixels;
 *   - the finite buffer is reused across refinement iterations (no re-extract).
 *
 * Standalone compile/test (no MATLAB):
 *   g++ -O3 -fopenmp -DTEST_MAIN modeVar_LeftHist_mex.cpp -o t && ./t
 *
 * Author: Eran Ofek (Jun 2026)
 *===========================================================================*/

#include <cmath>
#include <cctype>
#include <cstdint>
#include <cstring>
#include <vector>
#include <algorithm>
#ifdef _OPENMP
#include <omp.h>
#endif

/* ------------------------------- options -------------------------------- */
struct Opts {
    double VarianceRatio = 1.0;
    double RN2           = 12.0; // read-noise squared (same units as B)
    double BinFactor     = 0.2;
    double RangeLo       = 5.0;
    double RangeHi       = 5.0;
    double WinLo         = 3.0;
    double WinHi         = 0.5;
    double SmoothBins    = 3.0;
    int    Niter         = 1;
    int    MinBins       = 5;
    int    FastMedian    = 1;   // 1: fast single-pass core; 0: exact core
    int    OS            = 16;  // fine-histogram oversampling vs the working bin
};

struct Result {
    double Back   = NAN;
    double Var    = NAN;
    double Sigma0 = NAN;
    double VarPred= NAN;
    double Mode   = NAN;
    double Median = NAN;
    double Mean   = NAN;
    long long Npix = 0;
    int  Nbins    = 0;
    int  Niter    = 0;
    bool isFit    = false;
};

/* ---------------------------- small helpers ----------------------------- */

// MATLAB-style movmean over n points; window k, truncated at the endpoints.
static void movmean_vec(const double* c, int n, int k, double* out) {
    if (k <= 1) { std::memcpy(out, c, sizeof(double)*n); return; }
    int nb, naf;
    if (k & 1) { nb = (k - 1) / 2; naf = nb; }     // odd: centered
    else       { nb = k / 2;       naf = k/2 - 1; } // even: current + previous
    for (int i = 0; i < n; ++i) {
        int lo = i - nb; if (lo < 0) lo = 0;
        int hi = i + naf; if (hi > n - 1) hi = n - 1;
        double s = 0.0; int cnt = 0;
        for (int j = lo; j <= hi; ++j) { s += c[j]; ++cnt; }
        out[i] = s / (double)cnt;
    }
}

// Exact median; rearranges a[0..m). Matches MATLAB even-count averaging.
template <typename T>
static double median_inplace(T* a, size_t m) {
    size_t mid = m / 2;
    std::nth_element(a, a + mid, a + m);
    double upper = (double)a[mid];
    if (m & 1) return upper;
    double lower = (double)(*std::max_element(a, a + mid));
    return 0.5 * (lower + upper);
}

// Working scale: Sigma0^2 = (Level+RN2)/VR when (Level+RN2)>0, else a
// left-side MAD.
template <typename T>
static double local_scale(const T* a, size_t m, double Level, double VR, double RN2) {
    if (Level + RN2 > 0.0) return std::sqrt((Level + RN2) / VR);
    std::vector<double> left;
    left.reserve(m / 2 + 1);
    for (size_t i = 0; i < m; ++i) {
        double v = (double)a[i];
        if (v < Level) left.push_back(Level - v);   // = positive deviations
    }
    double s0;
    if (left.size() >= 5) {
        size_t mid = left.size() / 2;
        std::nth_element(left.begin(), left.begin() + mid, left.end());
        double up = left[mid];
        if (left.size() & 1) s0 = 1.4826 * up;
        else { double lo = *std::max_element(left.begin(), left.begin()+mid);
               s0 = 1.4826 * 0.5 * (lo + up); }
    } else {                                          // last resort: full MAD
        std::vector<double> ad(m);
        for (size_t i = 0; i < m; ++i) ad[i] = std::fabs((double)a[i] - Level);
        s0 = 1.4826 * median_inplace(ad.data(), m);
    }
    return (std::isfinite(s0) && s0 > 0.0) ? s0 : 1.0;
}

// Multi-threaded histogram of a[0..m) onto nb bins of width 1/invH from E0.
template <typename T>
static void histogram(const T* a, size_t m, double E0, double invH,
                      int nb, double Eend, long long* hist) {
    for (int b = 0; b < nb; ++b) hist[b] = 0;
#ifdef _OPENMP
    #pragma omp parallel
    {
        std::vector<long long> loc(nb, 0);
        #pragma omp for nowait schedule(static)
        for (ptrdiff_t i = 0; i < (ptrdiff_t)m; ++i) {
            double x = (double)a[i];
            double d = x - E0;
            if (d < 0.0) continue;
            int idx = (int)(d * invH);
            if (idx >= nb) { if (x <= Eend) idx = nb - 1; else continue; }
            ++loc[idx];
        }
        #pragma omp critical
        { for (int b = 0; b < nb; ++b) hist[b] += loc[b]; }
    }
#else
    for (size_t i = 0; i < m; ++i) {
        double x = (double)a[i];
        double d = x - E0;
        if (d < 0.0) continue;
        int idx = (int)(d * invH);
        if (idx >= nb) { if (x <= Eend) idx = nb - 1; else continue; }
        ++hist[idx];
    }
#endif
}

/* ------------------------------- core ----------------------------------- */
// Returns 0 on success, <0 if there are too few finite pixels.
template <typename T>
static int run_core(const T* d, size_t n, const Opts& o, Result& R) {
    // Pass 1: compact finite pixels (native type) and accumulate the mean.
    std::vector<T> buf(n);
    double sum = 0.0;
    size_t c = 0;
    for (size_t i = 0; i < n; ++i) {
        double v = (double)d[i];
        if (std::isfinite(v)) { buf[c++] = d[i]; sum += v; }
    }
    if (c < 10) return -1;
    const double mean = sum / (double)c;
    const double med  = median_inplace(buf.data(), c);   // reorders buf (ok)

    const double VR  = o.VarianceRatio;
    const double RN2 = o.RN2;
    double Center = med;
    double Sigma0 = local_scale(buf.data(), c, med, VR, RN2);

    bool   isFit   = false;
    double Back    = NAN;
    double ModeRaw = NAN;
    int    NbinsUse= 0;
    std::vector<double> winXc, winN;                     // last good window

    const int kSmooth = (int)std::lround(o.SmoothBins);

    for (int it = 0; it <= o.Niter; ++it) {
        const double H = o.BinFactor * Sigma0;
        if (!(H > 0.0)) break;
        const double E0   = Center - o.RangeLo * Sigma0;
        const double span = (o.RangeLo + o.RangeHi) * Sigma0;
        const int    nb   = (int)std::floor(span / H + 1e-9);   // #bins
        if (nb < 2) break;
        const double Eend = E0 + nb * H;
        const double invH = 1.0 / H;

        std::vector<long long> hist(nb);
        histogram(buf.data(), c, E0, invH, nb, Eend, hist.data());

        std::vector<double> cnt(nb), xc(nb), cs(nb);
        for (int i = 0; i < nb; ++i) { cnt[i] = (double)hist[i];
                                       xc[i]  = E0 + (i + 0.5) * H; }
        movmean_vec(cnt.data(), nb, kSmooth, cs.data());

        int ipk = 0; double best = cs[0];
        for (int i = 1; i < nb; ++i) if (cs[i] > best) { best = cs[i]; ipk = i; }
        const double M0 = xc[ipk];
        ModeRaw = M0;

        const double Lo = M0 - o.WinLo * Sigma0;
        const double Hi = M0 + o.WinHi * Sigma0;

        std::vector<double> wx, wn;
        wx.reserve(nb); wn.reserve(nb);
        for (int i = 0; i < nb; ++i)
            if (xc[i] >= Lo && xc[i] <= Hi && cnt[i] > 0.0) {
                wx.push_back(xc[i]); wn.push_back(cnt[i]);
            }
        NbinsUse = (int)wx.size();
        if (NbinsUse < o.MinBins) break;

        // Fixed-sigma location: weighted linear regression of
        // z = log(N) + xx^2/(2 Sigma0^2) on xx = Xc - M0; peak = M0 + s*Sigma0^2.
        const double s2 = Sigma0 * Sigma0;
        double Sw = 0, Sx = 0, Sz = 0;
        const int m = NbinsUse;
        std::vector<double> XX(m), ZZ(m);
        for (int i = 0; i < m; ++i) {
            double xx = wx[i] - M0;
            double zz = std::log(wn[i]) + xx * xx / (2.0 * s2);
            XX[i] = xx; ZZ[i] = zz;
            Sw += wn[i]; Sx += wn[i] * xx; Sz += wn[i] * zz;
        }
        double Xbar = Sx / Sw, Zbar = Sz / Sw, Sxx = 0, Sxz = 0;
        for (int i = 0; i < m; ++i) {
            double dx = XX[i] - Xbar;
            Sxx += wn[i] * dx * dx;
            Sxz += wn[i] * dx * (ZZ[i] - Zbar);
        }
        if (!(Sxx > 0.0) || !std::isfinite(Sxz)) break;
        double Slope    = Sxz / Sxx;
        double CandBack = M0 + Slope * s2;
        if (!(std::isfinite(CandBack) && CandBack >= Lo && CandBack <= Hi)) break;

        Back   = CandBack;
        isFit  = true;
        winXc  = wx;  winN = wn;                          // keep for Var stage

        Center = Back;
        Sigma0 = local_scale(buf.data(), c, Back, VR, RN2);
    }

    // Decoupled variance: peak fixed, regress log(N) on (Xc-Back)^2.
    double Var = NAN;
    if (isFit) {
        const int m = (int)winXc.size();
        double Sw = 0, Su = 0, Sy = 0;
        std::vector<double> UU(m), YY(m);
        for (int i = 0; i < m; ++i) {
            double u = winXc[i] - Back; u *= u;
            double y = std::log(winN[i]);
            UU[i] = u; YY[i] = y;
            Sw += winN[i]; Su += winN[i] * u; Sy += winN[i] * y;
        }
        double Ubar = Su / Sw, Ybar = Sy / Sw, Suu = 0, Suy = 0;
        for (int i = 0; i < m; ++i) {
            double du = UU[i] - Ubar;
            Suu += winN[i] * du * du;
            Suy += winN[i] * du * (YY[i] - Ybar);
        }
        if (Suu > 0.0) {
            double Q = -Suy / Suu;                        // slope is -q
            if (Q > 0.0 && std::isfinite(Q)) Var = 1.0 / (2.0 * Q);
        }
    }

    // Fallback / completion.
    if (!isFit || !std::isfinite(Back)) {
        Back   = 2.5 * med - 1.5 * mean;                  // SExtractor mode
        Var    = std::fmax(Back + RN2, 0.0) / VR;
        isFit  = false;
    } else if (!std::isfinite(Var)) {
        Var    = std::fmax(Back + RN2, 0.0) / VR;
    }

    R.Back    = Back;
    R.Var     = Var;
    R.Sigma0  = Sigma0;
    R.VarPred = std::fmax(Back + RN2, 0.0) / VR;
    R.Mode    = ModeRaw;
    R.Median  = med;
    R.Mean    = mean;
    R.Npix    = (long long)c;
    R.Nbins   = NbinsUse;
    R.Niter   = o.Niter;
    R.isFit   = isFit;
    return 0;
}

/* ----------------------- fast streaming core ---------------------------- */
// Single fused O(N) pass: exact sum/count + a fine histogram built straight
// from the read-only input (no copy, no per-element selection). The median is
// a sub-bin-interpolated quantile of the fine histogram (seed + fallback only),
// and every refinement iteration re-bins the fine histogram instead of
// re-scanning pixels. Returns 0 (ok), -1 (too few finite pixels), or -2 (the
// refined working range ran off the fine-histogram edge -> caller should retry
// with a wider range or use the exact core).
template <typename T>
static int run_fast(const T* d, size_t n, const Opts& o, double pad, Result& R) {
    const double VR  = o.VarianceRatio;
    const double RN2 = o.RN2;

    // (1) subsample for a provisional center C0 and scale S0 (sets the range).
    const size_t target = 20000;
    size_t stride = n / target; if (stride < 1) stride = 1;
    std::vector<double> samp; samp.reserve(n / stride + 1);
    for (size_t i = 0; i < n; i += stride) {
        double v = (double)d[i]; if (std::isfinite(v)) samp.push_back(v);
    }
    if (samp.size() < 10) return -1;
    size_t sm = samp.size() / 2;
    std::nth_element(samp.begin(), samp.begin() + sm, samp.end());
    double C0 = samp[sm];
    double S0;
    if (C0 + RN2 > 0.0) S0 = std::sqrt((C0 + RN2) / VR);
    else {                                              // sample MAD if C0+RN2<=0
        std::vector<double> a(samp.size());
        for (size_t i = 0; i < samp.size(); ++i) a[i] = std::fabs(samp[i] - C0);
        size_t mm = a.size() / 2;
        std::nth_element(a.begin(), a.begin() + mm, a.end());
        S0 = 1.4826 * a[mm];
    }
    if (!(S0 > 0.0)) S0 = 1.0;

    // (2) fine-histogram grid: generous range around C0, oversampled bins.
    const int    OS    = (o.OS >= 1) ? o.OS : 1;
    const double Hf    = o.BinFactor * S0 / OS;
    const double FE0   = C0 - (o.RangeLo + pad) * S0;
    const double Fspan = (o.RangeLo + o.RangeHi + 2.0 * pad) * S0;
    const int    FNB   = (int)std::floor(Fspan / Hf + 1e-9);
    if (FNB < 4) return -1;
    const double FEend = FE0 + FNB * Hf;
    const double invHf = 1.0 / Hf;

    // (3) ONE fused streaming pass over the read-only input.
    std::vector<long long> fh(FNB, 0);
    double sum = 0.0; long long cnt = 0;
#ifdef _OPENMP
    #pragma omp parallel
    {
        std::vector<long long> loc(FNB, 0); double ls = 0.0; long long lc = 0;
        #pragma omp for nowait schedule(static)
        for (ptrdiff_t i = 0; i < (ptrdiff_t)n; ++i) {
            double v = (double)d[i]; if (!std::isfinite(v)) continue;
            ls += v; ++lc;
            double dd = v - FE0; if (dd < 0.0) continue;
            int idx = (int)(dd * invHf);
            if (idx >= FNB) { if (v <= FEend) idx = FNB - 1; else continue; }
            ++loc[idx];
        }
        #pragma omp critical
        { sum += ls; cnt += lc; for (int b = 0; b < FNB; ++b) fh[b] += loc[b]; }
    }
#else
    for (size_t i = 0; i < n; ++i) {
        double v = (double)d[i]; if (!std::isfinite(v)) continue;
        sum += v; ++cnt;
        double dd = v - FE0; if (dd < 0.0) continue;
        int idx = (int)(dd * invHf);
        if (idx >= FNB) { if (v <= FEend) idx = FNB - 1; else continue; }
        ++fh[idx];
    }
#endif
    if (cnt < 10) return -1;
    const double mean = sum / (double)cnt;

    // Median: cumulative-count quantile with linear interpolation (sub-bin).
    long long half = cnt / 2, acc = 0; double med = C0;
    for (int b = 0; b < FNB; ++b) {
        if (acc + fh[b] >= half) {
            double frac = (half - acc) / (double)(fh[b] > 0 ? fh[b] : 1);
            med = FE0 + (b + frac) * Hf; break;
        }
        acc += fh[b];
    }

    // Re-bin: coarse [a,b) = sum of fine bins whose centers fall inside [a,b].
    auto coarse = [&](double a, double b) -> double {
        int ia = (int)std::ceil ((a - FE0) / Hf - 0.5);
        int ib = (int)std::floor((b - FE0) / Hf - 0.5);
        if (ia < 0) ia = 0; if (ib > FNB - 1) ib = FNB - 1;
        double s = 0.0; for (int j = ia; j <= ib; ++j) s += (double)fh[j]; return s;
    };

    // (4) iterate entirely on the fine histogram (no further pixel passes).
    double Center = C0, Sig = S0; bool isFit = false;
    double Back = NAN, ModeRaw = NAN; int NbinsUse = 0;
    std::vector<double> winXc, winN;
    const int    kSmooth = (int)std::lround(o.SmoothBins);
    const double EPS = 1e-9 * (std::fabs(FE0) + Fspan + 1.0);

    for (int it = 0; it <= o.Niter; ++it) {
        const double H = o.BinFactor * Sig; if (!(H > 0.0)) break;
        const double E0   = Center - o.RangeLo * Sig;
        const double span = (o.RangeLo + o.RangeHi) * Sig;
        const int    nb   = (int)std::floor(span / H + 1e-9); if (nb < 2) break;
        const double Ehi  = E0 + nb * H;
        if (E0 < FE0 - EPS || Ehi > FEend + EPS) return -2;   // edge guard

        std::vector<double> cnt2(nb), xc(nb), cs(nb);
        for (int i = 0; i < nb; ++i) {
            cnt2[i] = coarse(E0 + i * H, E0 + (i + 1) * H);
            xc[i]   = E0 + (i + 0.5) * H;
        }
        movmean_vec(cnt2.data(), nb, kSmooth, cs.data());
        int ipk = 0; double best = cs[0];
        for (int i = 1; i < nb; ++i) if (cs[i] > best) { best = cs[i]; ipk = i; }
        const double M0 = xc[ipk]; ModeRaw = M0;
        const double Lo = M0 - o.WinLo * Sig, Hi = M0 + o.WinHi * Sig;

        std::vector<double> wx, wn; wx.reserve(nb); wn.reserve(nb);
        for (int i = 0; i < nb; ++i)
            if (xc[i] >= Lo && xc[i] <= Hi && cnt2[i] > 0.0) {
                wx.push_back(xc[i]); wn.push_back(cnt2[i]);
            }
        NbinsUse = (int)wx.size(); if (NbinsUse < o.MinBins) break;

        const double s2 = Sig * Sig;
        double Sw = 0, Sx = 0, Sz = 0; const int m = NbinsUse;
        std::vector<double> XX(m), ZZ(m);
        for (int i = 0; i < m; ++i) {
            double xx = wx[i] - M0;
            double zz = std::log(wn[i]) + xx * xx / (2.0 * s2);
            XX[i] = xx; ZZ[i] = zz; Sw += wn[i]; Sx += wn[i]*xx; Sz += wn[i]*zz;
        }
        double Xbar = Sx/Sw, Zbar = Sz/Sw, Sxx = 0, Sxz = 0;
        for (int i = 0; i < m; ++i) { double dx = XX[i]-Xbar;
            Sxx += wn[i]*dx*dx; Sxz += wn[i]*dx*(ZZ[i]-Zbar); }
        if (!(Sxx > 0.0) || !std::isfinite(Sxz)) break;
        double Slope = Sxz / Sxx, CandBack = M0 + Slope * s2;
        if (!(std::isfinite(CandBack) && CandBack >= Lo && CandBack <= Hi)) break;

        Back   = CandBack; isFit = true; winXc = wx; winN = wn;
        Center = Back; Sig = (Back + RN2 > 0.0) ? std::sqrt((Back + RN2) / VR) : S0;
    }

    // Decoupled variance (peak fixed): regress log(N) on (Xc-Back)^2.
    double Var = NAN;
    if (isFit) {
        const int m = (int)winXc.size();
        double Sw = 0, Su = 0, Sy = 0; std::vector<double> UU(m), YY(m);
        for (int i = 0; i < m; ++i) {
            double u = winXc[i] - Back; u *= u; double y = std::log(winN[i]);
            UU[i] = u; YY[i] = y; Sw += winN[i]; Su += winN[i]*u; Sy += winN[i]*y;
        }
        double Ubar = Su/Sw, Ybar = Sy/Sw, Suu = 0, Suy = 0;
        for (int i = 0; i < m; ++i) { double du = UU[i]-Ubar;
            Suu += winN[i]*du*du; Suy += winN[i]*du*(YY[i]-Ybar); }
        if (Suu > 0.0) { double Q = -Suy / Suu;
            if (Q > 0.0 && std::isfinite(Q)) Var = 1.0 / (2.0 * Q); }
    }
    if (!isFit || !std::isfinite(Back)) {
        Back = 2.5 * med - 1.5 * mean; Var = std::fmax(Back + RN2, 0.0) / VR; isFit = false;
    } else if (!std::isfinite(Var)) {
        Var = std::fmax(Back + RN2, 0.0) / VR;
    }

    R.Back    = Back;
    R.Var     = Var;
    R.Sigma0  = Sig;
    R.VarPred = std::fmax(Back + RN2, 0.0) / VR;
    R.Mode    = ModeRaw;
    R.Median  = med;
    R.Mean    = mean;
    R.Npix    = (long long)cnt;
    R.Nbins   = NbinsUse;
    R.Niter   = o.Niter;
    R.isFit   = isFit;
    return 0;
}

/* ------------------------------ dispatch -------------------------------- */
// Default: fast streaming core. If its working range runs off the fine-grid
// edge, retry once with a wider range; if it still trips (or FastMedian==0),
// use the exact core. Returns 0 on success, <0 if too few finite pixels.
template <typename T>
static int run(const T* d, size_t n, const Opts& o, Result& R) {
    if (o.FastMedian) {
        int st = run_fast(d, n, o, 3.0, R);
        if (st == 0) return 0;
        if (st == -2) { st = run_fast(d, n, o, 8.0, R); if (st == 0) return 0; }
        // -1 (tiny/odd input) or persistent -2 -> fall through to the exact core
    }
    return run_core(d, n, o, R);
}

/* ============================ MATLAB gateway ============================= */
#ifdef MATLAB_MEX_FILE
#include "mex.h"

static bool ieq(const char* a, const char* b) {       // case-insensitive eq
    for (; *a && *b; ++a, ++b)
        if (std::tolower((unsigned char)*a) != std::tolower((unsigned char)*b))
            return false;
    return *a == *b;
}

static void parse_opts(int nrhs, const mxArray* prhs[], Opts& o) {
    for (int i = 1; i + 1 < nrhs; i += 2) {
        if (!mxIsChar(prhs[i]))
            mexErrMsgIdAndTxt("modeVar:arg", "Option names must be strings.");
        char name[64]; mxGetString(prhs[i], name, sizeof(name));
        double v = mxGetScalar(prhs[i + 1]);
        if      (ieq(name, "VarianceRatio")) o.VarianceRatio = v;
        else if (ieq(name, "RN2"))           o.RN2           = v;
        else if (ieq(name, "BinFactor"))     o.BinFactor     = v;
        else if (ieq(name, "RangeLo"))       o.RangeLo       = v;
        else if (ieq(name, "RangeHi"))       o.RangeHi       = v;
        else if (ieq(name, "WinLo"))         o.WinLo         = v;
        else if (ieq(name, "WinHi"))         o.WinHi         = v;
        else if (ieq(name, "SmoothBins"))    o.SmoothBins    = v;
        else if (ieq(name, "Niter"))         o.Niter         = (int)v;
        else if (ieq(name, "MinBins"))       o.MinBins       = (int)v;
        else if (ieq(name, "FastMedian"))    o.FastMedian    = (v != 0.0);
        else if (ieq(name, "OS"))            o.OS            = (int)v;
        else mexErrMsgIdAndTxt("modeVar:arg", "Unknown option '%s'.", name);
    }
}

void mexFunction(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[]) {
    if (nrhs < 1 || !mxIsNumeric(prhs[0]))
        mexErrMsgIdAndTxt("modeVar:in", "First argument must be a numeric image.");
    if (nrhs % 2 != 1)
        mexErrMsgIdAndTxt("modeVar:in", "Options must come in name/value pairs.");

    Opts o; parse_opts(nrhs, prhs, o);
    if (!(o.VarianceRatio > 0))
        mexErrMsgIdAndTxt("modeVar:in", "VarianceRatio must be positive.");

    const size_t n = (size_t)mxGetNumberOfElements(prhs[0]);
    const void*  p = mxGetData(prhs[0]);
    Result R; int st = -1;

    switch (mxGetClassID(prhs[0])) {
        case mxDOUBLE_CLASS: st = run((const double*)  p, n, o, R); break;
        case mxSINGLE_CLASS: st = run((const float*)   p, n, o, R); break;
        case mxUINT16_CLASS: st = run((const uint16_T*)p, n, o, R); break;
        case mxINT16_CLASS:  st = run((const int16_T*) p, n, o, R); break;
        case mxUINT32_CLASS: st = run((const uint32_T*)p, n, o, R); break;
        case mxINT32_CLASS:  st = run((const int32_T*) p, n, o, R); break;
        default:
            mexErrMsgIdAndTxt("modeVar:type",
                "Unsupported class (use double/single/uint16/int16/uint32/int32).");
    }
    if (st < 0)
        mexErrMsgIdAndTxt("modeVar:few", "Too few finite pixels for an estimate.");

    plhs[0] = mxCreateDoubleScalar(R.Back);
    if (nlhs >= 2) plhs[1] = mxCreateDoubleScalar(R.Var);
    if (nlhs >= 3) {
        const char* f[] = {"Method","Mode","Sigma0","VarPred",
                           "Npix","Nbins","Niter","Median","Mean"};
        mxArray* s = mxCreateStructMatrix(1, 1, 9, f);
        mxSetField(s, 0, "Method",  mxCreateString(R.isFit ? "fit" : "fallback"));
        mxSetField(s, 0, "Mode",    mxCreateDoubleScalar(R.Mode));
        mxSetField(s, 0, "Sigma0",  mxCreateDoubleScalar(R.Sigma0));
        mxSetField(s, 0, "VarPred", mxCreateDoubleScalar(R.VarPred));
        mxSetField(s, 0, "Npix",    mxCreateDoubleScalar((double)R.Npix));
        mxSetField(s, 0, "Nbins",   mxCreateDoubleScalar((double)R.Nbins));
        mxSetField(s, 0, "Niter",   mxCreateDoubleScalar((double)R.Niter));
        mxSetField(s, 0, "Median",  mxCreateDoubleScalar(R.Median));
        mxSetField(s, 0, "Mean",    mxCreateDoubleScalar(R.Mean));
        plhs[2] = s;
    }
}
#endif /* MATLAB_MEX_FILE */

/* ============================ standalone test =========================== */
#ifdef TEST_MAIN
#include <cstdio>
#include <random>
int main() {
    const size_t W = 2048, Hh = 2048, N = W * Hh;
    const double B = 1000.0, sig = std::sqrt(B);
    std::vector<double> img(N);
    std::mt19937_64 rng(42);
    std::normal_distribution<double> gN(B, sig);
    for (size_t i = 0; i < N; ++i) img[i] = gN(rng);

    // crowd ~30% of pixels with positive source flux
    std::uniform_real_distribution<double> U(0, 1);
    std::exponential_distribution<double> Eflux(1.0 / 400.0);
    for (size_t i = 0; i < N; ++i) if (U(rng) < 0.30) img[i] += Eflux(rng);

    Opts o; Result R;
    run(img.data(), N, o, R);
    std::printf("Back   = %.3f (true B = %.1f)\n", R.Back, B);
    std::printf("Var    = %.3f (VarPred = %.3f, true noise var = %.1f)\n",
                R.Var, R.VarPred, B);
    std::printf("Mode   = %.3f  Median = %.3f  Mean = %.3f\n",
                R.Mode, R.Median, R.Mean);
    std::printf("Npix=%lld Nbins=%d Method=%s\n",
                R.Npix, R.Nbins, R.isFit ? "fit" : "fallback");
    return 0;
}
#endif
