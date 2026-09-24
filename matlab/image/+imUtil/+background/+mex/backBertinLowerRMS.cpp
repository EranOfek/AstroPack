/*=============================================================================
 * backBertinLowerRMS.cpp   (single-file MEX)
 *
 *  [Back, Var, BackSmall, VarSmall] = backBertinLowerRMS(Image, BACK_SIZE, ...
 *                                        BACK_FILTERSIZE, BACK_FILTERTHRESH)
 *
 *  Same as backBertin for the BACKGROUND LEVEL (SExtractor / Bertin & Arnouts
 *  1996: kappa-sigma clip; clipped mean if sigma changed <20%, else the mode
 *  2.5*median-1.5*mean), but the VARIANCE is always estimated from the pixels
 *  BELOW the mesh background level:
 *
 *      sigma = sqrt( mean( (v - Back)^2   over pixels with v < Back ) )
 *      Var   = sigma^2
 *
 *  The half-normal RMS taken about the split point (Back) equals the full
 *  Gaussian sigma exactly, so the rescaling factor is 1 (no magic constant).
 *  This is robust to the positive source tail that inflates the ordinary
 *  clipped sigma in crowded fields. It ASSUMES the low side of the histogram
 *  is clean sky, so do NOT use it on difference / already-sky-subtracted images
 *  (where real negative pixels exist).
 *
 *  Efficiency: the level is found exactly as in backBertin (the mode median is
 *  computed only for crowded meshes). The below-Back variance is a single
 *  extra branchless pass over the mesh pixels, run only when Var/VarSmall is
 *  requested. All work is per-mesh and OpenMP-parallel over meshes.
 *
 *  Image             2-D real, class double or single (column-major). NaNs are
 *                    treated as masked and ignored.
 *  BACK_SIZE         mesh size [pix]. Scalar (square) or [sRows sCols]. Def 128.
 *  BACK_FILTERSIZE   median-filter size over the mesh maps (mesh units). Def 3.
 *  BACK_FILTERTHRESH replace a node by its median only if it differs by more
 *                    than this. Def 0 (always).
 *
 *  Back      background surface B(x,y),  size(Image), class double.
 *  Var       (optional) background variance sigma_bck^2(x,y). Built only if asked.
 *  BackSmall (optional) low-resolution background on the mesh grid, AFTER median
 *            filtering and BEFORE spline interpolation. Size [ny nx] =
 *            [ceil(M/bh) ceil(N/bw)]; BackSmall(iy,ix) = mesh row iy, column ix.
 *  VarSmall  (optional) low-resolution variance on the mesh grid = (median-
 *            filtered lower-RMS mesh)^2. Size [ny nx].
 *
 * Build (from MATLAB), OpenMP optional:
 *   mex -R2018a CXXFLAGS="$CXXFLAGS -O3 -fopenmp" LDFLAGS="$LDFLAGS -fopenmp" backBertinLowerRMS.cpp
 *   serial:   mex -R2018a CXXFLAGS="$CXXFLAGS -O3" backBertinLowerRMS.cpp
 *===========================================================================*/
#include "mex.h"
#include <cmath>
#include <vector>
#include <algorithm>
#include <cstddef>

/*===========================================================================
 * Numerical core. Images are column-major, M rows x N columns.
 * Mesh node values are stored with ix varying fastest: idx = ix + iy*nx.
 *=========================================================================*/
namespace bb {

static const int    BB_MAXIT = 100;    /* max kappa-sigma iterations (rarely >~5) */
static const double BB_TOL   = 1e-5;   /* relative convergence on sigma           */
static const double BB_KAPPA = 3.0;    /* clip at +/- 3 sigma                     */
static const double BB_CROWD = 0.20;   /* sigma changed >20% -> crowded -> mode   */

/* Median of a small scratch buffer (modifies a). Averages the two central
 * order statistics for even n. O(n) average via nth_element (no full sort). */
static inline double median_inplace(double* a, int n) {
    if (n <= 0) return 0.0;
    int k = n >> 1;
    std::nth_element(a, a + k, a + n);
    double m = a[k];
    if ((n & 1) == 0) {
        double lo = *std::max_element(a, a + k);
        m = 0.5 * (m + lo);
    }
    return m;
}

/* Natural cubic-spline 2nd derivatives, n equally spaced points (spacing h).
 * work must have length >= n. Handles n<3 (y2 = 0 -> linear/constant). */
static inline void splineNatural(const double* y, int n, double h,
                                 double* y2, double* work) {
    if (n < 3) { for (int i = 0; i < n; ++i) y2[i] = 0.0; return; }
    const double invh2 = 1.0 / (h * h);
    y2[0] = 0.0; work[0] = 0.0;
    for (int i = 1; i < n - 1; ++i) {
        const double p = 0.5 * y2[i - 1] + 2.0;
        y2[i]   = -0.5 / p;
        const double d = (y[i + 1] - 2.0 * y[i] + y[i - 1]) * invh2;
        work[i] = (3.0 * d - 0.5 * work[i - 1]) / p;
    }
    y2[n - 1] = 0.0;
    for (int k = n - 2; k >= 0; --k) y2[k] = y2[k] * y2[k + 1] + work[k];
}

/* Per-mesh statistics. Level exactly as in backBertin (mode median only when
 * crowded). If needVar, the RMS is taken from the pixels below the level
 * (branchless pass); otherwise the clipped sigma is stored. Outputs level[],
 * sigma[] (nx*ny, ix+iy*nx). */
template <class T>
void computeMesh(const T* img, int M, int N, int bw, int bh, int nx, int ny,
                 double* level, double* sigma, bool needVar) {
    const int tileMax = bw * bh;
#ifdef _OPENMP
    #pragma omp parallel
#endif
    {
        std::vector<double> buf(tileMax), medbuf(tileMax);
#ifdef _OPENMP
        #pragma omp for collapse(2) schedule(dynamic)
#endif
        for (int iy = 0; iy < ny; ++iy) {
            for (int ix = 0; ix < nx; ++ix) {
                const int r0 = iy * bh, r1 = std::min(r0 + bh, M);
                const int c0 = ix * bw, c1 = std::min(c0 + bw, N);

                int cnt = 0; double s = 0.0, ss = 0.0;
                double* b = buf.data();
                for (int c = c0; c < c1; ++c) {
                    const T* colp = img + (std::size_t)c * M;
                    for (int r = r0; r < r1; ++r) {
                        const double v = (double)colp[r];
                        if (std::isnan(v)) continue;
                        b[cnt++] = v; s += v; ss += v * v;
                    }
                }
                if (cnt == 0) { level[ix + iy * nx] = 0.0; sigma[ix + iy * nx] = 0.0; continue; }

                double mn  = s / cnt;
                double var = ss / cnt - mn * mn; if (var < 0.0) var = 0.0;
                const double sig0 = std::sqrt(var);
                double sig = sig0;

                if (sig0 > 0.0) {
                    for (int it = 0; it < BB_MAXIT; ++it) {
                        const double lo = mn - BB_KAPPA * sig;
                        const double hi = mn + BB_KAPPA * sig;
                        double s2 = 0.0, ss2 = 0.0, c2 = 0.0;
                        for (int i = 0; i < cnt; ++i) {
                            const double v = b[i];
                            const double in = (double)((v >= lo) & (v <= hi));
                            s2 += in * v; ss2 += in * v * v; c2 += in;
                        }
                        if (c2 < 1.0) break;
                        const double m2 = s2 / c2;
                        double v2 = ss2 / c2 - m2 * m2; if (v2 < 0.0) v2 = 0.0;
                        const double sg2 = std::sqrt(v2);
                        const bool conv = (sg2 <= 0.0) ||
                                          (std::fabs(sig - sg2) < BB_TOL * (sig > 0.0 ? sig : 1.0));
                        mn = m2; sig = sg2;
                        if (conv) break;
                    }
                }

                double lev;
                if (sig0 <= 0.0 || (sig0 - sig) < BB_CROWD * sig0) {
                    lev = mn;                                /* uncrowded: clipped mean */
                } else {
                    const double lo = mn - BB_KAPPA * sig;
                    const double hi = mn + BB_KAPPA * sig;
                    int mc = 0;
                    for (int i = 0; i < cnt; ++i) {
                        const double v = b[i];
                        if (v >= lo && v <= hi) medbuf[mc++] = v;
                    }
                    const double med = (mc > 0) ? median_inplace(medbuf.data(), mc) : mn;
                    lev = 2.5 * med - 1.5 * mn;               /* Bertin & Arnouts mode */
                }

                double rms = sig;                            /* fallback if Var unused */
                if (needVar) {
                    double sslo = 0.0, nlow = 0.0;           /* below-Back RMS, factor 1 */
                    for (int i = 0; i < cnt; ++i) {
                        const double v = b[i];
                        const double d = v - lev;
                        const double m = (double)(v < lev);
                        sslo += m * d * d; nlow += m;
                    }
                    rms = (nlow >= 2.0) ? std::sqrt(sslo / nlow) : sig;
                }

                level[ix + iy * nx] = lev;
                sigma[ix + iy * nx] = rms;
            }
        }
    }
}

/* Median filter the (small) nx*ny mesh map. Replace by window median only if
 * |value-median| > thresh (thresh<=0 -> always). */
static inline void medianFilterMesh(const double* in, double* out,
                                    int nx, int ny, int fsize, double thresh) {
    const int h = fsize / 2;
    std::vector<double> win(fsize * fsize);
    for (int iy = 0; iy < ny; ++iy) {
        for (int ix = 0; ix < nx; ++ix) {
            int n = 0;
            for (int jy = std::max(0, iy - h); jy <= std::min(ny - 1, iy + h); ++jy)
                for (int jx = std::max(0, ix - h); jx <= std::min(nx - 1, ix + h); ++jx)
                    win[n++] = in[jx + jy * nx];
            const double med = median_inplace(win.data(), n);
            const double val = in[ix + iy * nx];
            out[ix + iy * nx] = (std::fabs(val - med) > thresh) ? med : val;
        }
    }
}

/* Interpolate a filtered mesh map (nx*ny) onto the full M*N image with a natural
 * bicubic spline (tensor product of 1D natural splines): x first, then y down
 * each column, so output is written column-contiguous (MATLAB layout).
 * squareOut=true writes value^2 (turns the RMS surface into a variance map). */
static inline void interpMeshToImage(const double* bk, int nx, int ny,
                                     int bw, int bh, int M, int N,
                                     double* out, bool squareOut) {
    const double hx = (double)bw, hy = (double)bh;

    std::vector<double> d2x((std::size_t)nx * ny), work(std::max(nx, ny));
    if (nx >= 2)
        for (int iy = 0; iy < ny; ++iy)
            splineNatural(bk + iy * nx, nx, hx, d2x.data() + iy * nx, work.data());
    else
        std::fill(d2x.begin(), d2x.end(), 0.0);

    std::vector<int>    ky(M);
    std::vector<double> yA(M), yB(M), ycA(M), ycB(M);
    if (ny >= 2) {
        for (int cy = 0; cy < M; ++cy) {
            const double p = (double)cy;
            int k = (int)std::floor(p / hy - 0.5);
            if (k < 0) k = 0; if (k > ny - 2) k = ny - 2;
            const double A = ((k + 1.5) - p / hy);
            const double B = (p / hy - (k + 0.5));
            ky[cy] = k; yA[cy] = A; yB[cy] = B;
            ycA[cy] = (A * A * A - A) * hy * hy / 6.0;
            ycB[cy] = (B * B * B - B) * hy * hy / 6.0;
        }
    }
    std::vector<int>    jx(N);
    std::vector<double> xA(N), xB(N), xcA(N), xcB(N);
    if (nx >= 2) {
        for (int cx = 0; cx < N; ++cx) {
            const double p = (double)cx;
            int j = (int)std::floor(p / hx - 0.5);
            if (j < 0) j = 0; if (j > nx - 2) j = nx - 2;
            const double A = ((j + 1.5) - p / hx);
            const double B = (p / hx - (j + 0.5));
            jx[cx] = j; xA[cx] = A; xB[cx] = B;
            xcA[cx] = (A * A * A - A) * hx * hx / 6.0;
            xcB[cx] = (B * B * B - B) * hx * hx / 6.0;
        }
    }

#ifdef _OPENMP
    #pragma omp parallel
#endif
    {
        std::vector<double> temp(ny), d2t(ny), w(ny);
#ifdef _OPENMP
        #pragma omp for schedule(static)
#endif
        for (int cx = 0; cx < N; ++cx) {
            if (nx >= 2) {
                const int j = jx[cx];
                const double A = xA[cx], B = xB[cx], cA = xcA[cx], cB = xcB[cx];
                for (int iy = 0; iy < ny; ++iy) {
                    const double* row = bk  + iy * nx;
                    const double* d2  = d2x.data() + iy * nx;
                    temp[iy] = A * row[j] + B * row[j + 1] + cA * d2[j] + cB * d2[j + 1];
                }
            } else {
                for (int iy = 0; iy < ny; ++iy) temp[iy] = bk[iy * nx];
            }

            double* col = out + (std::size_t)cx * M;
            if (ny >= 2) {
                splineNatural(temp.data(), ny, hy, d2t.data(), w.data());
                for (int cy = 0; cy < M; ++cy) {
                    const int k = ky[cy];
                    double v = yA[cy] * temp[k] + yB[cy] * temp[k + 1]
                             + ycA[cy] * d2t[k] + ycB[cy] * d2t[k + 1];
                    col[cy] = squareOut ? v * v : v;
                }
            } else {
                const double v = squareOut ? temp[0] * temp[0] : temp[0];
                for (int cy = 0; cy < M; ++cy) col[cy] = v;
            }
        }
    }
}

static inline int nmesh(int len, int sz) { int n = (len + sz - 1) / sz; return n < 1 ? 1 : n; }

} /* namespace bb */

/*===========================================================================
 * MEX gateway
 *=========================================================================*/
static int getInt(const mxArray* a, int dflt) {
    if (!a || mxIsEmpty(a)) return dflt;
    return (int)std::floor(mxGetScalar(a) + 0.5);
}

void mexFunction(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[]) {
    if (nrhs < 1)
        mexErrMsgIdAndTxt("backBertinLowerRMS:nargin", "At least the Image is required.");
    const mxArray* IM = prhs[0];
    if (mxIsComplex(IM) || mxGetNumberOfDimensions(IM) != 2 || mxIsSparse(IM))
        mexErrMsgIdAndTxt("backBertinLowerRMS:type", "Image must be a 2-D real full matrix.");

    const int M = (int)mxGetM(IM);
    const int N = (int)mxGetN(IM);

    int bh = 128, bw = 128;
    if (nrhs >= 2 && !mxIsEmpty(prhs[1])) {
        const double* p = mxGetPr(prhs[1]);
        const mwSize ne = mxGetNumberOfElements(prhs[1]);
        bh = (int)std::floor(p[0] + 0.5);
        bw = (ne >= 2) ? (int)std::floor(p[1] + 0.5) : bh;
    }
    if (bh < 1 || bw < 1)
        mexErrMsgIdAndTxt("backBertinLowerRMS:size", "BACK_SIZE must be >= 1.");

    int fsize = getInt(nrhs >= 3 ? prhs[2] : 0, 3);
    if (fsize < 1) fsize = 1;
    double thresh = (nrhs >= 4 && !mxIsEmpty(prhs[3])) ? mxGetScalar(prhs[3]) : 0.0;

    const bool needVar = (nlhs >= 2);   /* Var or VarSmall both need the RMS mesh */

    const int nx = bb::nmesh(N, bw);
    const int ny = bb::nmesh(M, bh);

    std::vector<double> level((std::size_t)nx * ny), sigma((std::size_t)nx * ny);
    const mxClassID cls = mxGetClassID(IM);
    if (cls == mxDOUBLE_CLASS)
        bb::computeMesh<double>((const double*)mxGetData(IM), M, N, bw, bh, nx, ny,
                                level.data(), sigma.data(), needVar);
    else if (cls == mxSINGLE_CLASS)
        bb::computeMesh<float>((const float*)mxGetData(IM), M, N, bw, bh, nx, ny,
                               level.data(), sigma.data(), needVar);
    else
        mexErrMsgIdAndTxt("backBertinLowerRMS:class",
                          "Image must be double or single. Cast it first, e.g. double(Image).");

    std::vector<double> levelF((std::size_t)nx * ny), sigmaF((std::size_t)nx * ny);
    bb::medianFilterMesh(level.data(), levelF.data(), nx, ny, fsize, thresh);
    if (needVar)
        bb::medianFilterMesh(sigma.data(), sigmaF.data(), nx, ny, fsize, thresh);

    plhs[0] = mxCreateDoubleMatrix(M, N, mxREAL);
    bb::interpMeshToImage(levelF.data(), nx, ny, bw, bh, M, N, mxGetPr(plhs[0]), false);

    if (nlhs >= 2) {
        plhs[1] = mxCreateDoubleMatrix(M, N, mxREAL);
        bb::interpMeshToImage(sigmaF.data(), nx, ny, bw, bh, M, N, mxGetPr(plhs[1]), true);
    }

    /* Low-resolution mesh maps (post median-filter, pre-interpolation).
     * Internal layout is ix + iy*nx; a MATLAB [ny nx] matrix is iy + ix*ny,
     * so transpose while copying. */
    if (nlhs >= 3) {
        plhs[2] = mxCreateDoubleMatrix(ny, nx, mxREAL);
        double* BS = mxGetPr(plhs[2]);
        for (int iy = 0; iy < ny; ++iy)
            for (int ix = 0; ix < nx; ++ix)
                BS[iy + ix * ny] = levelF[ix + iy * nx];
    }
    if (nlhs >= 4) {
        plhs[3] = mxCreateDoubleMatrix(ny, nx, mxREAL);
        double* VS = mxGetPr(plhs[3]);
        for (int iy = 0; iy < ny; ++iy)
            for (int ix = 0; ix < nx; ++ix) {
                const double sg = sigmaF[ix + iy * nx];
                VS[iy + ix * ny] = sg * sg;
            }
    }
}
