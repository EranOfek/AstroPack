/*
 * radonCenter.cpp - Radon transform restricted to lines passing through the
 *                   center of an image stamp (or of every stamp in a cube).
 *
 *   [TanRot, RadonSums, Area] = radonCenter(Cube, LineWidth)
 *
 * Input
 *   Cube      - Ny x Nx x Nim real array (image index in the 3rd dimension).
 *               Classes: double, single, (u)int8/16/32/64.
 *   LineWidth - Positive odd integer: full width [pix] of the summed strip,
 *               measured PERPENDICULAR to the line. Default: 3.
 *
 * Output
 *   TanRot    - Nang x 1 vector of tan(theta), ordered by increasing theta in
 *               [0, 180) deg. theta is measured from the X axis (dim 2) toward
 *               the Y axis (dim 1), i.e. the line is
 *                     y - yc = tan(theta) * (x - xc),
 *               with (xc, yc) = ((Nx+1)/2, (Ny+1)/2) in 1-based pixel coords.
 *               theta = 90 deg is returned as +Inf.
 *               Angle in deg: Theta = mod(atand(TanRot), 180).
 *   RadonSums - Nang x Nim matrix, one column per stamp: the integral of the
 *               (pixel-wise constant) image over the strip. Class single for
 *               single input, double otherwise (accumulation is always double).
 *   Area      - (optional) Nang x 1 effective number of pixels (sum of the
 *               weights) per angle. For a constant image B: RadonSums = B*Area.
 *
 * Angular sampling (Nyquist, "pseudo-polar" grid)
 *   R = max(1, floor(max(Nx,Ny)/2)). Angles are chosen so that the line end
 *   point moves by exactly 1 pixel along the stamp boundary:
 *      |tan| <= 1 : tan(theta) = k/R ,  k = -R..R   (uniform in tan)
 *      |tan| >= 1 : cot(theta) = k/R ,  k = -R..R   (uniform in cot)
 *   giving Nang = 4R unique angles. The step is 1/R rad at 0/90 deg (the
 *   Nyquist step for a band-limited image of radius R) and 1/(2R) at 45 deg.
 *
 * Strip weights
 *   For |tan| <= 1 every column x contributes the exact 1-D integral of that
 *   column over [y0-H, y0+H], y0 = yc + (x-xc)*tan, H = W/2*sqrt(1+tan^2)
 *   (vertical extent of a strip of perpendicular width W). Partially covered
 *   end pixels get fractional weights, inner pixels weight 1. For |tan| > 1
 *   the same is done with x<->y and tan->cot. Hence the sums vary smoothly
 *   with angle (no jagged pixel-count aliasing), and at 0/90 deg with an odd
 *   stamp size the strip is exactly W whole pixels.
 *
 * Algorithm
 *   The pixel "runs" (start index, length, two end weights) of all angles are
 *   computed once per call and applied to every stamp: the cost is
 *   ~ 2*Nx*Ny*(W+1) additions per stamp (linear in the number of pixels).
 *   (stamp, angle) pairs are distributed over OpenMP threads.
 *
 * Notes
 *   - NaNs propagate. Replace them before calling, e.g. Cube(isnan(Cube))=0
 *     (or by the background).
 *   - The ~W x W core around the center is shared by all angles.
 *
 * Compile (from MATLAB)
 *   Linux (gcc):  mex -O CXXFLAGS='$CXXFLAGS -fopenmp' CXXOPTIMFLAGS='-O3 -march=native' LDFLAGS='$LDFLAGS -fopenmp' radonCenter.cpp
 *   Windows(MSVC):mex -O COMPFLAGS='$COMPFLAGS /openmp /O2' radonCenter.cpp
 *   macOS/other:  mex -O radonCenter.cpp          (single threaded)
 *   Number of threads: environment variable OMP_NUM_THREADS.
 */

#include "mex.h"
#include <cmath>
#include <cstddef>
#include <cstdint>
#include <vector>
#include <limits>
#include <algorithm>
#include <new>
#ifdef _OPENMP
#include <omp.h>
#endif

namespace {

typedef long long i64;

/* A run of pixels along the "across" direction belonging to one angle. */
struct Run {
    int32_t start;   /* 0-based linear index (within a stamp) of the first pixel */
    int32_t n;       /* number of pixels in the run */
    double  w0;      /* weight of the first pixel */
    double  w1;      /* weight of the last pixel (unused if n==1); inner pixels: 1 */
};

struct Plan {
    std::vector<Run>     runs;
    std::vector<size_t>  off;     /* runs of angle a: [off[a], off[a+1]) */
    std::vector<int32_t> stride;  /* memory stride between pixels of a run: 1 or Ny */
    std::vector<double>  tanRot;
    std::vector<double>  area;
};

/* Add the runs of one angle.
 * steep=false : |tan|<=1, step along x (columns), integrate along y, slope=tan
 * steep=true  : |tan|>=1, step along y (rows),    integrate along x, slope=cot  */
void addAngle(Plan& P, double slope, bool steep, i64 ny, i64 nx, double halfW)
{
    i64 nu, nv, su, sv;
    double uc, vc;
    if (!steep) { nu = nx; nv = ny; uc = 0.5*(nx-1); vc = 0.5*(ny-1); su = ny; sv = 1;  }
    else        { nu = ny; nv = nx; uc = 0.5*(ny-1); vc = 0.5*(nx-1); su = 1;  sv = ny; }

    const double H    = halfW * std::sqrt(1.0 + slope*slope);
    const double vMin = -0.5;
    const double vMax = (double)nv - 0.5;
    double area = 0.0;

    for (i64 u = 0; u < nu; ++u) {
        const double v0 = vc + ((double)u - uc) * slope;
        const double a  = std::max(v0 - H, vMin);
        const double b  = std::min(v0 + H, vMax);
        if (b - a <= 1e-12) continue;                 /* strip outside the stamp */
        const i64 ia = (i64)std::floor(a + 0.5);      /* pixel i covers [i-0.5, i+0.5] */
        const i64 ib = (i64)std::ceil (b + 0.5) - 1;
        Run r;
        r.start = (int32_t)(u*su + ia*sv);
        r.n     = (int32_t)(ib - ia + 1);
        if (ia == ib) { r.w0 = b - a;            r.w1 = 0.0; }
        else          { r.w0 = (ia + 0.5) - a;   r.w1 = b - (ib - 0.5); }
        P.runs.push_back(r);
        area += b - a;
    }
    P.off.push_back(P.runs.size());
    P.stride.push_back((int32_t)sv);
    P.area.push_back(area);
}

void buildPlan(Plan& P, i64 ny, i64 nx, double W)
{
    const i64    R     = std::max<i64>(1, std::max(ny, nx) / 2);
    const i64    nang  = 4 * R;
    const double halfW = 0.5 * W;
    const double Rd    = (double)R;

    P.off.reserve(nang + 1);
    P.stride.reserve(nang);
    P.tanRot.reserve(nang);
    P.area.reserve(nang);
    P.runs.reserve((size_t)(nang * std::max(nx, ny)));
    P.off.push_back(0);

    /* theta in [0, 45] : tan = k/R, k = 0..R */
    for (i64 k = 0; k <= R; ++k) {
        const double t = (double)k / Rd;
        addAngle(P, t, false, ny, nx, halfW);
        P.tanRot.push_back(t);
    }
    /* theta in (45, 135) : cot = k/R, k = R-1..-(R-1) */
    for (i64 k = R - 1; k >= -(R - 1); --k) {
        const double c = (double)k / Rd;
        addAngle(P, c, true, ny, nx, halfW);
        P.tanRot.push_back(k == 0 ? std::numeric_limits<double>::infinity() : Rd / (double)k);
    }
    /* theta in [135, 180) : tan = k/R, k = -R..-1 */
    for (i64 k = -R; k <= -1; ++k) {
        const double t = (double)k / Rd;
        addAngle(P, t, false, ny, nx, halfW);
        P.tanRot.push_back(t);
    }
}

/* Weighted sum over all runs of one angle in one stamp. */
template <typename T, bool UNIT>
inline double sumAngle(const T* img, const Run* r, const Run* rEnd, ptrdiff_t strideIn)
{
    const ptrdiff_t st = UNIT ? 1 : strideIn;
    double s = 0.0;
    for (; r != rEnd; ++r) {
        const T*  p = img + r->start;
        const int n = r->n;
        if (n == 1) { s += r->w0 * (double)p[0]; continue; }
        double acc = r->w0 * (double)p[0] + r->w1 * (double)p[(ptrdiff_t)(n - 1) * st];
        for (int j = 1; j < n - 1; ++j) acc += (double)p[(ptrdiff_t)j * st];
        s += acc;
    }
    return s;
}

template <typename T, typename TO>
void computeAll(const void* cubeV, void* outV, const Plan& P, i64 ny, i64 nx, i64 nim)
{
    const T*       cube   = static_cast<const T*>(cubeV);
    TO*            out    = static_cast<TO*>(outV);
    const i64      nang   = (i64)P.tanRot.size();
    const i64      ntot   = nang * nim;
    const i64      npix   = ny * nx;
    const Run*     runs   = P.runs.data();
    const size_t*  off    = P.off.data();
    const int32_t* stride = P.stride.data();
#ifdef _OPENMP
    const double work = (double)P.runs.size() * (double)nim;
    #pragma omp parallel for schedule(static) if (work > 2.0e4)
#endif
    for (i64 k = 0; k < ntot; ++k) {          /* out is Nang x Nim: k = a + im*nang */
        const i64  im = k / nang;
        const i64  a  = k - im * nang;
        const T*   img = cube + im * npix;
        const Run* r0 = runs + off[a];
        const Run* r1 = runs + off[a + 1];
        const double s = (stride[a] == 1)
                       ? sumAngle<T, true >(img, r0, r1, 1)
                       : sumAngle<T, false>(img, r0, r1, (ptrdiff_t)stride[a]);
        out[k] = (TO)s;
    }
}

bool supportedClass(mxClassID c)
{
    switch (c) {
        case mxDOUBLE_CLASS: case mxSINGLE_CLASS:
        case mxINT8_CLASS:   case mxUINT8_CLASS:
        case mxINT16_CLASS:  case mxUINT16_CLASS:
        case mxINT32_CLASS:  case mxUINT32_CLASS:
        case mxINT64_CLASS:  case mxUINT64_CLASS:
            return true;
        default:
            return false;
    }
}

} /* namespace */

void mexFunction(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[])
{
    /* ---------------- input checks ---------------- */
    if (nrhs < 1 || nrhs > 2)
        mexErrMsgIdAndTxt("radonCenter:nargin",
            "Usage: [TanRot, RadonSums, Area] = radonCenter(Cube, [LineWidth=3])");
    if (nlhs > 3)
        mexErrMsgIdAndTxt("radonCenter:nargout", "Too many output arguments (max 3).");

    const mxArray* C = prhs[0];
    if (!mxIsNumeric(C) || mxIsComplex(C) || mxIsSparse(C))
        mexErrMsgIdAndTxt("radonCenter:Cube", "Cube must be a real, full, numeric array.");
    const mxClassID cid = mxGetClassID(C);
    if (!supportedClass(cid))
        mexErrMsgIdAndTxt("radonCenter:Cube", "Unsupported class: %s.", mxGetClassName(C));
    const mwSize nd = mxGetNumberOfDimensions(C);
    if (nd > 3)
        mexErrMsgIdAndTxt("radonCenter:Cube", "Cube must be 2-D or 3-D (Ny x Nx x Nim).");
    const mwSize* d   = mxGetDimensions(C);
    const i64     ny  = (i64)d[0];
    const i64     nx  = (i64)d[1];
    const i64     nim = (nd == 3) ? (i64)d[2] : 1;
    if ((double)ny * (double)nx > 2147483647.0)
        mexErrMsgIdAndTxt("radonCenter:Cube", "Stamp too large (Ny*Nx must be < 2^31).");

    double W = 3.0;
    if (nrhs == 2 && !mxIsEmpty(prhs[1])) {
        const mxArray* L = prhs[1];
        if (!mxIsNumeric(L) || mxIsComplex(L) || mxGetNumberOfElements(L) != 1)
            mexErrMsgIdAndTxt("radonCenter:LineWidth", "LineWidth must be a real numeric scalar.");
        W = mxGetScalar(L);
        if (!std::isfinite(W) || W < 1.0 || W != std::floor(W) || std::fmod(W, 2.0) != 1.0)
            mexErrMsgIdAndTxt("radonCenter:LineWidth", "LineWidth must be a positive odd integer.");
    }

    const mxClassID oid = (cid == mxSINGLE_CLASS) ? mxSINGLE_CLASS : mxDOUBLE_CLASS;

    /* ---------------- empty stamp ---------------- */
    if (ny == 0 || nx == 0) {
        plhs[0] = mxCreateDoubleMatrix(0, 1, mxREAL);
        if (nlhs >= 2) plhs[1] = mxCreateNumericMatrix(0, (mwSize)nim, oid, mxREAL);
        if (nlhs >= 3) plhs[2] = mxCreateDoubleMatrix(0, 1, mxREAL);
        return;
    }

    /* ---------------- geometry (shared by all stamps) ---------------- */
    Plan P;
    bool ok = true;
    try { buildPlan(P, ny, nx, W); }
    catch (const std::bad_alloc&) { ok = false; }
    if (!ok) { P = Plan(); mexErrMsgIdAndTxt("radonCenter:memory", "Out of memory."); }

    const mwSize nang = (mwSize)P.tanRot.size();

    plhs[0] = mxCreateDoubleMatrix(nang, 1, mxREAL);
    std::copy(P.tanRot.begin(), P.tanRot.end(), static_cast<double*>(mxGetData(plhs[0])));

    /* ---------------- sums ---------------- */
    if (nlhs >= 2) {
        plhs[1] = mxCreateNumericMatrix(nang, (mwSize)nim, oid, mxREAL);
        const void* in  = mxGetData(C);
        void*       out = mxGetData(plhs[1]);
        switch (cid) {
            case mxDOUBLE_CLASS: computeAll<double,   double>(in, out, P, ny, nx, nim); break;
            case mxSINGLE_CLASS: computeAll<float,    float >(in, out, P, ny, nx, nim); break;
            case mxINT8_CLASS:   computeAll<int8_t,   double>(in, out, P, ny, nx, nim); break;
            case mxUINT8_CLASS:  computeAll<uint8_t,  double>(in, out, P, ny, nx, nim); break;
            case mxINT16_CLASS:  computeAll<int16_t,  double>(in, out, P, ny, nx, nim); break;
            case mxUINT16_CLASS: computeAll<uint16_t, double>(in, out, P, ny, nx, nim); break;
            case mxINT32_CLASS:  computeAll<int32_t,  double>(in, out, P, ny, nx, nim); break;
            case mxUINT32_CLASS: computeAll<uint32_t, double>(in, out, P, ny, nx, nim); break;
            case mxINT64_CLASS:  computeAll<int64_t,  double>(in, out, P, ny, nx, nim); break;
            case mxUINT64_CLASS: computeAll<uint64_t, double>(in, out, P, ny, nx, nim); break;
            default: break;  /* unreachable: checked above */
        }
    }

    /* ---------------- effective area ---------------- */
    if (nlhs >= 3) {
        plhs[2] = mxCreateDoubleMatrix(nang, 1, mxREAL);
        std::copy(P.area.begin(), P.area.end(), static_cast<double*>(mxGetData(plhs[2])));
    }
}
