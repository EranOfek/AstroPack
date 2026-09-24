/*
 * imageCutouts.cpp  —  fast 2-D image cutout extractor
 *
 * Syntax
 * ------
 *   CubeCutouts = imageCutouts(Image, X, Y, CutSize)
 *   CubeCutouts = imageCutouts(Image, X, Y, CutSize, PadValue)
 *
 * Inputs
 * ------
 *   Image    M x N array — any numeric or logical class
 *            (double, single, int8/16/32/64, uint8/16/32/64, logical)
 *   X        [] | scalar | vector (real single or double)
 *            Column centres of the cutouts, 1-indexed MATLAB convention.
 *            Sub-pixel values are rounded to the nearest integer.
 *   Y        [] | scalar | vector — same shape as X
 *            Row centres of the cutouts.
 *   CutSize  Positive integer scalar — side length of each square cutout.
 *   PadValue Scalar fill value for pixels outside the image (default 0).
 *            Clamped to the range of the input type for integer classes.
 *
 * Output
 * ------
 *   CubeCutouts  CutSize x CutSize x numel(X) array, same class as Image.
 *                CubeCutouts(:,:,k) is the k-th cutout centred at (X(k),Y(k)).
 *
 * Speed design
 * ------------
 *   1. Template dispatch over pixel type — zero runtime overhead in hot loops.
 *   2. OpenMP parallel for over cutouts — embarrassingly parallel.
 *   3. Fast path for interior cutouts: one memcpy() per column, no branches.
 *   4. __builtin_prefetch() on the next source column and next dst slot.
 *   5. can_use_memset(): if the pad byte pattern is uniform (all-zero, any
 *      uint8/int8/bool value, 0xFFFF for uint16, …) use memset(); otherwise
 *      a plain fill loop that the compiler auto-vectorises with -O3 -march=native.
 *   6. Zero-cost padding when PadValue == 0: MATLAB zero-initialises the output
 *      array from mxCreateNumericArray(), so the fill pass is skipped entirely.
 *   7. Correct clamp: min(cut_size, cols-x1) caps the column count so it
 *      never exceeds the allocated output slot (fixes the mexCutout overflow bug).
 *   8. __builtin_expect() hints the branch predictor toward the interior path.
 *
 * Compile
 * -------
 *   mex CXXFLAGS='$CXXFLAGS -std=c++14 -O3 -march=native -fopenmp' \
 *       LDFLAGS='$LDFLAGS -fopenmp' imageCutouts.cpp
 *
 *   Without OpenMP (single-threaded):
 *   mex CXXFLAGS='$CXXFLAGS -std=c++14 -O3 -march=native' imageCutouts.cpp
 *
 * Example
 * -------
 *   Img  = uint16(rand(4096,4096) * 65535);
 *   X    = rand(1000,1) * 4000 + 1;
 *   Y    = rand(1000,1) * 4000 + 1;
 *   C    = imageCutouts(Img, X, Y, 25);          % 25x25x1000 uint16
 *   C    = imageCutouts(Img, X, Y, 25, 0);       % same, explicit pad=0
 *   C    = imageCutouts(Img, X, Y, 25, NaN);     % float only; pads with NaN
 */

#include "mex.h"
#include <cstring>    /* memcpy, memset                              */
#include <cmath>      /* std::round, std::isnan                      */
#include <cstdarg>    /* va_list                                     */
#include <cstdio>     /* vsnprintf                                   */
#include <cstdint>    /* uint8_t … uint64_t                          */
#include <algorithm>  /* std::min, std::max                          */
#include <limits>     /* std::numeric_limits                         */
#include <vector>     /* std::vector                                 */

#ifdef _OPENMP
#  include <omp.h>
#endif


/* =========================================================================
 * 1.  Error helper
 * ========================================================================= */

[[noreturn]] static void die(const char* fmt, ...)
{
    char buf[512];
    va_list ap; va_start(ap, fmt);
    std::vsnprintf(buf, sizeof(buf), fmt, ap);
    va_end(ap);
    mexErrMsgIdAndTxt("imageCutouts:error", "%s", buf);
    __builtin_unreachable();
}


/* =========================================================================
 * 2.  Pad-value conversion: double scalar → pixel type T
 *
 *   bool    : non-zero double → true
 *   float   : direct cast (preserves NaN / Inf)
 *   double  : identity
 *   integer : NaN → 0; clamp to [min, max] of T
 * ========================================================================= */

template<typename T>
static T to_pad(double v)           /* generic: integer types */
{
    if (std::isnan(v)) return T(0);
    if (v < static_cast<double>(std::numeric_limits<T>::lowest()))
        return std::numeric_limits<T>::lowest();
    if (v > static_cast<double>(std::numeric_limits<T>::max()))
        return std::numeric_limits<T>::max();
    return static_cast<T>(v);
}
template<> bool   to_pad<bool>  (double v) { return v != 0.0; }
template<> float  to_pad<float> (double v) { return static_cast<float>(v); }
template<> double to_pad<double>(double v) { return v; }


/* =========================================================================
 * 3.  memset feasibility check
 *
 * Returns true and writes *out_byte when filling with that single byte
 * produces the correct bit pattern for `val`.
 *
 * Covers all-zero (any type), any uint8/int8/bool value, uint16 0xFFFF,
 * float 0.0f, and several other common patterns.
 * ========================================================================= */

template<typename T>
static bool can_use_memset(T val, int& out_byte) noexcept
{
    const auto* b = reinterpret_cast<const unsigned char*>(&val);
    out_byte = static_cast<int>(b[0]);
    for (std::size_t i = 1; i < sizeof(T); ++i)
        if (b[i] != static_cast<unsigned char>(out_byte)) return false;
    return true;
}


/* =========================================================================
 * 4.  Core worker — templated over pixel type T
 *
 * MATLAB memory layout is column-major:
 *   img  [row  + col  * rows   ]          — 2-D input image
 *   out  [row  + col  * cs + k * cs²]     — 3-D output cube
 *
 * For each cutout k centred at (cx, cy) (0-indexed C coordinates):
 *
 *   x1  = cx - half       first image column (may be < 0)
 *   y1  = cy - half       first image row    (may be < 0)
 *
 *   sx0 = max(0, -x1)     first valid dst column  (= push_x)
 *   sy0 = max(0, -y1)     first valid dst row     (= push_y)
 *   sx1 = min(cs, cols-x1)  one-past last valid dst column  ← NEVER > cs
 *   sy1 = min(cs, rows-y1)  one-past last valid dst row     ← NEVER > cs
 *
 * The min() cap on sx1/sy1 is the fix for the mexCutout "dst OUT OF RANGE"
 * bug: without it, cols-x1 can exceed cs when x1 < 0 and cols ≤ cs,
 * making the column loop overrun the allocated output slot.
 * ========================================================================= */

template<typename T>
static void do_cutouts(
    const T* __restrict__ img,     /* [rows × cols] col-major               */
    T*       __restrict__ out,     /* [cs × cs × num_cuts] col-major        */
    mwSize rows, mwSize cols,
    const double* __restrict__ xctr,  /* 0-indexed column centres (size num_cuts) */
    const double* __restrict__ yctr,  /* 0-indexed row    centres (size num_cuts) */
    mwSize num_cuts,
    int    cut_size,
    T      pad_val)
{
    const int    half = cut_size / 2;
    const mwSize cs   = static_cast<mwSize>(cut_size);
    const mwSize cs2  = cs * cs;

    /* Padding strategy decided once, reused for every edge cutout.          */
    const bool pad_is_zero = (pad_val == T(0));
    int        pad_byte    = 0;
    const bool use_mset    = can_use_memset(pad_val, pad_byte);

    #pragma omp parallel for schedule(static)
    for (mwIndex k = 0; k < static_cast<mwIndex>(num_cuts); ++k) {

        /* ── Guard against NaN / Inf positions ──
           static_cast<int>(NaN) is undefined behaviour (x86 returns INT_MIN),
           which then overflows in arithmetic.  Skip the slot; it stays at
           pad_value (zero-initialised by MATLAB for pad==0, or filled below). */
        if (!std::isfinite(xctr[k]) || !std::isfinite(yctr[k])) continue;

        /* ── Convert MATLAB 1-indexed centres to C 0-indexed ── */
        const int cx = static_cast<int>(std::round(xctr[k])) - 1;
        const int cy = static_cast<int>(std::round(yctr[k])) - 1;

        /* ── Top-left corner of this cutout in image space ── */
        const int x1 = cx - half;
        const int y1 = cy - half;

        /* ── Valid region in destination (cutout) coordinates ──
           Hard-clamped to [0, cut_size] with explicit max(0,…) guards so
           the result is correct even for extreme (large-negative) positions
           where unclamped int arithmetic could otherwise wrap.              */
        const int sx0 = std::max(0, std::min(-x1,   cut_size));
        const int sy0 = std::max(0, std::min(-y1,   cut_size));
        const int sx1 = std::max(0, std::min(cut_size, static_cast<int>(cols) - x1));
        const int sy1 = std::max(0, std::min(cut_size, static_cast<int>(rows) - y1));
        const int nc  = std::max(0, sx1 - sx0);
        const int nr  = std::max(0, sy1 - sy0);

        T* const dst = out + static_cast<mwSize>(k) * cs2;

        /* ═══ FAST PATH — cutout entirely inside the image ═══════════════
         * No padding needed; copy one contiguous column segment per column.
         * __builtin_expect hints the branch predictor toward the interior   */
        if (__builtin_expect(
                sx0 == 0 && sy0 == 0 && sx1 == cut_size && sy1 == cut_size, 1))
        {
            for (int j = 0; j < cut_size; ++j) {
                /* Prefetch source of the NEXT column while memcpy runs.     */
                __builtin_prefetch(
                    img + static_cast<mwSize>(x1 + j + 1) * rows + y1,
                    0 /*read*/, 1 /*L2*/);
                /* Prefetch the destination write-slot of the next column.   */
                __builtin_prefetch(
                    dst + static_cast<mwSize>(j + 1) * cs,
                    1 /*write*/, 1 /*L2*/);

                memcpy(dst + static_cast<mwSize>(j) * cs,
                       img + static_cast<mwSize>(x1 + j) * rows + y1,
                       cs * sizeof(T));
            }
        }
        else
        {
            /* ═══ SLOW PATH — edge cutout (straddles boundary) ═══════════
             * 1. Fill entire cs×cs slot with pad_val.
             *    Skipped for pad==0: MATLAB already zero-initialised output.
             * 2. Overwrite the valid region column by column.               */

            if (!pad_is_zero) {
                if (use_mset) {
                    memset(dst, pad_byte, cs2 * sizeof(T));
                } else {
                    /* Auto-vectorised by compiler on -O3 -march=native.     */
                    T* p = dst;  const T* const e = dst + cs2;
                    for (; p < e; ++p) *p = pad_val;
                }
            }

            if (nc > 0 && nr > 0) {
                for (int j = 0; j < nc; ++j) {
                    memcpy(dst + static_cast<mwSize>(sx0 + j) * cs + sy0,
                           img + static_cast<mwSize>(x1 + sx0 + j) * rows + (y1 + sy0),
                           static_cast<mwSize>(nr) * sizeof(T));
                }
            }
        }  /* end slow path */
    }
}


/* =========================================================================
 * 5.  Position-vector reader
 *     Accepts single or double MATLAB arrays; always returns double[].
 * ========================================================================= */

static std::vector<double> read_positions(const mxArray* arr)
{
    const mwSize n = mxGetNumberOfElements(arr);
    std::vector<double> v(n);

    if (mxIsDouble(arr)) {
        const double* p = static_cast<const double*>(mxGetData(arr));
        std::copy(p, p + n, v.begin());
    } else {   /* single */
        const float* p = static_cast<const float*>(mxGetData(arr));
        for (mwSize i = 0; i < n; ++i) v[i] = static_cast<double>(p[i]);
    }
    return v;
}


/* =========================================================================
 * 6.  MEX entry point
 * ========================================================================= */

void mexFunction(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[])
{
    /* ── Argument count ───────────────────────────────────────────────── */
    if (nrhs < 4 || nrhs > 5)
        die("Usage: C = imageCutouts(Image, X, Y, CutSize [, PadValue])");
    if (nlhs > 1)
        die("imageCutouts produces exactly one output.");

    const mxArray* img_arr = prhs[0];
    const mxArray* x_arr   = prhs[1];
    const mxArray* y_arr   = prhs[2];
    const mxArray* cs_arr  = prhs[3];

    /* ── Image validation ─────────────────────────────────────────────── */
    if (!mxIsNumeric(img_arr) && !mxIsLogical(img_arr))
        die("Image must be numeric or logical.");
    if (mxIsComplex(img_arr))
        die("Image must be real (non-complex).");
    if (mxGetNumberOfDimensions(img_arr) != 2)
        die("Image must be 2-D (got %u dimensions).",
            (unsigned)mxGetNumberOfDimensions(img_arr));

    const mwSize rows = mxGetM(img_arr);
    const mwSize cols = mxGetN(img_arr);

    /* Guard against images so large that (int)cols would overflow.        */
    if (rows > (mwSize)INT_MAX || cols > (mwSize)INT_MAX)
        die("Image dimensions exceed INT_MAX — not supported.");

    /* ── X / Y validation ─────────────────────────────────────────────── */
    const bool x_empty = mxIsEmpty(x_arr);
    const bool y_empty = mxIsEmpty(y_arr);

    if (x_empty != y_empty)
        die("X and Y must both be empty or both non-empty.");

    if (!x_empty) {
        if (!(mxIsSingle(x_arr) || mxIsDouble(x_arr)) || mxIsComplex(x_arr))
            die("X must be a real single or double array.");
        if (!(mxIsSingle(y_arr) || mxIsDouble(y_arr)) || mxIsComplex(y_arr))
            die("Y must be a real single or double array.");
        if (mxGetNumberOfElements(x_arr) != mxGetNumberOfElements(y_arr))
            die("X and Y must have the same number of elements (got %zu vs %zu).",
                (size_t)mxGetNumberOfElements(x_arr),
                (size_t)mxGetNumberOfElements(y_arr));
    }

    /* ── CutSize validation ───────────────────────────────────────────── */
    if (!mxIsNumeric(cs_arr) || mxIsComplex(cs_arr) ||
        mxGetNumberOfElements(cs_arr) != 1)
        die("CutSize must be a real scalar.");

    const int cut_size = static_cast<int>(mxGetScalar(cs_arr));
    if (cut_size < 1)
        die("CutSize must be >= 1 (got %d).", cut_size);

    /* ── PadValue (optional, default 0) ──────────────────────────────── */
    const double pad_d = (nrhs >= 5 && !mxIsEmpty(prhs[4]))
                         ? mxGetScalar(prhs[4]) : 0.0;

    /* ── Output allocation ────────────────────────────────────────────── */
    const mwSize num_cuts  = x_empty ? mwSize(0) : mxGetNumberOfElements(x_arr);
    const mxClassID cid    = mxGetClassID(img_arr);
    const mwSize out_dims[3] = { (mwSize)cut_size, (mwSize)cut_size, num_cuts };

    if (mxIsLogical(img_arr))
        plhs[0] = mxCreateLogicalArray(3, out_dims);
    else
        plhs[0] = mxCreateNumericArray(3, out_dims, cid, mxREAL);
    /* mxCreate* zero-initialises — pad_val==0 is already handled.         */

    /* Early exit: nothing to compute. */
    if (num_cuts == 0 || rows == 0 || cols == 0) return;

    /* ── Read position arrays as double ──────────────────────────────── */
    std::vector<double> xv = read_positions(x_arr);
    std::vector<double> yv = read_positions(y_arr);
    const double* xp = xv.data();
    const double* yp = yv.data();

    const void* img_data = mxGetData(img_arr);
    void*       out_data = mxGetData(plhs[0]);

    /* ── Type dispatch ────────────────────────────────────────────────── */
    /* Each case instantiates do_cutouts<T> with zero runtime overhead
       inside the hot loop — no per-pixel type switch.                      */
#define RUN(CTYPE) \
    do_cutouts<CTYPE>( \
        static_cast<const CTYPE*>(img_data), \
        static_cast<CTYPE*>(out_data), \
        rows, cols, xp, yp, num_cuts, cut_size, \
        to_pad<CTYPE>(pad_d)); break

    switch (cid) {
        case mxDOUBLE_CLASS:   RUN(double);
        case mxSINGLE_CLASS:   RUN(float);
        case mxUINT8_CLASS:    RUN(uint8_t);
        case mxINT8_CLASS:     RUN(int8_t);
        case mxUINT16_CLASS:   RUN(uint16_t);
        case mxINT16_CLASS:    RUN(int16_t);
        case mxUINT32_CLASS:   RUN(uint32_t);
        case mxINT32_CLASS:    RUN(int32_t);
        case mxUINT64_CLASS:   RUN(uint64_t);
        case mxINT64_CLASS:    RUN(int64_t);
        case mxLOGICAL_CLASS:  RUN(bool);
        default:
            die("Unsupported image class '%s'.", mxGetClassName(img_arr));
    }
#undef RUN
}
