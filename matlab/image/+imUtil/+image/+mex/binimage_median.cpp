/*
 * binimage_median.cpp  -  MEX binning of an image by the per-box median.
 *
 * (The user-facing help lives in binimage_median.m so that `help
 *  binimage_median` works; MATLAB runs this MEX and reads the .m for docs.)
 *
 *   BinImage = binimage_median(Image, BinSizeXY)
 *
 * Shrinks Image by the integer factors BinSizeXY = [X(cols) Y(rows)],
 * replacing each bin box by the median of its pixels. NaN pixels are
 * ignored; a box with no finite pixel yields NaN. The image is trimmed
 * (bottom/right) to an integer number of bins. Output keeps the input
 * class (single/double). Boxes are independent and computed in parallel
 * when built with OpenMP.
 *
 * Author : Eran Ofek (Jun 2026)
 */

#include "mex.h"
#include "matrix.h"
#include <algorithm>
#include <vector>
#include <cmath>
#ifdef _OPENMP
#include <omp.h>
#endif

template <typename T>
static void bin_median(const T* A, mwSize M,
                       mwSize by, mwSize bx,
                       T* B, mwSize outM, mwSize outN)
{
    const mwSize n        = by * bx;
    const mwSignedIndex Nblk = (mwSignedIndex)(outM * outN);
    const T nanT          = (T)mxGetNaN();

    #ifdef _OPENMP
    #pragma omp parallel
    #endif
    {
        std::vector<T> buf(n);                 // per-thread scratch
        #ifdef _OPENMP
        #pragma omp for schedule(static)
        #endif
        for (mwSignedIndex b = 0; b < Nblk; ++b) {
            const mwSize i = (mwSize)b % outM; // block row
            const mwSize j = (mwSize)b / outM; // block col

            // gather the finite pixels of box (i,j) into buf
            mwSize cnt = 0;
            for (mwSize cc = 0; cc < bx; ++cc) {
                const T* col = A + (j * bx + cc) * M + i * by;
                for (mwSize rr = 0; rr < by; ++rr) {
                    const T v = col[rr];
                    if (v == v) buf[cnt++] = v; // NaN != NaN -> skipped
                }
            }

            T med;
            if (cnt == 0) {
                med = nanT;
            } else {
                T* be = buf.data();
                const mwSize h = cnt >> 1;
                std::nth_element(be, be + h, be + cnt);
                if (cnt & 1) {                  // odd: single middle element
                    med = be[h];
                } else {                        // even: mean of the two middles
                    const T hi = be[h];
                    const T lo = *std::max_element(be, be + h);
                    med = (T)(0.5 * ((double)lo + (double)hi));
                }
            }
            B[b] = med;                         // b == i + j*outM (column-major)
        }
    }
}

void mexFunction(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[])
{
    if (nrhs != 2)
        mexErrMsgIdAndTxt("binimage_median:nargin",
                          "Two inputs required: binimage_median(Image, BinSizeXY).");
    if (nlhs > 1)
        mexErrMsgIdAndTxt("binimage_median:nargout", "Too many output arguments.");

    // ---- Image ----
    const mxArray* mImg = prhs[0];
    if (mxIsComplex(mImg) || mxIsSparse(mImg) || mxGetNumberOfDimensions(mImg) != 2)
        mexErrMsgIdAndTxt("binimage_median:image",
                          "Image must be a real 2-D non-sparse matrix.");
    const mxClassID cls = mxGetClassID(mImg);
    if (cls != mxDOUBLE_CLASS && cls != mxSINGLE_CLASS)
        mexErrMsgIdAndTxt("binimage_median:type", "Image must be single or double.");
    const mwSize M = mxGetM(mImg);
    const mwSize N = mxGetN(mImg);

    // ---- BinSizeXY ----
    const mxArray* mBin = prhs[1];
    if (!mxIsDouble(mBin) || mxIsComplex(mBin))
        mexErrMsgIdAndTxt("binimage_median:bin", "BinSizeXY must be a real double.");
    const mwSize nb = mxGetNumberOfElements(mBin);
    if (nb != 1 && nb != 2)
        mexErrMsgIdAndTxt("binimage_median:bin", "BinSizeXY must have 1 or 2 elements.");
    const double* pb = mxGetPr(mBin);
    const double bxd = pb[0];                 // X (columns)
    const double byd = (nb == 2) ? pb[1] : pb[0]; // Y (rows)
    if (!(bxd >= 1.0) || !(byd >= 1.0) ||
        bxd != std::floor(bxd) || byd != std::floor(byd))
        mexErrMsgIdAndTxt("binimage_median:bin", "BinSizeXY must be positive integers.");
    const mwSize bx = (mwSize)bxd;
    const mwSize by = (mwSize)byd;

    // ---- trim to an integer number of bins ----
    if ((M % by) != 0 || (N % bx) != 0)
        mexWarnMsgIdAndTxt("binimage_median:trim",
                           "Image trimmed (bottom/right) to fit an integer number of bins.");
    const mwSize outM = M / by;
    const mwSize outN = N / bx;

    // ---- output (same class as input) ----
    plhs[0] = mxCreateNumericMatrix(outM, outN, cls, mxREAL);
    if (outM == 0 || outN == 0)
        return;

    if (cls == mxDOUBLE_CLASS) {
        bin_median<double>((const double*)mxGetData(mImg), M, by, bx,
                           (double*)mxGetData(plhs[0]), outM, outN);
    } else {
        bin_median<float>((const float*)mxGetData(mImg), M, by, bx,
                          (float*)mxGetData(plhs[0]), outM, outN);
    }
}
