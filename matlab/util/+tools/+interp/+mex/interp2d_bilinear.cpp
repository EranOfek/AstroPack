#include <math.h>
#include <limits> // For NaN
#include "mex.h"
#include <omp.h> // For OpenMP

// mex interp2d_bilinear.cpp CXXFLAGS="\$CXXFLAGS -O3 -march=native -ffast-math -fopenmp" LDFLAGS="\$LDFLAGS -fopenmp"

void mirt2D_mexinterp(
    double *Z,
    double *S,
    double *T,
    double *F,
    int MN,
    int nrows,
    int ncols,
    int ndim) 
{
    const int nrowsncols = nrows * ncols;
    const double nan = std::numeric_limits<double>::quiet_NaN();

    #pragma omp parallel for
    for (int n = 0; n < MN; n++) {
        double t = T[n];
        double s = S[n];
        int fs = (int)floor(s);
        int ft = (int)floor(t);

        // Check if (s, t) is out of bounds
        if (fs < 1 || s > ncols || ft < 1 || t > nrows) {
            for (int i = 0; i < ndim; i++) {
                F[n + i * MN] = nan;
            }
            continue;
        }

        // Compute the base index once
        int ndx = ft + (fs - 1) * nrows;

        // Adjust for boundary conditions
        bool s_at_boundary = (s == ncols);
        bool t_at_boundary = (t == nrows);

        if (s_at_boundary) {
            s += 1;
            ndx -= nrows;
        }
        if (t_at_boundary) {
            t += 1;
            ndx -= 1;
        }

        s -= fs; // Fractional part of s
        t -= ft; // Fractional part of t

        // Interpolation coefficients
        double m4 = t * s;
        double m1 = 1 + m4 - t - s;
        double m2 = t - m4;
        double m3 = s - m4;

        // Precompute indices for interpolation
        int in1 = ndx - 1;
        int in2 = ndx;
        int in4 = ndx + nrows;
        int in3 = in4 - 1;

        // Perform interpolation for each dimension
        for (int i = 0; i < ndim; i++) {
            int Zshift = i * nrowsncols;
            F[n + i * MN] = Z[in1 + Zshift] * m1 +
                            Z[in2 + Zshift] * m2 +
                            Z[in3 + Zshift] * m3 +
                            Z[in4 + Zshift] * m4;
        }
    }
}

/* Input arguments */
#define IN_Z prhs[0]
#define IN_S prhs[1]
#define IN_T prhs[2]

/* Output arguments */
#define OUT_F plhs[0]

/* Gateway routine */
void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[]) {
    double *Z, *S, *T, *F;
    int M, N, MN, nrows, ncols, vol, ndim;
    const mwSize *dims;

    /* Check for input errors */
    if (nlhs > 1)
        mexErrMsgTxt("Wrong number of output parameters, usage: Output_images = mirt2D_mexinterp(Input_images, X, Y)");
    if (nrhs != 3)
        mexErrMsgTxt("Wrong number of input parameters, usage: Output_images = mirt2D_mexinterp(Input_images, X, Y)");
    if (!mxIsDouble(IN_Z) || !mxIsDouble(IN_S) || !mxIsDouble(IN_T))
        mexErrMsgTxt("mirt2D_mexinterp: Input arguments must be double.");
    if ((mxGetNumberOfDimensions(IN_S) != mxGetNumberOfDimensions(IN_T)) ||
        (mxGetNumberOfElements(IN_S) != mxGetNumberOfElements(IN_T)))
        mexErrMsgTxt("Inputs X, Y must have the same size");

    /* Get the sizes of each input argument */
    M = mxGetM(IN_S);
    N = mxGetN(IN_S);
    ndim = mxGetNumberOfDimensions(IN_Z);
    dims = mxGetDimensions(IN_Z);

    /* Size of the array to allocate for the interpolated points */
    mwSize newdims[3]; // Stack allocation instead of malloc
    newdims[0] = M;
    newdims[1] = N;
    newdims[2] = (ndim > 2) ? dims[2] : 1;
    MN = M * N;
    vol = (ndim > 2) ? dims[2] : 1;

    /* Create the array (2D or 3D) to put the interpolated points */
    OUT_F = mxCreateNumericArray(ndim, newdims, mxDOUBLE_CLASS, mxREAL);

    /* Input image size */
    nrows = dims[0];
    ncols = dims[1];

    /* Assign pointers to the input arguments */
    Z = mxGetPr(IN_Z);
    S = mxGetPr(IN_S);
    T = mxGetPr(IN_T);

    /* Assign pointers to the output arguments */
    F = mxGetPr(OUT_F);

    /* Do the actual computations in a subroutine */
    mirt2D_mexinterp(Z, S, T, F, MN, nrows, ncols, vol);
}
