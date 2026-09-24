#include <cmath>
#include <limits>
#include <omp.h>
#include "mex.h"

// Single-precision interpolation function
void mirt2D_mexinterp_single(
    float *Z,
    float *S,
    float *T,
    float *F,
    int MN,
    int nrows,
    int ncols,
    int ndim)
{
    const int nrowsncols = nrows * ncols;
    const float nan = std::numeric_limits<float>::quiet_NaN();

    #pragma omp parallel for schedule(static)
    for (int n = 0; n < MN; n++) {
        float t = T[n];
        float s = S[n];
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
        float m4 = t * s;
        float m1 = 1 + m4 - t - s;
        float m2 = t - m4;
        float m3 = s - m4;

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
    if (nrhs != 3)
        mexErrMsgTxt("Wrong number of input parameters, usage: Output_images = mirt2D_mexinterp(Input_images, X, Y)");
    if (nlhs > 1)
        mexErrMsgTxt("Too many output arguments.");

    // Verify input types
    if (!mxIsSingle(IN_Z) || !mxIsSingle(IN_S) || !mxIsSingle(IN_T))
        mexErrMsgTxt("Inputs Z, S, and T must be single precision.");

    // Input sizes
    int M = mxGetM(IN_S);
    int N = mxGetN(IN_S);
    int ndim = mxGetNumberOfDimensions(IN_Z);
    const mwSize *dims = mxGetDimensions(IN_Z);
    int nrows = dims[0];
    int ncols = dims[1];
    int MN = M * N;
    int vol = (ndim > 2) ? dims[2] : 1;

    // Output dimensions
    mwSize newdims[3] = {static_cast<mwSize>(M), static_cast<mwSize>(N), static_cast<mwSize>(vol)};
    OUT_F = mxCreateNumericArray(ndim, newdims, mxSINGLE_CLASS, mxREAL);

    // Assign pointers to input and output
    float *Z = (float *)mxGetData(IN_Z);
    float *S = (float *)mxGetData(IN_S);
    float *T = (float *)mxGetData(IN_T);
    float *F = (float *)mxGetData(OUT_F);

    // Perform interpolation
    mirt2D_mexinterp_single(Z, S, T, F, MN, nrows, ncols, vol);
}
