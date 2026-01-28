// trapzmat_mex.cpp
// Trapezoidal integration like trapzmat(), but WITHOUT abs(dx).
// X can be either:
//   - same size as Y, or
//   - a vector matching the integration dimension (trapz-like).
//
// Compile (Linux, gcc):
// mex -O CXXFLAGS="\$CXXFLAGS -O3 -march=native -ffast-math -DNDEBUG" trapzmat_mex.cpp
// OpenMP:
// mex -O CXXFLAGS="\$CXXFLAGS -O3 -march=native -ffast-math -DNDEBUG -fopenmp" LDFLAGS="\$LDFLAGS -fopenmp" trapzmat_mex.cpp

#include "mex.h"
#include <cstddef>
#include <cstdint>
#include <cstring>

#if defined(_OPENMP)
  #include <omp.h>
#endif

static int get_dim_arg(int nrhs, const mxArray* prhs[])
{
    if (nrhs < 3) return 1;
    if (!mxIsNumeric(prhs[2]) || mxIsComplex(prhs[2]) || mxGetNumberOfElements(prhs[2]) != 1) {
        mexErrMsgIdAndTxt("trapzmat_mex:Dim", "Dim must be a real numeric scalar (1 or 2).");
    }
    int Dim = (int)mxGetScalar(prhs[2]);
    if (Dim != 1 && Dim != 2) {
        mexErrMsgIdAndTxt("trapzmat_mex:Dim", "Dim must be 1 or 2.");
    }
    return Dim;
}

template<typename T>
static void integrate_dim1(T* out, const T* X, const T* Y, mwSize m, mwSize n, bool x_is_vector)
{
    const T half = (T)0.5;

    #if defined(_OPENMP)
    #pragma omp parallel for schedule(static)
    #endif
    for (mwIndex j = 0; j < n; ++j) {
        const T* xcol = x_is_vector ? X : (X + (std::size_t)j * m);
        const T* ycol = Y + (std::size_t)j * m;

        T acc = (T)0;
        const mwIndex mm1 = (m > 0) ? (m - 1) : 0;

        mwIndex i = 0;
        for (; i + 3 < mm1; i += 4) {
            T dx0 = xcol[i+1] - xcol[i];
            T dx1 = xcol[i+2] - xcol[i+1];
            T dx2 = xcol[i+3] - xcol[i+2];
            T dx3 = xcol[i+4] - xcol[i+3];

            T s0  = ycol[i]   + ycol[i+1];
            T s1  = ycol[i+1] + ycol[i+2];
            T s2  = ycol[i+2] + ycol[i+3];
            T s3  = ycol[i+3] + ycol[i+4];

            acc += half * (dx0*s0 + dx1*s1 + dx2*s2 + dx3*s3);
        }

        for (; i < mm1; ++i) {
            const T dx = xcol[i+1] - xcol[i];
            const T sy = ycol[i] + ycol[i+1];
            acc += half * dx * sy;
        }

        out[j] = acc;
    }
}

template<typename T>
static void integrate_dim2(T* out, const T* X, const T* Y, mwSize m, mwSize n, bool x_is_vector)
{
    const T half = (T)0.5;

    // For Dim=2:
    // - Y is m x n.
    // - If X is vector, it has length n and is used for all rows: x(c) = X[c]
    // - If X is matrix, use x(r,c) = X[r + c*m]

    #if defined(_OPENMP)
    #pragma omp parallel for schedule(static)
    #endif
    for (mwIndex r = 0; r < m; ++r) {
        T acc = (T)0;
        const mwIndex nm1 = (n > 0) ? (n - 1) : 0;

        if (x_is_vector) {
            // X is length n (stored contiguously)
            std::size_t idx = (std::size_t)r;
            const std::size_t stride = (std::size_t)m;

            for (mwIndex c = 0; c < nm1; ++c) {
                const T x0 = X[c];
                const T x1 = X[c+1];

                const T y0 = Y[idx];
                const T y1 = Y[idx + stride];

                acc += half * (x1 - x0) * (y0 + y1);
                idx += stride;
            }
        } else {
            // X is m x n
            std::size_t idx = (std::size_t)r;
            const std::size_t stride = (std::size_t)m;

            for (mwIndex c = 0; c < nm1; ++c) {
                const T x0 = X[idx];
                const T x1 = X[idx + stride];

                const T y0 = Y[idx];
                const T y1 = Y[idx + stride];

                acc += half * (x1 - x0) * (y0 + y1);
                idx += stride;
            }
        }

        out[r] = acc;
    }
}

static void zero_out(void* outp, mxClassID cls, mwSize outLen)
{
    std::size_t bytes = (cls == mxDOUBLE_CLASS) ? sizeof(double) * (std::size_t)outLen
                                                : sizeof(float)  * (std::size_t)outLen;
    std::memset(outp, 0, bytes);
}

void mexFunction(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[])
{
    if (nrhs < 2 || nrhs > 3) {
        mexErrMsgIdAndTxt("trapzmat_mex:nrhs", "Usage: Trapz=trapzmat_mex(X,Y,[Dim])");
    }
    if (nlhs > 1) {
        mexErrMsgIdAndTxt("trapzmat_mex:nlhs", "One output only.");
    }

    const mxArray* Xmx = prhs[0];
    const mxArray* Ymx = prhs[1];

    if (!mxIsNumeric(Xmx) || !mxIsNumeric(Ymx) || mxIsSparse(Xmx) || mxIsSparse(Ymx)) {
        mexErrMsgIdAndTxt("trapzmat_mex:type", "X and Y must be full (non-sparse) numeric arrays.");
    }
    if (mxIsComplex(Xmx) || mxIsComplex(Ymx)) {
        mexErrMsgIdAndTxt("trapzmat_mex:complex", "X and Y must be real (non-complex).");
    }
    if (mxGetNumberOfDimensions(Ymx) != 2 || mxGetNumberOfDimensions(Xmx) != 2) {
        mexErrMsgIdAndTxt("trapzmat_mex:ndims", "X and Y must be 2-D (matrices/vectors).");
    }

    const mwSize mY = mxGetM(Ymx);
    const mwSize nY = mxGetN(Ymx);

    const mxClassID cx = mxGetClassID(Xmx);
    const mxClassID cy = mxGetClassID(Ymx);
    if (cx != cy) {
        mexErrMsgIdAndTxt("trapzmat_mex:class", "X and Y must have the same class (single or double).");
    }
    if (!(cx == mxDOUBLE_CLASS || cx == mxSINGLE_CLASS)) {
        mexErrMsgIdAndTxt("trapzmat_mex:class", "Only single and double are supported.");
    }

    const int Dim = get_dim_arg(nrhs, prhs);

    // Decide whether X is matrix-sized or vector-sized
    const mwSize mX = mxGetM(Xmx);
    const mwSize nX = mxGetN(Xmx);
    const mwSize numelX = mxGetNumberOfElements(Xmx);

    bool x_is_matrix = (mX == mY && nX == nY);
    bool x_is_vector = false;

    if (!x_is_matrix) {
        if (Dim == 1) {
            // vector length must match mY
            x_is_vector = (numelX == mY);
        } else { // Dim==2
            // vector length must match nY
            x_is_vector = (numelX == nY);
        }
    }

    if (!x_is_matrix && !x_is_vector) {
        mexErrMsgIdAndTxt("trapzmat_mex:size",
            "X must be either the same size as Y, or a vector with length matching the integration dimension.");
    }

    // Output is row vector like your MATLAB code
    mwSize outLen = (Dim == 1) ? nY : mY;
    plhs[0] = mxCreateNumericMatrix(1, outLen, cx, mxREAL);

    if (outLen == 0) return;

    // Edge cases: if integration dimension length < 2, integral is 0
    if ((Dim == 1 && mY < 2) || (Dim == 2 && nY < 2)) {
        zero_out(mxGetData(plhs[0]), cx, outLen);
        return;
    }

    if (cx == mxDOUBLE_CLASS) {
        const double* X = (const double*)mxGetData(Xmx);
        const double* Y = (const double*)mxGetData(Ymx);
        double* out = (double*)mxGetData(plhs[0]);

        if (Dim == 1) integrate_dim1(out, X, Y, mY, nY, x_is_vector);
        else          integrate_dim2(out, X, Y, mY, nY, x_is_vector);

    } else { // single
        const float* X = (const float*)mxGetData(Xmx);
        const float* Y = (const float*)mxGetData(Ymx);
        float* out = (float*)mxGetData(plhs[0]);

        if (Dim == 1) integrate_dim1(out, X, Y, mY, nY, x_is_vector);
        else          integrate_dim2(out, X, Y, mY, nY, x_is_vector);
    }
}
