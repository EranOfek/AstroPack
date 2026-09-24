#include "mex.h"
#include <cmath>
#include <algorithm>
#include <omp.h> // Include OpenMP for parallelization

// Helper function to find the interpolation index
inline int findIndex(const double *arr, double value, int size) {
    int index = std::upper_bound(arr, arr + size, value) - arr - 1;
    if (index < 0) index = 0;
    if (index > size - 2) index = size - 2;
    return index;
}

// Helper function for cubic interpolation in one dimension
inline double cubicInterpolate(double p[4], double x) {
    return p[1] + 0.5 * x * (p[2] - p[0] + x * (2.0 * p[0] - 5.0 * p[1] + 4.0 * p[2] - p[3] + x * (3.0 * (p[1] - p[2]) + p[3] - p[0])));
}

// Bicubic interpolation
inline double cubicInterp(double x, double y, const double *X, const double *Y, const double *V, int nx, int ny) {
    int ix = findIndex(X, x, nx);
    int iy = findIndex(Y, y, ny);

    double arr[4][4];
    for (int m = -1; m <= 2; ++m) {
        for (int n = -1; n <= 2; ++n) {
            int ixm = std::min(std::max(ix + m, 0), nx - 1);
            int iyn = std::min(std::max(iy + n, 0), ny - 1);
            arr[m + 1][n + 1] = V[iyn + ixm * ny];
        }
    }

    double arrX[4];
    for (int i = 0; i < 4; ++i) {
        arrX[i] = cubicInterpolate(arr[i], x - X[ix]);
    }

    return cubicInterpolate(arrX, y - Y[iy]);
}

// Bilinear interpolation
inline double linearInterp(double x, double y, const double *X, const double *Y, const double *V, int nx, int ny) {
    int ix = findIndex(X, x, nx);
    int iy = findIndex(Y, y, ny);

    double x1 = X[ix], x2 = X[ix + 1];
    double y1 = Y[iy], y2 = Y[iy + 1];

    double Q11 = V[iy + ix * ny];
    double Q21 = V[iy + (ix + 1) * ny];
    double Q12 = V[iy + 1 + ix * ny];
    double Q22 = V[iy + 1 + (ix + 1) * ny];

    double denom = (x2 - x1) * (y2 - y1);
    if (denom == 0) return 0.0;

    return (Q11 * (x2 - x) * (y2 - y) +
            Q21 * (x - x1) * (y2 - y) +
            Q12 * (x2 - x) * (y - y1) +
            Q22 * (x - x1) * (y - y1)) / denom;
}

// Nearest neighbor interpolation
inline double nearestInterp(double x, double y, const double *X, const double *Y, const double *V, int nx, int ny) {
    int ix = findIndex(X, x, nx);
    int iy = findIndex(Y, y, ny);

    ix = (std::fabs(X[ix] - x) < std::fabs(X[ix + 1] - x)) ? ix : ix + 1;
    iy = (std::fabs(Y[iy] - y) < std::fabs(Y[iy + 1] - y)) ? iy : iy + 1;

    return V[iy + ix * ny];
}

// Main MEX function
void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[]) {
    if (nrhs != 6) {
        mexErrMsgTxt("Six inputs required: X, Y, V, XI, YI, method");
    }

    const double *X = mxGetPr(prhs[0]);
    const double *Y = mxGetPr(prhs[1]);
    const double *V = mxGetPr(prhs[2]);
    const double *XI = mxGetPr(prhs[3]);
    const double *YI = mxGetPr(prhs[4]);
    char *method = mxArrayToString(prhs[5]);

    int nx = mxGetNumberOfElements(prhs[0]);
    int ny = mxGetNumberOfElements(prhs[1]);
    
    mwSize nxi = mxGetM(prhs[3]);
    mwSize nyi = mxGetN(prhs[3]);

    plhs[0] = mxCreateDoubleMatrix(nxi, nyi, mxREAL);
    double *ZI = mxGetPr(plhs[0]);

    // Parallelize the loop with OpenMP
    #pragma omp parallel for collapse(2)
    for (mwSize j = 0; j < nxi; ++j) {
        for (mwSize i = 0; i < nyi; ++i) {
            double xi = XI[j + i * nxi];
            double yi = YI[j + i * nxi];

            if (strcmp(method, "nearest") == 0) {
                ZI[j + i * nxi] = nearestInterp(xi, yi, X, Y, V, nx, ny);
            } else if (strcmp(method, "linear") == 0) {
                ZI[j + i * nxi] = linearInterp(xi, yi, X, Y, V, nx, ny);
            } else if (strcmp(method, "cubic") == 0) {
                ZI[j + i * nxi] = linearInterp(xi, yi, X, Y, V, nx, ny);
            } else {
                mexErrMsgTxt("Unknown interpolation method.");
            }
        }
    }

    mxFree(method);
}
