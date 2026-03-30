#include "mex.h"
#include <math.h>
#include <float.h>  // For DBL_MAX

/* The nearest-neighbor interpolation function */
void nearestInterp2D(
    const double *X, const double *Y, const uint32_T *values,
    const double *outX, const double *outY,
    uint32_T *output, mwSize nRows, mwSize nCols, mwSize outRows, mwSize outCols)
{
    /* Define the grid spacing */
    double xMin = X[0];
    double xMax = X[(nCols - 1) * nRows];
    double yMin = Y[0];
    double yMax = Y[nRows - 1];
    double xSpacing = (xMax - xMin) / (nCols - 1);
    double ySpacing = (yMax - yMin) / (nRows - 1);

    #pragma omp parallel for
    for (mwSize j = 0; j < outCols; j++) {
        for (mwSize i = 0; i < outRows; i++) {
            double x = outX[j];
            double y = outY[i];

            /* Check if the point is outside the bounds of the input grid */
            if (x < xMin || x > xMax || y < yMin || y > yMax) {
                output[i + j * outRows] = (uint32_T)mxGetNaN();
            } else {
                /* Find the nearest indices in the input grid */
                mwSize nearest_j = (mwSize)((x - xMin) / xSpacing + 0.5);
                mwSize nearest_i = (mwSize)((y - yMin) / ySpacing + 0.5);

                /* Clamp indices to be within bounds */
                if (nearest_i >= nRows) nearest_i = nRows - 1;
                if (nearest_j >= nCols) nearest_j = nCols - 1;

                /* Assign the value at the nearest grid point to the output */
                output[i + j * outRows] = values[nearest_i + nearest_j * nRows];
            }
        }
    }
}

/* The gateway function */
void mexFunction(int nlhs, mxArray *plhs[],
                 int nrhs, const mxArray *prhs[])
{
    if (nrhs != 5) {
        mexErrMsgIdAndTxt("MATLAB:nearestInterp2D:invalidNumInputs",
                          "Five inputs required.");
    }
    if (nlhs != 1) {
        mexErrMsgIdAndTxt("MATLAB:nearestInterp2D:invalidNumOutputs",
                          "One output required.");
    }

    /* Input matrices */
    double *X = mxGetPr(prhs[0]);
    double *Y = mxGetPr(prhs[1]);
    uint32_T *values = (uint32_T *)mxGetData(prhs[2]);
    double *outX = mxGetPr(prhs[3]);
    double *outY = mxGetPr(prhs[4]);

    /* Dimensions */
    mwSize nRows = mxGetM(prhs[0]);
    mwSize nCols = mxGetN(prhs[0]);
    mwSize outRows = mxGetNumberOfElements(prhs[4]);
    mwSize outCols = mxGetNumberOfElements(prhs[3]);

    /* Create the output matrix */
    plhs[0] = mxCreateNumericMatrix(outRows, outCols, mxUINT32_CLASS, mxREAL);
    uint32_T *output = (uint32_T *)mxGetData(plhs[0]);

    /* Call the interpolation function */
    nearestInterp2D(X, Y, values, outX, outY, output, nRows, nCols, outRows, outCols);
}
