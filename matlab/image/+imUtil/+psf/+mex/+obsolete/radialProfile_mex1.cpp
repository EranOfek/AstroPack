#include "mex.h"
#include <cmath>
#include <vector>
#include <algorithm>
#include <omp.h>

// MEX function entry point
void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[]) {
    // Input validation
    if (nrhs != 5) {
        mexErrMsgIdAndTxt("mex_annulus_stats:invalidNumInputs", "Five inputs required: image, x, y, max_radius, step_size.");
    }
    if (nlhs != 3) {
        mexErrMsgIdAndTxt("mex_annulus_stats:invalidNumOutputs", "Three outputs required: radii, means, stds.");
    }

    // Input arguments
    const mxArray *imageArray = prhs[0];
    double x = mxGetScalar(prhs[1]);
    double y = mxGetScalar(prhs[2]);
    double maxRadius = mxGetScalar(prhs[3]);
    double stepSize = mxGetScalar(prhs[4]);

    // Validate the image input
    if (!mxIsDouble(imageArray) && !mxIsSingle(imageArray)) {
        mexErrMsgIdAndTxt("mex_annulus_stats:invalidImageType", "Image must be of type single or double.");
    }

    const mwSize *dims = mxGetDimensions(imageArray);
    size_t numRows = dims[0];
    size_t numCols = dims[1];
    bool isDouble = mxIsDouble(imageArray);

    // Prepare output vectors
    size_t numRadii = static_cast<size_t>(std::ceil(maxRadius / stepSize));
    plhs[0] = mxCreateDoubleMatrix(numRadii, 1, mxREAL); // Radii
    plhs[1] = mxCreateDoubleMatrix(numRadii, 1, mxREAL); // Means
    plhs[2] = mxCreateDoubleMatrix(numRadii, 1, mxREAL); // Stds

    double *radii = mxGetPr(plhs[0]);
    double *means = mxGetPr(plhs[1]);
    double *stds = mxGetPr(plhs[2]);

    // Initialize accumulators for each annulus
    std::vector<double> sum(numRadii, 0.0);
    std::vector<double> sumSq(numRadii, 0.0);
    std::vector<size_t> count(numRadii, 0);

    // Access image data
    const void *imageData = mxGetData(imageArray);

    // Define bounds for processing
    size_t startRow = static_cast<size_t>(std::max(0.0, y - maxRadius));
    size_t endRow = static_cast<size_t>(std::min(static_cast<double>(numRows - 1), y + maxRadius));
    size_t startCol = static_cast<size_t>(std::max(0.0, x - maxRadius));
    size_t endCol = static_cast<size_t>(std::min(static_cast<double>(numCols - 1), x + maxRadius));

    #pragma omp parallel for collapse(2) schedule(static)
    for (size_t row = startRow; row <= endRow; ++row) {
        for (size_t col = startCol; col <= endCol; ++col) {
            double dist2 = (x - (col + 1)) * (x - (col + 1)) + (y - (row + 1)) * (y - (row + 1));
            double dist = std::sqrt(dist2);

            // Determine which annulus the pixel belongs to
            size_t rIdx = static_cast<size_t>(std::floor(dist / stepSize));
            if (rIdx < numRadii) {
                double pixelValue = isDouble ?
                    static_cast<const double *>(imageData)[row + col * numRows] :
                    static_cast<const float *>(imageData)[row + col * numRows];

                #pragma omp atomic
                sum[rIdx] += pixelValue;

                #pragma omp atomic
                sumSq[rIdx] += pixelValue * pixelValue;

                #pragma omp atomic
                count[rIdx]++;
            }
        }
    }

    // Compute statistics for each annulus
    for (size_t rIdx = 0; rIdx < numRadii; ++rIdx) {
        radii[rIdx] = (rIdx + 1) * stepSize;
        if (count[rIdx] > 0) {
            means[rIdx] = sum[rIdx] / count[rIdx];
            stds[rIdx] = std::sqrt((sumSq[rIdx] / count[rIdx]) - (means[rIdx] * means[rIdx]));
        } else {
            means[rIdx] = 0.0;
            stds[rIdx] = 0.0;
        }
    }
}
