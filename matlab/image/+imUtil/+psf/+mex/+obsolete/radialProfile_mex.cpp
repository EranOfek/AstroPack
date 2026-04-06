#include "mex.h"
#include <cmath>
#include <vector>
#include <omp.h>


void mexFunction(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[]) {
    if (nrhs != 4) {
        mexErrMsgIdAndTxt("MATLAB:radialProfile:invalidNumInputs", "Usage: [R, Mean, Std] = radialProfile(Image, Center, Radius, Step).");
    }
    if (nlhs != 3) {
        mexErrMsgIdAndTxt("MATLAB:radialProfile:invalidNumOutputs", "Function must return three outputs: [R, Mean, Std].");
    }

    // Input: Image
    const mxArray* image = prhs[0];
    const mwSize* dims = mxGetDimensions(image);
    int rows = dims[0], cols = dims[1];
    double* imgData = mxGetPr(image);

    // Input: Center [X, Y]
    const double* center = mxGetPr(prhs[1]);
    double centerX = center[0] - 1, centerY = center[1] - 1;

    // Input: Max Radius and Step
    double maxRadius = mxGetScalar(prhs[2]);
    double step = mxGetScalar(prhs[3]);

    // Define radial bins
    int numBins = static_cast<int>(maxRadius / step);
    std::vector<double> R(numBins), Sum(numBins, 0.0), SumSq(numBins, 0.0);
    std::vector<int> N(numBins, 0);
    for (int i = 0; i < numBins; ++i) R[i] = (i + 0.5) * step;

    // Main loop
    #pragma omp parallel
    {
        std::vector<double> localSum(numBins, 0.0), localSumSq(numBins, 0.0);
        std::vector<int> localN(numBins, 0);

        #pragma omp for
        for (int y = static_cast<int>(centerY - maxRadius); y < static_cast<int>(centerY + maxRadius); ++y) {
            if (y < 0 || y >= rows) continue;
            for (int x = static_cast<int>(centerX - maxRadius); x < static_cast<int>(centerX + maxRadius); ++x) {
                if (x < 0 || x >= cols) continue;

                double dx = x - centerX;
                double dy = y - centerY;
                double r = std::sqrt(dx * dx + dy * dy);
                if (r > maxRadius) continue; // Skip pixels outside the maximum radius

                int binIndex = static_cast<int>(r / step);
                if (binIndex < numBins) {
                    double val = imgData[y + x * rows]; // Column-major access
                    localN[binIndex]++;
                    localSum[binIndex] += val;
                    localSumSq[binIndex] += val * val;
                }
            }
        }

        // Combine results from threads
        #pragma omp critical
        {
            for (int i = 0; i < numBins; ++i) {
                N[i] += localN[i];
                Sum[i] += localSum[i];
                SumSq[i] += localSumSq[i];
            }
        }
    }

    // Calculate mean and standard deviation
    std::vector<double> Mean(numBins), Std(numBins);
    for (int i = 0; i < numBins; ++i) {
        if (N[i] > 0) {
            Mean[i] = Sum[i] / N[i];
            double variance = (SumSq[i] / N[i]) - (Mean[i] * Mean[i]);
            Std[i] = std::sqrt(std::max(0.0, variance));
        } else {
            Mean[i] = 0.0;
            Std[i] = 0.0;
        }
    }

    // Output: R, Mean, Std
    plhs[0] = mxCreateDoubleMatrix(numBins, 1, mxREAL);
    plhs[1] = mxCreateDoubleMatrix(numBins, 1, mxREAL);
    plhs[2] = mxCreateDoubleMatrix(numBins, 1, mxREAL);

    std::copy(R.begin(), R.end(), mxGetPr(plhs[0]));
    std::copy(Mean.begin(), Mean.end(), mxGetPr(plhs[1]));
    std::copy(Std.begin(), Std.end(), mxGetPr(plhs[2]));
}
