#include "mex.h"
#include <cstddef>  // for size_t
#include <cstring>  // for memset
#include <omp.h>    // for OpenMP

// mex CXXFLAGS="\$CXXFLAGS -O3 -fopenmp" LDFLAGS="\$LDFLAGS -fopenmp" mex_histcounts2regularNE_double.cpp

// Function to calculate 2D histogram using OpenMP
void calculate2DHistogram(const double* X, const double* Y, 
                          double startX, double endX, double binWidthX, 
                          double startY, double endY, double binWidthY, 
                          size_t numPoints, uint32_T* histogram) 
{
    // Calculate number of bins
    size_t numBinsX = static_cast<size_t>((endX - startX) / binWidthX);
    size_t numBinsY = static_cast<size_t>((endY - startY) / binWidthY);

    // Initialize histogram with zeros
    std::memset(histogram, 0, numBinsX * numBinsY * sizeof(uint32_T));

    // Precompute edge offsets
    double offsetX = startX;
    double offsetY = startY;

    // Parallel region for histogram calculation
    #pragma omp parallel
    {
        // Local histogram for each thread
        uint32_T* localHistogram = new uint32_T[numBinsX * numBinsY];
        std::memset(localHistogram, 0, numBinsX * numBinsY * sizeof(uint32_T));

        // Parallel for loop to calculate histogram
        #pragma omp for
        for (size_t i = 0; i < numPoints; ++i) {
            // Compute bin index for X
            size_t binX = static_cast<size_t>((X[i] - offsetX) / binWidthX);
            // Compute bin index for Y
            size_t binY = static_cast<size_t>((Y[i] - offsetY) / binWidthY);

            // Check if bin indices are within the valid range
            if (binX < numBinsX && binY < numBinsY) {
                size_t binIndex = binY * numBinsX + binX;
                localHistogram[binIndex]++;
            }
        }

        // Combine local histograms into the global histogram
        #pragma omp critical
        {
            for (size_t i = 0; i < numBinsX * numBinsY; ++i) {
                histogram[i] += localHistogram[i];
            }
        }

        // Clean up
        delete[] localHistogram;
    }
}

// MATLAB gateway function
void mexFunction(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[]) 
{
    // Check for proper number of arguments
    if (nrhs != 8) {
        mexErrMsgTxt("Eight input arguments required.");
    }
    if (nlhs > 1) {
        mexErrMsgTxt("Too many output arguments.");
    }

    // Get input pointers
    const mxArray* XArray = prhs[0];
    const mxArray* YArray = prhs[1];
    double startX = mxGetScalar(prhs[2]);
    double endX = mxGetScalar(prhs[3]);
    double binWidthX = mxGetScalar(prhs[4]);
    double startY = mxGetScalar(prhs[5]);
    double endY = mxGetScalar(prhs[6]);
    double binWidthY = mxGetScalar(prhs[7]);

    if (!mxIsDouble(XArray) || !mxIsDouble(YArray)) {
        mexErrMsgTxt("X and Y inputs must be double arrays.");
    }

    // Get sizes of inputs
    size_t numPoints = mxGetNumberOfElements(XArray);

    // Calculate number of bins
    size_t numBinsX = static_cast<size_t>((endX - startX) / binWidthX);
    size_t numBinsY = static_cast<size_t>((endY - startY) / binWidthY);

    // Create output array
    const mwSize dims[2] = {numBinsY, numBinsX};
    plhs[0] = mxCreateNumericArray(2, dims, mxUINT32_CLASS, mxREAL);

    // Get pointers to input data
    const double* X = mxGetPr(XArray);
    const double* Y = mxGetPr(YArray);

    // Get pointer to output data
    uint32_T* histogram = reinterpret_cast<uint32_T*>(mxGetData(plhs[0]));

    // Calculate 2D histogram
    calculate2DHistogram(X, Y, startX, endX, binWidthX, startY, endY, binWidthY, numPoints, histogram);
}
