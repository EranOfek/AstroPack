#include "mex.h"
#include <cstddef>  // for size_t
#include <cstring>  // for memset
#include <omp.h>    // for OpenMP

// mex CXXFLAGS="\$CXXFLAGS -O3 -fopenmp" LDFLAGS="\$LDFLAGS -fopenmp" mex_histcounts2regular.cpp
// Function to calculate 2D histogram using OpenMP
void calculate2DHistogram(const double* X, const double* Y, 
                          const double* edgesX, const double* edgesY, 
                          size_t numPoints, size_t numEdgesX, size_t numEdgesY, 
                          uint32_T* histogram) 
{
    // Bin width
    double binWidthX = edgesX[1] - edgesX[0];
    double binWidthY = edgesY[1] - edgesY[0];

    // Initialize histogram with zeros
    std::memset(histogram, 0, (numEdgesX - 1) * (numEdgesY - 1) * sizeof(uint32_T));

    // Precompute edge offsets
    double offsetX = edgesX[0];
    double offsetY = edgesY[0];
    
    // Number of bins
    size_t numBinsX = numEdgesX - 1;
    size_t numBinsY = numEdgesY - 1;

    // Parallel region for histogram calculation
    #pragma omp parallel
    {
        // Local histogram for each thread
        uint32_T localHistogram[(numEdgesX - 1) * (numEdgesY - 1)] = {0};

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
            for (size_t i = 0; i < (numEdgesX - 1) * (numEdgesY - 1); ++i) {
                histogram[i] += localHistogram[i];
            }
        }
    }
}

// MATLAB gateway function
void mexFunction(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[]) 
{
    // Check for proper number of arguments
    if (nrhs != 4) {
        mexErrMsgIdAndTxt("MATLAB:2DHistogram:invalidNumInputs", "Four input arguments required.");
    }
    if (nlhs > 1) {
        mexErrMsgIdAndTxt("MATLAB:2DHistogram:invalidNumOutputs", "Too many output arguments.");
    }

    // Get input pointers
    const mxArray* XArray = prhs[0];
    const mxArray* YArray = prhs[1];
    const mxArray* edgesXArray = prhs[2];
    const mxArray* edgesYArray = prhs[3];

    if (!mxIsDouble(XArray) || !mxIsDouble(YArray) || !mxIsDouble(edgesXArray) || !mxIsDouble(edgesYArray)) {
        mexErrMsgIdAndTxt("MATLAB:2DHistogram:inputNotDouble", "Inputs must be double arrays.");
    }

    // Get sizes of inputs
    size_t numPoints = mxGetNumberOfElements(XArray);
    size_t numEdgesX = mxGetNumberOfElements(edgesXArray);
    size_t numEdgesY = mxGetNumberOfElements(edgesYArray);

    // Create output array
    const mwSize dims[2] = {numEdgesY - 1, numEdgesX - 1};
    plhs[0] = mxCreateNumericArray(2, dims, mxUINT32_CLASS, mxREAL);

    // Get pointers to input data
    const double* X = mxGetPr(XArray);
    const double* Y = mxGetPr(YArray);
    const double* edgesX = mxGetPr(edgesXArray);
    const double* edgesY = mxGetPr(edgesYArray);

    // Get pointer to output data
    uint32_T* histogram = reinterpret_cast<uint32_T*>(mxGetData(plhs[0]));

    // Calculate 2D histogram
    calculate2DHistogram(X, Y, edgesX, edgesY, numPoints, numEdgesX, numEdgesY, histogram);
}
