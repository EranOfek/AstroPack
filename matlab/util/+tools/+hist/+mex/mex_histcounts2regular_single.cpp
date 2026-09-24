#include "mex.h"
#include <cstddef>  // for size_t
#include <cstring>  // for memset
#include <omp.h>    // for OpenMP

// mex CXXFLAGS="\$CXXFLAGS -O3 -fopenmp" LDFLAGS="\$LDFLAGS -fopenmp" mex_histcounts2regular_single.cpp

// Function to calculate 2D histogram for double precision inputs
void calculate2DHistogramDouble(const double* X, const double* Y, 
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

// Function to calculate 2D histogram for single precision inputs
void calculate2DHistogramSingle(const float* X, const float* Y, 
                                const float* edgesX, const float* edgesY, 
                                size_t numPoints, size_t numEdgesX, size_t numEdgesY, 
                                uint32_T* histogram) 
{
    // Bin width
    float binWidthX = edgesX[1] - edgesX[0];
    float binWidthY = edgesY[1] - edgesY[0];

    // Initialize histogram with zeros
    std::memset(histogram, 0, (numEdgesX - 1) * (numEdgesY - 1) * sizeof(uint32_T));

    // Precompute edge offsets
    float offsetX = edgesX[0];
    float offsetY = edgesY[0];
    
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

    // Determine data type of input arrays
    mxClassID classID = mxGetClassID(XArray);

    // Get sizes of inputs
    size_t numPoints = mxGetNumberOfElements(XArray);
    size_t numEdgesX = mxGetNumberOfElements(edgesXArray);
    size_t numEdgesY = mxGetNumberOfElements(edgesYArray);

    // Create output array
    const mwSize dims[2] = {numEdgesY - 1, numEdgesX - 1};
    plhs[0] = mxCreateNumericArray(2, dims, mxUINT32_CLASS, mxREAL);

    // Get pointer to output data
    uint32_T* histogram = reinterpret_cast<uint32_T*>(mxGetData(plhs[0]));

    // Call the appropriate function based on input type
    if (classID == mxSINGLE_CLASS) {
        const float* X = static_cast<const float*>(mxGetData(XArray));
        const float* Y = static_cast<const float*>(mxGetData(YArray));
        const float* edgesX = static_cast<const float*>(mxGetData(edgesXArray));
        const float* edgesY = static_cast<const float*>(mxGetData(edgesYArray));
        calculate2DHistogramSingle(X, Y, edgesX, edgesY, numPoints, numEdgesX, numEdgesY, histogram);
    } else if (classID == mxDOUBLE_CLASS) {
        const double* X = static_cast<const double*>(mxGetData(XArray));
        const double* Y = static_cast<const double*>(mxGetData(YArray));
        const double* edgesX = static_cast<const double*>(mxGetData(edgesXArray));
        const double* edgesY = static_cast<const double*>(mxGetData(edgesYArray));
        calculate2DHistogramDouble(X, Y, edgesX, edgesY, numPoints, numEdgesX, numEdgesY, histogram);
    } else {
        mexErrMsgIdAndTxt("MATLAB:2DHistogram:inputTypeUnsupported", "Unsupported input type. Only single and double precision are supported.");
    }
}
