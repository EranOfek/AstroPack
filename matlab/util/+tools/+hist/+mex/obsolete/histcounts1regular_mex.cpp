#include "mex.h"
#include <algorithm>
#include <cmath>

// mex CXXFLAGS="\$CXXFLAGS -mavx2 -std=c++17" LDFLAGS="\$LDFLAGS -mavx2" histcounts1regular_mex.cpp

// Template function to calculate the histogram
template <typename T>
void computeHistogram(const T* data, size_t dataLen, T edgeMin, T edgeMax, size_t numBins, double* counts) {
    std::fill(counts, counts + numBins, 0.0); // Initialize counts to 0

    T binWidth = (edgeMax - edgeMin) / numBins;
    T invBinWidth = 1.0 / binWidth;

    for (size_t i = 0; i < dataLen; ++i) {
        if (data[i] >= edgeMin && data[i] < edgeMax) {
            size_t binIndex = static_cast<size_t>((data[i] - edgeMin) * invBinWidth);
            counts[binIndex]++;
        } else if (data[i] == edgeMax) {
            counts[numBins - 1]++;
        }
    }
}

// The gateway function
void mexFunction(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[]) {
    if (nrhs != 2) {
        mexErrMsgIdAndTxt("MyToolbox:histc:nrhs", "Two inputs required.");
    }
    if (nlhs != 1) {
        mexErrMsgIdAndTxt("MyToolbox:histc:nlhs", "One output required.");
    }

    // Get the class ID of the inputs
    mxClassID classID = mxGetClassID(prhs[0]);
    if (classID != mxGetClassID(prhs[1])) {
        mexErrMsgIdAndTxt("MyToolbox:histc:inputClassMismatch", "Inputs must have the same class.");
    }

    // Get the dimensions of the input vectors
    size_t dataLen = mxGetNumberOfElements(prhs[0]);
    size_t edgesLen = mxGetNumberOfElements(prhs[1]);

    if (edgesLen < 2) {
        mexErrMsgIdAndTxt("MyToolbox:histc:invalidEdges", "Edges vector must have at least two elements.");
    }

    // Prepare output
    plhs[0] = mxCreateNumericMatrix(1, edgesLen - 1, mxDOUBLE_CLASS, mxREAL);
    double* counts = static_cast<double*>(mxGetData(plhs[0]));

    // Process inputs based on their type
    switch (classID) {
        case mxSINGLE_CLASS: {
            const float* data = static_cast<const float*>(mxGetData(prhs[0]));
            const float* edges = static_cast<const float*>(mxGetData(prhs[1]));
            computeHistogram(data, dataLen, edges[0], edges[edgesLen - 1], edgesLen - 1, counts);
            break;
        }
        case mxDOUBLE_CLASS: {
            const double* data = static_cast<const double*>(mxGetData(prhs[0]));
            const double* edges = static_cast<const double*>(mxGetData(prhs[1]));
            computeHistogram(data, dataLen, edges[0], edges[edgesLen - 1], edgesLen - 1, counts);
            break;
        }
        default:
            mexErrMsgIdAndTxt("MyToolbox:histc:unsupportedClass", "Only single and double classes are supported.");
    }
}
