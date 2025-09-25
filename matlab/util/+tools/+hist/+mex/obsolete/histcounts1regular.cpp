#include "mex.h"
#include <cmath>
#include <cstring> // For memset
#include <cstdint> // For uint32_t
#include <omp.h>

// MEX compilation:
// mex histcounts1regular1.cpp CXXFLAGS="\$CXXFLAGS -O3 -march=native -fopenmp" LDFLAGS="\$LDFLAGS -fopenmp"

void mexFunction(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[]) {
    if (nrhs != 4) {
        mexErrMsgIdAndTxt("MATLAB:histogram_equal_bins:InvalidInput",
                          "Usage: hist = histogram_equal_bins(data, firstEdge, binSize, numBins)");
    }

    // Input arguments
    const mxArray* dataArray = prhs[0];
    double firstEdge = mxGetScalar(prhs[1]);
    double binSize = mxGetScalar(prhs[2]);
    mwSize numBins = static_cast<mwSize>(mxGetScalar(prhs[3]));

    if (binSize <= 0 || numBins < 1) {
        mexErrMsgIdAndTxt("MATLAB:histogram_equal_bins:InvalidParams",
                          "binSize must be positive, and numBins must be at least 1.");
    }

    // Validate input array type
    const mxClassID dataClass = mxGetClassID(dataArray);
    if (!(mxIsSingle(dataArray) || mxIsDouble(dataArray) ||
          mxIsUint32(dataArray) || mxIsInt32(dataArray))) {
        mexErrMsgIdAndTxt("MATLAB:histogram_equal_bins:InvalidDataType",
                          "Data must be single, double, uint32, or int32.");
    }

    const mwSize numData = mxGetNumberOfElements(dataArray);

    // Create histogram array
    plhs[0] = mxCreateNumericMatrix(numBins, 1, mxUINT32_CLASS, mxREAL);
    uint32_t* histogram = static_cast<uint32_t*>(mxGetData(plhs[0]));
    std::memset(histogram, 0, numBins * sizeof(uint32_t));

    if (dataClass == mxDOUBLE_CLASS) {
        const double* data = static_cast<const double*>(mxGetData(dataArray));
        #pragma omp parallel
        {
            uint32_t* localHist = new uint32_t[numBins]();
            #pragma omp for
            for (mwSize i = 0; i < numData; ++i) {
                double value = data[i];
                mwSize bin = static_cast<mwSize>(std::floor((value - firstEdge) / binSize));
                if (bin >= 0 && bin < numBins) {
                    localHist[bin]++;
                }
            }
            #pragma omp critical
            {
                for (mwSize j = 0; j < numBins; ++j) {
                    histogram[j] += localHist[j];
                }
            }
            delete[] localHist;
        }
    } else if (dataClass == mxSINGLE_CLASS) {
        const float* data = static_cast<const float*>(mxGetData(dataArray));
        #pragma omp parallel
        {
            uint32_t* localHist = new uint32_t[numBins]();
            #pragma omp for
            for (mwSize i = 0; i < numData; ++i) {
                float value = data[i];
                mwSize bin = static_cast<mwSize>(std::floor((value - firstEdge) / binSize));
                if (bin >= 0 && bin < numBins) {
                    localHist[bin]++;
                }
            }
            #pragma omp critical
            {
                for (mwSize j = 0; j < numBins; ++j) {
                    histogram[j] += localHist[j];
                }
            }
            delete[] localHist;
        }
    } else if (dataClass == mxUINT32_CLASS || dataClass == mxINT32_CLASS) {
        const int* data = static_cast<const int*>(mxGetData(dataArray));
        #pragma omp parallel
        {
            uint32_t* localHist = new uint32_t[numBins]();
            #pragma omp for
            for (mwSize i = 0; i < numData; ++i) {
                double value = static_cast<double>(data[i]);
                mwSize bin = static_cast<mwSize>(std::floor((value - firstEdge) / binSize));
                if (bin >= 0 && bin < numBins) {
                    localHist[bin]++;
                }
            }
            #pragma omp critical
            {
                for (mwSize j = 0; j < numBins; ++j) {
                    histogram[j] += localHist[j];
                }
            }
            delete[] localHist;
        }
    }
}
