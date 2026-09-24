#include "mex.h"
#include <cstdint> // For uint32_t and uint64_t

// The MEX function
void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[]) {
    // Check for the correct number of input and output arguments
    if (nrhs != 2) {
        mexErrMsgIdAndTxt("MATLAB:bitEncode:invalidNumInputs", "Two inputs required: bitLengths and values.");
    }
    if (nlhs != 1) {
        mexErrMsgIdAndTxt("MATLAB:bitEncode:invalidNumOutputs", "One output required.");
    }
    
    // Get the input arrays
    const mxArray *bitLengthsArray = prhs[0];
    const mxArray *valuesArray = prhs[1];

    // Check that inputs are double arrays
    if (!mxIsDouble(bitLengthsArray) || mxIsComplex(bitLengthsArray) ||
        !mxIsDouble(valuesArray) || mxIsComplex(valuesArray)) {
        mexErrMsgIdAndTxt("MATLAB:bitEncode:invalidInputType", "Inputs must be real double arrays.");
    }

    // Get the number of elements in the bitLengths array
    mwSize numElements = mxGetNumberOfElements(bitLengthsArray);

    // Get the dimensions of the values array
    mwSize numRows = mxGetM(valuesArray);
    mwSize numCols = mxGetN(valuesArray);
    
    // Ensure that the number of columns in values matches the length of bitLengths
    if (numCols != numElements) {
        mexErrMsgIdAndTxt("MATLAB:bitEncode:inputMismatch", "Number of columns in values must match the number of elements in bitLengths.");
    }

    // Get pointers to the input data
    double *bitLengths = mxGetPr(bitLengthsArray);
    double *values = mxGetPr(valuesArray);

    // Determine the maximum total bit length
    int totalBits = 0;
    for (mwSize i = 0; i < numElements; ++i) {
        totalBits += static_cast<int>(bitLengths[i]);
    }

    if (totalBits > 64) {
        mexErrMsgIdAndTxt("MATLAB:bitEncode:overflow", "Total bit length exceeds 64 bits.");
    }

    // Create the output column vector
    if (totalBits <= 32) {
        plhs[0] = mxCreateNumericMatrix(numRows, 1, mxUINT32_CLASS, mxREAL);
        uint32_t *output = reinterpret_cast<uint32_t *>(mxGetData(plhs[0]));

        #pragma omp parallel for
        for (mwSize row = 0; row < numRows; ++row) {
            uint32_t result = 0;
            int currentBit = 0;

            for (mwSize col = 0; col < numCols; ++col) {
                int bits = static_cast<int>(bitLengths[col]);
                uint32_t value = static_cast<uint32_t>(values[row + col * numRows]);

                // Check that the value fits within the specified bit length
                if (value >= (1U << bits)) {
                    mexErrMsgIdAndTxt("MATLAB:bitEncode:valueOutOfRange", "Value does not fit within specified bit length.");
                }

                // Shift value into the correct position
                result |= (value << currentBit);
                currentBit += bits;
            }

            output[row] = result;
        }
    } else {
        plhs[0] = mxCreateNumericMatrix(numRows, 1, mxUINT64_CLASS, mxREAL);
        uint64_t *output = reinterpret_cast<uint64_t *>(mxGetData(plhs[0]));

        #pragma omp parallel for
        for (mwSize row = 0; row < numRows; ++row) {
            uint64_t result = 0;
            int currentBit = 0;

            for (mwSize col = 0; col < numCols; ++col) {
                int bits = static_cast<int>(bitLengths[col]);
                uint64_t value = static_cast<uint64_t>(values[row + col * numRows]);

                // Check that the value fits within the specified bit length
                if (value >= (1ULL << bits)) {
                    mexErrMsgIdAndTxt("MATLAB:bitEncode:valueOutOfRange", "Value does not fit within specified bit length.");
                }

                // Shift value into the correct position
                result |= (value << currentBit);
                currentBit += bits;
            }

            output[row] = result;
        }
    }
}
