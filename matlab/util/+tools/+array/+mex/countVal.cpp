#include "mex.h"
#include <omp.h>

template <typename T>
T countOccurrences(const T *array, const mwSize numElements, const T val) {
    T count = 0;
    
    #pragma omp parallel for reduction(+:count)
    for (mwSize i = 0; i < numElements; ++i) {
        if (array[i] == val) {
            count++;
        }
    }

    return count;
}

void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[]) {
    if (nrhs != 2) {
        mexErrMsgIdAndTxt("MATLAB:countOccurrences:invalidNumInputs", "Two inputs required.");
    }
    if (nlhs > 1) {
        mexErrMsgIdAndTxt("MATLAB:countOccurrences:invalidNumOutputs", "One output required.");
    }

    // Ensure that both inputs are numeric and of the same type
    if (!mxIsNumeric(prhs[0]) || !mxIsNumeric(prhs[1])) {
        mexErrMsgIdAndTxt("MATLAB:countOccurrences:inputNotNumeric", "Both inputs must be numeric.");
    }
    if (mxGetClassID(prhs[0]) != mxGetClassID(prhs[1])) {
        mexErrMsgIdAndTxt("MATLAB:countOccurrences:inputTypeMismatch", "Both inputs must be of the same type.");
    }

    const mwSize numElements = mxGetNumberOfElements(prhs[0]);

    // Determine the type of the input and call the corresponding template function
    if (mxIsDouble(prhs[0])) {
        double *array = mxGetPr(prhs[0]);
        double val = mxGetScalar(prhs[1]);
        double count = countOccurrences(array, numElements, val);
        plhs[0] = mxCreateDoubleScalar(count);
    } else if (mxIsSingle(prhs[0])) {
        float *array = static_cast<float*>(mxGetData(prhs[0]));
        float val = static_cast<float>(mxGetScalar(prhs[1]));
        float count = countOccurrences(array, numElements, val);
        plhs[0] = mxCreateNumericMatrix(1, 1, mxSINGLE_CLASS, mxREAL);
        *static_cast<float*>(mxGetData(plhs[0])) = count;
    } else {
        mexErrMsgIdAndTxt("MATLAB:countOccurrences:unsupportedType", "Unsupported input type.");
    }
}
