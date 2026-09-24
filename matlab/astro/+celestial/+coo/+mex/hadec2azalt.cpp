#include "mex.h"
#include <cmath>
#include <omp.h>  // OpenMP for parallelization

// Template function to handle both single and double precision
template <typename T>
void hadec2azalt(const T* ha, const T* dec, T lat, T* az, T* alt, mwSize numElements) {
    const T sinLat = sin(lat);
    const T cosLat = cos(lat);
    const T two_pi = static_cast<T>(2.0 * M_PI); // Precompute 2*pi

    #pragma omp parallel for
    for (mwSize i = 0; i < numElements; ++i) {
        const T sinDec = sin(dec[i]);
        const T cosDec = cos(dec[i]);
        const T cosHA = cos(ha[i]);
        const T sinHA = sin(ha[i]);

        // Compute SinAlt
        const T sinAlt = sinDec * sinLat + cosDec * cosHA * cosLat;

        // Compute Alt
        alt[i] = asin(sinAlt);

        // Compute CosAlt using cos(Alt) = sqrt(1 - sin(Alt)^2)
        const T cosAlt = sqrt(static_cast<T>(1.0) - sinAlt * sinAlt);

        // Compute SinAz and CosAz directly
        const T sinAz = (-cosDec * sinHA) / cosAlt;
        const T cosAz = (sinDec * cosLat - cosDec * cosHA * sinLat) / cosAlt;

        // Compute Az using atan2 for correct quadrant
        az[i] = atan2(sinAz, cosAz);

        // Adjust Azimuth to be in the range [0, 2*pi]
        if (az[i] < 0) {
            az[i] += two_pi;
        }
    }
}

// MEX function entry point
void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[]) {
    // Input validation
    if (nrhs != 3) {
        mexErrMsgIdAndTxt("MATLAB:hadec2azalt:invalidNumInputs", "Three input arguments required (HA, Dec, Lat).");
    }
    if (nlhs != 2) {
        mexErrMsgIdAndTxt("MATLAB:hadec2azalt:invalidNumOutputs", "Two output arguments required (Az, Alt).");
    }

    // Get the inputs and check their type
    mxClassID classID = mxGetClassID(prhs[0]);

    if (classID == mxDOUBLE_CLASS) {
        // Double precision inputs
        double* ha = mxGetPr(prhs[0]);
        double* dec = mxGetPr(prhs[1]);
        double lat = mxGetScalar(prhs[2]);

        mwSize numElements = mxGetNumberOfElements(prhs[0]);

        // Verify input sizes
        if (mxGetNumberOfElements(prhs[1]) != numElements) {
            mexErrMsgIdAndTxt("MATLAB:hadec2azalt:inputSizeMismatch", "HA and Dec must have the same number of elements.");
        }

        // Create the output arrays
        plhs[0] = mxCreateDoubleMatrix(numElements, 1, mxREAL); // Az output
        plhs[1] = mxCreateDoubleMatrix(numElements, 1, mxREAL); // Alt output

        double* az = mxGetPr(plhs[0]);
        double* alt = mxGetPr(plhs[1]);

        // Call the template function for double precision
        hadec2azalt(ha, dec, lat, az, alt, numElements);

    } else if (classID == mxSINGLE_CLASS) {
        // Single precision inputs
        float* ha = (float*)mxGetData(prhs[0]);
        float* dec = (float*)mxGetData(prhs[1]);
        float lat = (float)mxGetScalar(prhs[2]);

        mwSize numElements = mxGetNumberOfElements(prhs[0]);

        // Verify input sizes
        if (mxGetNumberOfElements(prhs[1]) != numElements) {
            mexErrMsgIdAndTxt("MATLAB:hadec2azalt:inputSizeMismatch", "HA and Dec must have the same number of elements.");
        }

        // Create the output arrays
        plhs[0] = mxCreateNumericMatrix(numElements, 1, mxSINGLE_CLASS, mxREAL); // Az output
        plhs[1] = mxCreateNumericMatrix(numElements, 1, mxSINGLE_CLASS, mxREAL); // Alt output

        float* az = (float*)mxGetData(plhs[0]);
        float* alt = (float*)mxGetData(plhs[1]);

        // Call the template function for single precision
        hadec2azalt(ha, dec, lat, az, alt, numElements);

    } else {
        mexErrMsgIdAndTxt("MATLAB:hadec2azalt:invalidInputType", "Input must be either single or double.");
    }
}
