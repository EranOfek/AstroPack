// from https://www.mathworks.com/matlabcentral/answers/2132331-generated-mex-function-from-a-custom-c-code-but-gives-empty-output
# include <mex.h>
# include "lsd.h"

void mexFunction( int nargout, mxArray *pargout[], 
                  int nargin, const mxArray *pargin[] ) 
{
// Extract the inputs
    if (!mxIsSingle(pargin[0])) {
        mexErrMsgIdAndTxt("mex_elsed:badType",
                "Unsupported image type. Provide a single precision numeric matrix.");
    }
    float* img = mxGetSingles(pargin[0]);
    const mwSize *imSize;
    imSize = mxGetDimensions(pargin[0]);
    int X = imSize[0];
    int Y = imSize[1];
// Call the 'lsd' function
    int n_out;
    float* result = lsd(&n_out, img, X, Y);
// Create the output array
    mwSize dims[2] = { 7, n_out};
    pargout[0] = mxCreateNumericArray(2, dims, mxSINGLE_CLASS, mxREAL);
// Copy the result to the output array
    float* outData = mxGetSingles(pargout[0]);
    for (int i = 0; i < 7 * n_out; ++i) {
       outData[i] = result[i];
    }
    
// Free the result if it was dynamically allocated
    free(result);
}