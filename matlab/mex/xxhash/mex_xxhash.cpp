//
// Calculate xxHash of input array.
//
// ResultInt64 = mex_xxhash(Array, [Seed])
//
// Chen Tishler, 11/05/2023
//

#include "mex.h"
#include "xxhash64.h"

void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[])
{
    // check for correct number of input arguments
    if (nrhs < 1 || nrhs > 2) {
        mexErrMsgIdAndTxt("xxhash_mex:invalidArgs", "Invalid number of input arguments.");
        return;
    }

    // check for correct number of output arguments
    if (nlhs > 1) {
        mexErrMsgIdAndTxt("xxhash_mex:invalidArgs", "Invalid number of output arguments.");
        return;
    }

    // parse input arguments
    const void* buffer = mxGetData(prhs[0]);
    size_t element_size = mxGetElementSize(prhs[0]);
    size_t num_elements = mxGetNumberOfElements(prhs[0]);
    size_t size = element_size * num_elements;

    const uint64_t seed = (nrhs == 2) ? mxGetScalar(prhs[1]) : 0;

    // compute hash value
    uint64_t hash = XXHash64::hash(buffer, size, seed);

    // return hash value as output argument
    plhs[0] = mxCreateNumericMatrix(1, 1, mxUINT64_CLASS, mxREAL);
    uint64_t* output = static_cast<uint64_t*>(mxGetData(plhs[0]));
    *output = hash;
}
