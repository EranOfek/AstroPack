#include "mex.h"
#include "xxhash.h"

void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[])
{
    // check for correct number of input arguments
    if (nrhs < 2 || nrhs > 3) {
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
    const size_t size = mxGetNumberOfElements(prhs[0]);
    const uint64_t seed = (nrhs == 3) ? mxGetScalar(prhs[2]) : 0;

    // compute hash value
    const XXH64_hash_t hash = XXH64(buffer, size, seed);

    // return hash value as output argument
    plhs[0] = mxCreateNumericMatrix(1, 1, mxUINT64_CLASS, mxREAL);
    uint64_t* output = static_cast<uint64_t*>(mxGetData(plhs[0]));
    *output = hash;
}
