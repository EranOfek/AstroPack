//
// Calculate xxHash of input array.
//
// ResultInt64 = mex_xxhashFile(FileName, [Seed])
//
// Chen Tishler, 11/05/2023
//

#include "mex.h"
#include "matrix.h"
#include <cstdint>
#include <fstream>
#include <vector>
#include "xxhash64.h"

void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[]) {
    if (nrhs <= 1 || !mxIsChar(prhs[0])) {
        mexErrMsgIdAndTxt("MATLAB:mexFunction:invalidInput",
                          "Input must be a single filename string.");
    }

    if (nlhs > 1) {
        mexErrMsgIdAndTxt("MATLAB:mexFunction:invalidOutput",
                          "Output must be a single CRC64 value.");
    }

    const uint64_t seed = (nrhs == 2) ? *((uint64_t*)mxGetData(prhs[1])) : 0;    
    char *filename = mxArrayToString(prhs[0]);
    std::ifstream file(filename, std::ios::binary);
    mxFree(filename);

    if (!file) {
        mexErrMsgIdAndTxt("MATLAB:mexFunction:cannotOpenFile",
                          "Cannot open the specified file.");
    }

    const size_t chunk_size = 65536;
    std::vector<char> buffer(chunk_size);
    uint64_t xxhash64 = 0;

    XXHash64 myhash(seed);  
    while (file) {
        file.read(buffer.data(), chunk_size);
        size_t bytesRead = file.gcount();
        if (bytesRead > 0) {
            myhash.add(buffer.data(), bytesRead);        
        }        
    }         
    xxhash64 = myhash.hash();
    file.close();

    // Create and set output mxArray
    plhs[0] = mxCreateNumericMatrix(1, 1, mxUINT64_CLASS, mxREAL);
    *((uint64_t *)mxGetData(plhs[0])) = xxhash64;
}
