#include "mex.h"
#include <cstdio>
#include <cstring>
#include <cmath>


// Function to append new header fields and reattach "END"
void appendHeaderFields(const char* filename, const mxArray* headerFields) {

    //mexPrintf("appendHeaderFields: %s\n", filename);  

    FILE* file = fopen(filename, "r+b"); // Open file for update
    if (!file) {
        mexErrMsgIdAndTxt("MATLAB:fileOpenFailure", "Failed to open file.");
        return;
    }

    const int blockSize = 2880;    
    const size_t cardSize = 80; // Size of each FITS header card
    char block[blockSize]; // Buffer to hold a block of the FITS file
    bool endFound = false;
    size_t bytesRead;
    int res;

    // Read existing header, looking for the "END" keyword
    size_t blockPos = 0;
    size_t endPos = 0;
    while ((bytesRead = fread(block, 1, blockSize, file)) == blockSize && !endFound) {
        for (int i = 0; i <= blockSize - cardSize; i += cardSize) { // Skip 80 bytes for the next card
            if (strncmp(&block[i], "END     ", 8) == 0) { // Correctly matching "END" keyword and padding
                endPos = blockPos + i;
                //mexPrintf("END found at byte offset: %ld\n", endPos);  
                fseek(file, (long)endPos, SEEK_SET); // Position the file pointer right after the last card before "END"
                endFound = true;
                break;
            }
        }
        if (endFound)
            break;

        blockPos += blockSize;
    }

    if (!endFound) {
        fclose(file);
        mexErrMsgIdAndTxt("MATLAB:endNotFound", "The 'END' keyword was not found.");
        return;
    }

    //mexPrintf("ftell: %ld\n", ftell(file));  

    // Append new header fields from the cell array
    size_t numFields = mxGetNumberOfElements(headerFields);
    //mexPrintf("numFields: %d\n", (int)numFields);
    for (size_t i = 0; i < numFields; ++i) {
        mxArray* field = mxGetCell(headerFields, i);
        if (field && mxIsChar(field)) {
            char* fieldStr = mxArrayToString(field);
            res = fwrite(fieldStr, 1, strlen(fieldStr), file); // Write field
            if (res == 0) mexPrintf("res: %d\n", (int)res);
            mxFree(fieldStr);
        }
    }

    //mexPrintf("new end ftell: %ld\n", ftell(file));  

    // Reattach "END"
    const char* endStr = "END";
    res = fwrite(endStr, 1, strlen(endStr), file);
    if (res == 0) mexPrintf("res: %d\n", (int)res);
    // Pad the remaining bytes with spaces to complete the block
    int remaining = blockSize - (ftell(file) % blockSize);
    char* padding = new char[remaining];
    memset(padding, ' ', remaining);
    res = fwrite(padding, 1, remaining, file);
    if (res == 0) mexPrintf("res: %d\n", (int)res);
    delete[] padding;

    //mexPrintf("ftell: %ld\n", ftell(file));  
    //mexPrintf("calling fclose\n");
    fclose(file);
    //mexPrintf("done\n");
}


// MEX Function Gateway
void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[]) {
    if (nrhs != 2) {
        mexErrMsgIdAndTxt("MATLAB:arguments", "Requires 2 input arguments: filename, headerFields");
    }

    if (!mxIsChar(prhs[0])) {
        mexErrMsgIdAndTxt("MATLAB:fileInput", "First input must be a filename string.");
    }

    if (!mxIsCell(prhs[1])) {
        mexErrMsgIdAndTxt("MATLAB:headerInput", "Second input must be a cell array of header fields.");
    }

    char* filename = mxArrayToString(prhs[0]);
    appendHeaderFields(filename, prhs[1]);
    mxFree(filename);
}
