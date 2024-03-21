#include "mex.h"
#include <stdio.h>
#include <string.h>


// Helper function to write the header block
void writeHeader(const char *filename, const char *header, size_t headerSize) {
    FILE *file = fopen(filename, "r+b"); // Open for reading/updating binary
    if (!file) {
        mexErrMsgIdAndTxt("MATLAB:mex:fileOpenError", "Could not open the file.");
        return;
    }

    // Seek to the start of the file or the start of the header section
    fseek(file, 0, SEEK_SET);

    // Write the header block
    fwrite(header, sizeof(char), headerSize, file);

    // Close the file
    fclose(file);
}


// Main MEX function
void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[]) {
    // Input arguments check
    if (nrhs != 2) {
        mexErrMsgIdAndTxt("MATLAB:mex:invalidNumInputs", "Two inputs required.");
    }

    // Ensure the first input is a cell array and the second is a string
    if (!mxIsCell(prhs[0]) || !mxIsChar(prhs[1])) {
        mexErrMsgIdAndTxt("MATLAB:mex:invalidInputType", "Invalid input type.");
    }

    // Extract the filename
    char *filename = mxArrayToString(prhs[1]);

    // Initialize a large enough buffer for the header, considering padding
    char headerBuffer[2880]; // This might need to be dynamically sized based on actual header content
    memset(headerBuffer, ' ', sizeof(headerBuffer)); // Pre-fill with spaces for padding

    size_t currentPosition = 0;
    mxArray *cellElement;
    char *cellContent;
    for (mwIndex i = 0; i < mxGetNumberOfElements(prhs[0]); ++i) {
        cellElement = mxGetCell(prhs[0], i);
        if (cellElement && mxIsChar(cellElement)) {
            cellContent = mxArrayToString(cellElement);
            // Ensure each header card is 80 characters; pad or truncate as necessary
            // Note: Simplified for demonstration; actual implementation needs to handle formatting precisely
            snprintf(headerBuffer + currentPosition, 81, "%-80s", cellContent); 
            currentPosition += 80;
            mxFree(cellContent);
        }
    }

    // Calculate required padding to make the header a multiple of 2880 bytes
    // Note: This example assumes the initial buffer is sized correctly; actual code may need dynamic adjustment

    // Write the prepared header to the FITS file
    writeHeader(filename, headerBuffer, sizeof(headerBuffer));

    // Cleanup
    mxFree(filename);
}

