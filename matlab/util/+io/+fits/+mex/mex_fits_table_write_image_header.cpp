//
// Author : Chen Tishler (March 2024)
//
// Example: io.fits.mex.mex_fits_table_write_image_header(Header, 'myfile.fits')
// Example: headerBuffer = io.fits.mex.mex_fits_table_write_image_header(Header)
//
// mex util/+io/+fits/+mex/mex_fits_table_write_image_header.cpp
//

#include "mex.h"
#include <cstdio>
#include <cstring>
#include <cmath>
#include <algorithm>
#include <cstdint>

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wformat-truncation"
#pragma GCC diagnostic ignored "-Wformat-extra-args"

const size_t cardSize = 80;     // Each FITS header card is 80 bytes
const size_t blockSize = 2880;  // FITS headers are allocated in blocks of 2880 bytes


inline void addCard(mxChar* headerBuffer, size_t& bufferPos, const char* card)
{
    // Copy card into buffer
    strncpy((char*)headerBuffer + bufferPos, card, strlen(card)); 

    // Move to the next card position
    bufferPos += cardSize; 
}


void printValue(mxArray* valueElement, char* value, size_t valueSize, bool& isChar) 
{
    const size_t maxStringLength = 67; // Max length for continued strings/values

    isChar = false;
    if (mxIsChar(valueElement)) {
        // Add single quotes for string values
        isChar = true;
        char* tempStr = mxArrayToString(valueElement);
        if (strlen(tempStr) > maxStringLength)
            tempStr[maxStringLength] = '\0';
        snprintf(value, valueSize, "'%s'", tempStr);
        mxFree(tempStr);
    } 
    else if (mxIsLogical(valueElement)) {
        // Handle logical values
        mxLogical val = *mxGetLogicals(valueElement);
        snprintf(value, valueSize, "%s", val ? "T" : "F");
    }    
    else if (mxIsNumeric(valueElement)) {
        // Handling floating-point and integer types
        if (mxIsSingle(valueElement)) {
            float val = (float)mxGetScalar(valueElement);
            if (floorf(val) == val) 
                snprintf(value, valueSize, "%.0f.", val);
            else
                snprintf(value, valueSize, "%.7g", val);
        } 
        else if (mxIsDouble(valueElement)) {            
            double val = mxGetScalar(valueElement);
            if (floor(val) == val) 
                snprintf(value, valueSize, "%.0f.", val);
             else
                 snprintf(value, valueSize, "%.15g", val);
        }         
        else if (mxIsClass(valueElement, "int8") || mxIsClass(valueElement, "uint8") ||
                 mxIsClass(valueElement, "int16") || mxIsClass(valueElement, "uint16") ||
                 mxIsClass(valueElement, "int32") || mxIsClass(valueElement, "uint32") ||
                 mxIsClass(valueElement, "int64") || mxIsClass(valueElement, "uint64")) {
            // Cast to the largest possible integer and print. 
            // This simplistic approach may need refinement for exact data type representation.
            int64_t val = static_cast<int64_t>(mxGetScalar(valueElement));
            snprintf(value, valueSize, "%lld", (long long int)val);
        }
    } 
    else {
        // In case the value type is not supported or is unrecognized
        strncpy(value, "UNKNOWN", valueSize);
        value[valueSize - 1] = '\0'; // Ensure null-termination
    }
}


void fillHeaderBufferFromCellArray(mxChar* headerBuffer, size_t& bufferPos, const mxArray* cellArray) 
{
    const size_t maxStringLength = 67; // Max length for continued strings/values
    size_t numRows = mxGetM(cellArray); // Number of rows in the cell array

    for (size_t row = 0; row < numRows; ++row) {
        char card[cardSize + 1] = {'\0'}; // Initialize card buffer, +1 for null-termination
        char key[80], value[80], comment[256] = {'\0'}; // Initialize comment with null-termination

        // Extract Key
        mxArray* keyElement = mxGetCell(cellArray, row);
        if (keyElement != nullptr && mxIsChar(keyElement)) {
            mxGetString(keyElement, key, sizeof(key));
        }

        // Extract and Format Value using printValue
        mxArray* valueElement = mxGetCell(cellArray, row + numRows);
        bool isChar = false;
        if (valueElement != nullptr) {
            printValue(valueElement, value, sizeof(value), isChar);
        }

        // Extract Comment
        mxArray* commentElement = mxGetCell(cellArray, row + 2 * numRows);
        if (commentElement != nullptr && mxIsChar(commentElement)) {
            mxGetString(commentElement, comment, sizeof(comment));

            size_t maxCommentSize = sizeof(card) - 34; // Adjust based on key, value, and fixed characters
            snprintf(card, sizeof(card), "%-8.8s= %20.20s / %.*s", key, value, (int)maxCommentSize, comment);

            // Construct card string
            //snprintf(card, sizeof(card), "%-8s= %20s / %s", key, value, comment);            
        }

        // Construct card string
        else {
            #ifdef NEVER
            size_t totalLength = strlen(value);
            size_t numSegments = (size_t)ceil(static_cast<double>(totalLength) / maxStringLength);
            char tempValue[80];

            mexPrintf("long string: %d - %s\n", totalLength, value);
            mexPrintf("numSegments: %d\n", (int)numSegments);

            for (size_t i = 0; i < numSegments; ++i) {
                // Copy a segment of the string
                if (i < numSegments - 1) { // Not the last segment
                    strncpy(tempValue, value + i * maxStringLength, maxStringLength);
                    tempValue[maxStringLength] = '\0'; // Null-terminate
                    if (i == 0) {
                        // First segment with key or COMMENT
                        snprintf(card, sizeof(card), "%-8s= '%s&'", key, tempValue);
                    } else {
                        // Subsequent segments with CONTINUE
                        snprintf(card, sizeof(card), "CONTINUE  '%s&'", tempValue);
                    }
                } else { // Last segment
                    strcpy(tempValue, value + i * maxStringLength); // Copy the rest of the string
                    // Handle last segment differently, don't add '&'
                    snprintf(card, sizeof(card), "CONTINUE  '%s'", tempValue);
                }
                addCard(headerBuffer, bufferPos, card);
            }
            #endif

            snprintf(card, sizeof(card), "%-8.8s= %20s", key, value);
        }

        // Add the card to the buffer
        addCard(headerBuffer, bufferPos, card);
    }

    // Ensure to add an "END" card to mark the end of the header
    addCard(headerBuffer, bufferPos, "END                                                                             ");
}


// Main MEX function
mxArray* createFitsImageHeaderFromCellArray(const mxArray* headerArray, bool imageData, mwSize& allocatedSize)
{
    // Allocate a large enough buffer for the FITS header
    size_t bufferPos = 0;
    size_t numRows = mxGetM(headerArray); 
    size_t numCards = 8 + numRows;

    // Calculate the total size needed for the given number of cards
    size_t imageHeaderSize = numCards * cardSize;

    // Calculate the total size needed to make it a multiple of 2880 bytes
    if (imageHeaderSize % blockSize != 0) {
        imageHeaderSize = ((imageHeaderSize / blockSize) + 1) * blockSize;
    }

    // Additional block for the empty image data
    allocatedSize = imageHeaderSize;
    if (imageData)
        allocatedSize += blockSize;

    mxArray* outputBuffer = mxCreateCharArray(1, &allocatedSize);
    
    // Get a pointer to the output array
    mxChar *headerBuffer = mxGetChars(outputBuffer);

    // Initialize the buffer with spaces as per FITS standard    
    memset(headerBuffer, ' ', imageHeaderSize);

    // Initialize the image buffer
    if (imageData)
        memset(headerBuffer + imageHeaderSize, 0, blockSize);

    // Add required fields
    addCard(headerBuffer, bufferPos, "XTENSION= 'IMAGE   '           / IMAGE extension");
    addCard(headerBuffer, bufferPos, "BITPIX  =                  -32 / number of bits per data pixel");
    addCard(headerBuffer, bufferPos, "NAXIS   =                    2 / number of data axes");
    addCard(headerBuffer, bufferPos, "NAXIS1  =                    1 / length of data axis 1");
    addCard(headerBuffer, bufferPos, "NAXIS2  =                    1 / length of data axis 2");
    addCard(headerBuffer, bufferPos, "PCOUNT  =                    0 / required keyword; must = 0");
    addCard(headerBuffer, bufferPos, "GCOUNT  =                    1 / required keyword; must = 1");
    //addCard(headerBuffer, bufferPos, "EXTEND  =                    T / FITS dataset may contain extensions");
    //addCard(headerBuffer, bufferPos, "COMMENT FITS (Flexible Image Transport System) format is defined in 'Astronomy");
    //addCard(headerBuffer, bufferPos, "COMMENT and Astrophysics', volume 376, page 359; bibcode: 2001A&A...376..359H");
   
    // Append user-provided header fields
    fillHeaderBufferFromCellArray(headerBuffer, bufferPos, headerArray);

    return outputBuffer;
}

//===========================================================================

// Main MEX function
void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[]) 
{
    if (nrhs < 1) {
        mexErrMsgIdAndTxt("MATLAB:mex_fits_table_write_image_header:invalidNumInputs",
                          "One or two input required: header cell array [file name].");
        return;
    }
    if (!mxIsCell(prhs[0])) {
        mexErrMsgIdAndTxt("MATLAB:mex_fits_table_write_image_header:inputNotCell", "First input must be a cell array of header fields.");
        return;
    }
    
    char* filename = NULL;
    if (nrhs > 1) {
        if (!mxIsChar(prhs[1])) {
            mexErrMsgIdAndTxt("MATLAB:mex_fits_table_write_image_header:inputNotString", "Second input must be a filename string.");
            return;
        }
        filename = mxArrayToString(prhs[1]);
    }
    else {
        if (nlhs != 1) {
            mexErrMsgIdAndTxt("MATLAB:mex_fits_table_write_image_header:outputRequired", "Second input must be a filename string.");
            return;
        }
    }

    // Create buffer with image
    const mxArray* headerArray = prhs[0];
    mwSize allocatedSize;
    mxArray* outputBuffer = createFitsImageHeaderFromCellArray(headerArray, true, allocatedSize);
    //mexPrintf("allocatedSize: %lld\n", allocatedSize);

    // Filename is specified, append header at the end of the file (file must be closed)
    if (filename) {

        // mexPrintf("fopen: %s\n", filename);
        FILE* fp = fopen(filename, "ab");
        if (!fp) {
            mxFree(filename);
            mxFree(outputBuffer);
            mexErrMsgIdAndTxt("MATLAB:mex_fits_table_write_image_header:fileOpenFailed", "Could not open the file for writing (append).");
            return;
        }
	        
        // Write the header buffer to the file
        void* dataPtr = mxGetChars(outputBuffer);        
        fwrite(dataPtr, 1, allocatedSize, fp);
        fclose(fp);
        
        // Free allocated buffer and filename
        mxDestroyArray(outputBuffer);
        mxFree(filename);
    }

    // Return the allocated buffer, caller will do whatever she likes with it
    else {
        plhs[0] = outputBuffer;
    }
}
