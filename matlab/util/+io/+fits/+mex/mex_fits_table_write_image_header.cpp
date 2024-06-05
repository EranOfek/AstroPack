// Write image header at the end of FITS that contains table (catalog),
// or return the filled header buffer.
//
// Cllaed from from image/@FITS/writeTable1.m
//
// Author : Chen Tishler (March 2024)
// Updated: 05/06/2024
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

//===========================================================================

const size_t cardSize           = 80;       // Each FITS header card is 80 bytes
const size_t blockSize          = 2880;     // FITS headers are allocated in blocks of 2880 bytes
const size_t maxStringLength    = 67;       // Max length for continued strings/values
const size_t maxCommentSize     = 80 - 34;  // Max length of comment in line that contains KEY = VALUE
const size_t maxCommentLineSize = 80 - 8;   // Max length of text in line that starts with COMMENT

// Keys added by our mex code, we ignore them is also specified by the caller 
// to avoid duplicates in the FITS header
const char* omitted_keys_list[] = {"NAXIS", "NAXIS1", "NAXIS2", "BITPIX", "PCOUNT", "GCOUNT", "EXTEND", 0};

// Global to be accessed from addCard
size_t _allocatedSize = 0;

//===========================================================================

inline void addCard(char* headerBuffer, size_t& bufferPos, const char* card, size_t allocatedSize = 0)
{
    // Use global if not specified by caller
    if (allocatedSize == 0)
        allocatedSize = _allocatedSize;
		
    // Make sure that we still have space in the buffer
    if (bufferPos < allocatedSize-cardSize) {
        // Copy card into buffer and move to the next card position
        strncpy((char*)headerBuffer + bufferPos, card, strlen(card)); 
        bufferPos += cardSize; 
    }
    else {
        mexPrintf("addCard: out of buffer: %s\n", card);
    }
}


// Check if a given string is in the list
bool isInList(const char* str, const char* list[]) 
{
    for (int i = 0; list[i] != 0; ++i) {
        if (strcmp(str, list[i]) == 0) {
            return true;
        }
    }
    return false; 
}
//===========================================================================

// Print formatted String/Logical/Single/Double/Int8,16,32,64 to string
void printValue(char* key, mxArray* valueElement, char* value, size_t valueSize) 
{
    if (mxIsChar(valueElement)) {
        // Add single quotes for string values
        char tempStr[2048];
        mxGetString(valueElement, tempStr, sizeof(tempStr));
       if (strlen(tempStr) > maxStringLength)
            tempStr[maxStringLength] = '\0';
        snprintf(value, valueSize, "'%s'", tempStr);
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

//===========================================================================    

// Fill FITS header buffer with all key/value/comment entries in cellArray
void fillHeaderBufferFromCellArray(char* headerBuffer, size_t& bufferPos, const mxArray* cellArray) 
{
    size_t numRows = mxGetM(cellArray); // Number of rows in the cell array

    for (size_t row = 0; row < numRows; ++row) {
        char card[cardSize + 1] = {'\0'}; 
        char key[80] = {'\0'}, value[80] = {'\0'}, comment[256] = {'\0'}; 

        // Extract key
        mxArray* keyElement = mxGetCell(cellArray, row);
        if (keyElement != nullptr && mxIsChar(keyElement)) {
            mxGetString(keyElement, key, sizeof(key));
        }

        // Get value
        mxArray* valueElement = mxGetCell(cellArray, row + numRows);
        
        // Extract comment
        mxArray* commentElement = mxGetCell(cellArray, row + 2 * numRows);
        if (commentElement != nullptr && mxIsChar(commentElement)) {
            mxGetString(commentElement, comment, sizeof(comment));
        }

        // Skip this item if its not key/value or comment
        if (keyElement == nullptr && commentElement == nullptr)
            continue;

        //mexPrintf("key: %-8s  - comment: %s\n", key, comment);
        
        // Key = Value
        if (key[0] && (valueElement != nullptr)) {

            // Skip the omitted keys, they already appear in the fixed header we write
            if (isInList(key, omitted_keys_list))
                continue;

            // Value is string, add single quotes for string values
            if (mxIsChar(valueElement)) {            
                char* tempStr = mxArrayToString(valueElement);
                size_t totalLength = strlen(tempStr);

                // Short string with comment
                if ((totalLength <= 19) & comment[0]) {
                    sprintf(value, "'%s'", tempStr);
                    comment[maxCommentSize] = 0;
                    snprintf(card, sizeof(card), "%-8.8s= %20s / %s", key, value, comment);
                    addCard(headerBuffer, bufferPos, card);
                }

                // String that fits in one line, ignore the comment
                else if (totalLength <= 67) {
                    sprintf(value, "'%s'", tempStr);
                    snprintf(card, sizeof(card), "%-8.8s= %20s", key, value);
                    addCard(headerBuffer, bufferPos, card);
                }

                // Long string in multiple lines with '&' and CONTINUE, ignore the comment
                else {
                    int numSegments = (int)((totalLength-1) / maxStringLength) + 1;
                    //mexPrintf("totalLength: %d, segments: %d\n", (int)totalLength, (int)numSegments);
                    for (int i = 0; i < numSegments; ++i) {
                        if (i < numSegments - 1) {
                            strncpy(value, tempStr + i * maxStringLength, maxStringLength);
                            value[maxStringLength] = '\0';
                            if (i == 0) {
                                snprintf(card, sizeof(card), "%-8s= '%s&'", key, value);
                            } else {
                                snprintf(card, sizeof(card), "CONTINUE '%s&'", value);
                            }
                        }
                        else {
                            strcpy(value, tempStr + i * maxStringLength);
                            snprintf(card, sizeof(card), "CONTINUE '%s'", value);
                        }
                        addCard(headerBuffer, bufferPos, card);
                    }
                }

                mxFree(tempStr);
            }

            // Non-string value with/out comment
            else {
                printValue(key, valueElement, value, sizeof(value));                
                if (comment[0]) {
                    comment[maxCommentSize] = 0;
                    snprintf(card, sizeof(card), "%-8.8s= %20s / %s", key, value, comment);
                }
                else {
                    snprintf(card, sizeof(card), "%-8.8s= %20s", key, value);
                }

                // Add the card to the buffer
                addCard(headerBuffer, bufferPos, card);                
            }
        }

        // Comment only, one or more lines
        else if (comment[0]) {
            size_t totalLength = strlen(comment);
            int numSegments = (int)((totalLength-1) / maxCommentLineSize) + 1;  
            for (int i = 0; i < numSegments; ++i) {
                strcat(card, "COMMENT ");
                strncpy(card + 8, comment + i * maxCommentLineSize, maxCommentLineSize);
                addCard(headerBuffer, bufferPos, card);
            }
        }
    }

    // Ensure to add an "END" card to mark the end of the header
    addCard(headerBuffer, bufferPos, "END                                                                             ");
}

//===========================================================================

// Main MEX function
mxArray* createFitsImageHeaderFromCellArray(const mxArray* headerArray, bool imageData, mwSize& allocatedSize)
{
    // Allocate a large enough buffer for the FITS header
    size_t bufferPos = 0;
    size_t numRows = mxGetM(headerArray); 

    // Allocate more than needed because long multi-line string and long-comments
    // may need more space than we can calculate. On writing we will write
    // only the used size (in multiples of 2880)    
    size_t numCards = 8 + 2*numRows;

    // Calculate the total size needed for the given number of cards
    size_t imageHeaderSize = numCards * cardSize;

    // Calculate the total size needed to make it a multiple of 2880 bytes
    if (imageHeaderSize % blockSize != 0) {
        imageHeaderSize = ((imageHeaderSize / blockSize) + 1) * blockSize;
    }

    // Store in global to be used by addCard()    
    allocatedSize = imageHeaderSize;
    _allocatedSize = allocatedSize;   

    // Get a pointer to the output array
    char* headerBuffer = (char*)malloc(allocatedSize);
    memset(headerBuffer, ' ', imageHeaderSize);      

    // Add required fields at the beginning of the header, with image size 1*1 because
    // actually there is no image in the file, just the header in 1x1 image
    addCard(headerBuffer, bufferPos, "XTENSION= 'IMAGE   '           / IMAGE extension");
    addCard(headerBuffer, bufferPos, "BITPIX  =                  -32 / number of bits per data pixel");
    addCard(headerBuffer, bufferPos, "NAXIS   =                    2 / number of data axes");
    addCard(headerBuffer, bufferPos, "NAXIS1  =                    1 / length of data axis 1");
    addCard(headerBuffer, bufferPos, "NAXIS2  =                    1 / length of data axis 2");
    addCard(headerBuffer, bufferPos, "PCOUNT  =                    0 / required keyword; must = 0");
    addCard(headerBuffer, bufferPos, "GCOUNT  =                    1 / required keyword; must = 1");
    addCard(headerBuffer, bufferPos, "EXTEND  =                    T / FITS dataset may contain extensions");
    addCard(headerBuffer, bufferPos, "COMMENT FITS (Flexible Image Transport System) format is defined in 'Astronomy");
    addCard(headerBuffer, bufferPos, "COMMENT and Astrophysics', volume 376, page 359; bibcode: 2001A&A...376..359H");
   
    // Append user-provided header fields
    fillHeaderBufferFromCellArray(headerBuffer, bufferPos, headerArray);

    // Calculate the actual size used
    size_t headerBytesToWrite = bufferPos;
    if (headerBytesToWrite % blockSize != 0) {
        headerBytesToWrite = ((headerBytesToWrite / blockSize) + 1) * blockSize;
    }

    // Initialize the image buffer
    size_t bytesToWrite = headerBytesToWrite;    

    // Additional block for the empty image data    
    if (imageData)
        bytesToWrite += blockSize;

    //mexPrintf("bufferPos: %d, headerBytesToWrite: %d, bytesToWrite: %d\n", (int)bufferPos, (int)headerBytesToWrite, (int)bytesToWrite);

    mxArray* outputBuffer = mxCreateCharArray(1, &bytesToWrite);
    mxChar *outputData = mxGetChars(outputBuffer);
    memcpy(outputData, headerBuffer, bufferPos);
    if (imageData)
        memset(outputData + headerBytesToWrite, 0, blockSize);

    allocatedSize = bytesToWrite;

    free(headerBuffer);
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
