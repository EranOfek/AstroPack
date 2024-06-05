// Write header and image to FITS file, simple implementation without using cfitsio!
// Author : Chen Tishler (March 2024)
// Updated: 05/06/2024
// Example: io.fits.mex.mex_fits_write_image('myfile.fits', [10 100], Header)
//
// 
// mex util/+io/+fits/+mex/mex_fits_write_image.cpp
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
const char* omitted_keys_list[] = {"NAXIS", "NAXIS1", "NAXIS2", "BITPIX", 0};

// Global to be accessed from addCard
size_t _allocatedSize = 0;

//===========================================================================

// Allocate buffer using mxMalloc for entire FITS header (one or multiple blocks)
char* allocHeaderBuffer(size_t numCards, size_t& allocatedSize, bool init=true) 
{
    // Calculate the total size needed for the given number of cards
    size_t totalSize = numCards * cardSize;

    // Calculate the total size needed to make it a multiple of 2880 bytes
    if (totalSize % blockSize != 0) {
        totalSize = ((totalSize / blockSize) + 1) * blockSize;
    }

    // Allocate the buffer
    char* buffer = (char*)mxMalloc(totalSize);

    // Initialize the buffer with spaces as per FITS standard
    if (init)
        memset(buffer, ' ', totalSize);

    // Return the buffer and the allocated size through a reference parameter
    allocatedSize = totalSize;
    return buffer;
}


inline void addCard(char* headerBuffer, size_t& bufferPos, const char* card, size_t allocatedSize = 0)
{
    // Use global if not specified by caller
    if (allocatedSize == 0)
        allocatedSize = _allocatedSize;

    // Make sure that we still have space in the buffer
    if (bufferPos < allocatedSize-cardSize) {
        // Copy card into buffer and move to the next card position
        strncpy(headerBuffer + bufferPos, card, strlen(card)); 
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

// Determine BITPIX based on the mxArray's data type
int determineBitpix(mxClassID classID) 
{
    switch (classID) {
        case mxINT16_CLASS:    
        case mxUINT16_CLASS:
            return 16;
        case mxINT32_CLASS:            
        case mxUINT32_CLASS:
            return 32;
        case mxSINGLE_CLASS:
            return -32;
        default:
            mexErrMsgIdAndTxt("MATLAB:mex_fits_write_image:unsupportedType",
                              "Unsupported matrix type for FITS file.");
            return 0; // This line is never reached
    }
}


// Write the initial part of the FITS header
void writeInitialHeader(char* headerBuffer, size_t& bufferPos, const mxArray* imgMatrix) 
{
    char card[cardSize + 1]; // +1 for null-termination

    // SIMPLE
    addCard(headerBuffer, bufferPos, "SIMPLE  =                    T / file does conform to FITS standard");

    // BITPIX
    int bitpix = determineBitpix(mxGetClassID(imgMatrix));
    snprintf(card, sizeof(card), "BITPIX  = %20d / number of bits per data pixel", bitpix);
    addCard(headerBuffer, bufferPos, card);

    // NAXIS
    const mwSize* dims = mxGetDimensions(imgMatrix);
    size_t nDims = mxGetNumberOfDimensions(imgMatrix);
    snprintf(card, sizeof(card), "NAXIS   = %20d / number of data axes", (int)nDims);
    addCard(headerBuffer, bufferPos, card);

    // NAXIS1 and NAXIS2, assuming 2D image data
    if (nDims >= 1) { // NAXIS1
        snprintf(card, sizeof(card), "NAXIS1  = %20d / length of data axis 1", (int)dims[1]);
        addCard(headerBuffer, bufferPos, card);
    }
    if (nDims >= 2) { // NAXIS2
        snprintf(card, sizeof(card), "NAXIS2  = %20d / length of data axis 2", (int)dims[0]);
        addCard(headerBuffer, bufferPos, card);
    }

    // EXTEND (optional, you can decide based on your requirements)
    addCard(headerBuffer, bufferPos, "EXTEND  =                    T / FITS dataset may contain extensions");

    addCard(headerBuffer, bufferPos, "COMMENT   FITS (Flexible Image Transport System) format is defined in 'Astronomy");
    addCard(headerBuffer, bufferPos, "COMMENT   and Astrophysics', volume 376, page 359; bibcode: 2001A&A...376..359H");
}

//=========================================================================
//                  Helper functions for swapping bytes
//=========================================================================

// Check if the system is little-endian
inline bool isSystemLittleEndian() 
{
    uint16_t testValue = 0x1;
    uint8_t* testValuePtr = reinterpret_cast<uint8_t*>(&testValue);
    return testValuePtr[0] == 0x1;
}

inline uint16_t swapBytes16(std::uint16_t val) 
{
    return (val << 8) | (val >> 8);
}

inline uint32_t swapBytes32(std::uint32_t val) 
{
    return (val << 24) | ((val << 8) & 0x00FF0000) | ((val >> 8) & 0x0000FF00) | (val >> 24);
}

inline float swapBytesFloat(float val) 
{
    std::uint32_t temp = *reinterpret_cast<std::uint32_t*>(&val);
    temp = swapBytes32(temp);
    return *reinterpret_cast<float*>(&temp); // Ensure we return a float here
}

void convertInt16BufferToEndian(uint16_t* buffer, size_t numElements) 
{
    for (size_t i = 0; i < numElements; ++i) {
        buffer[i] = swapBytes16(buffer[i]);
    }
}

void convertInt32BufferToEndian(uint32_t* buffer, size_t numElements) 
{
    for (size_t i = 0; i < numElements; ++i) {
        buffer[i] = swapBytes32(buffer[i]);
    }
}

void convertFloatBufferToEndian(float* buffer, size_t numElements) 
{
    for (size_t i = 0; i < numElements; ++i) {
        // Direct use of swapBytes(float) function as described
        buffer[i] = swapBytesFloat(buffer[i]);
    }
}

//===========================================================================

// Transpose and change endian
template<typename T, typename SwapFunction>
void* reorderMatConvert(const T* dataPtr, size_t rows, size_t cols, SwapFunction swapFunc) 
{
    size_t numElements = rows * cols;
    T* tempBuffer = static_cast<T*>(mxMalloc(numElements * sizeof(T)));

    // Reorder data from column-major to row-major and convert to big-endian
    for (size_t col = 0; col < cols; ++col) {
        for (size_t row = 0; row < rows; ++row) {
            size_t srcIndex = col * rows + row; // MATLAB's column-major index
            size_t dstIndex = row * cols + col; // Desired row-major index
            tempBuffer[dstIndex] = swapFunc(dataPtr[srcIndex]);
        }
    }
    
    return tempBuffer;
}


// Transpose - Unused (testing only)
template<typename T>
void* reorderMat(const T* dataPtr, size_t rows, size_t cols) 
{
    size_t numElements = rows * cols;
    T* tempBuffer = static_cast<T*>(mxMalloc(numElements * sizeof(T)));

    // Reorder data from column-major to row-major and convert to big-endian
    for (size_t col = 0; col < cols; ++col) {
        for (size_t row = 0; row < rows; ++row) {
            size_t srcIndex = col * rows + row; // MATLAB's column-major index
            size_t dstIndex = row * cols + col; // Desired row-major index
            tempBuffer[dstIndex] = dataPtr[srcIndex];
        }
    }
    
    return tempBuffer;
}


// Change endian - Unused (testing only)
template<typename T, typename SwapFunction>
void* Convert(const T* dataPtr, size_t rows, size_t cols, SwapFunction swapFunc) 
{
    size_t numElements = rows * cols;
    T* tempBuffer = static_cast<T*>(mxMalloc(numElements * sizeof(T)));

    // Reorder data from column-major to row-major and convert to big-endian
    for (size_t n = 0; n < numElements; ++n) {
        tempBuffer[n] = swapFunc(dataPtr[n]);        
    }
    
    return tempBuffer;
}

//===========================================================================

void writeImageData(FILE* fp, const mxArray* imgMatrix) 
{
    mxClassID classID = mxGetClassID(imgMatrix);
    const mwSize* dims = mxGetDimensions(imgMatrix);
    size_t rows = dims[0];
    size_t cols = dims[1];
    size_t elSize;
    void* dataPtr = mxGetData(imgMatrix);
    
    void* tempBuffer = nullptr;
    
    // Allocate buffer and reorder data based on the data type    
    switch (classID) {
        case mxINT16_CLASS:
        case mxUINT16_CLASS:
            elSize = 2;
            if (isSystemLittleEndian()) 
                //tempBuffer = Convert(static_cast<uint16_t*>(dataPtr), rows, cols, swapBytes16);
                tempBuffer = reorderMatConvert(static_cast<uint16_t*>(dataPtr), rows, cols, swapBytes16);
            else
                tempBuffer = reorderMat(static_cast<uint16_t*>(dataPtr), rows, cols);
            break;
        case mxINT32_CLASS:
        case mxUINT32_CLASS:
            elSize = 4;
            if (isSystemLittleEndian()) 
                tempBuffer = reorderMatConvert(static_cast<uint32_t*>(dataPtr), rows, cols, swapBytes32);
            else
                tempBuffer = reorderMat(static_cast<uint32_t*>(dataPtr), rows, cols);
            break;
        case mxSINGLE_CLASS:
            elSize = 4;
            if (isSystemLittleEndian()) 
                tempBuffer = reorderMatConvert(static_cast<float*>(dataPtr), rows, cols, swapBytesFloat);
            else
                tempBuffer = reorderMat(static_cast<float*>(dataPtr), rows, cols);
            break;
        default:
            mexErrMsgIdAndTxt("MATLAB:mex_fits_write_image:unsupportedType", "Unsupported matrix type for FITS file.");
            return;
    }

    // Write the reordered and converted data to the file
    size_t totalDataSize = elSize * rows * cols;
    fwrite(tempBuffer, 1, totalDataSize, fp);
    
    // Free the temporary buffer
    mxFree(tempBuffer);

    // Add padding to make the size a multiple of 2880 if required, as before    
    size_t paddingSize = (2880 - (totalDataSize % 2880)) % 2880;    
    if (paddingSize > 0) {
        char* paddingBuffer = new char[paddingSize];
        memset(paddingBuffer, 0, paddingSize);
        fwrite(paddingBuffer, 1, paddingSize, fp);
        delete[] paddingBuffer;
    }    
}

//===========================================================================
//
//===========================================================================

// Main MEX function
void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[]) 
{
    if (nrhs != 3) {
        mexErrMsgIdAndTxt("MATLAB:mex_fits_write_image:invalidNumInputs",
                          "Three inputs required: filename, image matrix, header cell array.");
        return;
    }
    if (!mxIsChar(prhs[0])) {
        mexErrMsgIdAndTxt("MATLAB:mex_fits_write_image:inputNotString", "First input must be a filename string.");
        return;
    }
    if (!mxIsNumeric(prhs[1])) {
        mexErrMsgIdAndTxt("MATLAB:mex_fits_write_image:inputNotNumeric", "Second input must be a numeric matrix.");
        return;
    }
    if (!mxIsCell(prhs[2])) {
        mexErrMsgIdAndTxt("MATLAB:mex_fits_write_image:inputNotCell", "Third input must be a cell array of header fields.");
        return;
    }
    
    char* filename = mxArrayToString(prhs[0]);
    const mxArray* imgMatrix = prhs[1];
    const mxArray* headerArray = prhs[2];
    
    // Open the file
    FILE* fp = fopen(filename, "wb");
    if (!fp) {
        mxFree(filename);
        mexErrMsgIdAndTxt("MATLAB:mex_fits_write_image:fileOpenFailed", "Could not open the file for writing.");
        return;
    }

    // Allocate a large enough buffer for the FITS header
    char* headerBuffer;
    size_t bufferPos = 0;
    size_t numRows = mxGetM(headerArray); 
    size_t allocatedSize;

    // Allocate more than needed because long multi-line string and long-comments
    // may need more space than we can calculate. On writing we will write
    // only the used size (in multiples of 2880)
    headerBuffer = allocHeaderBuffer(9 + 2*numRows, allocatedSize);
	_allocatedSize = allocatedSize;
  
    // Write initial part of the FITS header based on matrix type and size
    // SIMPLE, BITPIX, NAXIS, NAXIS1, NAXIS2, and EXTEND cards
    writeInitialHeader(headerBuffer, bufferPos, imgMatrix);   
    
    // Append user-provided header fields
    fillHeaderBufferFromCellArray(headerBuffer, bufferPos, headerArray);
    
    // Calculate the actual size used by data, in multiples of blockSize (2880)
    size_t bytesToWrite = bufferPos;
    if (bytesToWrite % blockSize != 0) {
        bytesToWrite = ((bytesToWrite / blockSize) + 1) * blockSize;
    }

    // Write the header buffer to the file    
    fwrite(headerBuffer, 1, bytesToWrite, fp);
    
    // Write image data with endian change if required
    writeImageData(fp, imgMatrix);
    
    // Close the file and free memory
    fclose(fp);
    mxFree(filename);
    mxFree(headerBuffer);
}
