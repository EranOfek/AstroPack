// Author : Chen Tishler (March 2024)
// Example: io.fits.mex.fastWriteFITS('myfile.fits', [10 100], Header)

#include "mex.h"
#include <cstdio>
#include <cstring>
#include <cmath>
#include <algorithm>
#include <cstdint>


const size_t cardSize = 80;     // Each FITS header card is 80 bytes
const size_t blockSize = 2880;  // FITS headers are allocated in blocks of 2880 bytes

//===========================================================================

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


inline void addCard(char* headerBuffer, size_t& bufferPos, const char* card)
{
    // Copy card into buffer
    strncpy(headerBuffer + bufferPos, card, strlen(card)); 

    // Move to the next card position
    bufferPos += cardSize; 
}

//===========================================================================

void printValue(mxArray* valueElement, char* value, size_t valueSize) 
{
    const size_t maxStringLength = 67; // Max length for continued strings/values

    if (mxIsChar(valueElement)) {
        // Add single quotes for string values
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
            snprintf(value, valueSize, "%lld", val);
        }
    } 
    else {
        // In case the value type is not supported or is unrecognized
        strncpy(value, "UNKNOWN", valueSize);
        value[valueSize - 1] = '\0'; // Ensure null-termination
    }
}


void fillHeaderBufferFromCellArray(char* headerBuffer, size_t& bufferPos, const mxArray* cellArray) 
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
        if (valueElement != nullptr) {
            printValue(valueElement, value, sizeof(value));
        }

        // Extract Comment
        mxArray* commentElement = mxGetCell(cellArray, row + 2 * numRows);
        if (commentElement != nullptr && mxIsChar(commentElement)) {
            mxGetString(commentElement, comment, sizeof(comment));

            // Construct card string
            snprintf(card, sizeof(card), "%-8s= %20s / %s", key, value, comment);            
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
            snprintf(card, sizeof(card), "%-8s= %20s", key, value);
        }

        // Add the card to the buffer
        addCard(headerBuffer, bufferPos, card);
    }

    // Ensure to add an "END" card to mark the end of the header
    addCard(headerBuffer, bufferPos, "END                                                                             ");
}

//===========================================================================

// Function to determine BITPIX based on the mxArray's data type
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
            mexErrMsgIdAndTxt("MATLAB:createFitsFile:unsupportedType",
                              "Unsupported matrix type for FITS file.");
            return 0; // This line is never reached
    }
}


// Function to write the initial part of the FITS header
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

//===========================================================================

// Check if the system is little-endian
bool isSystemLittleEndian() 
{
    uint16_t testValue = 0x1;
    uint8_t* testValuePtr = reinterpret_cast<uint8_t*>(&testValue);
    return testValuePtr[0] == 0x1;
}

// Helper functions for swapping bytes
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
            mexErrMsgIdAndTxt("MATLAB:writeImageData:unsupportedType", "Unsupported matrix type for FITS file.");
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
        mexErrMsgIdAndTxt("MATLAB:createFitsFile:invalidNumInputs",
                          "Three inputs required: filename, image matrix, header cell array.");
        return;
    }
    if (!mxIsChar(prhs[0])) {
        mexErrMsgIdAndTxt("MATLAB:createFitsFile:inputNotString", "First input must be a filename string.");
        return;
    }
    if (!mxIsNumeric(prhs[1])) {
        mexErrMsgIdAndTxt("MATLAB:createFitsFile:inputNotNumeric", "Second input must be a numeric matrix.");
        return;
    }
    if (!mxIsCell(prhs[2])) {
        mexErrMsgIdAndTxt("MATLAB:createFitsFile:inputNotCell", "Third input must be a cell array of header fields.");
        return;
    }
    
    char* filename = mxArrayToString(prhs[0]);
    const mxArray* imgMatrix = prhs[1];
    const mxArray* headerArray = prhs[2];
    
    // Open the file
    FILE* fp = fopen(filename, "wb");
    if (!fp) {
        mxFree(filename);
        mexErrMsgIdAndTxt("MATLAB:createFitsFile:fileOpenFailed", "Could not open the file for writing.");
        return;
    }
    
    // Allocate a large enough buffer for the FITS header
    char* headerBuffer;
    size_t allocatedSize;
    size_t bufferPos = 0;
    size_t numRows = mxGetM(headerArray); 
    headerBuffer = allocHeaderBuffer(9 + numRows, allocatedSize);
  
    // Write initial part of the FITS header based on matrix type and size
    // SIMPLE, BITPIX, NAXIS, NAXIS1, NAXIS2, and EXTEND cards
    writeInitialHeader(headerBuffer, bufferPos, imgMatrix);   
    
    // Append user-provided header fields
    fillHeaderBufferFromCellArray(headerBuffer, bufferPos, headerArray);
    
    // Write the header buffer to the file
    fwrite(headerBuffer, 1, allocatedSize, fp);
    
    writeImageData(fp, imgMatrix);
    
    // Close the file
    fclose(fp);
    mxFree(filename);
    mxFree(headerBuffer);
    
    //mexPrintf("FITS file '%s' created successfully.\n", filename);
}
