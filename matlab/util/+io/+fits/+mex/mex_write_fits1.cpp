#include "mex.h"
#include <cstdio>
#include <cstring>
#include <cmath>
#include <algorithm>
//#include "matrix.h" // For mxGetClassID, etc.

const size_t card_size = 80;    // Each FITS header card is 80 bytes
const size_t blockSize = 2880;  // FITS headers are allocated in blocks of 2880 bytes

//===========================================================================

char* allocHeaderBuffer(size_t numCards, size_t& allocatedSize, bool init=false) 
{
    // Calculate the total size needed for the given number of cards
    size_t totalSize = numCards * cardSize;

    // Calculate the total size needed to make it a multiple of 2880 bytes
    if (totalSize % blockSize != 0) {
        totalSize = ((totalSize / blockSize) + 1) * blockSize;
    }

    // Allocate the buffer
    char* buffer = mxMalloc(totalSize);

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
    if (mxIsChar(valueElement)) {
        // Add single quotes for string values
        char* tempStr = mxArrayToString(valueElement);
        snprintf(value, valueSize, "'%s'", tempStr);
        mxFree(tempStr);
    } 
    else if (mxIsNumeric(valueElement)) {
        // Handling floating-point and integer types
        if (mxIsDouble(valueElement) || mxIsSingle(valueElement)) {
            double val = mxGetScalar(valueElement);
            snprintf(value, valueSize, "%g", val);
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
    else if (mxIsLogical(valueElement)) {
        // Handle logical values
        mxLogical val = *mxGetLogicals(valueElement);
        snprintf(value, valueSize, "%s", val ? "T" : "F");
    }
    else {
        // In case the value type is not supported or is unrecognized
        strncpy(value, "UNKNOWN", valueSize);
        value[valueSize - 1] = '\0'; // Ensure null-termination
    }
}


void fillHeaderBufferFromCellArray(char* headerBuffer, size_t& bufferPos, const mxArray* cellArray) 
{
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
        }

        // Construct card string
        snprintf(card, sizeof(card), "%-8s= %-20s / %s", key, value, comment);

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
        case mxUINT16_CLASS:
            return 16;
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
    snprintf(card, sizeof(card), "BITPIX  =                   %d / number of bits per data pixel", bitpix);
    addCard(headerBuffer, bufferPos, card);

    // NAXIS
    const mwSize* dims = mxGetDimensions(imgMatrix);
    size_t nDims = mxGetNumberOfDimensions(imgMatrix);
    snprintf(card, sizeof(card), "NAXIS   =                    %lu / number of data axes", nDims);
    addCard(headerBuffer, bufferPos, card);

    // NAXIS1 and NAXIS2, assuming 2D image data
    if (nDims >= 1) { // NAXIS1
        snprintf(card, sizeof(card), "NAXIS1  =           %10lu / length of data axis 1", dims[0]);
        addCard(headerBuffer, bufferPos, card);
    }
    if (nDims >= 2) { // NAXIS2
        snprintf(card, sizeof(card), "NAXIS2  =           %10lu / length of data axis 2", dims[1]);
        addCard(headerBuffer, bufferPos, card);
    }

    // EXTEND (optional, you can decide based on your requirements)
    addCard(headerBuffer, bufferPos, "EXTEND  =                    T / FITS dataset may contain extensions");
}

//===========================================================================

// Check if the system is little-endian
bool isSystemLittleEndian() 
{
    uint16_t testValue = 0x1;
    uint8_t* testValuePtr = reinterpret_cast<uint8_t*>(&testValue);
    return testValuePtr[0] == 0x1;
}


// Utility function to convert data from native endian to big endian in place
template<typename T>
void convertToBigEndian(T* data, size_t numElements) {
    uint8_t* bytePtr;
    for (size_t i = 0; i < numElements; ++i) {
        bytePtr = reinterpret_cast<uint8_t*>(data + i);
        std::reverse(bytePtr, bytePtr + sizeof(T));
    }
}


// Helper functions for swapping bytes.
inline uint16_t swapBytes(std::uint16_t val) 
{
    return (val << 8) | (val >> 8);
}

inline uint32_t swapBytes(std::uint32_t val) 
{
    return (val << 24) | ((val << 8) & 0x00FF0000) | ((val >> 8) & 0x0000FF00) | (val >> 24);
}

inline float swapBytes(float val) {
    std::uint32_t temp = *reinterpret_cast<std::uint32_t*>(&val);
    temp = swapBytes(temp);
    return *reinterpret_cast<float*>(&temp); // Ensure we return a float here
}


void convertInt16BufferToEndian(uint16_t* buffer, size_t numElements) {
    for (size_t i = 0; i < numElements; ++i) {
        buffer[i] = swapBytes(buffer[i]);
    }
}

void convertInt32BufferToEndian(uint32_t* buffer, size_t numElements) {
    for (size_t i = 0; i < numElements; ++i) {
        buffer[i] = swapBytes(buffer[i]);
    }
}

void convertFloatBufferToEndian(float* buffer, size_t numElements) {
    for (size_t i = 0; i < numElements; ++i) {
        // Direct use of swapBytes(float) function as described
        buffer[i] = swapBytes(buffer[i]);
    }
}

//===========================================================================

void writeImageData(FILE* fp, const mxArray* imgMatrix) 
{
    mxClassID classID = mxGetClassID(imgMatrix);
    void* dataPtr = mxGetData(imgMatrix);
    size_t numElements = mxGetNumberOfElements(imgMatrix);
    
    // Determine the data type and size
    size_t dataSize;
    switch (classID) {
        case mxINT16_CLASS:
            dataSize = sizeof(int16_t);
            break;
        case mxINT32_CLASS:
            dataSize = sizeof(int32_t);
            break;
        case mxSINGLE_CLASS:
            dataSize = sizeof(float);
            break;
        default:
            mexErrMsgIdAndTxt("MATLAB:writeImageData:unsupportedType", "Unsupported matrix type for FITS file.");
            return;
    }
    
    // Allocate a temporary buffer for data conversion if necessary
    void* tempBuffer = nullptr;
    if (isSystemLittleEndian()) {
        tempBuffer = mxMalloc(numElements * dataSize);
        memcpy(tempBuffer, dataPtr, numElements * dataSize);
        
        // Convert the data to big-endian format
        switch (classID) {
            case mxINT16_CLASS:
                convertInt16BufferToEndian(static_cast<int16_t*>(tempBuffer), numElements);
                break;
            case mxINT32_CLASS:
                convertInt32BufferToEndian(static_cast<int32_t*>(tempBuffer), numElements);
                break;
            case mxSINGLE_CLASS:
                convertFloatBufferToEndian(static_cast<float*>(tempBuffer), numElements);
                break;
        }
        
        // Write the converted data to the file
        fwrite(tempBuffer, dataSize, numElements, fp);
        
        // Free the temporary buffer
        mxFree(tempBuffer);
    } 
    else {
        // System is big-endian, write data directly
        fwrite(dataPtr, dataSize, numElements, fp);
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
    headerBuffer = allocHeaderBuffer(12 + numRows, allocatedSize);
  
    // Write initial part of the FITS header based on matrix type and size
    // SIMPLE, BITPIX, NAXIS, NAXIS1, NAXIS2, and EXTEND cards
    writeInitialHeader(headerBuffer, bufferPos, imgMatrix);   
    
    // Append user-provided header fields
    fillHeaderBufferFromCellArray(headerBuffer, bufferPos, headerArray);
    
    // Write the header buffer to the file
    fwrite(headerBuffer, 1, bufferPos, fp);
    
    writeImageData(fp, imgMatrix);
    
    // Close the file
    fclose(fp);
    mxFree(filename);
    mxFree(headerBuffer);
    
    mexPrintf("FITS file '%s' created successfully.\n", filename);
}
