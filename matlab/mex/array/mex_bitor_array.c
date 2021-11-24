// Improved version of mex_WriteMatrix.c
// Write contents of matrix Z to csv delimited file
//
//  Usage:
//     mex_WriteMatrix(filename, matrix, format, delimiter, writemode, header, struct_array);
//
//  Parameters:
//     filename  - full path for CSV file to export
//     matrix    - matrix of type 'double' values to be exported
//     format    - format of export (sprintf) , e.g. '%10.6f'
//     delimiter - delimiter, for example can be ',' or ';'
//     writemode - write mode 'w+' for rewriting file 'a+' for appending
//     header    -
//     struct_array -
//

#include "mex.h"
#include "matrix.h"
#define _WIN32

// For Ctrl-C detection http://www.caam.rice.edu/~wy1/links/mex_ctrl_c_trick/
#if defined (_WIN32)
    #include <windows.h>
#elif defined (__linux__)
    #include <unistd.h>
#endif

#ifdef __cplusplus
    extern "C" bool utIsInterruptPending();
#else
    extern bool utIsInterruptPending();
#endif


#define FUNCNAME "MATLAB:mex_bitor_array:error"

#ifdef never
//-------------------------------------------------------------------------
// #define uint32_t unsigned int
    
// Write to file routine
void bitor_array32(uint32_t* mat, size_t cols, size_t rows, char* fname, char* fmt, char* dlm, char* wmode,
        char* header, const mxArray* struct_array)
{    
	int row, col, result;
	mxArray* field;
	int nfields;
	size_t NStructElems;
	int status;
	size_t fldlen;
	char* fldval;

	nfields = mxGetNumberOfFields(struct_array);
	NStructElems = mxGetNumberOfElements(struct_array);

	mexPrintf("nfields: %d, NStructElems: %d\n", nfields, NStructElems);
	mexPrintf("rows: %d, cols: %d\n", rows, cols);
	
	//
	if (NStructElems != rows) {
	   mexErrMsgIdAndTxt(FUNCNAME, "Struct array items must be same as matrix rows");
	}

	//
    for (col=0;  col < cols;  col++) {    
        result = 0;
        for (row=0;  row < rows;  row++) {
            result |= 
        


		for (ifield=0;  ifield < nfields;  ifield++) {

            field = mxGetFieldByNumber(struct_array, i, ifield);

			if (!mxIsChar(field)) { // && !mxIsNumeric(field)) || mxIsSparse(field)) {
				//mexPrintf("%s%d\t%s%d\n", "FIELD: ", ifield+1, "STRUCT INDEX :", jstruct+1);
				mexErrMsgIdAndTxt(FUNCNAME, "Above field must have either string or numeric non-sparse data");
			}

			//const char *field_name = mxGetFieldNameByNumber(const mxArray *pm, int fieldnumber);

			fldlen = mxGetN(field) + 1;
			fldval = (char*)mxCalloc(fldlen, sizeof(char));
			status = mxGetString(field, fldval, (mwSize)fldlen);

			fprintf(fd, "%s", fldval);
			fprintf(fd, dlm);				
		}

	    // Matrix
		for (j=0;  j < cols;  j++) {
			fprintf(fd, fmt, z[i + j*rows]);
			if (j < cols-1) {
				fprintf(fd, dlm);
			}
		}
		fprintf(fd, "\n");

		// Check for Ctrl-C event
		if (utIsInterruptPending()) {
			mexPrintf("Ctrl-C Detected.\n\n");
			fclose(fd);
			mxFree(fldval);
			return;
		}
	}

   // Close file
   fclose(fd);
   mxFree(fldval);

}
#endif
    
    
//-------------------------------------------------------------------------
// The gateway function
//
// mexFunction is not a routine you call. Rather, mexFunction is the name 
// of the gateway function in C which every MEX function requires. 
// When you invoke a MEX function, MATLAB finds and loads the corresponding 
// MEX function of the same name. MATLAB then searches for a symbol named 
// mexFunction within the MEX function. If it finds one, it calls the MEX 
// function using the address of the mexFunction symbol. MATLAB displays 
// an error message if it cannot find a routine named mexFunction inside 
// the MEX function.
//
// Input Arguments    
//
//    int nlhs              - Number of output arguments
//    mxArray* plhs[]       - Output arguments
//    int nrhs              - Number of input arguments
//    const mxArray* prhs[] - Input arguments
// 
        
        
// 
// mxIsClass(pm, "double");
// 
// is equivalent to calling either of these forms:
// 
// mxIsDouble(pm);
// strcmp(mxGetClassName(pm), "double")==0;
//
    
void mexFunction( 
        int nlhs, mxArray *plhs[],          // Output arguments
        int nrhs, const mxArray *prhs[])    // Input arguments
{
    // Input
    int* inputMatrix;
    mwSize inputDimsNum;  
    const mwSize* inputDims;    
    const char *inputType;
    int argDim = 1;
    
    // Output
    mwSize outputDimsNum;        
    mwSize outputDims[8];
    
    // Need at least one argument
    if (nrhs < 1) {
        return;
    }

    // Get dim
    if ((nrhs > 1) && !mxIsScalar(prhs[1])
        argDim = (int)mxGetScalar(prhs[0]);
    }
    
    inputDimsNum = mxGetNumberOfDimensions(prhs[0]);
    inputDims = mxGetDimensions(prhs[0]);    
   
    inputType = mxGetClassName(prhs[0]);
    
    mexPrintf("inMatrixDimsNum: %d\n", inputDimsNum);  
    mexPrintf("inMatrixType: %s\n", inputType);
    mexPrintf("Dim: %d\n", argDim);  
        
    outputDimsNum = inputDimsNum - 1;
    if (outputDimsNum < 1) {
        outputDimsNum = 1;
        outputDims = inputDims;
    }
    else {
        outputDims = inputDims + 1;
    }

    
    // Create output matrix
    plhs[0] = mxCreateNumericArray(outputDimsNum, outputDims, mxUINT32_CLASS, mxREAL);

}
