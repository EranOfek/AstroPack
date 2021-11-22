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


#define FUNCNAME "MATLAB:mex_WriteMatrix2:error"

//-------------------------------------------------------------------------
// Write to file routine
void writemat2(double* z, size_t cols, size_t rows, char* fname, char* fmt, char* dlm, char* wmode,
        char* header, const mxArray* struct_array)
{

	int i, j, ifield;
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

	// Open file
	FILE* fd = fopen(fname, wmode);
	if (fd == NULL) {
         mexErrMsgIdAndTxt(FUNCNAME, "fopen() Error!");
         exit;
	}

	// Write header
	if (header[0]) {
		fprintf(fd, "%s\n", header);
	}

	fldval = (char*)mxCalloc(1024, sizeof(char));

	//
	for (i=0;  i < rows;  i++) {

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

//-------------------------------------------------------------------------
// The gateway function
void mexFunction( int nlhs, mxArray *plhs[],
                  int nrhs, const mxArray *prhs[])
{
    double *inMatrix;           // MxN input matrix

    size_t rows;                // M = number of rows of matrix
    size_t cols;                // N = number of cols of matrix

    size_t fnamelen;            // filename string buffer length
    char  *fname;               // filename string

    size_t fmtlen;   			// value format string buffer length
    char  *fmt;                 // value format string

    size_t dlmlen;   			// delimiter string buffer length
    char  *dlm;                 // value format string

    size_t wmodelen;   			// write mode string buffer length
    char  *wmode;               // value format string

    size_t headerlen;   		// header string buffer length
    char  *header;              // header

    int    status;


    // Check for proper number of arguments
    if (nrhs != 7) {
        mexErrMsgIdAndTxt(FUNCNAME, "Seven input parameters required (filename, matrix, value format, separator, write mode)");
    }

    if (!mxIsChar(prhs[0])) {
        mexErrMsgIdAndTxt(FUNCNAME, "Filename must be a string");
    }

    if (!mxIsDouble(prhs[1]) || mxIsComplex(prhs[1])) {
        mexErrMsgIdAndTxt(FUNCNAME, "Input matrix Z must be type double");
    }

    if (!mxIsChar(prhs[2])) {
        mexErrMsgIdAndTxt(FUNCNAME,"Value format must be a string (e.g. %%10.6f)");
    }

    if (!mxIsChar(prhs[3])) {
        mexErrMsgIdAndTxt(FUNCNAME, "Separator must be a char type");
    }

    if (!mxIsChar(prhs[4])) {
        mexErrMsgIdAndTxt(FUNCNAME, "Write mode must be a char (e.g. w+ or a+)");
    }

    if (!mxIsChar(prhs[5])) {
        mexErrMsgIdAndTxt(FUNCNAME, "Header mode must be a char");
    }

    if (!mxIsStruct(prhs[6])) {
		mexErrMsgIdAndTxt(FUNCNAME, "Struct array expected");
	}

    // Process first input = filename
    fnamelen = mxGetN(prhs[0]) + 1;
    fname = (char*)mxCalloc(fnamelen, sizeof(char));
    status = mxGetString(prhs[0], fname, (mwSize)fnamelen);

    // Process second input = matrix
    inMatrix = mxGetPr(prhs[1]);
    rows = mxGetN(prhs[1]);
    cols = mxGetM(prhs[1]);

    // Process 3rd input = value format
    fmtlen = mxGetN(prhs[2]) + 1;
    fmt = (char*)mxCalloc(fmtlen, sizeof(char));
    status = mxGetString(prhs[2], fmt, (mwSize)fmtlen);

    // Process 4th input = delimiter
    dlmlen = mxGetN(prhs[3]) + 1;
    dlm = (char*)mxCalloc(dlmlen, sizeof(char));
    status = mxGetString(prhs[3], dlm, (mwSize)dlmlen);

    // Process 5th input = write mode
    wmodelen = mxGetN(prhs[4]) + 1;
    wmode = (char*)mxCalloc(wmodelen, sizeof(char));
    status = mxGetString(prhs[4], wmode, (mwSize)wmodelen);

    // Process 6th input = header
    headerlen = mxGetN(prhs[5]) + 1;
    header = (char*)mxCalloc(headerlen, sizeof(char));
    status = mxGetString(prhs[5], header, (mwSize)headerlen);

    // Call the actual function
    writemat2(inMatrix, rows, cols, fname, fmt, dlm, wmode, header, prhs[6]);

    // Free strings
    mxFree(fname);
    mxFree(fmt);
    mxFree(dlm);
    mxFree(wmode);
	mxFree(header);
}
//-------------------------------------------------------------------------

