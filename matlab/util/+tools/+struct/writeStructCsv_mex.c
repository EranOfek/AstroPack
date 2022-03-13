// Write contents of struct array to csv delimited file
// Authoer: Chen Tishler, 03/2022
//
//  Usage:
//     mex_WriteStructCsv(filename, struct_array)
//
//  Parameters:
//     filename  - full path for CSV file to export
//     struct_array -
//

#include <stdio.h>
#include "mex.h"
#include "matrix.h"

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


#define FUNCNAME "MATLAB:mex_WriteStructArray:error"

//-------------------------------------------------------------------------
// The gateway function
void mexFunction( int nlhs, mxArray *plhs[],
                  int nrhs, const mxArray *prhs[])
{
    size_t fnamelen;            // filename string buffer length
    char  *fname;               // filename string

    int    status;

    // struct array processing
    mwSize      NStructElems;
    mwIndex     jstruct;
    int         ifield, nfields;
    mxClassID*  classIDflags;
    mxArray*    fld;
    const char **fnames;       	// pointers to field names
	void* p_data;
	
    // String
    size_t str_len;             //
    size_t str_len_alloc;       //
    char  *str;                 //

    // Numerical
    double f_double;
    float f_single;
    int f_int;
    unsigned int f_uint;
    long long f_int64;
    unsigned long long f_uint64;

    // Check for proper number of arguments
    if (nrhs != 2) {
        mexErrMsgIdAndTxt(FUNCNAME, "Seven input parameters required (filename, matrix, value format, separator, write mode)");
    }

    // Validate struct
    if (!mxIsStruct(prhs[0])) {
        mexErrMsgIdAndTxt(FUNCNAME, "Struct array expected");
    }

    // Validate filename
    if (!mxIsChar(prhs[1])) {
        mexErrMsgIdAndTxt(FUNCNAME, "Filename must be a string");
    }

    // Process 1st input = struct array
    nfields = mxGetNumberOfFields(prhs[0]);
    NStructElems = mxGetNumberOfElements(prhs[0]);

    // Process 2nd input = filename
    fnamelen = mxGetN(prhs[1]) + 1;
    fname = (char*)mxCalloc(fnamelen, sizeof(char));
    status = mxGetString(prhs[1], fname, (mwSize)fnamelen);

    //
    mexPrintf("nfields: %d, NStructElems: %d\n", nfields, NStructElems);

    // Allocate memory  for storing classIDflags
    classIDflags = mxCalloc(nfields, sizeof(mxClassID));

    // Check empty field, proper data type, and data type consistency;
    // and get classID for each field
    for (jstruct = 0;  jstruct < NStructElems;  jstruct++) {
        for (ifield=0;  ifield < nfields;  ifield++) {

            // Validate that field is not empty
            fld = mxGetFieldByNumber(prhs[0], jstruct, ifield);
            if (fld == NULL) {
                mexPrintf("%s%d\t%s%d\n", "FIELD: ", ifield+1, "STRUCT INDEX:", jstruct+1);
                mexErrMsgIdAndTxt(FUNCNAME, "Field is empty!");
            }

            // First item - validate supported field types
            if (jstruct == 0) {
                if( (!mxIsChar(fld) && !mxIsNumeric(fld)) || mxIsSparse(fld)) {
                    mexPrintf("%s%d\t%s%d\n", "FIELD: ", ifield+1, "STRUCT INDEX:", jstruct+1);
                    mexErrMsgIdAndTxt(FUNCNAME, "Field must have either string or numeric non-sparse data");
                }
                classIDflags[ifield] = mxGetClassID(fld);
            }

            // Second and following items - validate that field types are the same
            else {
                if (mxGetClassID(fld) != classIDflags[ifield]) {
                    mexPrintf("%s%d\t%s%d\n", "FIELD: ", ifield+1, "STRUCT INDEX:", jstruct+1);
                    mexErrMsgIdAndTxt(FUNCNAME, "Inconsistent data type in field!");
                }
                else if (!mxIsChar(fld) && ((mxIsComplex(fld) || mxGetNumberOfElements(fld) != 1))) {
                    mexPrintf("%s%d\t%s%d\n", "FIELD: ", ifield+1, "STRUCT INDEX :", jstruct+1);
                    mexErrMsgIdAndTxt(FUNCNAME, "Numeric data in above field must be scalar and noncomplex!");
                }
            }
        }
    }

    // Allocate memory for storing pointers
    fnames = mxCalloc(nfields, sizeof(*fnames));

    // Get field name pointers
    for (ifield=0; ifield < nfields; ifield++) {
        fnames[ifield] = mxGetFieldNameByNumber(prhs[0], ifield);
    }

    // Open file
    FILE* fd = fopen(fname, "w+");
    if (fd == NULL) {
         mexErrMsgIdAndTxt(FUNCNAME, "fopen() Error!");
         return;
    }

    // Write header
    for (ifield=0; ifield < nfields; ifield++) {
        fprintf(fd, "%s", fnames[ifield]);
		if (ifield < nfields-1) {
			fprintf(fd, ",");
		}
    }
    fprintf(fd, "\n");


    // Pre-allocate string
    str_len_alloc = 256;
    str = (char*)mxCalloc(str_len, sizeof(char));

    // Write data
    for (jstruct = 0;  jstruct < NStructElems;  jstruct++) {
        for (ifield=0;  ifield < nfields;  ifield++) {

            // Validate that field is not empty
            fld = mxGetFieldByNumber(prhs[0], jstruct, ifield);
			
			// From some reason, mxGetDoubles() etc. does not work, so we use raw pointer
			p_data = mxGetData(fld);
			
            // https://www.mathworks.com/help/matlab/apiref/mxclassid.html
            // https://www.mathworks.com/help/matlab/matlab_external/c-matrix-api-typed-data-access.html
            switch (classIDflags[ifield]) {
                case mxCHAR_CLASS:
                    str_len = mxGetN(fld) + 1;
                    if (str_len > str_len_alloc) {
                        mxFree(str);
                        str_len_alloc = str_len * 2;
                        str = (char*)mxCalloc(str_len_alloc, sizeof(char));
                    }
                    status = mxGetString(fld, str, (mwSize)str_len);
                    fprintf(fd, "%s", str);
                    break;

                // single
                case mxSINGLE_CLASS:
					f_single = *((float*)p_data);
                    //f_single = (float*)*mxGetSingles(fld);
                    fprintf(fd, "%.9g", f_single);
                    break;

                // double
                case mxDOUBLE_CLASS:
					f_double = *((double*)p_data);
                    //f_double = (double)*mxGetDoubles(fld);
                    fprintf(fd, "%.17g", f_double);
                    break;

                case mxINT8_CLASS:
					f_int = *((char*)p_data);
                    //f_int = (int)*mxGetInt8s(fld);
                    fprintf(fd, "%d", f_int);
                    break;

                case mxUINT8_CLASS:
					f_uint = *((unsigned char*)p_data);
                    //f_uint = (unsigned)*mxGetUint8s(fld);
                    fprintf(fd, "%u", f_uint);
                    break;

                case mxINT16_CLASS:
					f_int = *((short*)p_data);
                    //f_int = (int)*mxGetInt16s(fld);
                    fprintf(fd, "%d", f_int);
                    break;

                case mxUINT16_CLASS:
					f_uint = *((unsigned short*)p_data);
                    //f_uint = (unsigned)*mxGetUint16s(fld);
                    fprintf(fd, "%u", f_uint);
                    break;

                case mxINT32_CLASS:
					f_int = *((int*)p_data);
                    //f_int = (int)*mxGetInt32s(fld);
                    fprintf(fd, "%d", f_int);
                    break;

                case mxUINT32_CLASS:
					f_uint = *((unsigned int*)p_data);				
                    //f_uint = (unsigned)*mxGetUint32s(fld);
                    fprintf(fd, "%u", f_uint);
                    break;

                case mxINT64_CLASS:
					f_int64 = *((long long*)p_data);
                    //f_int64 = (long long)*mxGetInt64s(fld);
                    fprintf(fd, "%lld", f_int64);
                    break;

                case mxUINT64_CLASS:
					f_uint64 = *((unsigned long long*)p_data);
                    //f_uint64 = (unsigned long long)*mxGetUint64s(fld);
                    fprintf(fd, "%llu", f_uint64);
                    break;
					
				default:
                    mexPrintf("%s%d\t%s%d\n", "FIELD: ", ifield+1, "STRUCT INDEX:", jstruct+1);
                    mexErrMsgIdAndTxt(FUNCNAME, "Unsupported data type: %d", classIDflags[ifield]);
					break;
            }
			
			if (ifield < nfields-1) {
				fprintf(fd, ",");
			}

        }

        fprintf(fd, "\n");

        // Check for Ctrl-C event
        if (utIsInterruptPending()) {
            mexPrintf("Ctrl-C Detected.\n\n");
            break;
        }
    }

    // Close file and free memory
    fclose(fd);
    mxFree(fname);
    mxFree(classIDflags);
    mxFree(str);
}
//-------------------------------------------------------------------------

