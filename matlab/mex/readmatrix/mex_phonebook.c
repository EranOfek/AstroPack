/* ==========================================================================
 * phonebook.c 
 * example for illustrating how to manipulate structure and cell array
 *
 * takes a (MxN) structure matrix and returns a new structure (1x1)
 * containing corresponding fields: for string input, it will be (MxN)
 * cell array; and for numeric (noncomplex, scalar) input, it will be (MxN)
 * vector of numbers with the same classID as input, such as int, double
 * etc..
 *
 * This is a MEX-file for MATLAB.
 * Copyright 1984-2011 The MathWorks, Inc.
 *==========================================================================*/

#include "mex.h"
#include "string.h"


#define MAXCHARS 80   // max length of string contained in each field


// the gateway routine
void mexFunction( int nlhs, mxArray *plhs[],
                  int nrhs, const mxArray *prhs[] )
{
    const char **fnames;       /* pointers to field names */
    const mwSize *dims;
    mxArray    *tmp, *fout;
    char       *pdata = NULL;
    int        ifield, nfields;
    mxClassID  *classIDflags;
    mwIndex    jstruct;
    mwSize     NStructElems;
    mwSize     ndim;
    
    // Check proper input and output
    if (nrhs != 1)
        mexErrMsgIdAndTxt( "MATLAB:phonebook:invalidNumInputs", "One input required.");
				
    else if (nlhs > 1)
        mexErrMsgIdAndTxt( "MATLAB:phonebook:maxlhs", "Too many output arguments.");
				
    else if (!mxIsStruct(prhs[0]))
        mexErrMsgIdAndTxt( "MATLAB:phonebook:inputNotStruct", "Input must be a structure.");
    
    // Get input arguments
    nfields = mxGetNumberOfFields(prhs[0]);
    NStructElems = mxGetNumberOfElements(prhs[0]);
	
	mexPrintf("nfields: %d, NStructElems: %d\n", nfields, NStructElems);
	
    // Allocate memory  for storing classIDflags
    classIDflags = mxCalloc(nfields, sizeof(mxClassID));
    
    // Check empty field, proper data type, and data type consistency;
    // and get classID for each field
    for (ifield=0;  ifield < nfields;  ifield++) {
		
        for (jstruct = 0;  jstruct < NStructElems;  jstruct++) {
			
            tmp = mxGetFieldByNumber(prhs[0], jstruct, ifield);
			
            if (tmp == NULL) {
                mexPrintf("%s%d\t%s%d\n", "FIELD: ", ifield+1, "STRUCT INDEX :", jstruct+1);
                mexErrMsgIdAndTxt("MATLAB:phonebook:fieldEmpty", "Above field is empty!");
            }
			
            if (jstruct==0) {
                if( (!mxIsChar(tmp) && !mxIsNumeric(tmp)) || mxIsSparse(tmp)) {
                    mexPrintf("%s%d\t%s%d\n", "FIELD: ", ifield+1, "STRUCT INDEX :", jstruct+1);
                    mexErrMsgIdAndTxt( "MATLAB:phonebook:invalidField",
                            "Above field must have either string or numeric non-sparse data.");
                }
                classIDflags[ifield]=mxGetClassID(tmp);
            } 
			
			else {
                if (mxGetClassID(tmp) != classIDflags[ifield]) {
                    mexPrintf("%s%d\t%s%d\n", "FIELD: ", ifield+1, "STRUCT INDEX :", jstruct+1);
                    mexErrMsgIdAndTxt( "MATLAB:phonebook:invalidFieldType",
                            "Inconsistent data type in above field!");
                } 
				else if (!mxIsChar(tmp) &&
                        ((mxIsComplex(tmp) || mxGetNumberOfElements(tmp)!=1))){
                    mexPrintf("%s%d\t%s%d\n", "FIELD: ", ifield+1, "STRUCT INDEX :", jstruct+1);
                    mexErrMsgIdAndTxt( "MATLAB:phonebook:fieldNotRealScalar",
                            "Numeric data in above field must be scalar and noncomplex!");
                }
            }
        }
    }
    
    // Allocate memory for storing pointers
    fnames = mxCalloc(nfields, sizeof(*fnames));
    
	// Get field name pointers
    for (ifield=0; ifield< nfields; ifield++){
        fnames[ifield] = mxGetFieldNameByNumber(prhs[0],ifield);
    }
	
    // Create a 1x1 struct matrix for output
    plhs[0] = mxCreateStructMatrix(1, 1, nfields, fnames);
    mxFree((void *)fnames);
    ndim = mxGetNumberOfDimensions(prhs[0]);
    dims = mxGetDimensions(prhs[0]);
	
    for (ifield=0;  ifield < nfields;  ifield++) {
        
		// Create cell/numeric array
        if (classIDflags[ifield] == mxCHAR_CLASS) {
            fout = mxCreateCellArray(ndim, dims);
        }
		else {
            fout = mxCreateNumericArray(ndim, dims, classIDflags[ifield], mxREAL);
            pdata = mxGetData(fout);
        }
		
        // Copy data from input structure array
        for (jstruct=0;  jstruct < NStructElems;  jstruct++) {
            tmp = mxGetFieldByNumber(prhs[0],jstruct,ifield);
            if( mxIsChar(tmp)) {
                mxSetCell(fout, jstruct, mxDuplicateArray(tmp));
            }
			else {
                mwSize     sizebuf;
                sizebuf = mxGetElementSize(tmp);
                memcpy(pdata, mxGetData(tmp), sizebuf);
                pdata += sizebuf;
            }
        }
		
        // Set each field in output structure
        mxSetFieldByNumber(plhs[0], 0, ifield, fout);
    }
	
    mxFree(classIDflags);
    return;
}
