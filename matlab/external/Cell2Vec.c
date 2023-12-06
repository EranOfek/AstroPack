// Cell2Vec.c
// CELL2VEC - Concatenate cell elements to a vector
// The elements of all elements of a cell array are concatenated to one vector.
// This equals CELL2MAT, when the cell elements are vectors, but it is up to 5
// times faster.
//
// V = Cell2Vec(C)
// INPUT:
//   C: Cell array of any size and classes:
//        DOUBLE, SINGLE, (U)INT8/16/32/64, LOGICAL, CHAR.
//      All non-empty cell elements must be the same class.
// OUTPUT:
//   V: [1 x N] vector of all elements.
//
// NOTES:
// - The larger the cell array, the higher is the speedup compared to CAT.
// - It is not likely, that the concatenation of the contents of a cell is a
//   bottleneck for the runtime of a program. But it was so easy to implement
//   this faster than with Matlab's CAT or CELL2MAT.
//
// COMPILE: ("mex -setup" on demand)
// The C-file is compiled automatically when the function is called the first
// time. But it can be compiled manually also:
//   Windows: mex -O Cell2Vec.c
//   Linux:   mex -O CFLAGS="\$CFLAGS -std=c99" Cell2Vec.c
// Precompiled MEX files can be downloaded: http://www.n-simon.de/mex
// Run the unit-test uTest_Cell2Vec after compiling.
//
// Tested: Matlab 6.5, 7.7, 7.8, 7.13, WinXP/32, Win7/64
//         Compiler: LCC2.4/3.8, BCC5.5, OWC1.8, MSVC2008/2010
// Assumed Compatibility: higher Matlab versions, Mac, Linux
// Author: Jan Simon, Heidelberg, (C) 2010-2015 matlab.2010(a)n(MINUS)simon.de
//
// See also CELL2MAT, CAT.
// FEX: CStr2String.

/*
% $JRev: R-z V:013 Sum:390LBmfvK1nA Date:16-Feb-2015 00:05:24 $
% $License: BSD (use/copy/change/redistribute on own risk, mention the author) $
% $UnitTest: uTest_Cell2Vec $
% $File: Tools\Mex\Source\Cell2Vec.c $
% History:
% 014: 03-May-2015 19:17, Improved error messages.
*/

#include "mex.h"
#include <string.h>

// Assume 32 bit array dimensions for Matlab 6.5:
// See MEX option "compatibleArrayDims" for MEX in Matlab >= 7.7.
#ifndef MWSIZE_MAX
#define mwSize  int32_T           // Defined in tmwtypes.h
#define mwIndex int32_T
#define MWSIZE_MAX MAX_int32_T
#endif

// Error messages do not contain the function name in Matlab 6.5! This is not
// necessary in Matlab 7, but it does not bother:
#define ERR_HEAD "Cell2Vec: "
#define ERR_ID   "JSimon:Cell2Vec:"
#define ERROR_2(id,msg) mexErrMsgIdAndTxt(ERR_ID id, ERR_HEAD msg);

// Prototypes:
void Core(    char *P, const mxArray *C, mwSize nC, mwSize ElementSize);
void CoreNULL(char *P, const mxArray *C, mwSize nC, mwSize ElementSize);

// Main function ===============================================================
void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[]) {
  const mxArray *C, *aC;
  mwSize SumLen = 0, Len, nC, Dims[2], ElementSize;
  mwIndex iC;
  mxClassID ClassID = mxUNKNOWN_CLASS;
  bool anyNULL = false;
   
  // Check proper number of arguments:
  if (nrhs != 1) {
     ERROR_2("BadNInput", "1 input required.");
  }
  if (nlhs > 1) {
     ERROR_2("BadNOutput", "1 output allowed.");
  }
   
  // Create a pointer to the input cell and check the type:
  C = prhs[0];
  if (!mxIsCell(C)) {
     ERROR_2("BadtypeInput1", "Input must be a cell.");
  }
   
  // Get number of dimensions of the input string and cell:
  nC = mxGetNumberOfElements(C);
  if (nC == 0) {
     plhs[0] = mxCreateDoubleMatrix(0, 0, mxREAL);
     return;
  }
   
  // Get type of first cell element:
  aC = mxGetCell(C, 0);
  if (aC != NULL) {
     ClassID     = mxGetClassID(aC);
     ElementSize = mxGetElementSize(aC);
     if (!(mxIsNumeric(aC) || mxIsChar(aC) || mxIsLogical(aC))
         || mxIsComplex(aC)) {
        ERROR_2("BadCellElement",
                "Cell elements must be numeric or char arrays.");
     }
     
  } else {  // Cell element is NULL pointer - treat it as empty double matrix:
     ClassID     = mxDOUBLE_CLASS;
     ElementSize = sizeof(double);
  }
   
  // Get sum of lenghts and check type of cell elements:
  for (iC = 0; iC < nC; iC++) {
     aC = mxGetCell(C, iC);
     if (aC != NULL) {
        Len = mxGetNumberOfElements(aC);
        if (Len != 0) {
           if (mxGetClassID(aC) != ClassID) {
              ERROR_2("BadCellElement", "Cell elements have different types.");
           }
           SumLen += Len;
        }
        
     } else {  // NULL element:
        // NULL is treated as empty double matrix as usual in Matlab. Such
        // NULLs appears when the cell is created by e.g. "cell(1,1)" without
        // populating the elements.
        anyNULL = true;
     }
  }
   
  // Create output vector and copy the data: -----------------------------------
  if (SumLen == 0) {
     if (ClassID == mxCHAR_CLASS) {
        Dims[0] = 0;
        Dims[1] = 0;
        plhs[0] = mxCreateCharArray(2, Dims);
     } else {
        plhs[0] = mxCreateNumericMatrix(0, 0, ClassID, mxREAL);
     }
  
  } else {
     if (ClassID == mxCHAR_CLASS) {
        Dims[0] = 1;
        Dims[1] = SumLen;
        plhs[0] = mxCreateCharArray(2, Dims);
     } else {
        plhs[0] = mxCreateNumericMatrix(1, SumLen, ClassID, mxREAL);
     }
     
     // Copy array elements into the vector:
     if (anyNULL) {
        CoreNULL((char *) mxGetData(plhs[0]), C, nC, ElementSize);
     } else {
        Core((char *) mxGetData(plhs[0]), C, nC, ElementSize);
     }
  }
  
  return;
}

// *****************************************************************************
void Core(char *P, const mxArray *C, mwSize nC, mwSize ElementSize)
{
  mwSize iC = 0, Len;
  const mxArray *aC;
  
  // No NULL elements - omit the test, which wastes time:
  while (iC < nC) {
     aC  = mxGetCell(C, iC++);
     Len = mxGetNumberOfElements(aC) * ElementSize;
     memcpy(P, mxGetData(aC), Len);
     P  += Len;
  }
  
  return;
}


// *****************************************************************************
void CoreNULL(char *P, const mxArray *C, mwSize nC, mwSize ElementSize)
{
  mwSize iC = 0, Len;
  const mxArray *aC;
  
  while (iC < nC) {
     aC = mxGetCell(C, iC++);
     if (aC != NULL) {
        Len = mxGetNumberOfElements(aC) * ElementSize;
        memcpy(P, mxGetData(aC), Len);
        P  += Len;
     }
  }
  
  return;
}
