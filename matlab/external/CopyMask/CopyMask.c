// CopyMask.c
// CopyMask - Logical indexing
// Y = CopyMask(X, Mask, Dim)
// INPUT:
//   X:    Array of type: DOUBLE, SINGLE, (U)INT8/16/32/64), LOGICAL, CHAR.
//         X can be complex.
//   Mask: Mask as LOGICAL vector.
//   Dim:  Specify dimension for masking. If omitted or the empty matrix []
//         linear indexing is applied. Optional, default: [].
// OUTPUT:
//   Y:    Array of same type as X. For linear indexing Y is a [N x 1] vector.
//
// NOTES:
// - Equivalent Matlab code: Y = X(Mask)
// - Difference to Matlab's logical indexing:
//   * 2 to 3 times faster.
//   * A column vector is replied in every case.
//   * Mask cannot be longer than the array, while Matlab allows additional
//     trailing values, when they are FALSE.
// - If Dim is specified and X is not small, this function is only some percent
//   faster than the equivalent Matlab code. See output of the unit test.
//
// EXAMPLES:
//   X = rand(2,3,4);
//   Y = CopyMask(X, X > 0.2);    % Matlab: X(X > 0.2)
//   M = [true, false, true];
//   Z = CopyMask(X, M, 2);       % Matlab: X(:, M, :)
//
// COMPILATION:
// - If not done before:  mex -setup
// - The Mex file is compliled automatically when the M-file is called the first
//   time.
// - The file can be compiled manually also:
//     mex -O CopyMask.c
//   Linux: consider C99 comments:
//     mex -O CFLAGS="\$CFLAGS -std=c99" Cell2Vec.c
// - Pre-compiled files can be downloaded: http://www.n-simon.de/mex
// - Run the unit-test uTest_CopyMask after compiling manually.
//
// Tested: Matlab 6.5, 7.7, 7.8, 7.13, WinXP/32, Win7/64
//         Compiler: LCC2.4/3.8, BCC5.5, OWC1.8, MSVC2008/2010
// Assumed Compatibility: higher Matlab versions, Mac, Linux
// Author: Jan Simon, Heidelberg, (C) 2015 j@n-simon.de

/*
% $JRev: R-c V:002 Sum:I+6EAduoDQzT Date:25-Jan-2015 22:19:38 $
% $License: BSD (use/copy/change/redistribute on own risk, mention the author) $
% $UnitTest: uTest_CopyMask $
% $File: Tools\Mex\Source\CopyMask.c $
% History:
% 01: 25-Jan-2015 18:24, First version.
*/

#include "mex.h"

// Assume 32 bit addressing for Matlab 6.5:
// See MEX option "compatibleArrayDims" for MEX in Matlab >= 7.7.
#ifndef MWSIZE_MAX
#define mwSize        int32_T           // Defined in tmwtypes.h
#define mwIndex       int32_T
#define mwSignedIndex int32_T
#define MWSIZE_MAX    MAX_int32_T
#endif

// Error messages do not contain the function name in Matlab 6.5! This is not
// necessary in Matlab 7, but it does not bother:
#define ERR_ID   "JSimon:CopyMask:"
#define ERR_HEAD "*** CopyMask[mex]: "
#define ERROR(id,msg) mexErrMsgIdAndTxt(ERR_ID id, ERR_HEAD msg);

// There is an undocumented method to create a shared data copy. This is much
// faster, if the replied object is not changed, because it does not duplicate
// the contents of the array in the memory.
mxArray *mxCreateSharedDataCopy(const mxArray *mx);
#define COPY_ARRAY mxCreateSharedDataCopy
// #define COPY_ARRAY mxDuplicateArray    // slower, but documented

// Prototypes:
mwSize GetChunkLen(const mwSize *XDim, const mwSize Dim);
void GetMask(const mxLogical *Mask, const mwSize nMask,
            mwSignedIndex *nTrue, mwSignedIndex *iTrue, mwSignedIndex *fTrue);

void Copy_8R(double *X, mwSize lenChunk, mwSize lenOp, mwSize nChunk,
            mxLogical *Mask,
            mwSignedIndex nTrue, mwSignedIndex iTrue, mwSignedIndex fTrue,
            double *Y);
void Copy_4R(int32_T *X, mwSize lenChunk, mwSize lenOp, mwSize nChunk,
            mxLogical *Mask,
            mwSignedIndex nTrue, mwSignedIndex iTrue, mwSignedIndex fTrue,
            int32_T *Y);
void Copy_2R(int16_T *X, mwSize lenChunk, mwSize lenOp, mwSize nChunk,
            mxLogical *Mask,
            mwSignedIndex nTrue, mwSignedIndex iTrue, mwSignedIndex fTrue,
            int16_T *Y);
void Copy_1R(int8_T *X, mwSize lenChunk, mwSize lenOp, mwSize nChunk,
            mxLogical *Mask,
            mwSignedIndex nTrue, mwSignedIndex iTrue, mwSignedIndex fTrue,
            int8_T *Y);

void Copy_8C(double *XR, double *XI,
            mwSize lenChunk, mwSize lenOp, mwSize nChunk, mxLogical *Mask,
            mwSignedIndex nTrue, mwSignedIndex iTrue, mwSignedIndex fTrue,
            double *YR, double *YI);
void Copy_4C(int32_T *XR, int32_T *XI,
            mwSize lenChunk, mwSize lenOp, mwSize nChunk, mxLogical *Mask,
            mwSignedIndex nTrue, mwSignedIndex iTrue, mwSignedIndex fTrue,
            int32_T *YR, int32_T *YI);
void Copy_2C(int16_T *XR, int16_T *XI,
            mwSize lenChunk, mwSize lenOp, mwSize nChunk, mxLogical *Mask,
            mwSignedIndex nTrue, mwSignedIndex iTrue, mwSignedIndex fTrue,
            int16_T *YR, int16_T *YI);
void Copy_1C(int8_T *XR, int8_T *XI,
            mwSize lenChunk, mwSize lenOp, mwSize nChunk, mxLogical *Mask,
            mwSignedIndex nTrue, mwSignedIndex iTrue, mwSignedIndex fTrue,
            int8_T *YR, int8_T *YI);

// Main function ===============================================================
void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[])
{
  mwSize        ndimX, nX, widthX, dimOp, nMask, lenOp, lenChunk, nChunk, *DimY;
  mwSignedIndex nTrue, iTrue, fTrue;
  const mwSize  *dimX;
  mxClassID     classX;
  double        dimOp_d;
  mxLogical     *Mask;
  bool          LinearDim;
  void          *Y, *X, *XI=NULL, *YI=NULL;
  mxComplexity  complexX;
  
  // Check number of inputs and outputs: ---------------------------------------
  if (nrhs < 2 || nrhs > 3) {
     ERROR("BadNInput", "2 or 3 inputs allowed.");
  }
  
  if (nlhs > 1) {
     ERROR("BadNOutput", "1 output allowed.");
  }
  
  // Get 1st input, the data array: --------------------------------------------
  X        = mxGetData(prhs[0]);
  XI       = mxGetImagData(prhs[0]);
  ndimX    = mxGetNumberOfDimensions(prhs[0]);
  dimX     = mxGetDimensions(prhs[0]);
  nX       = mxGetNumberOfElements(prhs[0]);
  widthX   = mxGetElementSize(prhs[0]);
  classX   = mxGetClassID(prhs[0]);
  complexX = mxIsComplex(prhs[0]) ? mxCOMPLEX : mxREAL;
  
  // Currently CELL and STRUCT are not supported:
  if (!mxIsNumeric(prhs[0]) && classX != mxLOGICAL_CLASS &&
      classX != mxCHAR_CLASS) {
     ERROR("BadTypeX", "The typ of X must be numerical, logical or char.");
  }
          
  // Get 2nd input, the Mask: -------------------------------------------------
  if (!mxIsLogical(prhs[1])) {
     ERROR("BadNOutput", "2nd input must be logical.");
  }
  Mask  = (mxLogical *) mxGetData(prhs[1]);
  nMask = mxGetNumberOfElements(prhs[1]);
  
  // Get 3rd input, the dimension: ---------------------------------------------
  LinearDim = true;  // Linear indexing as default
  if (nrhs >= 3) {
     if (!mxIsNumeric(prhs[2])) {
        ERROR("BadInput3", "Dim must be empty or scalar.");
     }
     
     switch (mxGetNumberOfElements(prhs[2])) {
        case 0:                               // Linear indexing for empty Dim:
           LinearDim = true;
           break;
           
        case 1:
           LinearDim = false;
           dimOp_d   = mxGetScalar(prhs[2]);  // 1-based index, signed double
           if (dimOp_d != floor(dimOp_d)) {
              ERROR("BadInput3", "Dim must be an integer.");
           }
           if (dimOp_d < 1.0 || dimOp_d > (double) ndimX) {
              ERROR("BadInput3", "Dim exceeds array dimensions.");
           }
           dimOp = ((mwSize) dimOp_d) - 1;    // 0-based
           break;
           
        default:
           ERROR("BadInput3", "Dim must be empty or scalar.");
     }
  }
    
  // Get length of output array and the first and last element: ----------------
  GetMask(Mask, nMask, &nTrue, &iTrue, &fTrue);

  // Create the output: --------------------------------------------------------
  if (LinearDim) {
     lenOp    = nX;   // Number of elements of X
     lenChunk = 1;    // Step width between elements
     nChunk   = 1;    // Number of chunks
     
     // Reject masks, which are longer than the array:
     if (nMask > nX) {
        ERROR("BadInput3", "Mask exceeds array size.");
     }
     
     // Simple copy if all indices are TRUE:
     if (nTrue == nX) {
        plhs[0] = COPY_ARRAY(prhs[0]);
        mxSetM(plhs[0], nX);
        mxSetN(plhs[0], nTrue > 0 ? 1:0);
        return;
     }
     
     // Create output:
     plhs[0] = mxCreateNumericMatrix(nTrue, nTrue > 0 ? 1:0, classX, complexX);
     
  } else {  // Dimension to operate on was specified:
     // Get step width between elements of the subvectors in dimension
     // Dim and number of subvectors:
     lenOp    = dimX[dimOp];
     lenChunk = GetChunkLen(dimX, dimOp);
     nChunk   = nX / (lenChunk * lenOp);
     
     // Reject masks, which exceed the specified dimension:
     if (nMask > lenOp) {
        ERROR("BadInput3", "Mask exceeds array size.");
     }
     
     // Simple copy if all indices are TRUE:
     if (nTrue == lenOp) {
        plhs[0] = COPY_ARRAY(prhs[0]);
        return;
     }
     
     // Create output:
     if ((DimY = (mwSize *) mxMalloc(ndimX * sizeof(mwSize))) == NULL) {
        ERROR("NoMemory", "Cannot get memory for dimensions of output.");
     }
     memcpy(DimY, dimX, ndimX * sizeof(mwSize));
     DimY[dimOp] = (mwSize) nTrue;
     plhs[0]     = mxCreateNumericArray(ndimX, DimY, classX, complexX);
     mxFree(DimY);
  }
  
  // Pointers to output data:
  Y  = mxGetData(plhs[0]);
  YI = mxGetImagData(plhs[0]);   // NULL for real values
  
  // The processing: -----------------------------------------------------------
  if (complexX == mxREAL) {  // Input is real:
     switch (widthX) {
        case 8:
           Copy_8R((double *)X, lenChunk, lenOp, nChunk,
                   Mask, nTrue, iTrue, fTrue, (double *)Y);
           break;
        case 4:
           Copy_4R((int32_T *)X, lenChunk, lenOp, nChunk,
                   Mask, nTrue, iTrue, fTrue, (int32_T *)Y);
           break;
        case 2:
           Copy_2R((int16_T *)X, lenChunk, lenOp, nChunk,
                   Mask, nTrue, iTrue, fTrue, (int16_T *)Y);
           break;
        case 1:
           Copy_1R((int8_T *)X, lenChunk, lenOp, nChunk,
                   Mask, nTrue, iTrue, fTrue, (int8_T *)Y);
           break;
        default:
           ERROR("NoMemory", "Data type not handled.");
     }
     
  } else {                   // X is complex: ----------------------------------
     switch (widthX) {
        case 8:
           Copy_8C((double *)X, (double *)XI, lenChunk, lenOp, nChunk,
                   Mask, nTrue, iTrue, fTrue, (double *)Y, (double *)YI);
           break;
        case 4:
           Copy_4C((int32_T *)X, (int32_T *)XI, lenChunk, lenOp, nChunk,
                   Mask, nTrue, iTrue, fTrue, (int32_T *)Y, (int32_T *)YI);
           break;
        case 2:
           Copy_2C((int16_T *)X, (int16_T *)XI, lenChunk, lenOp, nChunk,
                   Mask, nTrue, iTrue, fTrue, (int16_T *)Y, (int16_T *)YI);
           break;
        case 1:
           Copy_1C((int8_T *)X, (int8_T *)XI, lenChunk, lenOp, nChunk,
                   Mask, nTrue, iTrue, fTrue, (int8_T *)Y, (int8_T *)YI);
           break;
        default:
           ERROR("NoMemory", "Data type not handled.");
     }
  }
  
  return;
}

// =============================================================================
mwSize GetChunkLen(const mwSize *Xdim, const mwSize N)
{
  // Get step size between elements of a subvector in the N'th dimension.
  // This is the product of the leading dimensions.
  const mwSize *XdimEnd;
  mwSize       Step;
  
  XdimEnd = Xdim + N;
  for (Step = 1; Xdim < XdimEnd; Step *= *Xdim++) ; // empty loop
  
  return (Step);
}

// =============================================================================
void GetMask(const mxLogical *Mask, const mwSize nMask,
              mwSignedIndex *nTrue, mwSignedIndex *iTrue, mwSignedIndex *fTrue)
{
  // Find the first, last and number of TRUEs.
  mwSignedIndex len = 0, ini, fin, i, fMask;
  
  fMask = (mwSignedIndex) nMask;
  for (ini = 0; ini < fMask && Mask[ini] == 0; ini++) ;         // empty loop
  *iTrue = ini;
  
  for (fin = nMask - 1; fin >= ini && Mask[fin] == 0; fin--) ;  // empty loop
  *fTrue = fin;
  
  for (i = ini; i <= fin; len += Mask[i++]) ;                   // empty loop
  *nTrue = len;
  
  return;
}

// =============================================================================
// ==                            REAL DATA                                    ==
// =============================================================================
void Copy_8R(double *X, mwSize lenChunk, mwSize lenOp, mwSize nChunk,
            mxLogical *Mask,
            mwSignedIndex nTrue, mwSignedIndex iTrue, mwSignedIndex fTrue,
            double *Y)
{
  // Copy DOUBLE and U/INT64 values.
  // INPUT:
  // X:        Pointer to input data.
  // lenChunk: Number of elements in leading dimensions, 1 for linear indexing
  //           or when operating on first non-singleton dimension.
  // lenOp:    Number of elements in dimensions to operate on.
  // nChunk:   Number of chunks, product of the trailing dimensions.
  // Mask:     Pointer to mask array
  // nTrue, iTrue, fTrue: Number of TRUEs in Mask and the initial and final
  //           position.
  // Y:        Pointer to output data.
  
  mwSignedIndex i;
  mwSize        w = lenChunk * sizeof(*X);
  
  if (lenChunk == 1) {  // First non-singelton dimension or linear indexing:
     // If more than 2.5% of the Mask values are TRUE, it is cheaper to avoid
     // branching with an IF:
     if (nTrue * 40 > (fTrue - iTrue)) {
        while (nChunk-- != 0) {
           for (i = iTrue; i <= fTrue; i++) {
              *Y = X[i];
              Y += Mask[i];  // Advance pointer if Mask is TRUE
           }
           X += lenOp;
        }
        
     } else {  // Faster with less copies for sparse indices:
        while (nChunk-- != 0) {
           for (i = iTrue; i <= fTrue; i++) {
              if (Mask[i]) {
                 *Y++ = X[i];
              }
           }
           X += lenOp;
        }
     }
     
  } else {     // Operate on other than first singelton dimension:
     while (nChunk-- != 0) {
        for (i = iTrue; i <= fTrue; i++) {
           if (Mask[i]) {
              memcpy(Y, X + i * lenChunk, w);
              Y += lenChunk;
           }
        }
        X += lenChunk * lenOp;
     }
  }
  
  return;
}

// =============================================================================
void Copy_4R(int32_T *X, mwSize lenChunk, mwSize lenOp, mwSize nChunk,
            mxLogical *Mask,
            mwSignedIndex nTrue, mwSignedIndex iTrue, mwSignedIndex fTrue,
            int32_T *Y)
{
  // Copy SINGLE and U/INT32 values, same code as for Copy_8R.
  mwSignedIndex i;
  mwSize        w = lenChunk * sizeof(*X);
  
  if (lenChunk == 1) {
     if (nTrue * 40 > (fTrue - iTrue)) {
        while (nChunk-- != 0) {
           for (i = iTrue; i <= fTrue; i++) {
              *Y = X[i];
              Y += Mask[i];
           }
           X += lenOp;
        }
        
     } else {
        while (nChunk-- != 0) {
           for (i = iTrue; i <= fTrue; i++) {
              if (Mask[i]) {
                 *Y++ = X[i];
              }
           }
           X += lenOp;
        }
     }
     
  } else {
     while (nChunk-- != 0) {
        for (i = iTrue; i <= fTrue; i++) {
           if (Mask[i]) {
              memcpy(Y, X + i * lenChunk, w);
              Y += lenChunk;
           }
        }
        X += lenChunk * lenOp;
     }
  }
  
  return;
}

// =============================================================================
void Copy_2R(int16_T *X, mwSize lenChunk, mwSize lenOp, mwSize nChunk,
            mxLogical *Mask,
            mwSignedIndex nTrue, mwSignedIndex iTrue, mwSignedIndex fTrue,
            int16_T *Y)
{
  // Copy CHAR and U/INT16 values, same code as for Copy_8R.
  mwSignedIndex i;
  mwSize        w = lenChunk * sizeof(*X);
  
  if (lenChunk == 1) {
     if (nTrue * 40 > (fTrue - iTrue)) {
        while (nChunk-- != 0) {
           for (i = iTrue; i <= fTrue; i++) {
              *Y = X[i];
              Y += Mask[i];
           }
           X += lenOp;
        }
        
     } else {
        while (nChunk-- != 0) {
           for (i = iTrue; i <= fTrue; i++) {
              if (Mask[i]) {
                 *Y++ = X[i];
              }
           }
           X += lenOp;
        }
     }
     
  } else {
     while (nChunk-- != 0) {
        for (i = iTrue; i <= fTrue; i++) {
           if (Mask[i]) {
              memcpy(Y, X + i * lenChunk, w);
              Y += lenChunk;
           }
        }
        X += lenChunk * lenOp;
     }
  }
  
  return;
}

// =============================================================================
void Copy_1R(int8_T *X, mwSize lenChunk, mwSize lenOp, mwSize nChunk,
            mxLogical *Mask,
            mwSignedIndex nTrue, mwSignedIndex iTrue, mwSignedIndex fTrue,
            int8_T *Y)
{
  // Copy LOGICAL and U/INT8 values, same code as for Copy_8R.
  mwSignedIndex i;
  mwSize        w = lenChunk * sizeof(*X);
  
  if (lenChunk == 1) {
     if (nTrue * 40 > (fTrue - iTrue)) {
        while (nChunk-- != 0) {
           for (i = iTrue; i <= fTrue; i++) {
              *Y = X[i];
              Y += Mask[i];
           }
           X += lenOp;
        }
        
     } else {
        while (nChunk-- != 0) {
           for (i = iTrue; i <= fTrue; i++) {
              if (Mask[i]) {
                 *Y++ = X[i];
              }
           }
           X += lenOp;
        }
     }
     
  } else {
     while (nChunk-- != 0) {
        for (i = iTrue; i <= fTrue; i++) {
           if (Mask[i]) {
              memcpy(Y, X + i * lenChunk, w);
              Y += lenChunk;
           }
        }
        X += lenChunk * lenOp;
     }
  }
  
  return;
}

// =============================================================================
// ==                         COMPLEX DATA                                    ==
// =============================================================================
void Copy_8C(double *XR, double *XI,
            mwSize lenChunk, mwSize lenOp, mwSize nChunk, mxLogical *Mask,
            mwSignedIndex nTrue, mwSignedIndex iTrue, mwSignedIndex fTrue,
            double *YR, double *YI)
{
  // Copy DOUBLE and U/INT64 values, complex data.
  mwSignedIndex i;
  mwSize        w = lenChunk * sizeof(*XR);

  if (lenChunk == 1) {  // First non-singelton dimension or linear indexing:
     // If more than 3.5% of the Mask values are TRUE, it is cheaper to avoid
     // branching with an IF:
     if (nTrue * 29 > (fTrue - iTrue)) {
        while (nChunk-- != 0) {
           for (i = iTrue; i <= fTrue; i++) {
              *YR = XR[i];
              *YI = XI[i];
              YR += Mask[i];
              YI += Mask[i];
           }
           XR += lenOp;
           XI += lenOp;
        }
        
     } else {   // Faster with less copies for sparse indices:
        while (nChunk-- != 0) {
           for (i = iTrue; i <= fTrue; i++) {
              if (Mask[i]) {
                 *YR++ = XR[i];
                 *YI++ = XI[i];
              }
           }
           XR += lenOp;
           XI += lenOp;
        }
     }

  } else {  // Operate on other than first singelton dimension:
     while (nChunk-- != 0) {
        for (i = iTrue; i <= fTrue; i++) {
           if (Mask[i]) {
              memcpy(YR, XR + i * lenChunk, w);
              memcpy(YI, XI + i * lenChunk, w);
              YR += lenChunk;
              YI += lenChunk;
           }
        }
        XR += lenChunk * lenOp;
        XI += lenChunk * lenOp;
     }
  }
     
  return;
}

// =============================================================================
void Copy_4C(int32_T *XR, int32_T *XI,
            mwSize lenChunk, mwSize lenOp, mwSize nChunk, mxLogical *Mask,
            mwSignedIndex nTrue, mwSignedIndex iTrue, mwSignedIndex fTrue,
            int32_T *YR, int32_T *YI)
{
  // Copy SINGLE and U/INT32 values, complex data.
  mwSignedIndex i;
  mwSize        w = lenChunk * sizeof(*XR);
  
  // Same code as for Copy_8C, see there for comments.
  if (lenChunk == 1) {
     if (nTrue * 29 > (fTrue - iTrue)) {
        while (nChunk-- != 0) {
           for (i = iTrue; i <= fTrue; i++) {
              *YR = XR[i];
              *YI = XI[i];
              YR += Mask[i];
              YI += Mask[i];
           }
           XR += lenOp;
           XI += lenOp;
        }
        
     } else {
        while (nChunk-- != 0) {
           for (i = iTrue; i <= fTrue; i++) {
              if (Mask[i]) {
                 *YR++ = XR[i];
                 *YI++ = XI[i];
              }
           }
           XR += lenOp;
           XI += lenOp;
        }
     }

  } else {
     while (nChunk-- != 0) {
        for (i = iTrue; i <= fTrue; i++) {
           if (Mask[i]) {
              memcpy(YR, XR + i * lenChunk, w);
              memcpy(YI, XI + i * lenChunk, w);
              YR += lenChunk;
              YI += lenChunk;
           }
        }
        XR += lenChunk * lenOp;
        XI += lenChunk * lenOp;
     }
  }
   
  return;
}

// =============================================================================
void Copy_2C(int16_T *XR, int16_T *XI,
            mwSize lenChunk, mwSize lenOp, mwSize nChunk, mxLogical *Mask,
            mwSignedIndex nTrue, mwSignedIndex iTrue, mwSignedIndex fTrue,
            int16_T *YR, int16_T *YI)
{
  // Copy CHAR and U/INT16 values, complex data.
  mwSignedIndex i;
  mwSize        w = lenChunk * sizeof(*XR);
  
  // Same code as for Copy_8C, see there for comments.
  if (lenChunk == 1) {
     if (nTrue * 29 > (fTrue - iTrue)) {
        while (nChunk-- != 0) {
           for (i = iTrue; i <= fTrue; i++) {
              *YR = XR[i];
              *YI = XI[i];
              YR += Mask[i];
              YI += Mask[i];
           }
           XR += lenOp;
           XI += lenOp;
        }
        
     } else {
        while (nChunk-- != 0) {
           for (i = iTrue; i <= fTrue; i++) {
              if (Mask[i]) {
                 *YR++ = XR[i];
                 *YI++ = XI[i];
              }
           }
           XR += lenOp;
           XI += lenOp;
        }
     }

  } else {
     while (nChunk-- != 0) {
        for (i = iTrue; i <= fTrue; i++) {
           if (Mask[i]) {
              memcpy(YR, XR + i * lenChunk, w);
              memcpy(YI, XI + i * lenChunk, w);
              YR += lenChunk;
              YI += lenChunk;
           }
        }
        XR += lenChunk * lenOp;
        XI += lenChunk * lenOp;
     }
  }

  return;
}

// =============================================================================
void Copy_1C(int8_T *XR, int8_T *XI,
            mwSize lenChunk, mwSize lenOp, mwSize nChunk, mxLogical *Mask,
            mwSignedIndex nTrue, mwSignedIndex iTrue, mwSignedIndex fTrue,
            int8_T *YR, int8_T *YI)
{
  // Copy LOGICAL and U/INT8 values, complex data.
  mwSignedIndex i;
  mwSize        w = lenChunk * sizeof(*XR);
  
  // Same code as for Copy_8C, see there for comments.
  if (lenChunk == 1) {
     if (nTrue * 29 > (fTrue - iTrue)) {
        while (nChunk-- != 0) {
           for (i = iTrue; i <= fTrue; i++) {
              *YR = XR[i];
              *YI = XI[i];
              YR += Mask[i];
              YI += Mask[i];
           }
           XR += lenOp;
           XI += lenOp;
        }
        
     } else {
        while (nChunk-- != 0) {
           for (i = iTrue; i <= fTrue; i++) {
              if (Mask[i]) {
                 *YR++ = XR[i];
                 *YI++ = XI[i];
              }
           }
           XR += lenOp;
           XI += lenOp;
        }
     }

  } else {
     while (nChunk-- != 0) {
        for (i = iTrue; i <= fTrue; i++) {
           if (Mask[i]) {
              memcpy(YR, XR + i * lenChunk, w);
              memcpy(YI, XI + i * lenChunk, w);
              YR += lenChunk;
              YI += lenChunk;
           }
        }
        XR += lenChunk * lenOp;
        XI += lenChunk * lenOp;
     }
  }
  
  return;
}
