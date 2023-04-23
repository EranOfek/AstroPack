// ScaleTime.c
// Fast linear interpolation
// This is a fast replacement for:
//   Xi = interp1(1:size(X, 1), X, Ti, 'linear');
// See ScaleTime.m for a detailed description.
//
// Fast step mode:
//   YI = ScaleTime(Y, initial, final, number)
//   Specific functions for: step size is < 1, integer, or > 1.
// General index mode (XI need not be monotonically non-decreasing):
//   YI = ScaleTime(Y, XI)
//   Specific function for: Y is a vector or an array.
//
// NOTE: This function is explicitely designed for speed. Operating on other
//   than the first dimension would be possible, but this is much slower.
//   Permuting the input is a solution, but if you really need speed, create Y
//   such that the interpolation appears in the first dimension only!
//
// COMPILE:
//   mex -O ScaleTime.c
// On Linux the C99 comments must be considered (thanks Sebastiaan Breedveld):
//   mex -O CFLAGS="\$CFLAGS -std=c99" ScaleTime.c
// Pre-compiled Mex: http://www.n-simon.de/mex
//
// Tested: Matlab 2009a, 2015b(32/64), 2016b, 2018b, Win7/10
//         Compiler: WinSDK7.1, MSVC 2008/2010/2017
// Assumed Compatibility: higher Matlab versions, Mac, Linux
// Author: Jan Simon, Heidelberg, (C) 2009-2020 j@n-simon.de

/*
% $JRev: R-z V:026 Sum:BZIeh9wIb9Xq Date:19-Jul-2020 23:26:22 $
% $License: BSD (use/copy/change/redistribute on own risk, mention the author) $
% $File: Tools\Mex\Source\ScaleTime.c $
% History:
% 001: 29-Sep-2009 09:20, MEX version of ScaleTime, new index input.
% 005: 17-Dec-2009 00:04, Tried to make it 64-bit proof. I cannot test it!
% 007: 25-Jan-2010 00:39, Handle multi-dim input.
% 013: 23-Aug-2010 23:30, Faster operation on vectors in index mode.
%      Specific subfunction in index mode for integer step length.
% 017: 02-Oct-2011 11:25, BUGFIX: Bad sign conversion under 64 bit.
% 026: 12-Jul-2020 21:14, X can be SINGLE also.
*/

// Includes and defines: -------------------------------------------------------
#include "mex.h"
#include <stdlib.h>
#include <string.h>
#include <math.h>

// Error messages do not contain the function name in Matlab 6.5! This is not
// necessary in Matlab 7, but it does not bother:
#define ERR_ID   "JSimon:ScaleTime:"
#define ERR_HEAD "ScaleTime[mex]: "
#define ERROR(id,msg) mexErrMsgIdAndTxt(ERR_ID id, ERR_HEAD msg);

// Assume 32 bit addressing for Matlab 6.5:
// See MEX option "compatibleArrayDims" for MEX in Matlab >= 7.7.
#ifndef MWSIZE_MAX
#define mwSize  int32_T           // Defined in tmwtypes.h
#define mwIndex int32_T
#define MWSIZE_MAX MAX_int32_T
#endif

// Prototypes: -----------------------------------------------------------------
mxArray *CreateOutput(mwSize numTI, const mxArray *X, mwSize *MX, mwSize *NX);

// Functions for input of class DOUBLE:
void CoreStepGT1(double *X, const mwSize MX, const mwSize nRow,
                 const mwSize iniT, const mwSize numT,
                 const mwSize fullStep, const double fracStep,
                 const double iniFrac, double *R);
void CoreStepLT1(double *X, const mwSize MX, const mwSize nRow,
                 const mwSize iniT, const mwSize numT,
                 const double fracStep, const double iniFrac, double *R);
void CoreStepInt(double *X, const mwSize MX, const mwSize NX, const mwSize iniT,
                 const mwSize finT, const mwSize Step, double *R);
          
void CoreIndexMatrix(double *X, const mwSize MX, const mwSize NX,
                 double *T, const mwSize nT, double *R);
void CoreIndexVector(double *X, const mwSize MX, double *T, const mwSize nT,
                 double *R);

// Functions for input of class SINGLE:
void CoreStepGT1_f(float *X, const mwSize MX, const mwSize nRow,
                 const mwSize iniT, const mwSize numT,
                 const mwSize fullStep, const double fracStep,
                 const double iniFrac, float *R);
void CoreStepLT1_f(float *X, const mwSize MX, const mwSize nRow,
                 const mwSize iniT, const mwSize numT,
                 const double fracStep, const double iniFrac, float *R);
void CoreStepInt_f(float *X, const mwSize MX, const mwSize NX,
                 const mwSize iniT, const mwSize finT, const mwSize Step,
                 float *R);
          
void CoreIndexMatrix_f(float *X, const mwSize MX, const mwSize NX,
                 double *T, const mwSize nT, float *R);
void CoreIndexVector_f(float *X, const mwSize MX, double *T, const mwSize nT,
                 float *R);

// Main function ===============================================================
void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[])
{
  mwSize        MX, NX, numTI;
  double        iniTD, finTD, numTD, fracStepD, fullStepD, iniFracD, *Xd, *Rd;
  float         *Xf, *Rf;
  const mxArray *X;
  mxClassID     classX;
  bool          doubleX;
  
  // Proper number of arguments
  if (nlhs > 1) {
     ERROR("BadNOutput", "1 output allowed.");
  }
  
  // Get input matrix:
  X       = prhs[0];
  classX  = mxGetClassID(X);
  doubleX = (classX == mxDOUBLE_CLASS);
  if (mxIsComplex(X)) {
     ERROR("BadComplexIn1", "Input array must be a real numeric matrix.");
  }
  if (doubleX) {
     Xd = mxGetPr(X);
  } else if (classX == mxSINGLE_CLASS) {
     Xf = (float* ) mxGetData(X);
  } else {
     ERROR("BadClassIn1", "Input array must be a real numeric matrix.");
  }
  
  // Either one vector or 2 scalars to specify the interpolation times:
  if (nrhs == 4) {
     // 3 scalars: Ti = iFrame:((fFrame - iFrame) / (N - 1)):fFrame ------------
     if (mxGetNumberOfElements(prhs[1]) != 1 || !mxIsNumeric(prhs[1]) ||
         mxGetNumberOfElements(prhs[2]) != 1 || !mxIsNumeric(prhs[2]) ||
         mxGetNumberOfElements(prhs[3]) != 1 || !mxIsNumeric(prhs[3])) {
        ERROR("BadSizeFrames", "3 numeric scalars required to specify frames.");
     }
     
     // Get inputs as DOUBLEs:
     iniTD = mxGetScalar(prhs[1]);
     finTD = mxGetScalar(prhs[2]);
     numTD = mxGetScalar(prhs[3]);
     
     // Check in DOUBLE format:
     if (numTD != floor(numTD)) {
        ERROR("BadValueFrames", "The number of frames must be integer.");
     }
             
     // Reply the empty matrix if no interpolation steps are wanted:
     if (numTD < 1.0 || iniTD > finTD) {
        plhs[0] = mxCreateNumericMatrix(0, 0, classX, mxREAL);
        return;
     }
     numTI = (mwSize) numTD;  // Cannot be negative now

     // Calculate step size and fractional part of first point:
     if (numTI > 1) {
        fracStepD = modf((finTD - iniTD) / (numTD - 1.0), &fullStepD);
     } else {
        fracStepD = 0.0;
        fullStepD = 1.0;
     }
     iniFracD = fmod(iniTD, 1.0);
    
     // Create output matrix and interpolate:
     plhs[0] = CreateOutput(numTI, X, &MX, &NX);
    
     // Check limits:
     if (iniTD < 1.0 || (mwSize) finTD > MX) {
        ERROR("OutOfRange", "Interpolation frames out of range.");
     }
     
     // Start the computations:
     if (doubleX) {                     // DOUBLE:
        Rd = mxGetPr(plhs[0]);
        if (fracStepD == 0.0 && iniFracD == 0.0) {
           CoreStepInt(Xd, MX, NX, (mwSize) iniTD, numTI,
                       (mwSize) fullStepD, Rd);
           
        } else if (fullStepD != 0.0) {  // Step size >= 1.0:
           CoreStepGT1(Xd, MX, NX, (mwSize) iniTD, numTI,
                      (mwSize) fullStepD, fracStepD, iniFracD, Rd);
         
        } else {                        // Step size < 1.0:
           CoreStepLT1(Xd, MX, NX, (mwSize) iniTD, numTI,
                       fracStepD, iniFracD, Rd);
        }

     } else {                           // SINGLE:
        Rf = (float *) mxGetData(plhs[0]);
        if (fracStepD == 0.0 && iniFracD == 0.0) {
           CoreStepInt_f(Xf, MX, NX, (mwSize) iniTD, numTI,
                         (mwSize) fullStepD, Rf);
           
        } else if (fullStepD != 0.0) {  // Step size >= 1.0:
           CoreStepGT1_f(Xf, MX, NX, (mwSize) iniTD, numTI,
                         (mwSize) fullStepD, fracStepD, iniFracD, Rf);
         
        } else {                        // Step size < 1.0:
           CoreStepLT1_f(Xf, MX, NX, (mwSize) iniTD, numTI,
                         fracStepD, iniFracD, Rf);
        }
     }
     

  } else if (nrhs == 2) {  // Ti provided as vector: ---------------------------
     // Ti must be a double vector (or convert it with mexCallMATLAB):
     if (!mxIsDouble(prhs[1])) {
        ERROR("BadClassIn2", "Index vector must be a DOUBLE.");
     }
     
     numTI = mxGetNumberOfElements(prhs[1]);
     if (numTI != 0) {
        // Create a row vector or a matrix as output:
        plhs[0] = CreateOutput(numTI, X, &MX, &NX);
                
        if (doubleX) {  // DOUBLE:
           Rd = mxGetPr(plhs[0]);
           if (NX == 1) {
              CoreIndexVector(Xd, MX,     mxGetPr(prhs[1]), numTI, Rd);
           } else {
              CoreIndexMatrix(Xd, MX, NX, mxGetPr(prhs[1]), numTI, Rd);
           }
           
        } else {        // SINGLE:
           Rf = (float *) mxGetData(plhs[0]);
           if (NX == 1) {
              CoreIndexVector_f(Xf,     MX, mxGetPr(prhs[1]), numTI, Rf);
           } else {
              CoreIndexMatrix_f(Xf, MX, NX, mxGetPr(prhs[1]), numTI, Rf);
           }
        }
        
     } else {
        plhs[0] = mxCreateNumericMatrix(0, 0, classX, mxREAL);
     }
     
  } else {
     ERROR("BadNInput", "2 or 4 inputs required.");
  }
  
  return;
}

// =============================================================================
mxArray *CreateOutput(mwSize numTI, const mxArray *X, mwSize *M, mwSize *N)
{
  // Create output, consider row vectors and multi-dimensional arrays.
  // INPUT:
  //   numTI:   Number of interpolation steps.
  //   X:       Pointer to input to get the dimensions and class.
  // OUTPUT:
  //   myArray: Output array, which is replied to the caller.
  //   M:       Length of column (or row for row vector input).
  //   N:       Number of row-vectors to process.
  
  mwSize    nDim, *outDim;
  mxArray   *Out;
  mxClassID classX;
  
  *M     = mxGetM(X);  // [M x N] or [M x N1 x N2 x N3 x ...]
  *N     = mxGetN(X);
  nDim   = mxGetNumberOfDimensions(X);
  classX = mxGetClassID(X);
  if (nDim == 2) {
     if (*M == 1 && *N > 1) {  // Row vector replies a row vector
        *M  = *N;              // Swap dimensions
        *N  = 1;
        Out = mxCreateNumericMatrix(1, numTI, classX, mxREAL);
     } else {                  // Column vector or matrix:
        Out = mxCreateNumericMatrix(numTI, *N, classX, mxREAL);
     }
     
  } else {  // Multi-dim array:
     // Create array for output dimensions:
     if ((outDim = (mwSize *) mxMalloc(nDim * sizeof(mwSize))) == NULL) {
        ERROR("noMemory", "Cannot get memory for dimensions of output.");
     }
     
     // Copy original size and insert number of interpolation points as 1st dim:
     memcpy(outDim, mxGetDimensions(X), nDim * sizeof(mwSize));
     *outDim = numTI;
     Out     = mxCreateNumericArray(nDim, outDim, classX, mxREAL);
     mxFree(outDim);
  }
  
  return Out;
}

// =============================================================================
void CoreStepInt(double *X, const mwSize MX, const mwSize NX, const mwSize iniT,
                 const mwSize numT, const mwSize fullStep, double *R)
{
  // Method for integer step size.
  
  double *Xp, *Xf;
  mwSize iN, L = iniT - 1 + numT * fullStep;
  
  for (iN = 0; iN < NX; iN++) {
    Xp = X + (iniT - 1);
    Xf = X + L;
    while (Xp < Xf) {
      *R++ = *Xp;
      Xp  += fullStep;
    }
    
    X += MX;  // Next column
  }
  
  return;
}

// =============================================================================
void CoreStepGT1(double *X, const mwSize MX, const mwSize NX,
                 const mwSize iniT, const mwSize numT,
                 const mwSize fullStep, const double fracStep,
                 const double iniFrac, double *R)
{
  // Method for step size greater than 1.0. Then the step is divided in an
  // integer and a fractional part. While the integer part determines the index
  // of the elements of X, the fractional part determines the weight between the
  // neighboring values.
  
  double *Xp, frac;
  mwSize iT, iN;

  for (iN = 0; iN < NX; iN++) {
    Xp   = X + (iniT - 1);
    frac = iniFrac;
    for (iT = 1; iT < numT; iT++) {  // Start from 1: Last T on boundary?
      *R++ = *Xp * (1.0 - frac) + *(Xp + 1) * frac;
      Xp  += fullStep;
      if ((frac += fracStep) >= 1.0) {
        Xp++;
        frac -= 1.0;
      }
    }
    
    // Last step on the boundary?
    if (Xp - X == MX - 1) {
      *R++ = *Xp;
    } else {
      *R++  = *Xp * (1.0 - frac) + *(Xp + 1) * frac;
    }
    X += MX;
  }
  
  return;
}

// =============================================================================
void CoreStepLT1(double *X, const mwSize MX, const mwSize NX,
                 const mwSize iniT, const mwSize numT,
                 const double fracStep, const double iniFrac, double *R)
{
  // Method for step size lower than 1.0.
  // Same as for >= 1.0 except for the omitted line "Xp += fullStep;".
  // This is not remarkably faster for LCC and BCC, but perhaps modern compilers
  // can benefit from this.
  
  double *Xp, frac;
  mwSize iT, iN;
  
  for (iN = 0; iN < NX; iN++) {
    Xp   = X + (iniT - 1);
    frac = iniFrac;
    for (iT = 1; iT < numT; iT++) {  // Start from 1: Last T on boundary?
      *R++ = *Xp * (1.0 - frac) + *(Xp + 1) * frac;
      if ((frac += fracStep) >= 1.0) {
        Xp++;
        frac -= 1.0;
      }
    }

    // Last step on the boundary?
    if (Xp - X == MX - 1) {
      *R++ = *Xp;
    } else {
      *R++  = *Xp * (1.0 - frac) + *(Xp + 1) * frac;
    }

    X += MX;
  }
}

// =============================================================================
void CoreIndexMatrix(double *X, const mwSize MX, const mwSize NX,
                     double *T, const mwSize nT, double *R)
{
  // Interpolation frames as vector, data is a matrix.
  // Only the first and last elements of T are tested for exceeding the limits.
  
  double *Tp, *TEnd, *fracList, *fracP, floorT;
  mwSize iN, *indexList, *indexP, *indexEnd;
  
  TEnd = T + (nT - 1);  // Last T needs checking of boundary
  if (*T < 1.0 || *TEnd > (double) MX) {
    ERROR("OutOfRange", "Interpolation frames out of range.");
  }
  if ((indexList = (mwSize *) mxMalloc(nT * sizeof(mwSize))) == NULL) {
    ERROR("NoMemory", "Cannot get memory for index list.");
  }
  if ((fracList = (double *) mxMalloc(nT * sizeof(double))) == NULL) {
    ERROR("NoMemory", "Cannot get memory for fractions list.");
  }
  
  // First column of X - store the indices and fractions:
  fracP  = fracList;
  indexP = indexList;
  for (Tp = T; Tp < TEnd; Tp++) {
    floorT  = floor(*Tp);
    *indexP = (mwSize) floorT;
    *fracP  = *Tp - floorT;
    *R++    = X[*indexP - 1] * (1.0 - *fracP) + X[*indexP] * *fracP;
    fracP++;
    indexP++;
  }
  
  // Check if last interpolation frame is on the boundary:
  floorT = floor(*Tp);
  if (floorT == MX) {
    *indexP = (mwSize) floorT - 1;
    *fracP  = 1.0;
    *R++    = X[*indexP];
  } else {  // Last frame not on boundary:
    *indexP = (mwSize) floorT;
    *fracP  = *Tp - floorT;
    *R++    = X[*indexP - 1] * (1.0 - *fracP) + X[*indexP] * *fracP;
  }
  
  // 2nd to last column re-use the created indexP vector:
  indexEnd = indexList + nT;
  for (iN = 1; iN < NX; iN++) {
    X     += MX;
    fracP  = fracList;
    indexP = indexList;
    while (indexP != indexEnd) {
      *R++ = X[*indexP - 1] * (1.0 - *fracP) + X[*indexP] * *fracP;
      fracP++;
      indexP++;
    }
  }

  mxFree(indexList);
  mxFree(fracList);
  
  return;
}

// =============================================================================
void CoreIndexVector(double *X, const mwSize MX, double *T, const mwSize nT,
                     double *R)
{
  // Interpolation frames as vector, data is a vector only.
  // Only the first and last elements of T are tested for exceeding the limits.
  
  double *Tp, *TEnd, frac, floorT;
  mwSize index;
  
  TEnd = T + (nT - 1);  // Last T needs checking of boundary
  if (*T < 1.0 || *TEnd > (double) MX) {
    ERROR("OutOfRange", "Interpolation frames out of range.");
  }
  
  // X has one column only:
  for (Tp = T; Tp < TEnd; Tp++) {
    floorT = floor(*Tp);
    index  = (mwSize) floorT;
    frac   = *Tp - floorT;
    *R++   = X[index - 1] * (1.0 - frac) + X[index] * frac;
  }
  
  // Check if last interpolation frame is on the boundary:
  floorT = floor(*Tp);
  if ((mwSize) floorT == MX) {
    *R    = X[MX - 1];
  } else {  // Last frame not on boundary:
    index = (mwSize) floorT;
    frac  = *Tp - floorT;
    *R    = X[index - 1] * (1.0 - frac) + X[index] * frac;
  }
  
  return;
}

// =============================================================================
// == SINGLE METHODS: ==========================================================
// =============================================================================
void CoreStepInt_f(float *X, const mwSize MX, const mwSize NX, const mwSize iniT,
                   const mwSize numT, const mwSize fullStep, float *R)
{
  // Method for integer step size.
  
  float *Xp, *Xf;
  mwSize iN, L = iniT - 1 + numT * fullStep;
  
  for (iN = 0; iN < NX; iN++) {
    Xp = X + (iniT - 1);
    Xf = X + L;
    while (Xp < Xf) {
      *R++ = *Xp;
      Xp  += fullStep;
    }
    
    X += MX;  // Next column
  }
  
  return;
}

// =============================================================================
void CoreStepGT1_f(float *X, const mwSize MX, const mwSize NX,
                   const mwSize iniT, const mwSize numT,
                   const mwSize fullStep, const double fracStep,
                   const double iniFrac, float *R)
{
  // Method for step size greater than 1.0. Then the step is divided in an
  // integer and a fractional part. While the integer part determines the index
  // of the elements of X, the fractional part determines the weight between the
  // neighboring values.
  
  float  *Xp;
  double frac;
  mwSize iT, iN;

  for (iN = 0; iN < NX; iN++) {
    Xp   = X + (iniT - 1);
    frac = iniFrac;
    for (iT = 1; iT < numT; iT++) {  // Start from 1: Last T on boundary?
      *R++ = *Xp * (1.0 - frac) + *(Xp + 1) * frac;
      Xp  += fullStep;
      if ((frac += fracStep) >= 1.0) {
        Xp++;
        frac -= 1.0;
      }
    }
    
    // Last step on the boundary?
    if (Xp - X == MX - 1) {
      *R++ = *Xp;
    } else {
      *R++  = *Xp * (1.0 - frac) + *(Xp + 1) * frac;
    }
    X += MX;
  }
  
  return;
}

// =============================================================================
void CoreStepLT1_f(float *X, const mwSize MX, const mwSize NX,
                   const mwSize iniT, const mwSize numT,
                   const double fracStep, const double iniFrac, float *R)
{
  // Method for step size lower than 1.0.
  // Same as for >= 1.0 except for the omitted line "Xp += fullStep;".
  // This is not remarkably faster for LCC and BCC, but perhaps modern compilers
  // can benefit from this.
  
  float  *Xp;
  double frac;
  mwSize iT, iN;
  
  for (iN = 0; iN < NX; iN++) {
    Xp   = X + (iniT - 1);
    frac = iniFrac;
    for (iT = 1; iT < numT; iT++) {  // Start from 1: Last T on boundary?
      *R++ = *Xp * (1.0 - frac) + *(Xp + 1) * frac;
      if ((frac += fracStep) >= 1.0) {
        Xp++;
        frac -= 1.0;
      }
    }

    // Last step on the boundary?
    if (Xp - X == MX - 1) {
      *R++ = *Xp;
    } else {
      *R++ = *Xp * (1.0 - frac) + *(Xp + 1) * frac;
    }

    X += MX;
  }
}

// =============================================================================
void CoreIndexMatrix_f(float *X, const mwSize MX, const mwSize NX,
                       double *T, const mwSize nT, float *R)
{
  // Interpolation frames as vector, data is a matrix.
  // Only the first and last elements of T are tested for exceeding the limits.
  
  double *Tp, *TEnd, *fracList, *fracP, floorT;
  mwSize iN, *indexList, *indexP, *indexEnd;
  
  TEnd = T + (nT - 1);  // Last T needs checking of boundary
  if (*T < 1.0 || *TEnd > (double) MX) {
    ERROR("OutOfRange", "Interpolation frames out of range.");
  }
  if ((indexList = (mwSize *) mxMalloc(nT * sizeof(mwSize))) == NULL) {
    ERROR("NoMemory", "Cannot get memory for index list.");
  }
  if ((fracList = (double *) mxMalloc(nT * sizeof(double))) == NULL) {
    ERROR("NoMemory", "Cannot get memory for fractions list.");
  }
  
  // First column of X - store the indices and fractions:
  fracP  = fracList;
  indexP = indexList;
  for (Tp = T; Tp < TEnd; Tp++) {
    floorT  = floor(*Tp);
    *indexP = (mwSize) floorT;
    *fracP  = *Tp - floorT;
    *R++    = X[*indexP - 1] * (1.0 - *fracP) + X[*indexP] * *fracP;
    fracP++;
    indexP++;
  }
  
  // Check if last interpolation frame is on the boundary:
  floorT = floor(*Tp);
  if (floorT == MX) {
    *indexP = (mwSize) floorT - 1;
    *fracP  = 1.0;
    *R++    = X[*indexP];
  } else {  // Last frame not on boundary:
    *indexP = (mwSize) floorT;
    *fracP  = *Tp - floorT;
    *R++    = X[*indexP - 1] * (1.0 - *fracP) + X[*indexP] * *fracP;
  }
  
  // 2nd to last column re-use the created indexP vector:
  indexEnd = indexList + nT;
  for (iN = 1; iN < NX; iN++) {
    X     += MX;
    fracP  = fracList;
    indexP = indexList;
    while (indexP != indexEnd) {
      *R++ = X[*indexP - 1] * (1.0 - *fracP) + X[*indexP] * *fracP;
      fracP++;
      indexP++;
    }
  }

  mxFree(indexList);
  mxFree(fracList);
  
  return;
}

// =============================================================================
void CoreIndexVector_f(float *X, const mwSize MX, double *T, const mwSize nT,
                       float *R)
{
  // Interpolation frames as vector, data is a vector only.
  // Only the first and last elements of T are tested for exceeding the limits.
  
  double *Tp, *TEnd, frac, floorT;
  mwSize index;
  
  TEnd = T + (nT - 1);  // Last T needs checking of boundary
  if (*T < 1.0 || *TEnd > (double) MX) {
    ERROR("OutOfRange", "Interpolation frames out of range.");
  }
  
  // X has one column only:
  for (Tp = T; Tp < TEnd; Tp++) {
    floorT = floor(*Tp);
    index  = (mwSize) floorT;
    frac   = *Tp - floorT;
    *R++   = X[index - 1] * (1.0 - frac) + X[index] * frac;
  }
  
  // Check if last interpolation frame is on the boundary:
  floorT = floor(*Tp);
  if ((mwSize) floorT == MX) {
    *R    = X[MX - 1];
  } else {  // Last frame not on boundary:
    index = (mwSize) floorT;
    frac  = *Tp - floorT;
    *R    = X[index - 1] * (1.0 - frac) + X[index] * frac;
  }
  
  return;
}
