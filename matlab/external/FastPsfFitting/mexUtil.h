// Author: Simon Christoph Stein
// E-Mail: scstein@phys.uni-goettingen.de
// Date: June 2015

// % Copyright (c) 2015, Simon Christoph Stein
// % All rights reserved.
// % 
// % Redistribution and use in source and binary forms, with or without
// % modification, are permitted provided that the following conditions are met:
// % 
// % 1. Redistributions of source code must retain the above copyright notice, this
// %    list of conditions and the following disclaimer.
// % 2. Redistributions in binary form must reproduce the above copyright notice,
// %    this list of conditions and the following disclaimer in the documentation
// %    and/or other materials provided with the distribution.
// % 
// % THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
// % ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
// % WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
// % DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
// % ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
// % (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
// % LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
// % ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
// % (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
// % SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
// % 
// % The views and conclusions contained in the software and documentation are those
// % of the authors and should not be interpreted as representing official policies,
// % either expressed or implied, of the FreeBSD Project.

#pragma once


// MEX
#include "mex.h"

// stdlib
#include <streambuf>
#include <climits>


//  -- mexStreambuf can be used to redirect std::cout to MATLAB console --
// To overload cout use:
//    mexStream mout;
//    std::streambuf *outbuf = std::cout.rdbuf(&mout);  // Replace the std stream with the 'matlab' stream
//      -- your code --
//    std::cout.rdbuf(outbuf);   // Restore the std stream buffer (important!)
class mexStream : public std::streambuf {
public:
protected:
  virtual std::streamsize xsputn(const char *s, std::streamsize n)
  {
    mexPrintf("%.*s",n,s);
    return n;
  }
  virtual int overflow(int c = EOF)
  {
      if (c != EOF) {
      mexPrintf("%.1s",&c);
    }
    return 1;   
  }
}; 

//  -- Used to ignore std::cout --
// We need this for linux, as std::cout automatically redirects to the MATLAB console in linux
// To overload cout use:
//    nullStream nullStr;
//    std::streambuf *outbuf = std::cout.rdbuf(&nullStr);  // Replace the std stream with the 'matlab' stream
//      -- your code --
//    std::cout.rdbuf(outbuf);   // Restore the std stream buffer (important!)
class nullStream : public std::streambuf {
public:
protected:
  virtual std::streamsize xsputn(const char *s, std::streamsize n)
  {
    // Do nothing
    return n;
  }
  virtual int overflow(int c = EOF)
  {
    // Do nothing
    return 1;   
  }
}; 

    
template <typename Type>
void checkDatatypeCompatability(const mxArray* mexPtr)
{        
    // Logicals are always accepted
     if(mxGetClassID(mexPtr) == mxLOGICAL_CLASS)
         return;
    
        unsigned int typeBits = CHAR_BIT*sizeof(Type); // Bits
        unsigned int matlabBits = 0;
        
        switch(mxGetClassID(mexPtr))
        {
            case mxDOUBLE_CLASS: matlabBits = 64; break;
            case mxSINGLE_CLASS: matlabBits = 32; break;
            case mxINT8_CLASS:   matlabBits = 8;  break;
            case mxUINT8_CLASS:  matlabBits = 8; break;
            case mxINT16_CLASS:  matlabBits = 16; break;
            case mxUINT16_CLASS: matlabBits = 16; break;
            case mxINT32_CLASS:  matlabBits = 32; break;
            case mxUINT32_CLASS: matlabBits = 32; break;
            case mxINT64_CLASS:  matlabBits = 64; break;
            case mxUINT64_CLASS: matlabBits = 64; break;
            
            default:
            {
//                 mxUNKNOWN_CLASS,  mxCELL_CLASS, mxSTRUCT_CLASS, 
//                 mxCHAR_CLASS, mxVOID_CLASS, mxFUNCTION_CLASS
                mexErrMsgTxt("ERROR: Tried to create Array for non-numerical datatype.");
                break;
            }
        }
        
        if( typeBits != matlabBits )
            mexErrMsgTxt("ERROR: Tried to create Array with wrong bitdepth for numerical datatype.");
}


//  ---- Array1D_t, Array2D_t, Array3D_t classes ----:
//
// These are simple wrappers written to simplify accessing MATLAB memory while being fast.
// The classes are templated for use with different datatypes. For simplified use, define a
// typedef, in most cases with MATLAB this should be:
//   typedef Array1D_t<double> Array1D;
//   typedef Array2D_t<double> Array2D;
//   typedef Array3D_t<double> Array3D;
//
// I) CREATION
// (x) Usually input memory will be accessed like this:
//  void mexFunction(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[]){
//   Array2D img( prhs[0] );     /* Map access to first input variable */
// (x) Similar for output memory. First create MATLAB memory, than map:
//   const int nDims = 1; // Number of dimensions for output
//   mwSize dim_out0[nDims] = { 5 }; // Size of each dimension
//   plhs[0] = mxCreateNumericArray( nDims, dim_out0 , mxDOUBLE_CLASS, mxREAL);
//   Array1D fin_params ( plhs[0] ); // Map access to output
//
// (x) There are always 3 different constructors available. Example for Array3D:
//   1) Array3D matlab_3d_stack(prhs[1]); // uses MATLAB mxArray*
//   2) Array3D existing_3d_stack(myDataPtr, num_rows, num_cols, num_depth); // Maps exisiting myDataPtr with num_rows*num_cols*num_depth elements to 3D array. Does NOT handle memory allocation / deletion!
//   3) Array3D new_3d_stack(num_rows, num_cols, num_depth); // Creates new 3D Array from scratch WITH memory management.
//
// II) ACCESS
//  (x) Similar to MATLAB, each class can be accessed either by the full position
//        3d_stack(num_rows, num_cols, num_depth);
//      Or by linear indexing:
//        3d_stack(linear_index); 
//   Note: Linear indexing is faster than full position access, as no index needs to be calculated
//   TODO: May implement row_iterator, column_iterator etc. to improve speed (no index calculation required)

template <class Type>
struct Array1D_t
{
    //Note: every mxArray is >= 2 dimensional
    Array1D_t (const mxArray* mexPtr)
    :  ptr( (Type*) mxGetData(mexPtr) ),
       nRows( mxGetDimensions(mexPtr)[0] ),
       nCols( mxGetDimensions(mexPtr)[1] ),
       nElements( (nRows>nCols) ? nRows:nCols ),
       isColVector( nRows>nCols ),
       standalone(false)
    {
        checkDatatypeCompatability<Type>(mexPtr);
           
        // Check dimensions. mxGetNumberOfDimensions always returns '2' or more
        const unsigned int nDim = mxGetNumberOfDimensions(mexPtr);
        if (nDim != 2 || (nRows > 1 && nCols > 1) )
        {
            mexErrMsgTxt("ERROR: Tried to create Array1D from non-1D data.");
        }
    }
    
    Array1D_t(Type* ptr_arg, int nElements_arg)
    :  ptr( (Type*) ptr_arg),
       nRows(nElements_arg),
       nCols(1),
       nElements(nElements_arg),
       isColVector(true),
       standalone(false)
    {       
    }
    
    // Create a new array without binding to MATLAB
    // Allocate memory for this
    Array1D_t(int nElements_arg)
    :  ptr( NULL ),
       nRows(nElements_arg),
       nCols(1),
       nElements(nElements_arg),
       isColVector(true),
       standalone(true)
    {
        ptr = new Type[nElements_arg];
    }
    
    // Copy constructor (needed for standalone arrays to take deep copy of data)
    Array1D_t( const Array1D_t& other ):
       ptr( other.ptr ),
       nRows( other.nRows ),
       nCols( other.nCols ),
       nElements(other.nElements),
       isColVector( other.isColVector ),
       standalone( other.standalone )
    {
           // Take deep copy of the other array
           if(standalone)
           {
               ptr = new Type[nElements];
               for(int i=0; i<nElements; ++i)
                   (*this)[i] = other[i];
           }
           
    }
           
    
    // Destructor
    ~Array1D_t() 
    {
        if(standalone) {
            delete[] ptr;
        }
    }
    
    Type& operator[](int pos) 
    {
        return ptr[pos]; 
    }
    
    Type& operator()(int pos) 
    {
        return ptr[pos]; 
    }
    
    bool isEmpty() const
    {
      return nElements==0;   
    }
    
    //Attributes
    Type* ptr;
    const unsigned int nRows;
    const unsigned int nCols;
    const unsigned int nElements;
    
    const bool isColVector;
    const bool standalone; // Array created without binding to MATLAB
};


template <class Type>
struct Array2D_t
{
    Array2D_t (const mxArray* mexPtr)
    :     ptr( (Type*) mxGetData(mexPtr) ),
          nRows( mxGetDimensions(mexPtr)[0] ),
          nCols( mxGetDimensions(mexPtr)[1] ),
          nElements( nRows*nCols ),
          standalone(false)
    {
        checkDatatypeCompatability<Type>(mexPtr);
        
        // Check dimensions. mxGetNumberOfDimensions always returns '2' or more
        const unsigned int nDim = mxGetNumberOfDimensions(mexPtr);
        if ( nDim != 2 )
        {
            mexErrMsgTxt("ERROR: Tried to create Array2D from non-2D data.");
        }
//         if ( !(nRows > 1 && nCols > 1) )
//         {
//             char warnMsg[256];
//             sprintf(warnMsg, "Created Array2D with singleton dimensions (dim: %ix%i)", nRows, nCols);
//             mexWarnMsgTxt(warnMsg);
//         }
    }
    
    
    Array2D_t(Type* ptr_arg, int num_rows, int num_cols)
        : ptr( (Type*) ptr_arg ),
          nRows( num_rows ),
          nCols( num_cols ),
          nElements( nRows*nCols ),
          standalone(false)
    {
    }
        
    // Create a new array without binding to MATLAB
    // Allocate memory for this
    Array2D_t(int num_rows, int num_cols)
        : ptr(),
          nRows( num_rows ),
          nCols( num_cols ),
          nElements( nRows*nCols ),
          standalone(true)
    {
            ptr = new Type[nElements];
    }
        
    // Copy constructor (needed for standalone arrays to take deep copy of data)
    Array2D_t( const Array2D_t& other ):
       ptr( other.ptr ),
       nRows( other.nRows ),
       nCols( other.nCols ),
       nElements(other.nElements),
       standalone( other.standalone )
    {
           // Take deep copy of the other array
           if(standalone)
           {
               ptr = new Type[nElements];
               for(int i=0; i<nElements; ++i)
                   (*this)[i] = other[i];
           }
           
    }
        
        
    ~Array2D_t()
    {
        if(standalone) {
            delete[] ptr;
        }
    }
    
    Type& operator[](int pos)
    {
        return ptr[pos];
    }
        
    Type& operator()(int iRow, int iCol)
    {
        return *(ptr + iCol*nRows + iRow);
    }
    
    bool isEmpty() const
    {
      return nElements==0;   
    }
    
    // Attributes
public:
    Type* ptr;
    const unsigned int nRows;
    const unsigned int nCols;
    const unsigned int nElements;
    
    const bool standalone; // Array created without binding to MATLAB
};


template <class Type>
struct Array3D_t
{
    Array3D_t (const mxArray* mexPtr)
    : ptr( (Type*) mxGetData(mexPtr) ),
      nRows ( mxGetDimensions( mexPtr)[0] ),
      nCols ( mxGetDimensions( mexPtr)[1] ),
      nDepth( mxGetDimensions( mexPtr)[2] ),
      nElements( nRows*nCols*nDepth ),
      nRowsXnCols( nRows*nCols ),
      standalone(false)
    {
        checkDatatypeCompatability<Type>(mexPtr);
        
        // Check dimensions. mxGetNumberOfDimensions always returns '2' or more
        const unsigned int nDim = mxGetNumberOfDimensions(mexPtr);
        if (nDim != 3 )
        {
            mexErrMsgTxt("ERROR: Tried to create Array3D from non-3D data.");
        }
//         if ( !(nRows > 1 && nCols > 1 && nDepth>1) )
//         {
//             char warnMsg[256];
//             sprintf(warnMsg, "Created Array3D with singleton dimensions (dim: %ix%ix%i)", nRows, nCols, nDepth);
//             mexWarnMsgTxt(warnMsg);
//         }
    }
    
    
    Array3D_t(Type* ptr_arg, int num_rows, int num_cols, int num_depth)
    : ptr( (Type*) ptr_arg ),
      nRows ( num_rows ),
      nCols ( num_cols ),
      nDepth( num_depth ),
      nElements( nRows*nCols*nDepth ),
      nRowsXnCols( nRows*nCols ),
      standalone(false)
    {
    }
    
    Array3D_t(int num_rows, int num_cols, int num_depth)
    : ptr( NULL ),
      nRows ( num_rows ),
      nCols ( num_cols ),
      nDepth( num_depth ),
      nElements( nRows*nCols*nDepth ),
      nRowsXnCols( nRows*nCols ),
      standalone(true)
    {
        ptr = new Type[nElements];
    }
    
    // Copy constructor (needed for standalone arrays to take deep copy of data)
    Array3D_t( const Array3D_t& other ):
       ptr( other.ptr ),
       nRows( other.nRows ),
       nCols( other.nCols ),
       nDepth( other.num_depth ),
       nElements(other.nElements),
       nRowsXnCols( other.nRowsXnCols ),
       standalone( other.standalone )
    {
           // Take deep copy of the other array
           if(standalone)
           {
               ptr = new Type[nElements];
               for(int i=0; i<nElements; ++i)
                   (*this)[i] = other[i];
           }
           
    }
    
    ~Array3D_t()
    {
        if(standalone) {
            delete[] ptr;
        }
    }
    
    Type& operator[](int pos)
    {
        return ptr[pos];
    }
        
    Type& operator()(int iRow, int iCol, int iZ)
    {
       return *(ptr + iZ*nRowsXnCols + iCol*nRows + iRow); 
    }
    
    bool isEmpty() const
    {
      return nElements==0;
    }
    
    // Attributes
public:
    Type* ptr;
    const unsigned int nRows;
    const unsigned int nCols;
    const unsigned int nDepth;
    const unsigned int nElements;
    
    const unsigned int nRowsXnCols; // cache value to save performance
    const bool standalone; // Array created without binding to MATLAB
};

