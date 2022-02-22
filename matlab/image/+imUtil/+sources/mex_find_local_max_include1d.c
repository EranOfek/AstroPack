// See: edit([matlabroot '/extern/examples/mex/explore.c']);
// See: edit([matlabroot '/extern/examples/refbook/matrixDivide.c']);
//
// Access element in 2D array:
//    val = input[ col*rows + row ];
//
// Access element in 3D array:
//    val = input[ col*rows + row + dep*cols*rows ];
//
// originally at D:\GitHub\AstroPack\matlab\image\+imUtil\+sources
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


// Input
void mexFunction(
    int nlhs, mxArray *plhs[],          // Output arguments
    int nrhs, const mxArray *prhs[])    // Input arguments
{
    __FLOAT* input;
    // not a pointer: ok ?
    __FLOAT val;
    int *outListInd, *outII, *outJJ, *outBW;
    int *outListInd_, *outII_, *outJJ_, *outBW_;
    int dim = 1;
    mwSize rows, cols;
    mwSize row, col;
    __FLOAT val1, val2, val3, val4;

    mxClassID     class_id;
    const char*   input_type;
    mwSize        input_ndims;
    const mwSize* input_size;
    // default values of inputs
    __FLOAT Thresh = 200.;
    __FLOAT AllocateFrac = 0.1;
    int Conn = 4;

    // Output
    mwSize output_ndims = 1;
    mwSize output_size[2] = {0,0};

    //mexPrintf("nlhs: %d, nrhs: %d\n", nlhs, nrhs);
    //return;

    // Get dim
    //mexPrintf(nrhs);
    class_id    = mxGetClassID(prhs[0]);
    input       = (__FLOAT*)mxGetData(prhs[0]);
    input_ndims = mxGetNumberOfDimensions(prhs[0]);
    input_size  = mxGetDimensions(prhs[0]);
    input_type  = mxGetClassName(prhs[0]);

    // Get Thresh
    if ((nrhs > 1) && mxIsScalar(prhs[1]))
    {
        Thresh = (__FLOAT)mxGetScalar(prhs[1]);
        //mexPrintf("Thresh: %0.5f\n", Thresh);
    }

    // Get Connectivity
    if ((nrhs > 2) && mxIsScalar(prhs[2]))
    {
        Conn = (int)mxGetScalar(prhs[2]);
        //mexPrintf("Conn: %d\n", Conn);
    }

    // Get AllocFrac
    if ((nrhs > 3) && mxIsScalar(prhs[3]))
    {
        AllocateFrac = (__FLOAT)mxGetScalar(prhs[3]);
       // mexPrintf("AllocateFrac: %0.5f\n", AllocateFrac);
    }

    if (mxIsSparse(prhs[0])) {
        mexPrintf("Sparse array is not supported yet\n");
        return;
    }

    // Validate matrix dimentions
    if (input_ndims != 2)  {
        mexPrintf("Invalid ndims of input matrix, supported ndims is 2 or 3: %d\n", input_ndims);
        return;
    }

    mexPrintf("Hi 1\n");
    //return;

    // Parse input matrix


    // Validate matrix data type
    if (class_id != MEX_FLOAT)
    {
        mexPrintf("Input matrix data type does not match this function\n");
        return;
    }



    mexPrintf("input_ndims: %d\n", input_ndims);
    mexPrintf("input_size:  %d, %d\n", input_size[0], input_size[1]);
    mexPrintf("class_id:    %d\n", class_id);
    mexPrintf("input_type:  %s\n", input_type);
    mexPrintf("dim:         %d\n", dim);


    // Validate input params



    //-------------------------------------------- Input 2D

    rows = input_size[0];
    cols = input_size[1];

   // mexPrintf("Rows: %d, Cols: %d\n", rows, cols);

    // Allocate output 1D array of integers (array of indexes)
    int SizeList = (int)ceil((__FLOAT)rows * (__FLOAT)cols * AllocateFrac) ;
    mwSize SizeListInd[1] = { (mwSize)ceil((__FLOAT)rows * (__FLOAT)cols * AllocateFrac) };
    mexPrintf("SizeListInd: %d\n", (int)SizeListInd[0]);
    if (SizeListInd[0] < 1)
    {
        mexPrintf("SizeListInd is zero, AllocateFrac is too small?\n");
        return;
    }
    // mxREAL - The output is real and not complex
    // mxINT32_CLASS - The output type is 32bit integer
    mexPrintf("size dynamic array %d\n",SizeList);
    outListInd = (int*) malloc(SizeList);
    outListInd[10] = 20;
    if (nlhs > 1)
    {
        plhs[1] = mxCreateNumericArray(output_ndims, SizeListInd, mxINT32_CLASS, mxREAL);
        outII = (int*)mxGetData(plhs[1]);
    }
    if (nlhs > 2)
    {
        plhs[2] = mxCreateNumericArray(output_ndims, SizeListInd, mxINT32_CLASS, mxREAL);
        outJJ = (int*)mxGetData(plhs[2]);
    }
    if (nlhs > 3)
    {
        output_ndims = 2;
        plhs[3] = mxCreateNumericArray(output_ndims, input_size, mxINT32_CLASS, mxREAL);
        outBW = (int*)mxGetData(plhs[3]);
    }

    int count = 0;
    if (Conn==4) 
    {
        // skip the edges of the image
        bool flag =true;
        for (col = 1;  (col < cols - 1)&&flag;  col++) {

            for (row = 1;  (row < rows - 1)&&flag ;  row++) {
                val = input[ col*rows + row ];
                if (val>Thresh) {
                    val1 = input[ (col-1)*rows + row ];
                    val2 = input[ (col+1)*rows + row ];
                    val3 = input[ col*rows + row-1 ];
                    val4 = input[ col*rows + row+1 ];
                    if (val>val1 && val>val2 && val>val3 && val>val4)
                    {
                        //mwSize Indd = (__INT)col*(__INT)rows + (__INT)row;
                        mwSize Indd = col*rows + row + 1;
                        //outListInd[count] = (int)Indd;
                        //outListInd[count] = 1;
                        //mexPrintf("col: %d, row: %d, ind: %d\n", col, row, (int)Indd);
                        if (nlhs > 2) {
                            outII[count] = (int)(Indd-1)%(int)rows+1;
                            outJJ[count] = (int)ceil((Indd+.00000001)/rows);
                        }

                        count = count + 1;
                        if (count == SizeList) 
                        {
                            flag = false;
                        }
                    }
                }
            }
        }
    }
    if (nlhs > 3)
    {
        for (int i =0;i<count;i++) 
        {
            
            outBW[outII[i]+ (outJJ[i]-1)*rows-1] = 1;
           // mexPrintf("col: %d, row: %d\n", (int)outII[i], (int)outJJ[i] );
        }
    }
    mwSize  SizeListM[1] = {(mwSize)count};
    plhs[0] = mxCreateNumericArray(output_ndims, SizeListM, mxINT32_CLASS, mxREAL);
    // Get pointer to output array
    outListInd_ = (int*)mxGetData(plhs[0]);
    //for (int i =0;i<count;i++) 
//    {
  //      outListInd_[i] = outListInd[i];
   //}
 free(outListInd);
}

//mexPrintf("output_ndims: %d\n", output_ndims);
//mexPrintf("output_size:  %d, %d, %d\n", output_size[0], output_size[1], output_size[2]);

