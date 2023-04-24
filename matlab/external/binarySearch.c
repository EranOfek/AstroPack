/* BINARY SEARCH ALGORITHM
 * Description : 
 *    mex function that performs the binary search algorithm to find "item(s)"
 *    (the values to be searched for) among some pre-sorted "data" vector.
 *    By default, the algorithm returns the index of the first instance of each "item"
 *    (if there are multiple copies found), and returns the index of the closest item
 *    if the item(s) are not found (although these behaviors can be changed 
 *    with appropriate optional input parameters.)
 *
 *  Note : by default, the algorithm does not check whether the input data is sorted (since 
 *   that would be an O(N) procedure, which would defeat the purpose of the
 *   algorithm.  If the input data is not sorted, the output values will be incorrect.
 *
 * 
 * Matlab call syntax:
 *    pos = binarySearchMatlab(data, items, [dirIfFound], [dirIfNotFound], [checkIfSorted_flag])
 *
 * Matlab compile command:
 *    mex binarySearch.c
 *
 * Input: This function requires (pre-sorted) reference data vector "data", 
 *  as well as a second input, "items" to search for. "items" can be any size. 
 *
 * Output : "pos" is the same size as the input "items".
 *   
 * * Optional input arguments: 'dirIfFound' and 'dirIfNotFound' specify
 *   the function's behavior if the items are not found, or if multiple 
 *   items are found: (Supply an empty vector [] to leave as the default.)
 *   Note, if you like, you can change the default behavior in each case by
 *   modifying the DEFAULT values in the #define section below.
 *
 *   dirIfFound  specifies the function's behavior if *multiple* copies of the 
 *      value in "items" are found.
 *     -1, or 'first' : [default] the position of the *first* occurence of 'item' is returned
 *     +1, or 'last'  : the position of the *last* occurence of 'item' is returned. 
 *      0, or 'any'   : the position of the first item that the algorithm discovers is found
 *           (ie. not necessarily the first or last occurence)
 *
 *   dirIfNotFound specifies the behavior if the value in "items" is *not* found.
 *        0, or 'exact'   : the value 0 is returned.            
 *       -1, or 'down'    : the position of the last item smaller than 'item' is returned.
 *       +1, or 'up'      : the position of the first item greater than 'item' is returned.
 *        2, or 'closest' : [default], the position of the *closest* item closest to 'item' 
 *						    is returned
 *        0.5, or 'frac'  : the function returns a *fractional* value, indicating, the 
 *							relative position between the two data items between which 'item' 
 *							would be located if it was in the data vector. 
 *					        (eg if you are searching for the number 5 (and "data" starts off 
 *							with [ 2, 3, 4, 7,...], then the algorithm returns 3.333, because 
 *							5 is 1/3 of the way between the 3th and the 4th elements of "data".
 *
 *  checkIfSorted_flag
 *     By default, this program is set *not* to check that the input data vector is sorted.
 *    (although you can change this by setting the defined CHECK_IF_INPUT_SORTED_DEFAULT as 1)
 *    However, if you provide a non-empty 5th argument the input data will be checked. 
 *    (You might use this, for example, while debugging your  code, and remove it later 
 *     to improve performance)
 *
 *  Example:
       data = 1:100;
       items = [pi, 42, -100]
       binarySearch(data, items)
       ans =
            3    42     1
 *
 *
 *
 *  Please send bug reports / comments to :
 *  Avi Ziskind
 *  avi.ziskind@gmail.com
 *  
 *  last updated: May 2013.
 *
 *  update on 5/2/2013:
 *    * fixed a memory leak that occurs if strings are passed as 3rd or 4th arguments (you need to 
 *      call mxFree if you use mxArrayToString)
 *    * added out-of-bounds check to the binary search core (if item is out of bounds, we can skip 
 *      the search altogether)
 *    * allow for both single-precision or double-precision inputs.
 *      ('data' and 'item' can both be either double or single). This is done by having two copies
 *      of the core search function, one that is used if 'data' is double, and one for if 'data' is
 *      single (and each 'item' is cast accordingly). If anyone knows of a better way to do this 
 *      that isn't too cumbersome and won't add too much overhead, please let me know.
 */


#include "mex.h"

#define CHECK_IF_INPUT_SORTED_DEFAULT  0
// Change to 1 if you want the algorithm to always check whether the input data vector is sorted.

#define DIR_IF_FOUND_DEFAULT  -1
// Controls the behavior of the algorithm if multiple copies are found.
// (see above for details and other options)

#define DIR_IF_NOT_FOUND_DEFAULT 2
// Controls the behavior of the algorithm if the item being search for is not found.
// (see above for details and other options)


double abs2(double x) {
    if (x >= 0)
        return x;
    else 
        return -x;
}


bool issorted(double* pdArray, long N) {
    // tests if the vector pdArray[1..N] is sorted
    long i;
    for (i=1; i<N; i++) {
        if (pdArray[i] > pdArray[i+1])
            return false;
    }	
    return true;
}

double binarySearch_double(double* pdData, long N, double dItem, double dirIfFound, double dirIfNotFound) {
    
    long iPos = 0;
    double dPos = 0;
    bool foundItem = false;
    long lower = 1;
    long upper = N;
    long mid;
        
    // check if item is outside the bounds -- no need to do the search
//     if (dItem < pdData[1]) {
//         upper = 1;
//     }
//     if (dItem > pdData[N]) {
//         lower = N;
//     }
    
    while ((upper > lower+1) && (iPos == 0)) {
        mid = ((upper+lower)/2);
        if (pdData[mid] == dItem) {
            iPos = mid;
            foundItem = true;
        }
        else {
            if (pdData[mid] > dItem)
                upper = mid;
            else
                lower = mid;
        }
    }
    
    if (!foundItem) {    /* didn't find during search: test upper & lower bounds */
        if (pdData[upper] == dItem) {
            iPos = upper;
            foundItem = true;
        }
        else if (pdData[lower] == dItem) {
            iPos = lower;
            foundItem = true;
        }
    }
        
    if (foundItem) {
        if (dirIfFound == -1)
            while ((iPos > 1) && (pdData[iPos-1] == pdData[iPos]))
                iPos--;
        
        else if (dirIfFound == +1)
            while ((iPos < N) && (pdData[iPos+1] == pdData[iPos]))
                iPos++;
    }

    else if (!foundItem) {
        if (dirIfNotFound == -1) {
            if (dItem > pdData[upper])  // this could be true if upper is at the end of the array
                iPos = upper;
            else 
                iPos = lower;     
        }
        
        else if (dirIfNotFound == 0) {
            iPos = 0;  
        }
        
        else if (dirIfNotFound == 0.5) {
            dPos = lower + (dItem-pdData[lower])/(pdData[upper]-pdData[lower]);          
        }

        else if (dirIfNotFound == 1) {
            if (dItem < pdData[lower])   // this could be true if lower is at the start of the array
                iPos = lower;
            else 
                iPos = upper;
        }

        else if (dirIfNotFound == 2) {
            if (abs2(pdData[upper]-dItem) < abs2(pdData[lower]-dItem))
                iPos = upper;
            else
                iPos = lower;
        }
        
    }

    if (dPos == 0) { // most of the time, except when dirIfNotFound = 0.5
        dPos = iPos;
    }
    
    return dPos;
}


double binarySearch_float(float* pfData, long N, float fItem, double dirIfFound, double dirIfNotFound) {
    
    long iPos = 0;
    double dPos = 0;
    bool foundItem = false;
    long lower = 1;
    long upper = N;
    long mid;
            
    
    while ((upper > lower+1) && (iPos == 0)) {
        mid = ((upper+lower)/2);
        if (pfData[mid] == fItem) {
            iPos = mid;
            foundItem = true;
        }
        else {
            if (pfData[mid] > fItem)
                upper = mid;
            else
                lower = mid;
        }
    }
    
    if (!foundItem) {    /* didn't find during search: test upper & lower bounds */
        if (pfData[upper] == fItem) {
            iPos = upper;
            foundItem = true;
        }
        else if (pfData[lower] == fItem) {
            iPos = lower;
            foundItem = true;
        }
    }
        
    if (foundItem) {
        if (dirIfFound == -1)
            while ((iPos > 1) && (pfData[iPos-1] == pfData[iPos]))
                iPos--;
        
        else if (dirIfFound == +1)
            while ((iPos < N) && (pfData[iPos+1] == pfData[iPos]))
                iPos++;
    }

    else if (!foundItem) {
        if (dirIfNotFound == -1) {
            if (fItem > pfData[upper])  // this could be true if upper is at the end of the array
                iPos = upper;
            else 
                iPos = lower;     
        }
        
        else if (dirIfNotFound == 0) {
            iPos = 0;  
        }
        
        else if (dirIfNotFound == 0.5) {
            dPos = lower + (fItem-pfData[lower])/(pfData[upper]-pfData[lower]);          
        }

        else if (dirIfNotFound == 1) {
            if (fItem < pfData[lower])   // this could be true if lower is at the start of the array
                iPos = lower;
            else 
                iPos = upper;
        }

        else if (dirIfNotFound == 2) {
            if (abs2(pfData[upper]-fItem) < abs2(pfData[lower]-fItem))
                iPos = upper;
            else
                iPos = lower;
        }
        
    }

    if (dPos == 0) { // most of the time, except when dirIfNotFound = 0.5
        dPos = iPos;
    }
    
    return dPos;
}



void mexFunction( int nlhs, mxArray *plhs[],
                 int nrhs, const mxArray *prhs[] )  {
    
    // INPUT:
    double *pdData, *pdItems, dItem;
    double dirIfFound    = DIR_IF_FOUND_DEFAULT;     // default value
    double dirIfNotFound = DIR_IF_NOT_FOUND_DEFAULT; // default value
    float *pfData, *pfItems, fItem;
    int dataIsDouble, itemIsDouble;
    
    // OUTPUT:
    double *pdPos;        
    
    // Local:    
    long i, N, nItems;
    int checkIfSorted;
    mwSize  nrows,ncols;
    const mxArray * pArg;
    char *pArgStr;

    const mwSize    *dims;    
    mwSize          numDims;        
    
    /* --------------- Check inputs ---------------------*/
    if (nrhs < 2)
        mexErrMsgTxt("at least 2 inputs required");
    if (nrhs > 5)  
        mexErrMsgTxt("maximum of 5 inputs");
    
    /// ------------------- data ----------
	pArg = prhs[0];
    nrows = mxGetM(pArg); ncols = mxGetN(pArg);
	if(!mxIsNumeric(pArg) || mxIsEmpty(pArg) || mxIsComplex(pArg) || ((nrows > 1) && (ncols >1)) ) { 
            mexErrMsgTxt("Input 1 (data) must be a noncomplex, non-empty vector of doubles.");
    }
    dataIsDouble = mxIsDouble(pArg);
    if (dataIsDouble) {
        pdData  = mxGetPr(prhs[0])-1; // subtract 1  for 1..N indexing
    } else {
        pfData  = ((float*) mxGetData(prhs[0])) -1; // subtract 1  for 1..N indexing
    }
    N = nrows * ncols;
    
    /// ------------------- items ----------
    pArg = prhs[1];
    nrows = mxGetM(pArg); ncols = mxGetN(pArg);
	if(!mxIsNumeric(pArg) || mxIsComplex(pArg) ) { 
            mexErrMsgTxt("Input 2 (items) must be a noncomplex double matrix.");
    }
    itemIsDouble = mxIsDouble(pArg);
    if (itemIsDouble) {
        pdItems = mxGetPr(prhs[1])-1; // subtract 1  for 1..N indexing
    } else {
        pfItems  = ((float*) mxGetData(prhs[1])) -1; // subtract 1  for 1..N indexing
    }        
    nItems = nrows * ncols;

    /// ------------------- dirIfFound ----------
    if (nrhs >= 3) {
        
        pArg = prhs[2];
        if (mxIsEmpty(pArg)) {            
            dirIfFound = DIR_IF_FOUND_DEFAULT;  
        } else if (mxIsChar(pArg)) {
            pArgStr = mxArrayToString(pArg);
            if (strcmp(pArgStr, "first")==0) {
                dirIfFound = -1.0;       
            } else if (strcmp(pArgStr, "last")==0) {
                dirIfFound = 1.0;
            } else if (strcmp(pArgStr, "any")==0)
                dirIfFound = 0.0;
            else 
                mexErrMsgTxt("Input 3 (dirIfFound), if input as a string, must be either 'first', 'last', or 'any'");
            
            mxFree(pArgStr);  // free up memory used by mxArrayToString
        } else {
            if(!mxIsNumeric(pArg)  || mxIsComplex(pArg) || mxGetN(pArg)*mxGetM(pArg) != 1)
                mexErrMsgTxt("Input 3 (dirIfFound) must be a real scalar or a string");

            dirIfFound = mxGetScalar(prhs[2]);
        
            if (!( (dirIfFound == -1) || (dirIfFound == 0) || (dirIfFound == 1) )  )
                mexErrMsgTxt("Input 3 (dirIfFound) must be either -1, 0, or 1");
        }
    }
    
    /// ------------------- dirIfNotFound ----------
    if (nrhs >= 4) {
        pArg = prhs[3];
        if (mxIsEmpty(pArg)) {            
            dirIfNotFound == DIR_IF_NOT_FOUND_DEFAULT;  // default value
        } else if (mxIsChar(pArg)) {  
            pArgStr = mxArrayToString(pArg);
            if (strcmp(pArgStr, "exact")==0)
                dirIfNotFound = 0.0;
            else if (strcmp(pArgStr, "down")==0)
                dirIfNotFound = -1.0;
            else if (strcmp(pArgStr, "up")==0)
                dirIfNotFound = 1.0;
            else if (strcmp(pArgStr, "closest")==0)
                dirIfNotFound = 2.0;
            else if (strcmp(pArgStr, "frac")==0)
                dirIfNotFound = 0.5;
            else 
                mexErrMsgTxt("Input 4 (dirIfNotFound), if input as a string, must be either 'exact', 'down', 'up', 'closest', or 'frac'");
            
            mxFree(pArgStr);  // free up memory used by mxArrayToString
        } else {  
            if (!mxIsNumeric(pArg) || mxIsComplex(pArg) || mxGetN(pArg)*mxGetM(pArg) != 1) {
                mexErrMsgTxt("Input 4 (dirIfNotFound) must be a real scalar"); 
            } 
            dirIfNotFound = mxGetScalar(prhs[3]);

            if (!  ((dirIfNotFound == -1) || (dirIfNotFound == 0) || (dirIfNotFound == 0.5) || (dirIfNotFound == 1) || (dirIfNotFound == 2)) )
                mexErrMsgTxt("Input 4 (dirIfNotFound) must be either -1, 0, 1, 2, or 0.5");
        }
    }

    checkIfSorted = CHECK_IF_INPUT_SORTED_DEFAULT;
    if (nrhs == 5) {
        if (!mxIsEmpty(prhs[4]))
            checkIfSorted = 1;
    }

    if (checkIfSorted) {
        if (!issorted(pdData, N)) 
            mexErrMsgTxt("Input 1 (data) must be sorted.");
    }
   
    /// ------------------- pos (OUTPUT)----------    
    numDims = mxGetNumberOfDimensions(prhs[1]);  // this outputs with the same dimensions as the input
    dims = mxGetDimensions(prhs[1]);    
    plhs[0] = mxCreateNumericArray(numDims, dims, mxDOUBLE_CLASS, mxREAL);
        
    pdPos = mxGetPr(plhs[0])-1;

    // CALL BINARY SEARCH FUNCTION;
    if (dataIsDouble) {
        for (i=1; i<=nItems; i++) {
            if (itemIsDouble) {
                dItem = pdItems[i];
            } else {
                dItem = (double) pfItems[i];
            }
            
            pdPos[i] = binarySearch_double(pdData, N, dItem, dirIfFound, dirIfNotFound);
        }
    } else {
        for (i=1; i<=nItems; i++) {
            
            if (itemIsDouble) {
                fItem = (float) pdItems[i];
            } else {
                fItem = pfItems[i];                
            }
            
            pdPos[i] = binarySearch_float(pfData, N, fItem, dirIfFound, dirIfNotFound);
        }                
    }

}
