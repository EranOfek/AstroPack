#include "mex.h"
#include "matrix.h"
#include <stdio.h>
#include <math.h>
#include <string.h>

// USAGE: [cutouts, image_subtracted] = mexCutout(image, positions, cut_size, pad_value=0, replace_value=0, debug_bit=0, use_memcpy=1\n"); 
//
//
// Crash test:
//      load III.mat;  MaxRadius = 12;
//      [Cube] = imUtil.cut.mexCutout(Image,double([X, Y]),MaxRadius.*2+1);
//
//      a = reshape(1:36,[6,6]);   pos = double([0, 0]);
//      [Cube] = imUtil.cut.mexCutout(a, pos, 36);
//===========================================================================

void msglog(const char* s)
{
    mexPrintf("%s\n", s);
    FILE* fp = fopen("mexCoutout.log", "a");
    if (fp) {
        fwrite(s, 1, strlen(s), fp);
        fwrite("\n", 1, 1, fp);
        fclose(fp);
    }
}

//===========================================================================
// Calculate position of low corner
int low_corner(int center, int size)
{
	int half_size = size/2;
	return center - half_size;
}
//===========================================================================
void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[])
{
    char s[256];
    
    sprintf(s, "\nmexFunction: nlhs = %d, nrhs = %d", nlhs, nrhs);
    msglog(s);
            
    // Validate number of arguments
    if (nrhs < 3) {
		printf("USAGE: [cutouts, image_subtracted] = mexCutout(image, positions, cut_size, pad_value=0, replace_value=0, debug_bit=0, use_memcpy=1\n"); 
		return; 
	}
	
	// Check argument 0 (image)
	if (mxIsNumeric(prhs[0])==0 && mxIsLogical(prhs[0])==0) {
		mexErrMsgIdAndTxt("MATLAB:util:img:mexCutout:inputNotNumeric", "Input 1 to mexCutout is not numeric nor logical!");
	}
	
    // Empty input matrix just returns an empty matrix
	if (mxIsEmpty(prhs[0])) { 
		plhs[0] = mxDuplicateArray(prhs[0]);
		if (nlhs > 1) {
			plhs[1] = mxDuplicateArray(prhs[0]);
		}
		return; 
	}
	
	if (mxGetN(prhs[0] )< 2 || mxGetM(prhs[0]) < 2 || mxGetNumberOfDimensions(prhs[0]) > 3) {
        mexErrMsgIdAndTxt("MATLAB:util:img:mexCutout:inputNotMatrix", "Must input a (2D or 3D) matrix to mexCutout!");
    }

	// Check argument 1 (positions)
	if (mxIsNumeric(prhs[1])==0 || mxIsDouble(prhs[1])==0) {
		mexErrMsgIdAndTxt("MATLAB:util:img:mexCutout:inputNotNumeric", "Input 2 to mexCutout is not numeric double precision...");
	}
	
	if (mxGetN(prhs[1]) != 2) {
		mexErrMsgIdAndTxt("MATLAB:util:img:mexCutout:inputWrongSize", "Second input must be a Nx2 matrix of x and y center positions");
	}
	
	// Check argument 2 (cut_size)
	if (mxIsNumeric(prhs[2])==0) {
		mexErrMsgIdAndTxt("MATLAB:util:img:mexCutout:inputNotNumeric", "Input 3 to mexCutout is not numeric...");
	}
	
    // Get cut_size
	int cut_size = (int)mxGetScalar(prhs[2]);
    
    // cut_size must be odd
    if (cut_size % 2 != 1) {
        mexErrMsgIdAndTxt("MATLAB:util:img:mexCutout:inputWrongSize", "cut_size (argument 3) must be ODD value...");
        return;
    }
	
	// Check argument 3 (pad_value)
	double pad_value = 0;
	if (nrhs > 3 && mxIsEmpty(prhs[3])==0) {
		if (mxIsNumeric(prhs[3])==0) {
			mexErrMsgIdAndTxt("MATLAB:util:img:mexCutout:inputNotNumeric", "Input 4 to mexCutout is not numeric...");
		}
		pad_value = mxGetScalar(prhs[3]);
	}
	
	// Check argument 4 (replace_value)
	double replace_value = 0;
	if(nrhs > 4 && mxIsEmpty(prhs[4])==0){
		if (mxIsNumeric(prhs[4])==0) {
			mexErrMsgIdAndTxt("MATLAB:util:img:mexCutout:inputNotNumeric", "Input 5 to mexCutout is not numeric...");
		}
		replace_value = mxGetScalar(prhs[4]);
	}
	
	// Check argument 5 (debug_bit)
	int debug_bit = 0;
	if (nrhs>5 && mxIsEmpty(prhs[5])==0){
		if (mxIsNumeric(prhs[5])==0) {
			mexErrMsgIdAndTxt("MATLAB:util:img:mexCutout:inputNotNumeric", "Input 6 to mexCutout is not numeric...");
		}
		debug_bit = (int)mxGetScalar(prhs[5]);
	}
	
	// Check argument 6 (use_memcpy)
	int use_memcpy = 0;
	if (nrhs > 6 && mxIsEmpty(prhs[6])==0){
		if (mxIsNumeric(prhs[6])==0) {
			mexErrMsgIdAndTxt("MATLAB:util:img:mexCutout:inputNotNumeric", "Input 7 to mexCutout is not numeric...");
		}
		use_memcpy = (int)mxGetScalar(prhs[6]);
	}
	
    sprintf(s, "pad_value: %f, replace_value: %f, debug_bit: %d, use_memcpy: %d", pad_value, replace_value, debug_bit, use_memcpy);    
    msglog(s);
    
	if (debug_bit) {
		mexPrintf("pad_value= %f | replace_value= %f | debug_bit= %d | use_memcpy= %d\n", pad_value, replace_value, debug_bit, use_memcpy);
	}
	
	size_t rows = mxGetM(prhs[0]);
	size_t cols = mxGetN(prhs[0]);
	size_t pages = 1;
    
    // Check if input image is more than 2D (assumed to be 3D ?)
	if (mxGetNumberOfDimensions(prhs[0]) > 2) {        		
		const mwSize *dims=mxGetDimensions(prhs[0]);
		rows = dims[0];
		cols = dims[1];
		pages = dims[2];		
	}
	
	if (debug_bit) {
		mexPrintf("rows= %d | cols= %d | pages= %d\n", rows, cols, pages);
	}
    
    sprintf(s, "rows: %d, cols: %d", (int)rows, (int)cols);   
    msglog(s);
	
    // Get num_cuts which is the number of rows in positions[] argument
	size_t num_cuts = mxGetM(prhs[1]);
	
	// Get the cut positions into an array:
	double *pos_ptr = (double*) mxGetPr(prhs[1]);
	int *pos[2];
	
    // Allocate pos[0] and pos[1] - the x,y of each cut
	for(int j=0;  j < 2;  j++) {	
		pos[j]= (int*) mxCalloc(num_cuts, sizeof(int));		
	}
	
    // Prepare pos
    sprintf(s, "Preparing pos, num_cuts: %d", (int)num_cuts);   
    msglog(s);
    
	for (int i=0; i < num_cuts; i++){
	
        // Minus one for conversion btw matlab indices and C indices
		for(int j=0; j < 2; j++) {
			pos[j][i] = (int)round(pos_ptr[num_cuts*j+i])-1; 
		}
		
		if (debug_bit > 2) {
			mexPrintf("pos: %d %d\n", pos[0][i], pos[1][i]);
		}			
	}
	
	if (debug_bit) {
		mexPrintf("input class: %s\n", mxGetClassName(prhs[0]));
	}
	
    // Prepare output matrix
	const size_t out_dims[4] = { (size_t)cut_size, (size_t)cut_size, (size_t)pages, (size_t)num_cuts };
    
    // 
    size_t num_in_elements = (size_t)(rows * cols * pages);
    size_t num_out_elements = (size_t)(cut_size * cut_size * pages * num_cuts);    
	sprintf(s, "out_dims: %d, %d, %d, %d, total elements: %d", (int)cut_size, (int)cut_size, (int)pages, (int)num_cuts, (int)num_out_elements);
    msglog(s);
    
	unsigned char pad_value_bytes[8] = {0};       // can contain any value type 
	unsigned char replace_value_bytes[8] = {0};   // can contain any value type 	
	int num_bytes = 0;                            // bytes per pixel
	
    // Input image is uint16
	if (mxIsClass(prhs[0], "uint16")){
        
        // Cannot have NaN or negative values in the uint16 matrix
		if (isnan(pad_value) || pad_value < 0) {
			pad_value=0; 
		}
		
        // Cannot have NaN or negative values in the uint16 matrix
		if (isnan(replace_value) || replace_value < 0) { 
			replace_value=0; 
		}
		
		num_bytes = sizeof(unsigned short int);        
		plhs[0] = mxCreateNumericArray(4, out_dims, mxUINT16_CLASS, mxREAL);
        sprintf(s, "Allocated output matrix of mxUINT16_CLASS: %p", plhs[0]);  
        msglog(s);
        
        // Only copy the first 2 bytes into this array... 		
		unsigned short int pad_value_uint16 = (unsigned short int) pad_value;
		memcpy(&pad_value_bytes, &pad_value_uint16, num_bytes); 
        
        // Only copy the first 2 bytes into this array... 
		unsigned short int replace_value_uint16=(unsigned short int) replace_value;
		memcpy(&replace_value_bytes, &replace_value_uint16, num_bytes); 		
	}
    
    // Input image is double
	else if (mxIsClass(prhs[0], "double")){
		num_bytes = sizeof(double);        
		plhs[0] = mxCreateNumericArray(4, out_dims, mxDOUBLE_CLASS, mxREAL);
        sprintf(s, "Allocated output matrix of mxDOUBLE_CLASS: %p", plhs[0]);  
        msglog(s);
        
        // Copy all 8 bytes into this array... 		
		double pad_value_double = (double)pad_value;
		memcpy(&pad_value_bytes, &pad_value_double, num_bytes); 
        
        // Copy all 8 bytes into this array... 
		double replace_value_double = (double) replace_value;
		memcpy(&replace_value_bytes, &replace_value_double, num_bytes); 
	}
    
    // Input image is single
	else if (mxIsClass(prhs[0], "single")){
		num_bytes = sizeof(float);
		plhs[0] = mxCreateNumericArray(4, out_dims, mxSINGLE_CLASS, mxREAL);
        sprintf(s, "Allocated output matrix of mxSINGLE_CLASS: %p", plhs[0]);  
        msglog(s);
        
        // Only copy the first 4 bytes into this array... 		
		float pad_value_float = (float)pad_value;
		memcpy(&pad_value_bytes, &pad_value_float, num_bytes); 
       
        // Only copy the first 4 bytes into this array... 
		float replace_value_float = (float)replace_value;
		memcpy(&replace_value_bytes, &replace_value_float, num_bytes); 
	}
    
    // Input image is bool
	else if(mxIsLogical(prhs[0])){        
        // Cannot have NaN or negative values in the boolean matrix
		if (isnan(pad_value) || pad_value < 0) {
			pad_value=0; 
		}
		
        // Cannot have NaN or negative values in the boolean matrix
		if (isnan(replace_value) || replace_value < 0) {
			replace_value=0; 
		}
		
		num_bytes = sizeof(bool);
		plhs[0] = mxCreateLogicalArray(4, out_dims);
        sprintf(s, "Allocated output matrix of LOGICAL: %p", plhs[0]);  
        msglog(s);
        
		bool pad_value_bool = (bool)pad_value;
		memcpy(&pad_value_bytes, &pad_value_bool, num_bytes);
        
		bool replace_value_bool = (bool)replace_value;
		memcpy(&replace_value_bytes, &replace_value_bool, num_bytes);
	}
    
    // Input image is not supported
	else {
		mexErrMsgIdAndTxt("MATLAB:util:img:mexCutout:inputTypeNotRecongnized", "Input must be uint16 or double or single...");
    }
	
	sprintf(s, "Allocated output array: %d bytes", (int)(num_out_elements * num_bytes));
    msglog(s);
   
    
	// C type arrays hold the data from the input and for the output:
	unsigned char *in_array = (unsigned char*) mxGetData(prhs[0]);
    unsigned char *in_array_end = in_array + (num_in_elements * num_bytes);
    
	// unsigned char *out_array=(unsigned char*) mxCalloc(cut_size*cut_size*pages*num_cuts, num_bytes); 
	unsigned char *out_array = (unsigned char *) mxGetData(plhs[0]);
    unsigned char *out_array_end = out_array + (num_out_elements * num_bytes);
	
    sprintf(s, "in_array:  %p, size: %d, end: %p", (void*)in_array,  (num_in_elements * num_bytes), (void*)in_array_end);   msglog(s);
    sprintf(s, "out_array: %p, size: %d, end: %p", (void*)out_array, (num_out_elements * num_bytes), (void*)out_array_end);  msglog(s);    
        
	// start by initializing everything to the pad value:
	if (pad_value_bytes) {
        msglog("Copying pad values");
		for (int c=0; c < num_cuts; c++) {
			for (int p=0; p < pages; p++) {
				for (int j=0; j < cut_size; j++) {
					for (int i=0; i < cut_size; i++) { 
                        // Use the one-by-one method for non-zero pad values
                        // Copy num_bytes to each location
						memcpy(&out_array[(c*cut_size*cut_size*pages+p*cut_size*cut_size+j*cut_size+i)*num_bytes], &pad_value_bytes, num_bytes); 
					}
				}
			}
		}
        msglog("Passed initializing pad");        
    } 
    
	unsigned char *src = 0;
	unsigned char *dst = 0;
	int N = 0;
		
	// Copy the right part of the input matrix
    sprintf(s, "Copying input matrix: num_cuts=%d, cut_size: %d, pages=%d, use_memcpy=%d, num_bytes=%d", (int)num_cuts, (int)cut_size, (int)pages, (int)use_memcpy, (int)num_bytes);
    msglog(s);
    
	for (int c=0; c < num_cuts;  c++){
	
		int x1 = low_corner(pos[0][c], cut_size);
		int y1 = low_corner(pos[1][c], cut_size);
		int cut_size_x = cut_size;
		int cut_size_y = cut_size;			
		int push_x = 0;
		int push_y = 0;
			
		// Make sure x/y values are not negative
		if (x1 < 0) {            
            cut_size_x += x1;
            push_x = 0;
			//push_x = -x1;
            //sprintf(s, "NEG push_x: %d", push_x);  msglog(s);
        }		
		if (y1 < 0) {            
            cut_size_y += y1;
            push_y = 0;
			//push_y = -y1;
            //sprintf(s, "NEG push_y: %d", push_y);  msglog(s);
        }
			        
		// Make sure x/y values are not larger than array boundary
		if (cut_size_x + x1 + push_x >= cols) {
			//cut_size_x = cols - x1;
		}
		
		if (cut_size_y + y1 + push_y >= rows) {
			//cut_size_y = rows - y1;
		}
		
		for (int p=0;  p < pages;  p++) {
            
            // X Axis
            for (int j=push_x;  j < cut_size_x;  j++) {
                
                // pointer to the start of the line in the source matrix			
                src = &in_array[ ((rows * cols * p) + (x1 * rows) + (j * rows) + y1 + push_y) * num_bytes ];
                
                // pointer to the start of the line in the destination cutout                
                dst = &out_array[ ((c * cut_size * cut_size * pages) + (p * cut_size * cut_size) + (j * cut_size) + push_y) * num_bytes ];
                
                // how many bytes need to be copied...
                N = (cut_size_y-push_y) * num_bytes; 
			
                
                // Y-Axis: Use the new version of the code (default) based on memcpy
                if (use_memcpy) {
                    memcpy(dst, src, N); 
                }
                
                // Y-Axis: Old code (for benchmarking) goes over data one-by-one (copy num_bytes in each location)                
                else {
                    for (int i=push_y;  i < cut_size_y;  i++) {
    					//memcpy(&dst[i*num_bytes], &src[i*num_bytes], num_bytes); 
                        
                        // @Chen: Debug only, copy always to the same cut
                        unsigned char *a = &src[i * num_bytes];
                        if (a >= in_array && a < in_array_end) {
                            sprintf(s, "src OK          : c=%d, p=%d, j=%d, i=%d, N=%d, cut_size_x: %d, cut_size_y: %d, push_x: %d, push_y: %d, ptr: %p", c, p, j, i, N, cut_size_x, cut_size_y, push_x, push_y, (void*)a);
                            msglog(s);
                        }
                        else {
                            sprintf(s, "src out of range: c=%d, p=%d, j=%d, i=%d, N=%d, cut_size_x: %d, cut_size_y: %d, push_x: %d, push_y: %d, ptr: %p", c, p, j, i, N, cut_size_x, cut_size_y, push_x, push_y, (void*)a);
                            msglog(s);
                        }
                        
                        unsigned char *b = &dst[i * num_bytes];
                        if (b >= out_array && b < out_array_end) {
                            sprintf(s, "dst OK          : c=%d, p=%d, j=%d, i=%d, N=%d, cut_size_x: %d, cut_size_y: %d, push_x: %d, push_y: %d, ptr: %p", c, p, j, i, N, cut_size_x, cut_size_y, push_x, push_y, (void*)b);
                            msglog(s);                            
                            memcpy(&dst[i*num_bytes], &src[i*num_bytes], num_bytes); 
                        }
                        else {
                            sprintf(s, "dst out of range: c=%d, p=%d, j=%d, i=%d, N=%d, cut_size_x: %d, cut_size_y: %d, push_x: %d, push_y: %d, ptr: %p", c, p, j, i, N, cut_size_x, cut_size_y, push_x, push_y, (void*)b);
                            msglog(s);
                        }
    				}
    			}
    		}		
    	}
    }
    
    msglog("Copying input matrix done");
        
	// mxSetData(plhs[0], out_array);
	
    // Replace the INPUT matrix values where the cutouts were with some filler (zero or NaN usually)    
	if (nlhs > 1) { 
        msglog("Replacing the INPUT matrix...");
        
        // Make a deep copy and remove stars from that
		plhs[1] = mxDuplicateArray(prhs[0]); 
		unsigned char *out_array2 = (unsigned char*) mxGetData(plhs[1]);
		
		for (int c=0;  c < num_cuts;  c++) {			
			int x1 = low_corner(pos[0][c], cut_size);
			int y1 = low_corner(pos[1][c], cut_size);
			int cut_size_x = cut_size;
			int cut_size_y = cut_size;			
			int push_x=0;
			int push_y=0;
			
			// Make sure x/y values are not negative
			if (x1 < 0) {
				push_x = -x1;
			}			
			if (y1 < 0) {
				push_y = -y1;
			}
			
			// Make sure x/y values are not larger than array boundary
			if (cut_size_x+x1+push_x >= cols) {
				cut_size_x = cols-x1;
			}			
			if (cut_size_y+y1+push_y >=rows) {
				cut_size_y = rows-y1;
			}
			
			for (int p=0;  p < pages;  p++) {
				for (int j=push_x; j<cut_size_x; j++) {
				 
                    // pointer to the start of the line in the source matrix
                    src = &out_array2[(rows*cols*p+x1*rows+y1+push_y+j*rows)*num_bytes]; 
                    
                    // how many bytes need to be copied...
                    N = (cut_size_y-push_y)*num_bytes; 
				
                    // for zero use quick replace (memset)
                    if (replace_value_bytes==0) {
    					memset(src, 0, N); 
                    }
    				else {
                        
                        // for non-zero must go over memory locations one-by-one (copy num_bytes in each location)
    					for (int i=push_y; i<cut_size_y; i++) {
    						memcpy(&src[i*num_bytes], &replace_value_bytes, num_bytes); 
    					}
                    }
				}
			}
		}				
            
        msglog("Replacing the INPUT matrix done");
	}

    msglog("Free pos");        
	for (int j=0; j < 2; j++) {
		mxFree(pos[j]);
	}
        
    msglog("Done");
}
