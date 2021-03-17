#include "mex.h"
#include "matrix.h"
#include <stdio.h>
#include <math.h>
#include <string.h>

// USAGE: [cutouts, image_subtracted] = mexCutout(image, positions, cut_size, pad_value=0, replace_value=0, debug_bit=0, use_memcpy=1\n"); 

int low_corner(int center, int size){
	
	int half_size = size/2;
	
	return center-half_size;
	
}

void mexFunction( int nlhs, mxArray *plhs[],
                  int nrhs, const mxArray *prhs[] ){

    if(nrhs<3){ printf("USAGE: [cutouts, image_subtracted] = mexCutout(image, positions, cut_size, pad_value=0, replace_value=0, debug_bit=0, use_memcpy=1\n"); return; }
	
	// check argument 0 
	if(mxIsNumeric(prhs[0])==0 && mxIsLogical(prhs[0])==0) mexErrMsgIdAndTxt("MATLAB:util:img:mexCutout:inputNotNumeric", "Input 1 to mexCutout is not numeric nor logical!");
	if(mxIsEmpty(prhs[0])){ // empty input matrix just returns an empty matrix
		plhs[0]=mxDuplicateArray(prhs[0]);
		if(nlhs>1) plhs[1]=mxDuplicateArray(prhs[0]);
		return; 
	}
	if( mxGetN(prhs[0])<2 || mxGetM(prhs[0])<2 || mxGetNumberOfDimensions(prhs[0])>3 ) mexErrMsgIdAndTxt("MATLAB:util:img:mexCutout:inputNotMatrix", "Must input a (2D or 3D) matrix to mexCutout!");

	// check argument 1
	if(mxIsNumeric(prhs[1])==0 || mxIsDouble(prhs[1])==0) mexErrMsgIdAndTxt("MATLAB:util:img:mexCutout:inputNotNumeric", "Input 2 to mexCutout is not numeric double precision...");
	if( mxGetN(prhs[1])!=2) mexErrMsgIdAndTxt("MATLAB:util:img:mexCutout:inputWrongSize", "Second input must be a Nx2 matrix of x and y center positions");
	
	// check argument 2
	if(mxIsNumeric(prhs[2])==0) mexErrMsgIdAndTxt("MATLAB:util:img:mexCutout:inputNotNumeric", "Input 3 to mexCutout is not numeric...");
	int cut_size=(int) mxGetScalar(prhs[2]);
	
	// check argument 3
	double pad_value=0;
	if(nrhs>3 && mxIsEmpty(prhs[3])==0){
		if(mxIsNumeric(prhs[3])==0) mexErrMsgIdAndTxt("MATLAB:util:img:mexCutout:inputNotNumeric", "Input 4 to mexCutout is not numeric...");
		pad_value=mxGetScalar(prhs[3]);
	}
	
	// check argument 4
	double replace_value=0;
	if(nrhs>4 && mxIsEmpty(prhs[4])==0){
		if(mxIsNumeric(prhs[4])==0) mexErrMsgIdAndTxt("MATLAB:util:img:mexCutout:inputNotNumeric", "Input 5 to mexCutout is not numeric...");
		replace_value=mxGetScalar(prhs[4]);
	}
	
	// check argument 5
	int debug_bit=0;
	if(nrhs>5 && mxIsEmpty(prhs[5])==0){
		if(mxIsNumeric(prhs[5])==0) mexErrMsgIdAndTxt("MATLAB:util:img:mexCutout:inputNotNumeric", "Input 6 to mexCutout is not numeric...");
		debug_bit=(int) mxGetScalar(prhs[5]);
	}
	
	// check argument 6
	int use_memcpy=0;
	if(nrhs>6 && mxIsEmpty(prhs[6])==0){
		if(mxIsNumeric(prhs[6])==0) mexErrMsgIdAndTxt("MATLAB:util:img:mexCutout:inputNotNumeric", "Input 7 to mexCutout is not numeric...");
		use_memcpy=(int) mxGetScalar(prhs[6]);
	}
	
	if(debug_bit) mexPrintf("pad_value= %f | replace_value= %f | debug_bit= %d | use_memcpy= %d\n", pad_value, replace_value, debug_bit, use_memcpy);
		
	size_t rows=mxGetM(prhs[0]);
	size_t cols=mxGetN(prhs[0]);
	size_t pages = 1;
	if(mxGetNumberOfDimensions(prhs[0])>2){
		
		const mwSize *dims=mxGetDimensions(prhs[0]);
		rows=dims[0];
		cols=dims[1];
		pages=dims[2];
		
	}
	
	if(debug_bit) mexPrintf("rows= %d | cols= %d | pages= %d\n", rows, cols, pages);
	
	size_t num_cuts=mxGetM(prhs[1]);
	
	// get the cut positions into an array:
	double *pos_ptr=(double*) mxGetPr(prhs[1]);
	int *pos[2];
	
	for(int j=0; j<2; j++){
		
		pos[j]= (int*) mxCalloc(num_cuts, sizeof(int));
		
	}// for j
	
	for(int i=0;i<num_cuts; i++){
	
		for(int j=0; j<2; j++){
			
			pos[j][i]=(int) round(pos_ptr[num_cuts*j+i])-1; // minus one for conversion btw matlab indices and C indices
			
		}// for j
		
		if(debug_bit>2) mexPrintf("pos: %d %d\n", pos[0][i], pos[1][i]);
			
	}// for ii
	
	if(debug_bit) mexPrintf("input class: %s\n", mxGetClassName(prhs[0]));
	const size_t out_dims[4]={cut_size, cut_size, pages, num_cuts};
	
	unsigned char pad_value_bytes[8]={0}; // can contain any value type 
	unsigned char replace_value_bytes[8]={0}; // can contain any value type 
	
	int num_bytes=0; // how many bytes per pixel
	
	if(mxIsClass(prhs[0], "uint16")){
		if(isnan(pad_value) || pad_value<0) pad_value=0; // cannot have NaN or negative values in the uint16 matrix
		if(isnan(replace_value) || replace_value<0) replace_value=0; // cannot have NaN or negative values in the uint16 matrix
		num_bytes=sizeof(unsigned short int);
		plhs[0]=mxCreateNumericArray(4, out_dims, mxUINT16_CLASS, mxREAL);
		unsigned short int pad_value_uint16=(unsigned short int) pad_value;
		memcpy(&pad_value_bytes, &pad_value_uint16, num_bytes); // only copy the first 2 bytes into this array... 		
		unsigned short int replace_value_uint16=(unsigned short int) replace_value;
		memcpy(&replace_value_bytes, &replace_value_uint16, num_bytes); // only copy the first 2 bytes into this array... 
		
	}
	else if(mxIsClass(prhs[0], "double")){
		num_bytes=sizeof(double);
		plhs[0]=mxCreateNumericArray(4, out_dims, mxDOUBLE_CLASS, mxREAL);
		double pad_value_double=(double) pad_value;
		memcpy(&pad_value_bytes, &pad_value_double, num_bytes); // copy all 8 bytes into this array... 		
		double replace_value_double=(double) replace_value;
		memcpy(&replace_value_bytes, &replace_value_double, num_bytes); // copy all 8 bytes into this array... 

	}
	else if(mxIsClass(prhs[0], "single")){
		num_bytes=sizeof(float);
		plhs[0]=mxCreateNumericArray(4, out_dims, mxSINGLE_CLASS, mxREAL);
		float pad_value_float=(float) pad_value;
		memcpy(&pad_value_bytes, &pad_value_float, num_bytes); // only copy the first 4 bytes into this array... 		
		float replace_value_float=(float) replace_value;
		memcpy(&replace_value_bytes, &replace_value_float, num_bytes); // only copy the first 4 bytes into this array... 
	}
	else if(mxIsLogical(prhs[0])){
		if(isnan(pad_value) || pad_value<0) pad_value=0; // cannot have NaN or negative values in the boolean matrix
		if(isnan(replace_value) || replace_value<0) replace_value=0; // cannot have NaN or negative values in the boolean matrix
		num_bytes=sizeof(bool);
		plhs[0]=mxCreateLogicalArray(4,out_dims);
		bool pad_value_bool=(bool) pad_value;
		memcpy(&pad_value_bytes, &pad_value_bool, num_bytes);
		bool replace_value_bool=(bool) replace_value;
		memcpy(&replace_value_bytes, &replace_value_bool, num_bytes);
	}
	else{
		mexErrMsgIdAndTxt("MATLAB:util:img:mexCutout:inputTypeNotRecongnized", "Input must be uint16 or double or single...");
    }
	
	
	// C type arrays hold the data from the input and for the output:
	unsigned char *in_array=(unsigned char*) mxGetData(prhs[0]);
	// unsigned char *out_array=(unsigned char*) mxCalloc(cut_size*cut_size*pages*num_cuts, num_bytes); 
	unsigned char *out_array=(unsigned char *) mxGetData(plhs[0]);
	
	// start by initializing everything to the pad value:
	if(pad_value_bytes) for(int c=0; c<num_cuts; c++) for(int p=0; p<pages; p++) for(int j=0; j<cut_size; j++) for(int i=0; i<cut_size; i++) // use the one-by-one method for non-zero pad values
		memcpy(&out_array[(c*cut_size*cut_size*pages+p*cut_size*cut_size+j*cut_size+i)*num_bytes], &pad_value_bytes, num_bytes); // copy num_bytes to each location
	
	unsigned char *src=0;
	unsigned char *dst=0;
	int N=0;
		
	// copy the right part of the input matrix
	for(int c=0; c<num_cuts; c++){
	
		int x1= low_corner(pos[0][c], cut_size);
		int y1= low_corner(pos[1][c], cut_size);
		int cut_size_x = cut_size;
		int cut_size_y = cut_size;
			
		int push_x=0;
		int push_y=0;
			
		// make sure x/y values are not negative
		if(x1<0) push_x=-x1;
		if(y1<0) push_y=-y1;
			
		// make sure x/y values are not larger than array boundary
		if(cut_size_x+x1+push_x>=cols) cut_size_x=cols-x1;
		if(cut_size_y+y1+push_y>=rows) cut_size_y=rows-y1;
		
		for(int p=0; p<pages; p++) for(int j=push_x; j<cut_size_x; j++){
			
			src=&in_array[(rows*cols*p+x1*rows+y1+push_y+j*rows)*num_bytes]; // pointer to the start of the line in the source matrix
			dst=&out_array[(c*cut_size*cut_size*pages+p*cut_size*cut_size+j*cut_size+push_y)*num_bytes]; // pointer to the start of the line in the destination cutout
			N=(cut_size_y-push_y)*num_bytes; // how many bytes need to be copied...
			
			if(use_memcpy) memcpy(dst, src, N); // use the new version of the code (default) based on memcpy
			else for(int i=push_y; i<cut_size_y; i++) memcpy(&dst[i*num_bytes], &src[i*num_bytes], num_bytes); // old code (for benchmarking) goes over data one-by-one (copy num_bytes in each location)
			
		}
		
	}

	// mxSetData(plhs[0], out_array);
	
	if(nlhs>1){ // replace the INPUT matrix values where the cutouts were with some filler (zero or NaN usually)
		plhs[1]=mxDuplicateArray(prhs[0]); // make a deep copy and remove stars from that
		unsigned char *out_array2=(unsigned char*) mxGetData(plhs[1]);
		
		for(int c=0; c<num_cuts; c++){
			
			int x1= low_corner(pos[0][c], cut_size);
			int y1= low_corner(pos[1][c], cut_size);
			int cut_size_x = cut_size;
			int cut_size_y = cut_size;
			
			int push_x=0;
			int push_y=0;
			
			// make sure x/y values are not negative
			if(x1<0) push_x=-x1;
			if(y1<0) push_y=-y1;
			
			// make sure x/y values are not larger than array boundary
			if(cut_size_x+x1+push_x>=cols) cut_size_x=cols-x1;
			if(cut_size_y+y1+push_y>=rows) cut_size_y=rows-y1;
		
			for(int p=0; p<pages; p++) for(int j=push_x; j<cut_size_x; j++){
				
				src=&out_array2[(rows*cols*p+x1*rows+y1+push_y+j*rows)*num_bytes]; // pointer to the start of the line in the source matrix
				N=(cut_size_y-push_y)*num_bytes; // how many bytes need to be copied...
				
				if(replace_value_bytes==0) memset(src, 0, N); // for zero use quick replace (memset)
				else for(int i=push_y; i<cut_size_y; i++) memcpy(&src[i*num_bytes], &replace_value_bytes, num_bytes); // for non-zero must go over memory locations one-by-one (copy num_bytes in each location)
			}
		}
				
	}
	
	for(int j=0; j<2; j++) mxFree(pos[j]);
	
}