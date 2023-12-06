#include "mex.h"
#include "matrix.h"

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <thread>

#define MAX_NUM_THREADS 20

/*

This function does the heavy lifting 
of adding together rows with the correct shifts. 
It leaves to MATLAB the job of creating matrices 
in memory in the outside loop, as well as sending
partial Radon matrices to the Finder, if needed. 

This has a multithread mode but tests on my computers
didn't show any substantial speed-up. 
Probably due to the fact that fetching rows in memory
takes more time than actually adding them. 
Right now it is faster to just split a batch of images
to different processes and do streak detection on each 
sub-batch in parallel. 

*/

// function prototypes
void shift_add(double *line_out, double *line_prev1, double *line_prev2, int N, int gap); 
void add_lines(double *M_out, double *M_prev, int *output_size, int *input_size, double *dy, int ij_start, int ij_end, int debug_level=0);

void mexFunction( int nlhs, mxArray *plhs[],
                  int nrhs, const mxArray *prhs[] ){
					  
	if(nrhs<3){ printf("USAGE: coreFRT(M_out, M_prev, dy, num_threads=0, debug_level=0)\n"); return; }

	// get the output (zeros) matrix and read the dimensions
	double *M_out=(double*) mxGetData(prhs[0]);
	mwSize M_out_ndims=mxGetNumberOfDimensions(prhs[0]);
	const mwSize *M_out_dims=mxGetDimensions(prhs[0]);
	
	int output_size[3]={1,1,1};
	for(int i=0;i<M_out_ndims;i++) output_size[i]=(int) M_out_dims[i];
	
	// get the input (previous) matrix and read the dimensions
	double *M_prev=(double*) mxGetData(prhs[1]);
	mwSize M_prev_ndims=mxGetNumberOfDimensions(prhs[1]);
	const mwSize *M_prev_dims=mxGetDimensions(prhs[1]);
	
	int input_size[3]={1,1,1};
	for(int i=0;i<M_prev_ndims;i++) input_size[i]=(int) M_prev_dims[i];

	// get the list of shifts we need to fill out for the output matrix
	double *dy=(double*) mxGetData(prhs[2]);
	int Ndy=(int) mxGetN(prhs[2]); // the length of dy's we need to go over (j loop)
	
	// if not empty, read the requested number of threads
	int num_threads=0;
	if(nrhs>3 && mxIsEmpty(prhs[3])==0){
		if(mxIsScalar(prhs[3])==0) mexErrMsgIdAndTxt("MATLAB:radon:coreFRT:input_not_scalar", "num_threads input must be scalar!");
		num_threads=(int) mxGetScalar(prhs[3]);	
	}
	
	int debug_level=0;
	if(nrhs>4 && mxIsEmpty(prhs[4])==0){
		if(mxIsScalar(prhs[4])==0) mexErrMsgIdAndTxt("MATLAB:radon:coreFRT:input_not_scalar", "debug_level input must be scalar!");
		debug_level=(int) mxGetScalar(prhs[4]);			
	}
	
	// input verification...
	if(output_size[0]!=input_size[0]) mexErrMsgIdAndTxt("MATLAB:radon:coreFRT:size_mismatch_dim1", "input and output first dimension must match!");
	if(output_size[1]*2!=input_size[1]) mexErrMsgIdAndTxt("MATLAB:radon:coreFRT:size_mismatch_dim2", "input and output second dimension must be 2 times bigger!");
	if(output_size[2]!=Ndy) mexErrMsgIdAndTxt("MATLAB:radon:coreFRT:size_mismatch_ndy", "output third dimension does not match Ndy!");
	
	int Npassive=output_size[0]; // image size on the shared/passive dimension
	
	if(debug_level>1){
		printf("input size is %dx%dx%d\n", input_size[0], input_size[1], input_size[2]);
		printf("output size is %dx%dx%d\n", output_size[0], output_size[1], output_size[2]);
		printf("length of dy is %d \n", Ndy);
	}
	
	if(num_threads<1){// single thread execution
	
		add_lines(M_out, M_prev, output_size, input_size, dy, 0, output_size[1]*output_size[2], debug_level);
		
	}
	else{ // the multithreaded case
		
		double total_lines=(double) output_size[1]*output_size[2];
		int num_lines_per_thread = ceil(total_lines/num_threads);

		if(debug_level>0) printf("dividing %d lines between %d threads, each getting %d lines...\n", (int) total_lines, num_threads, num_lines_per_thread);
		
		std::thread thread_array[MAX_NUM_THREADS];
		
		if(num_threads>MAX_NUM_THREADS){ num_threads=MAX_NUM_THREADS; printf("Number of threads requested (%d) exceeds built-in maximum (%d)\n", num_threads, MAX_NUM_THREADS); }
		
		int line=0;
		
		for(int t=0;t<num_threads;t++){
						
			int line_end=line+num_lines_per_thread;
			if(line_end>=total_lines) line_end=total_lines; // round down the last thread
						
			thread_array[t]=std::thread(add_lines, M_out, M_prev, output_size, input_size, dy, line, line_end, debug_level);
			
			// add_lines(M_out, M_prev, output_size, input_size, dy, line, line_end, debug_level);
			
			line+=num_lines_per_thread;
			if(line>=total_lines) break;
						
		}// for t
		
		if(debug_level>0) printf("---All threads have been launched!---\n");
		
		for(int t=0;t<num_threads;t++) thread_array[t].join();
			
	}
	
}

void add_lines(double *M_out, double *M_prev, int *output_size, int *input_size, double *dy, int ij_start, int ij_end, int debug_level){
	
	if(debug_level>1) printf("running thread on lines %d to %d\n", ij_start, ij_end-1);
	
	for(int ij=ij_start;ij<ij_end;ij++){
			
		int j=ij%output_size[2];
		int i=ij/output_size[2];
		int i_prev=i*2;
		
		int dy_prev=(int) trunc(dy[j]/2); // this is the shift value inside M_prev that we need
		
		int j_prev=dy_prev+(int) floor(input_size[2]/2); // this is the index to use in M_prev's third dimension
		
		int gap_y=(int) dy[j]-dy_prev; // this is the gap that remains (we need to shift this)
		
		// find the pointers for the beginning of the correct lines in M_out and M_prev
		double *line_out=&M_out[i*output_size[0]+j*output_size[0]*output_size[1]];
		double *line_prev1=&M_prev[i_prev*input_size[0]+j_prev*input_size[0]*input_size[1]];
		double *line_prev2=&M_prev[(i_prev+1)*input_size[0]+j_prev*input_size[0]*input_size[1]];
		
		// this just loops over the passive (common) axis and sums with the requested shift.
		shift_add(line_out, line_prev1, line_prev2, output_size[0], -gap_y);
						
		
	}// for ij
	
	if(debug_level>1) printf("finished thread on lines %d to %d\n", ij_start, ij_end-1);

}		
			
void shift_add(double *line_out, double *line_prev1, double *line_prev2, int N, int gap){
	
	if(gap==0){
		for(int k=0;k<N;k++) line_out[k]=line_prev1[k]+line_prev2[k];
	}
	else if(gap>N || gap<-N){
		for(int k=0;k<N;k++) line_out[k]=line_prev1[k];
	}
	else if(gap>0){
		for(int k=0;k<gap;k++) line_out[k]=line_prev1[k];
		for(int k=gap;k<N;k++) line_out[k]=line_prev1[k]+line_prev2[k-gap];
	}
	else if(gap<0){
		for(int k=0;k<N+gap;k++) line_out[k]=line_prev1[k]+line_prev2[k-gap];
		for(int k=N+gap;k<N;k++) line_out[k]=line_prev1[k];
	}

}