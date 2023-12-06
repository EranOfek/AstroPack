#include "mex.h"
#include <omp.h>
#include <immintrin.h>

//
// mex -v CXXFLAGS='$CXXFLAGS -fopenmp -mavx2' LDFLAGS='$LDFLAGS -fopenmp' CXXOPTIMFLAGS='-O3 -DNDEBUG' mex_timesDouble_avx2.cpp
//

typedef long long int64;

void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[]) 
{
	mxClassID class_id;
	   
    // Check for the number of arguments
    if (nrhs < 2 || nrhs > 3) {
        mexErrMsgIdAndTxt("MATLAB:mex_times:invalidNumInputs", "Two or three inputs required.");
    }

    /*
    // Check if the input arguments are of the correct type
	class_id = mxGetClassID(prhs[0]);
	if ((class_id != MEX_TYPE) && (class_id != MEX_UTYPE)) 
		mexErrMsgIdAndTxt("MATLAB:bitsetFlag", "Array: Input matrix type does not match this function");  
	if (mxIsSparse(prhs[0])) 
		mexErrMsgIdAndTxt("MATLAB:bitsetFlag", "Array: Sparse array is not supported yet");  
	
	class_id = mxGetClassID(prhs[1]);
	if ((class_id != MEX_TYPE) && (class_id != MEX_UTYPE))
		mexErrMsgIdAndTxt("MATLAB:bitsetFlag", "Array: Input matrix type does not match this function");  
	if (mxIsSparse(prhs[1])) 
		mexErrMsgIdAndTxt("MATLAB:bitsetFlag", "Array: Sparse array is not supported yet");  	

    // Check if the input arguments are of the same size
    if (mxGetNumberOfElements(prhs[0]) != mxGetNumberOfElements(prhs[1])) {
        mexErrMsgIdAndTxt("MATLAB:mex_times:sizeMismatch", "First two inputs must be of the same size.");
    }

    */

    // Get pointers to the data in the input arrays
    __Type *A = (__Type*)mxGetData(prhs[0]);
    __Type *B = (__Type*)mxGetData(prhs[1]);

    // Get the number of elements in the input arrays
    int64 numel = mxGetNumberOfElements(prhs[0]);

    // Check if the optional argument is provided and is scalar
    bool useOpenMP = (nrhs == 3) && (*((int*)mxGetData(prhs[2])) != 0);
    //mexPrintf("OpenMP: %d\n", useOpenMP);
   
#ifdef _OPENMP
    // Get the maximum number of threads    
    //int maxThreads = omp_get_max_threads();
    //mexPrintf("Maximum number of threads: %d\n", maxThreads);
    //omp_set_num_threads(32);
    //maxThreads = omp_get_max_threads();
    //mexPrintf("Maximum number of threads AFTER SET: %d\n", maxThreads);

    // Get the current thread number
    //int threadNum = omp_get_thread_num();
    //mexPrintf("Current thread number: %d\n", threadNum);
#endif

    int64 remainder = numel % 8;
    int64 simd_size = numel - remainder;

    // Perform the element-wise multiplication and store the result in A
    if (useOpenMP) 
	{       
        #pragma omp parallel for
        for (int64 i = 0; i < simd_size; i += 8) {

            // 4x doubles
            __m256d vecA1 = _mm256_load_pd(&A[i]);			              
            __m256d vecB1 = _mm256_load_pd(&B[i]);
			__m256d result1 = _mm256_mul_pd(vecA1, vecB1);
			_mm256_store_pd(&A[i], result1);

            // 4x doubles
            __m256d vecA2 = _mm256_load_pd(&A[i+4]);			              
            __m256d vecB2 = _mm256_load_pd(&B[i+4]);
			__m256d result2 = _mm256_mul_pd(vecA2, vecB2);
			_mm256_store_pd(&A[i+4], result2);            
        }        
    } 
	else 
	{
        for (int64 i = 0; i < simd_size; i += 8) {

            // 4x doubles
            __m256d vecA1 = _mm256_load_pd(&A[i]);			              
            __m256d vecB1 = _mm256_load_pd(&B[i]);
			__m256d result1 = _mm256_mul_pd(vecA1, vecB1);
			_mm256_store_pd(&A[i], result1);

            // 4x doubles
            __m256d vecA2 = _mm256_load_pd(&A[i+4]);			              
            __m256d vecB2 = _mm256_load_pd(&B[i+4]);
			__m256d result2 = _mm256_mul_pd(vecA2, vecB2);
			_mm256_store_pd(&A[i+4], result2);            
        }        
    }

    // Handle remainder    
    if (numel-simd_size > 0) {
        //mexPrintf("remainder: %d\n", numel-simd_size);
        for (int64 i = simd_size; i < numel;  i++) {
            A[i] *= B[i];
        }    
    }
}
