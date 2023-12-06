
#include "matrix.h"

void
mexFunction (int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[])
{
    /*Declare variables*/
    size_t ninputs  = mxGetNumberOfElements(prhs[1]);
    int i,j;
    int ndims = nrhs;
    
    double *dims         = mxGetPr(prhs[0]);
    double *cur_dim_subs = mxGetPr(prhs[1]);
    
    double *indices;
    double cp = 1;
    
    /*Associate outputs*/
    plhs[0] = mxDuplicateArray(prhs[1]);
    indices = mxGetPr(plhs[0]);
    
    /*Initialize indices to 1 (in matlab, indices start from 1)*/
    for (j = 0; j < ninputs; j++)
        indices[j] = cur_dim_subs[j];
    
    /*Perform the actual computation*/
    for(i=2;i<ndims;i++){
        cp *= *dims++;
        
        /*Load current dimension subscripts*/
        cur_dim_subs = mxGetPr(prhs[i]);
        j=0;
        while(j < ninputs) 
            indices[j++] += (*cur_dim_subs++-1) * cp;
    }
}