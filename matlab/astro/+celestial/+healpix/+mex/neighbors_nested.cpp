#include "mex.h" 
#include "healpix_base.h"

// mex neighbors_nested.cpp -I/home/sasha/ExternalLib/Healpix_3.83/src/cxx/Healpix_cxx -I/home/sasha/ExternalLib/Healpix_3.83/src/cxx/cxxsupport -L/home/sasha/ExternalLib/Healpix_3.83/lib /home/sasha/ExternalLib/Healpix_3.83/lib/libhealpix_cxx.a -lstdc++ CXXFLAGS="\$CXXFLAGS -std=c++11"
// example: ne = celestial.healpix.mex.neighbors_nested(256,int64(20567)) 

void mexFunction(int nlhs, mxArray *plhs[],
                 int nrhs, const mxArray *prhs[])
{
    if (nrhs != 2)
        mexErrMsgTxt("Usage: neigh = healpix_neighbors_nest(nside, ipix)");

    long nside = (long) mxGetScalar(prhs[0]);
    int order  = __builtin_ctzll(nside);    
    
    if (!mxIsInt64(prhs[1]))
        mexErrMsgTxt("ipix must be int64");

    int64_t *ipix_in = (int64_t*) mxGetData(prhs[1]);
    mwSize N = mxGetNumberOfElements(prhs[1]);
  
    plhs[0] = mxCreateNumericMatrix(8, N, mxINT64_CLASS, mxREAL);
    int64_t *out = (int64_t*) mxGetData(plhs[0]);

    T_Healpix_Base<long> hp(order, NEST);  

    for (mwSize i = 0; i < N; i++)
    {
        
        fix_arr<long,8> neigh;  
        hp.neighbors((long)ipix_in[i], neigh);  

        for (int k = 0; k < 8; k++)
            out[k + 8*i] = neigh[k];
    }
}
