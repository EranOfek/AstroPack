#include "mex.h"
#include "healpix_base.h"

// mex neighbors_nested.cpp -I/home/sasha/Downloads/Healpix_3.83/src/cxx/Healpix_cxx -I/home/sasha/Downloads/Healpix_3.83/src/cxx/cxxsupport -L/home/sasha/Downloads/Healpix_3.83/lib /home/sasha/Downloads/Healpix_3.83/lib/libhealpix_cxx.a -lstdc++ CXXFLAGS="\$CXXFLAGS -std=c++11"
// example: ne = celestial.healpix.mex.neighbors_nested(8,20567) // NB! here 8 means nside = 2^8

void mexFunction(int nlhs, mxArray *plhs[],
                 int nrhs, const mxArray *prhs[])
{
    if (nrhs != 2)
        mexErrMsgTxt("Usage: neigh = healpix_neighbors_nest(nside [as power of 2], ipix)");

    long nside = (long) mxGetScalar(prhs[0]);  

    double *ipix_in = mxGetPr(prhs[1]);
    mwSize N = mxGetNumberOfElements(prhs[1]);

    plhs[0] = mxCreateDoubleMatrix(8, N, mxREAL);
    double *out = mxGetPr(plhs[0]);

    T_Healpix_Base<long> hp(nside, NEST);  

    for (mwSize i = 0; i < N; i++)
    {
        
        fix_arr<long,8> neigh;  
        hp.neighbors((long)ipix_in[i], neigh);  

        for (int k = 0; k < 8; k++)
            out[k + 8*i] = neigh[k];
    }
}
