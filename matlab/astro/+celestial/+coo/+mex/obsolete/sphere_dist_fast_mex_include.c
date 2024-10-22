//function [Dist,Ang,PA]=sphere_dist_fast(RA_1,Dec_1,RA_2,Dec_2)
//--------------------------------------------------------------------------
// sphere_dist_fast function                                          ephem
// Description: Calculate the angular distance between two points on the
//              celestial sphere. See sphere_dist.m (and built in distance.m)
//                for a more general function. This function is ~10 time
//              faster than sphere_dist.m, but it works only with radians
//                and calculate only the distance.
// Input  : - Matrix of logitudes for the first point [radian].
//          - Matrix of latitudes for the first point [radian].
//          - Matrix of logitudes for the second point [radian].
//          - Matrix of latitudes for the second point [radian].
// Output : - Matrix of distances between points [radian].
//          - Matrix of position angles between points [radian].
//            Measured westward. Take 2*pi-PA to get the PA measured
//            Eastward.
//          - Matrix of P.A. of the first point relative to the second point
//            (eastwrad from the north).

//     By : Eran O. Ofek                    Feb 2013
//    URL : http://weizmann.ac.il/home/eofek/matlab/
// See also: sphere_dist.m, sphere_dist_cosd.m (and built in distance.m).
// Example: D=celestial.coo.sphere_dist_fast(RA1,Dec1,RA2,Dec2);
// Reliable: 1
// Translated to mex by Uri I.  March 2022
//--------------------------------------------------------------------------
#include <stdio.h>
#include "mex.h"
#include "math.h"
#include "matrix.h"
#include <complex.h>
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
    __FLOAT *Dist, *Ang, *dRA, *SinPA, *CosPA, *PA;
    __FLOAT *Dec_1, *Dec_2, *RA_1, *RA_2;
    //const __FLOAT pi = 3.14159265359;
    const double pi = 3.14159265359;
    //const double pi = acos(0);
    double complex;
    mxClassID     class_id;
    const char*   input_type;
    mwSize        input_ndims;
    const mwSize* input_size;
    class_id    = mxGetClassID(prhs[0]);
    RA_1       = (__FLOAT*)mxGetData(prhs[0]);
    input_ndims = mxGetNumberOfDimensions(prhs[0]);
    input_size  = mxGetDimensions(prhs[0]);
    input_type  = mxGetClassName(prhs[0]);

    Dec_1       = (__FLOAT*)mxGetData(prhs[1]);
    RA_2       = (__FLOAT*)mxGetData(prhs[2]);
    Dec_2       = (__FLOAT*)mxGetData(prhs[3]);

    plhs[0] = mxCreateNumericArray(input_ndims, input_size, mxDOUBLE_CLASS, mxREAL);
    Dist = (__FLOAT*)mxGetData(plhs[0]);

    int Tot_size;
    if (input_ndims>=1)
    {
        Tot_size = 1;
    }
    else
    {
        Tot_size = 0;
    }

    for (int i =0;i<input_ndims;i++)
    {
        Tot_size = Tot_size*(int)input_size[i];
    }
    for (int i =0;i<Tot_size;i++)
    {
        Dist[i] = acos(sin(Dec_1[i])*sin(Dec_2[i]) + cos(Dec_1[i])*cos(Dec_2[i])*cos(RA_1[i]-RA_2[i]));
    }
    if (nlhs>1)
    {
        plhs[1] = mxCreateNumericArray(input_ndims, input_size, mxDOUBLE_CLASS, mxREAL);
        Ang = (__FLOAT*)mxGetData(plhs[1]);
        if (nlhs>2)
        {
            plhs[2] = mxCreateNumericArray(input_ndims, input_size, mxDOUBLE_CLASS, mxREAL);
            PA = (__FLOAT*)mxGetData(plhs[2]);
        }
        dRA = (__FLOAT*)mxMalloc(Tot_size*sizeof(__FLOAT));
        SinPA = (__FLOAT*)mxMalloc(Tot_size*sizeof(__FLOAT));
        CosPA = (__FLOAT*)mxMalloc(Tot_size*sizeof(__FLOAT));
        for (int i =0;i<Tot_size;i++)
        {

            dRA[i] = RA_1[i]-RA_2[i];

            SinPA[i] = sin(dRA[i])*cos(Dec_2[i])/sin(Dist[i]);
            CosPA[i] = (sin(Dec_2[i])*cos(Dec_1[i]) - cos(Dec_2[i])*sin(Dec_1[i])*cos(dRA[i]))/sin(Dist[i]);
            //mexPrintf("mex Sin %f  Cos %f \n",SinPA[i],CosPA[i]);
            Ang[i]    = atan2((SinPA[i]),(CosPA[i]));
            if (Ang[i]<0)
            {
                Ang[i] = Ang[i] + 2*pi; 
            }
            


            if (nlhs>2)
            {

                 PA[i]    = atan2((SinPA[i]),-(CosPA[i]));
                //PA[i]    = atan2(creal(SinPA[i]),-creal(CosPA[i]));
                if (PA[i]<0)
                {
                    PA[i] = PA[i] + 2*pi; //(__FLOAT)
                }
            }

        }
        mxFree(dRA);
        mxFree(SinPA);
        mxFree(CosPA);
    }
}