
// Authors: Simon Christoph Stein and Jan Thiart
// E-Mail: scstein@phys.uni-goettingen.de
// Date: March 2016

// Optimization is done using the ceres-solver library.
// http://ceres-solver.org, New BSD license.
// Copyright 2015 Google Inc. All rights reserved.

// % Copyright (c) 2016, Simon Christoph Stein
// % All rights reserved.
// % 
// % Redistribution and use in source and binary forms, with or without
// % modification, are permitted provided that the following conditions are met:
// % 
// % 1. Redistributions of source code must retain the above copyright notice, this
// %    list of conditions and the following disclaimer.
// % 2. Redistributions in binary form must reproduce the above copyright notice,
// %    this list of conditions and the following disclaimer in the documentation
// %    and/or other materials provided with the distribution.
// % 
// % THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
// % ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
// % WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
// % DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
// % ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
// % (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
// % LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
// % ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
// % (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
// % SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
// % 
// % The views and conclusions contained in the software and documentation are those
// % of the authors and should not be interpreted as representing official policies,
// % either expressed or implied, of the FreeBSD Project.

// mex
#include "mex.h"

// stdlib
#include <cmath>
#include <math.h>

// our headers
#include "mexUtil.h"

// -- Type definitions (must go before further inclusions!) --
typedef Array1D_t<double> Array1D;
typedef Array2D_t<double> Array2D;
typedef Array3D_t<double> Array3D;

#include "mx_psfFit_OptimizerFunctions.h"

using namespace std;

// % Short usage: params = psfFit( img );  (fits an isotropic gaussian)
// % Full usage: [ params, exitflag ] = psfFit( img, param_init, param_optimizeMask, useIntegratedGauss, useMLErefine)
// %  Fit a gaussian point spread function model to the given image.
// % 
// % Coordinate convention: Integer coordinates are centered in pixels. I.e.
// % the position xpos=3, ypos=5 is exactly the center of pixel img(5,3). Thus
// % pixel center coordinates range from 1 to size(img,2) for x and 1 to
// % size(img,1) for y coordinates.
// %
// % Use empty matrix [] for parameters you don't want to specify.
// %
// % Gaussian fitting covers three basic cases:
// %   - fitting isotropic gaussian  (sigma_y and angle initial values not specified and they should not be optimized)
// %   - fitting anisotropic gaussian  (angle initial value not specified and should not be optimized)
// %   - fitting anisotropic rotated gaussian
// %
// % The fitting case is selected based on the set of specified initial
// % parameters together with the set of parameters that should be optimized.
// % The angle input/output should be in degree.
// %
// % Input:
// %   img - NxM image to fit to. (internally converted to double)
// %   param_init - Initial values for parameters. You can specify up to [xpos;ypos;A;BG;sigma_x,sigma_y,angle].
// %                If negative or no values are given, the fitter estimates a value for that parameter if neccessary, 
// %                with the exception of the angle, which is estimated if angle==0 and if it should be optimized.
// %                If parameters are not optimized (see next entry) their values are kept constant during optimization.
// %   param_optimizeMask - Must be true(1)/false(0) for every parameter [xpos,ypos,A,BG,sigma_x,sigma_y,angle].
// %                Parameters with value 'false' are not fitted. | default: [1,1,1,1,1,0,0] -> 'optimize x,y,A,BG,sigma' (isoptric gaussian)
// %   useIntegratedGauss - Wether to use pixel integrated gaussian or not 
// %                        not supported for non-isotropic arbitrarily roated gaussians | default: false
// %   useMLErefine - Use Poissonian noise based maximum likelihood estimation after
// %                  least squares fit. Make sure to input image intensities in photons 
// %                  for this to make sense. | default: false
// %
// % Output
// %   params - Final parameters [xpos,ypos,A,BG,sigma_x,sigma_y,angle].
// %   exitflag - Return state of optimizer. Positive = 'good'. Negative = 'bad'.
// %         1 - CONVERGENCE
// %        -1 - NO_CONVERGENCE
// %        -2 - FAILURE
// %
// % Optimization is done using the ceres-solver library.
// % http://ceres-solver.org, New BSD license.
// % Copyright 2015 Google Inc. All rights reserved.
// %
// % Authors: Simon Christoph Stein and Jan Thiart
// % Date: March 2016
// % E-Mail: scstein@phys.uni-goettingen.de
// % 
void mexFunction(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[])
{
    // Replace the std stream with the 'matlab' stream to see cout outputs in the MATLAB console
    mexStream mout;
    std::streambuf *outbuf = std::cout.rdbuf(&mout);
    
        /// -- Input parsing -- ///          
    /* Check for proper number of arguments. */
    if(nrhs<1)
       mexErrMsgTxt("psfFit is hungry! Please feed me at least an image!");
    
     if(mxGetClassID(prhs[0]) != mxDOUBLE_CLASS)
         mexErrMsgTxt("psfFit: input image datatype must be double!");
            
    /* Map access to input data */
    Array2D img( prhs[0] );
    Array2D param_init( prhs[1] );
    int nr_given_params = param_init.nElements;
    
    std::vector<bool> param_optimMask(7, 1); // default all 1's: optimize every parameter
    bool usePixelIntegratedGauss = false;
    bool useMLErefine = false;
       
    if(nrhs>2)
    {
       Array1D p_optimMask( prhs[2] );
       if( !p_optimMask.isEmpty() ) 
       {
         if(p_optimMask.nElements != 7)
             mexErrMsgTxt("Specify for every parameter if it should be optimized. [xpos,ypos,A,BG,sigma].\n Use false to not optimize a parameter.");
         else
         {
            bool* logical = mxGetLogicals(prhs[2]);
            for(int iParam=0; iParam<7; ++iParam)
                param_optimMask[iParam] = logical[iParam];
         }        
       }       
    }    
    
    if(nrhs>3)
    {
       Array1D usePixelIntegration( prhs[3] );
       if( !usePixelIntegration.isEmpty() )
       {
           usePixelIntegratedGauss = mxIsLogicalScalarTrue(prhs[3]);
       }
    }
    
    if(nrhs>4)
    {
       Array1D useMLE( prhs[4] );
       if( !useMLE.isEmpty() )
       {
           useMLErefine = mxIsLogicalScalarTrue(prhs[4]);
       }
    }
    
    
        /// -- Here goes the fitting! -- ///
    
    // Set intital conditions
    std::vector<double> p_init(7);
    p_init[0] = (nr_given_params >= 1) ? param_init[0]:-1;
    p_init[1] = (nr_given_params >= 2) ? param_init[1]:-1;
    p_init[2] = (nr_given_params >= 3) ?  param_init[2]:-1;
    p_init[3] = (nr_given_params >= 4) ?  param_init[3]:-1;
    p_init[4] = (nr_given_params >= 5) ?  param_init[4]:-1;
    p_init[5] = (nr_given_params >= 6) ?  param_init[5]:-1;
    // Angle is special as any value (-infty,infty) can make sense.
    // Also we convert it from degrees to radian here, which is the input needed by fitPSF.
    p_init[6] = (nr_given_params >= 7) ?  param_init[6]*M_PI/180.:0;
    
    // Built coordinate system to use
    std::vector<int> xCoords(img.nCols);
    std::vector<int> yCoords(img.nRows);    
    for(unsigned int iCol = 0; iCol<img.nCols; ++iCol)
          xCoords[iCol] = iCol+1; // We use Matlab coordinates
    for(unsigned int iRow = 0; iRow<img.nRows; ++iRow)
          yCoords[iRow] = iRow+1; // We use Matlab coordinates
    
    // Fit image, collect results
    std::vector<double> results = fitPSF(img, xCoords, yCoords, p_init, param_optimMask, usePixelIntegratedGauss, useMLErefine);
    
        ///  --  Output to MATLAB -- //
    const int nDims = 1; // Number of dimensions for output
    mwSize dim_out0[nDims] = { 7 };
    plhs[0] = mxCreateNumericArray( nDims, dim_out0 , mxDOUBLE_CLASS, mxREAL);
    
    // fitted parameter values
    Array1D fin_params ( plhs[0] ); // Map access to output
    fin_params[0] = results[0]; // x
    fin_params[1] = results[1]; // y
    fin_params[2] = results[2]; // A
    fin_params[3] = results[3]; // BG
    // results[4]; // q1 (related to sigma_x)        
    // results[5]; // q2 (related to sigma_y)
    // results[6]; // q3 (related to angle)

    double sigma_x, sigma_y, angle;
    convert_Qs_To_SxSyAngle( results[4], results[5], results[6], sigma_x, sigma_y, angle);
    fin_params[4] = sigma_x;       
	fin_params[5] = sigma_y;
	fin_params[6] = angle*180./M_PI; // back-convert from radian to degree;
    
    
    // exit state of optimizer
    mwSize dim_out1[1] = { 1 };
    plhs[1] = mxCreateNumericArray( 1, dim_out1 , mxDOUBLE_CLASS, mxREAL);
    double& exitflag  = *mxGetPr(plhs[1]);
    exitflag = results[7]; // exitflag
    
    // Restore the std stream buffer
    std::cout.rdbuf(outbuf);
}


