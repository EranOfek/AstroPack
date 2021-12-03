// Authors: Simon Christoph Stein and Jan Thiart
// E-Mail: scstein@phys.uni-goettingen.de
// Date: June 2015

// Optimization is done using the ceres-solver library.
// http://ceres-solver.org, New BSD license.
// Copyright 2015 Google Inc. All rights reserved.

// % Copyright (c) 2015, Simon Christoph Stein
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

// Use OpenMP if available to parallelize computation
#ifdef _OPENMP
    #include <omp.h>
#endif

// mex
#include "mex.h"

// stdlib
#include <cmath>
#include <iostream>
#include <algorithm>

// our headers
#include "mexUtil.h"

// -- Type definitions (must go before further inclusions) --
typedef Array1D_t<double> Array1D;
typedef Array2D_t<double> Array2D;
typedef Array3D_t<double> Array3D;

#include "mx_psfFit_OptimizerFunctions.h"

using namespace std;



// Rounds ONLY positive numbers. Will also fail if x is outside the integer range!
int round_positive(double x)
{
    return (int)(x + 0.5);
}

/*
% Short usage: params = psfFit_Image( img, param_init );
% Full usage: [ params, exitflag ] = psfFit_Image( img, param_init, param_optimizeMask, useIntegratedGauss, useMLErefine, hWinSize, global_init )
%  Fit multiple spot candidates in a given image with a gaussian point spread function model.
%
% Coordinate convention: Integer coordinates are centered in pixels. I.e.
% the position xpos=3, ypos=5 is exactly the center of pixel img(5,3). Thus
% pixel center coordinates range from 1 to size(img,2) for x and 1 to
% size(img,1) for y coordinates.
%
% Use empty matrix [] for parameters you don't want to specify.
%
% Gaussian fitting covers three basic cases:
%   - fitting isotropic gaussian  (sigma_y and angle initial values not specified and they should not be optimized)
%   - fitting anisotropic gaussian  (angle initial value not specified and should not be optimized)
%   - fitting anisotropic rotated gaussian
%
% The fitting case is selected based on the set of specified initial
% parameters together with the set of parameters that should be optimized.
% The angle input/output should be in degree.
%
% Input:
%   img        - Image to fit to. (internally converted to double)
%   param_init - Initial values PxN to fit N spot candidates in the given image with
%                initial conditions for P parameters. At least position [xpos; ypos]
%                must be specified (P>=2). You can specify up to [xpos;ypos;A;BG;sigma_x,sigma_y,angle].
%                If negative or no values are given, the fitter estimates a value for that parameter if neccessary, 
%                with the exception of the angle, which is estimated if angle==0 and if it should be optimized.
%                If parameters are not optimized (see next entry) their values are kept constant during optimization.
%   param_optimizeMask - Must be true(1)/false(0) for every parameter [xpos,ypos,A,BG,sigma_x,sigma_y,angle].
%                Parameters with value 'false' are not fitted.  | default: [1,1,1,1,1,0,0] -> 'optimize x,y,A,BG,sigma' (isoptric gaussian)
%   useIntegratedGauss - Wether to use pixel integrated gaussian or not 
%                  not supported for non-isotropic arbitrarily roated gaussians | default: false
%   useMLErefine - Use Poissonian noise based maximum likelihood estimation after
%                  least squares fit. Make sure to input image intensities in photons
%                  for this to make sense. | default: false
%   hWinSize   - Each candidates fit takes intensites in a window of (2*hWinsize+1)x(2*hWinsize+1) into account | default: 5
%   global_init - For convenience (up to) [sigma_x,sigma_y,angle] can also be given as an extra parameter.
%               This simply sets all candidates initial values, eventually overwriting their param_init values.
%
% Output
%   params     -  Fitted parameters 8xN. Columns are in order
%                 [xpos,ypos,A,BG,sigma_x,sigma_y,angle; exitflag].
%
%           The last row 'exitflag' returns the state of optimizer.
%           Positive = 'good'. Negative = 'bad'.
%             1 - CONVERGENCE
%            -1 - NO_CONVERGENCE
%            -2 - FAILURE
%
% Authors: Simon Christoph Stein and Jan Thiart
% Date:   March 2016
% E-Mail: scstein@phys.uni-goettingen.de
*/
void mexFunction(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[])
{
    // Replace the std stream with the 'matlab' stream to see cout outputs in the MATLAB console
    mexStream mout;
    std::streambuf *outbuf = std::cout.rdbuf(&mout);
    
    #ifdef DEBUG
        #ifdef _OPENMP
            printf("OpenMP number of threads @ start: %i\n", omp_get_num_threads()); // Test if openMP is included correctly
        #endif
    #endif
        
        /// -- Input parsing -- ///   
    
    /* Check for proper number of arguments. */
    if(nrhs<2)
       mexErrMsgTxt("psfFit_Image is hungry! Please feed me at least an image and one spot candidate!");
            
    if(mxGetClassID(prhs[0]) != mxDOUBLE_CLASS)
         mexErrMsgTxt("psfFit: input image datatype must be double!");
        
    /* Map access to input data */
    Array2D img( prhs[0] );
    Array2D param_init( prhs[1] );
    
    int nr_candidates = param_init.nCols;
    int nr_given_params = param_init.nRows;
    std::vector<bool> param_optimMask(7, 1); // default all 1's: optimize every parameter
    bool usePixelIntegratedGauss = false;
    bool useMLErefine = false;
    
    if( param_init.nElements == 0 )
        mexErrMsgTxt("Feed me at least one spot candidate! Specify [xpos;ypos]  or [xpos;ypos;A;BG;sigma_x;sigma_y;angle]");
    
    if( nr_given_params < 2)
        mexErrMsgTxt("Specify initial condition for at least position [xpos,ypos]! Or speficy in order [xpos;ypos;A;BG;sigma_x;sigma_y;angle]. \n Use negative values for parameters you don't want to specify! (Except for angle, there use angle==0)");
        
    if(nrhs>2)
    {
       Array1D p_optimMask( prhs[2] );
       if( !p_optimMask.isEmpty() ) 
       {
         if(p_optimMask.nElements != 7)
             mexErrMsgTxt("Specify for every parameter if it should be optimized. [xpos,ypos,A,BG,sigma_x,sigma_y,angle].\n Use false to not optimize a parameter.");
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
//            useMLErefine = useMLE[0];
           useMLErefine = mxIsLogicalScalarTrue(prhs[4]);
       }
    }
    
    int hWinSize = 5;
    if(nrhs>5)
    {
        Array1D input_winsize( prhs[5] );
        if( !input_winsize.isEmpty() )
            hWinSize = int(input_winsize[0] + 0.5);
    }
    
    
    std::vector<double> global_init_vals;
    if(nrhs>6)
    {
        Array1D global_init( prhs[6] );
        for(int i=0; i<global_init.nElements; ++i)            
            global_init_vals.push_back(global_init[i]);
    }
    
    
    
       ///  --  Allocate MATLAB output memory -- //
    const int nDims = 2; // Number of dimensions for output
    mwSize dim_out0[nDims] = { 8, nr_candidates }; // nr_param +1 (due to errorflag)
    plhs[0] = mxCreateNumericArray( nDims, dim_out0 , mxDOUBLE_CLASS, mxREAL);
    // fitted parameter values
    Array2D fin_params ( plhs[0] ); // Map access to output
        
       
        /// -- Here goes the fitting! -- ///
    // Fit every candidate using intensities in a window around him
    #ifdef DEBUG
       std::cout << "Starting fit of " << nr_candidates << " candidates." << std::endl;
    #endif
    
    #pragma omp parallel for schedule(static,1)
    for(int iCand = 0; iCand<nr_candidates; ++iCand)
    {        
        int LowX  = std::max<int>(1,round_positive(param_init(0,iCand))-hWinSize);
        int HighX = std::min<int>(img.nCols,round_positive(param_init(0,iCand))+hWinSize);
        int LowY  = std::max<int>(1,round_positive(param_init(1,iCand))-hWinSize);
        int HighY = std::min<int>(img.nRows,round_positive(param_init(1,iCand))+hWinSize);
        
        // Built coordinate system to use
        std::vector<int> xCoords(HighX-LowX+1);
        std::vector<int> yCoords(HighY-LowY+1);    
        
        int xVal = LowX;
        int yVal = LowY;
        #ifdef DEBUG
          std::cout << "Candidate window edges: LowX " << LowX << ", HighX " << HighX << ", LowY " << LowY << ", HighY " << HighY << std::endl;
        #endif
                    
        for(unsigned int iX = 0; iX<xCoords.size(); ++iX, ++xVal)
          xCoords[iX] = xVal;
        for(unsigned int iY = 0; iY<yCoords.size(); ++iY, ++yVal)
          yCoords[iY] = yVal;
          
        // Copy data of subwindow
        Array2D sub_img(yCoords.size(), xCoords.size());
        
        int sub_iRow = 0;
        int sub_iCol = 0;
        for(unsigned int iCol = LowX-1; iCol<HighX; ++iCol)
        {
            sub_iRow = 0;
            for(unsigned int iRow = LowY-1; iRow<HighY; ++iRow)
            {
                sub_img(sub_iRow, sub_iCol) = img(iRow,iCol);
                ++sub_iRow;
            }
            ++sub_iCol;
        }
        
        // Copy intital conditions
        std::vector<double> p_init(7);
        p_init[0] = param_init(0,iCand);
        p_init[1] = param_init(1,iCand);        
        p_init[2] = (nr_given_params >= 3) ?  param_init(2,iCand):-1;
        p_init[3] = (nr_given_params >= 4) ?  param_init(3,iCand):-1;
        p_init[4] = (nr_given_params >= 5) ?  param_init(4,iCand):-1;
		p_init[5] = (nr_given_params >= 6) ?  param_init(5,iCand):-1;
        // Angle is special as any value (-infty,infty) can make sense.
        // Also we convert it from degrees to radian here, which is the input needed by fitPSF.
		p_init[6] = (nr_given_params >= 7) ?  param_init(6,iCand)*M_PI/180.:0;
        
        // If user gave global [sigma_x,sigma_y,angle] this takes precedence
        for(int i=0; i<global_init_vals.size(); ++i)
        {
            p_init[i+4] = global_init_vals[i];
        }

        // Fit image, collect results
        std::vector<double> results = fitPSF(sub_img, xCoords, yCoords, p_init, param_optimMask, usePixelIntegratedGauss, useMLErefine);
        
        // Output
        fin_params(0,iCand) = results[0]; // x
        fin_params(1,iCand) = results[1]; // y
        fin_params(2,iCand) = results[2]; // A
        fin_params(3,iCand) = results[3]; // BG        
        // results[4]; // q1 (related to sigma_x)        
		// results[5]; // q2 (related to sigma_y)
		// results[6]; // q3 (related to angle)
        
        double sigma_x, sigma_y, angle;
        convert_Qs_To_SxSyAngle( results[4], results[5], results[6], sigma_x, sigma_y, angle);
        fin_params(4,iCand) = sigma_x;    
		fin_params(5,iCand) = sigma_y;
		fin_params(6,iCand) = angle*180./M_PI; // back-convert from radian to degree
        
        fin_params(7,iCand) = results[7]; // exitflag
    }
    
     
    
    // Restore the std stream buffer
    std::cout.rdbuf(outbuf);
}


