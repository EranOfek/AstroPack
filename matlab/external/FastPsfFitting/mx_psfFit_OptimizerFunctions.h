// Author: Simon Christoph Stein
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


#pragma once

// ceres
#include "ceres/ceres.h"
#include "glog/logging.h"

// stdlib
#include <cmath>
#include <vector>
#include <algorithm>
#include <utility>
#include <limits>

// our headers
#include "mexUtil.h"
#include "mx_psfFit_CostFunctions.h"

using namespace std;

// Ceres
using ceres::AutoDiffCostFunction;
using ceres::CostFunction;
using ceres::Problem;
using ceres::Solver;
using ceres::Solve;

using ceres::GradientProblem;
using ceres::GradientProblemSolver;



/// --  Prototypes -- ///


// Convert the q values we optimize into sigma_x, sigma_y, angle
void
convert_Qs_To_SxSyAngle(double q_1, double q_2, double q_3, double& sigma_x, double& sigma_y, double& angle);

/* Fit point spread function model to given image
 * Input:
 *   img - image to fit to
 *   xCoords - vector of x-coordinates to use (length must be img.nCols)
 *   yCoords - vector of y-coordinates to use (length must be img.nRows)
 *   param_init - 7 element vector of initial parameters [xpos,ypos,A,BG,sigma_x,sigma_y,angle]. For negative values the function estimates the initial guess from the image (except for angle, there use 0).
 *   param_optimizeMask - 7 element vector which is >0 for every element that should be optimized. Elements with mask <= 0 are kept constant.
 *   usePixelIntegratedGauss - Uses a pixel integrated gaussian model instead of a center-sampled one (slower but usually more accurate).
 *   useMLErefine - Refine least squares fit with Poissonian noise based maximum likelihood estimation.  Make sure to input image intensities in photons for this to make sense. This is computationally very expensive.
 *
 *  Note: the angle input/output to fitPSF is in radian, not degree!
 *
 * Output:
 *   results - 8 element vector of fitted parameters [xpos,ypos,A,BG,q_1,q_2,q_3, exitflag]. Exitflag contains the exit state of the solver, which is positive for success and negative for failure. (for more detail see getTerminationType)
*/   
std::vector<double> 
fitPSF(Array2D& img, std::vector<int>& xCoords, std::vector<int>& yCoords, std::vector<double>& param_init, std::vector<bool>& param_optimMask, bool usePixelIntegratedGauss, bool useMLErefine = false);

// Estimate the intital conditions for the fit.
// Estimates are only computed for input parameters with negative values (e.g. giving xpos=-1) to the function (except for angle, there use 0).
// If positive values are given, these are left untouched and taken as the initial guess.
void 
estimateInitialConditions(Array2D& img, std::vector<int>& xCoords, std::vector<int>& yCoords, double& xpos,double& ypos,double& A,double& BG,double& sigma_x,double& sigma_y,double& angle, double& q_1, double& q_2, double& q_3, std::vector<bool>& param_optimMask);

// Set lower and upper bounds of parameters for the optimization
void
setParameterBounds(Problem& problem, Array2D& img, std::vector<int>& xCoords, std::vector<int>& yCoords, double& xpos,double& ypos,double& A,double& BG,double& q_1,double& q_2,double& q_3);

// Remap the termination type (enum) of the summary into an integer number
// return: 2 - USER_SUCCESS // (cannot happen in our case)
//         1 - CONVERGENCE
//        -1 - NO_CONVERGENCE
//        -2 - FAILURE
//        -3 - USER FAILURE // (cannot happen in our case)
// For detailed description see function implementation
int
getTerminationType(Solver::Summary& summary);
int
getTerminationType(GradientProblemSolver::Summary& summary);

// Convert the q values we optimize into sigma_x, sigma_y, angle
void
convert_Qs_To_SxSyAngle(double q_1, double q_2, double q_3, double& sigma_x, double& sigma_y, double& angle)
{
    if(q_3 != 0)
        angle = 0.5*atan(2*q_3/(q_2-q_1));
    else
        angle = 0;
        
    if(angle != 0)
    {
        sigma_x = 1/sqrt(q_1+q_2-2*q_3/sin(2*angle));
        sigma_y = 1/sqrt(q_1+q_2+2*q_3/sin(2*angle));
    } else
    {
        sigma_x = 1/sqrt(2*q_1);
        if(q_2>=0)
            sigma_y = 1/sqrt(2*q_2);
        else
            sigma_y = -1;
    }
}


/// --  Functions -- ///
std::vector<double> 
fitPSF(Array2D& img, std::vector<int>& xCoords, std::vector<int>& yCoords, std::vector<double>& param_init, std::vector<bool>& param_optimMask, bool usePixelIntegratedGauss, bool useMLErefine)
{   
    // The variables to solve for: x,y,A,BG,sigma
    double xpos, ypos, A, BG, sigma_x,sigma_y,angle, q_1, q_2, q_3;
    
      /// -- Setup intial conditions -- ///          
    xpos  = param_init[0];
    ypos  = param_init[1];
    A     = param_init[2];
    BG    = param_init[3];
    sigma_x = param_init[4];
	sigma_y = param_init[5];
	angle = param_init[6];
    
    #ifdef DEBUG  
        std::cout << "param_optimMask: x:" << std::boolalpha << param_optimMask[0] << ", y:" << param_optimMask[1] << ", A:" << param_optimMask[2] << ", BG:" << param_optimMask[3] << ", sx:" << param_optimMask[4] << ", sy:" << param_optimMask[5] << ", angle:" << param_optimMask[6] << "\n";
        std::cout << "fitPSF input parameters [ xpos,ypos,A,BG,sigma_x,sigma_y,angle[rad] ]: " << xpos << ", " << ypos << ", " << A << ", " << BG << ", " << sigma_x << ", " << sigma_y << ", " << angle << " | usePixelIntegratedGauss:" << usePixelIntegratedGauss << "   useMLErefine:" << useMLErefine << "\n";
    #endif
    
    estimateInitialConditions(img, xCoords,yCoords, xpos, ypos, A, BG, sigma_x, sigma_y, angle, q_1,q_2,q_3, param_optimMask);
    
    #ifdef DEBUG
      std::cout << "Estimated initial parameters [xpos,ypos,A,BG,q_1 (sigma_x),q_2 (sigma_y),q_3 (angle[rad])]: " << xpos << ", " << ypos << ", " << A << ", " << BG << ", " << q_1 << " ("<<sigma_x<<"), " << q_2 << " ("<<sigma_y<<"), " << q_3 << " ("<<angle<<")" << "\n";
    #endif
      
    
//         /// -- Build the problem to optimize -- ///
    Problem problem;
    
    CostFunction* cost_function;
		if(param_optimMask[6] || angle != 0)
		{
			if(usePixelIntegratedGauss)
            {
                mexWarnMsgTxt("Pixel integrated non-isotropic arbitrarily rotated gaussians are not supported. Falling back to center-sampled gaussian.");
				cost_function = new AutoDiffCostFunction<SampledGaussAnisoAngleResidual, ceres::DYNAMIC, 1,1,1,1,1,1,1>(  new SampledGaussAnisoAngleResidual(img, xCoords, yCoords), img.nElements);
				// not implemented yet
				// cost_function = new AutoDiffCostFunction<IntegratedGaussAnisoAngleResidual, ceres::DYNAMIC, 1,1,1,1,1,1,1>(  new IntegratedGaussAnisoAngleResidual(img, xCoords, yCoords), img.nElements);
            }
			else
				cost_function = new AutoDiffCostFunction<SampledGaussAnisoAngleResidual, ceres::DYNAMIC, 1,1,1,1,1,1,1>(  new SampledGaussAnisoAngleResidual(img, xCoords, yCoords), img.nElements);

			problem.AddResidualBlock(cost_function, NULL, &xpos, &ypos, &A, &BG, &q_1, &q_2, &q_3);
			if( !param_optimMask[4] )  problem.SetParameterBlockConstant(&q_1);
			if( !param_optimMask[5] )  problem.SetParameterBlockConstant(&q_2);
			if( !param_optimMask[6] )  problem.SetParameterBlockConstant(&q_3);
		}
		else
		{
			if(param_optimMask[5] || sigma_y>=0)
			{
				if(usePixelIntegratedGauss)
					cost_function = new AutoDiffCostFunction<IntegratedGaussAnisoResidual, ceres::DYNAMIC, 1,1,1,1,1,1>(  new IntegratedGaussAnisoResidual(img, xCoords, yCoords), img.nElements);
				else
					cost_function = new AutoDiffCostFunction<SampledGaussAnisoResidual, ceres::DYNAMIC, 1,1,1,1,1,1>(  new SampledGaussAnisoResidual(img, xCoords, yCoords), img.nElements);

				problem.AddResidualBlock(cost_function, NULL, &xpos, &ypos, &A, &BG, &q_1, &q_2);
				if( !param_optimMask[4] )  problem.SetParameterBlockConstant(&q_1);
				if( !param_optimMask[5] )  problem.SetParameterBlockConstant(&q_2);
			}
			else
			{
				if(usePixelIntegratedGauss)
					cost_function = new AutoDiffCostFunction<IntegratedGaussResidual, ceres::DYNAMIC, 1,1,1,1,1>(  new IntegratedGaussResidual(img, xCoords, yCoords), img.nElements);
				else
					cost_function = new AutoDiffCostFunction<SampledGaussResidual, ceres::DYNAMIC, 1,1,1,1,1>(  new SampledGaussResidual(img, xCoords, yCoords), img.nElements);

				problem.AddResidualBlock(cost_function, NULL, &xpos, &ypos, &A, &BG, &q_1);
				if( !param_optimMask[4] )  problem.SetParameterBlockConstant(&q_1); //only when dealing with an isotropic Gaussian can we decide if sigma should be fitted or not
			}
		}
    
     // Keep specified parameters fixed
    if( !param_optimMask[0] )  problem.SetParameterBlockConstant(&xpos);
    if( !param_optimMask[1] )  problem.SetParameterBlockConstant(&ypos);        
    if( !param_optimMask[2] )  problem.SetParameterBlockConstant(&A);
    if( !param_optimMask[3] )  problem.SetParameterBlockConstant(&BG);
 
    
    // Set bounds of parameters///
    setParameterBounds(problem, img, xCoords, yCoords, xpos, ypos, A, BG, q_1, q_2, q_3);
    
    
        /// -- Set solver options -- ///
    Solver::Options options;
    options.minimizer_progress_to_stdout = false;
    options.max_num_iterations = 100;
    options.logging_type = ceres::SILENT; // default: ceres::PER_MINIMIZER_ITERATION
//   options.function_tolerance = 1e-6; // default: 1e-6
//   options.gradient_tolerance = 1e-10; // default: 1e-10
//   options.num_threads = 4; // Threads to use for Jacobian evaluation
    
    
        ///  -- Run the solver! -- ///
    // (Default optimization is trust-region levenberg marquardt)
    Solver::Summary summary;
    Solve(options, &problem, &summary);
    
    #ifdef DEBUG   
      std::cout << summary.BriefReport() << "\n";
      convert_Qs_To_SxSyAngle(q_1, q_2, q_3, sigma_x, sigma_y, angle);    
      std::cout << "Final parameters [xpos,ypos,A,BG,q_1 (sigma_x),q_2 (sigma_y),q_3 (angle[rad])]: " << xpos << ", " << ypos << ", " << A << ", " << BG << ", " << q_1 << " ("<<sigma_x<<"), " << q_2 << " ("<<sigma_y<<"), " << q_3 << " ("<<angle<<")" << std::endl << std::endl;
    #endif
      
    /// -- Output -- ///
    int exitflag = getTerminationType(summary);
        
      if(useMLErefine && exitflag>0)
      {
          int nr_optim_params = 0;
          for(int i =0; i<param_optimMask.size(); ++i)
              nr_optim_params += param_optimMask[i];
          
          // As some parameters might be specified as fixed, we need to build 
          // the parameter vector only using the ones that should be optimized
          double* parameters = new double[nr_optim_params];
          int cnt = 0;
          if(param_optimMask[0]) {parameters[cnt] = xpos; ++cnt;}
          if(param_optimMask[1]) {parameters[cnt] = ypos; ++cnt;}
          if(param_optimMask[2]) {parameters[cnt] = A; ++cnt;}
          if(param_optimMask[3]) {parameters[cnt] = BG; ++cnt;}
          if(param_optimMask[4]) {parameters[cnt] = q_1; ++cnt;}
		  if(param_optimMask[5]) {parameters[cnt] = q_2; ++cnt;}
		  if(param_optimMask[6]) {parameters[cnt] = q_3; ++cnt;}
          
          // Build the problem
          ceres::FirstOrderFunction* MLE_cost_function;

		  if(param_optimMask[6])
		  {
			  if(usePixelIntegratedGauss)
				  MLE_cost_function = new SampledGaussAnisoAngle_MLE_Cost(img, xCoords,yCoords, param_optimMask, xpos, ypos, A, BG, q_1, q_2, q_3);
				  // not implemented yet
				  // MLE_cost_function = new IntegratedGaussAnisoAngle_MLE_Cost(img, xCoords,yCoords, param_optimMask, xpos, ypos, A, BG, q_1, q_2, q_3);
			  else
				  MLE_cost_function = new SampledGaussAnisoAngle_MLE_Cost(img, xCoords,yCoords, param_optimMask, xpos, ypos, A, BG, q_1, q_2, q_3);
		  }
		  else
		  {
			  if(param_optimMask[5])
			  {
				if(usePixelIntegratedGauss)
				  MLE_cost_function = new IntegratedGaussAniso_MLE_Cost(img, xCoords,yCoords, param_optimMask, xpos, ypos, A, BG, q_1, q_2);
				else
				  MLE_cost_function = new SampledGaussAniso_MLE_Cost(img, xCoords,yCoords, param_optimMask, xpos, ypos, A, BG, q_1, q_2);
			  }
			  else
			  {
				  if(usePixelIntegratedGauss)
					MLE_cost_function = new IntegratedGauss_MLE_Cost(img, xCoords,yCoords, param_optimMask, xpos, ypos, A, BG, q_1);
				  else
					MLE_cost_function = new SampledGauss_MLE_Cost(img, xCoords,yCoords, param_optimMask, xpos, ypos, A, BG, q_1);
			  }
		  }

          GradientProblem MLEproblem(MLE_cost_function);

          GradientProblemSolver::Options MLEoptions;
          MLEoptions.max_num_iterations = 100;
          MLEoptions.logging_type = ceres::SILENT; // default: ceres::PER_MINIMIZER_ITERATION
    //               MLEoptions.function_tolerance = 1e-6; // default: 1e-6
    //               MLEoptions.gradient_tolerance = 1e-10; // default: 1e-10
          MLEoptions.line_search_direction_type = ceres::NONLINEAR_CONJUGATE_GRADIENT; // Note: default L-BFGS solver does not handle negative cost functions well (works somehow, but never reports convergence)
          MLEoptions.minimizer_progress_to_stdout = false;
          
          GradientProblemSolver::Summary MLEsummary;
          Solve(MLEoptions, MLEproblem, parameters, &MLEsummary);

          // Assign the result. 
          // Again: The parameter vector contains only the non-fixed parameters.
          cnt = 0;
          if(param_optimMask[0]) {xpos  = parameters[cnt]; ++cnt;}
          if(param_optimMask[1]) {ypos  = parameters[cnt]; ++cnt;}
          if(param_optimMask[2]) {A     = parameters[cnt]; ++cnt;}
          if(param_optimMask[3]) {
              // Minimum Background for MLE is 0.1 (see cost functions)
              if( parameters[cnt] < 0.01)
                  BG = 0.01;
              else
                  BG    = parameters[cnt]; ++cnt;
          }
          if(param_optimMask[4]) {q_1 = parameters[cnt]; ++cnt;}
		  if(param_optimMask[5]) {q_2 = parameters[cnt]; ++cnt;}
		  if(param_optimMask[6]) {q_3 = parameters[cnt]; ++cnt;}
          
          // If one of the solvers returned convergence without the other one reporting failure,
          // we take the optimization as succeeded. (Sometimes the MLE will return NO_CONVERGENCE
          // if the least squared fit was already very good and not change anything).
          int MLEexitflag = getTerminationType(MLEsummary);
          if( MLEexitflag == 1 || exitflag == 1) // one solver converged?
              exitflag = 1;
          if(MLEexitflag == -2 || exitflag == -2) // one solver failed?
              exitflag = -2;

          #ifdef DEBUG
            std::cout << MLEsummary.BriefReport() << "\n";
            std::cout << "Final MLE parameters [xpos,ypos,A,BG,q_1,q_2,q_3]: " << xpos << ", " << ypos << ", " << A << ", " << BG << ", " << q_1 << ", " << q_2 << ", " << q_3 << "\n";
          #endif
          
          delete[] parameters; // Cleanup
      }
    
    std::vector<double> results(8);
    results[0] = xpos;
    results[1] = ypos;
    results[2] = A;
    results[3] = BG;
    results[4] = q_1;
	results[5] = q_2;
	results[6] = q_3;
    results[7] = exitflag;
    
    return results;
}




void estimateInitialConditions(Array2D& img, std::vector<int>& xCoords, std::vector<int>& yCoords, double& xpos,double& ypos,double& A,double& BG,double& sigma_x,double& sigma_y,double& angle, double& q_1, double& q_2, double& q_3, std::vector<bool>& param_optimMask)
{
	q_1 = std::numeric_limits<double>::quiet_NaN();
	q_2 = q_1;
	q_3 = q_1;

	double sigma_x_guess, sigma_y_guess, angle_guess;
	
	// Background guess
    // -> take mean along the border pixels
    if(BG<0)
    {
        BG = 0;
        {
            unsigned int iCol = 0;
            unsigned int iRow = 0;

            for(iRow = 0; iRow<img.nRows; ++iRow)
                BG += img(iRow,iCol);

            iCol = img.nCols-1;
            for(iRow = 0; iRow<img.nRows; ++iRow)
                BG += img(iRow,iCol);

            iRow = 0;
            for(iCol = 1; iCol<img.nCols-1; ++iCol)
                BG += img(iRow,iCol);

            iRow = img.nRows-1;
            for(iCol = 1; iCol<img.nCols-1; ++iCol)
                BG += img(iRow,iCol);

            BG = BG/(2*img.nRows + 2*(img.nCols-2));
        }
    }

	if( !(sigma_x>0 && sigma_y>0 && angle!= 0 ) )
	{
	// sigma y and angle are not given and should not be optimized
	bool psf_is_isotropic(  !param_optimMask[5] && !param_optimMask[6] && sigma_y<0 && angle==0 );
	
	double m_00 = 0, m_10 = 0, m_01 = 0, m_11 = 0, m_20 = 0, m_02 = 0, img_tmp = 0, tau = 0, delta = 0, theta = 0, q_tmp, sigma_part = 0;
	if( !psf_is_isotropic || sigma_x < 0)
	{
		// calculate intensity moments
		for(unsigned int iCol = 0; iCol<img.nCols; ++iCol)
		{
			for(unsigned int iRow = 0; iRow<img.nRows; ++iRow)
			{
                // Set img_tmp to zero if negative
                img_tmp = (img(iRow,iCol)-BG);
                if( img_tmp< 0 )
                    continue;
                
				m_00 += img_tmp;
				m_10 += iCol*img_tmp;
				m_01 += iRow*img_tmp;
				m_11 += iCol*iRow*img_tmp;
				m_20 += iCol*iCol*img_tmp;
				m_02 += iRow*iRow*img_tmp;
			}
		}

		//normalize and build cov matrix
		m_10 /= m_00, m_01 /= m_00, m_11 /= m_00, m_20 /= m_00, m_02 /= m_00;
		m_20 -= m_10*m_10, m_02 -= m_01*m_01, m_11 -= m_10*m_01;


		//calculate trace and determinant and from that, q_1 to q_3
		tau = m_20+m_02;
		delta = m_20*m_02-m_11*m_11;
		theta = 0.5*atan(2*m_11/(m_20-m_02));

		sigma_part = sqrt(tau*tau-4*delta);
	}
	

	// If fitting a non-isotropic rotated Gaussian, initial parameters will be guessed by SVD
	if(param_optimMask[6] || angle != 0)
	{
		if (tau > sigma_part) // is positive definite?
		{
			if (theta != 0)
			{
				q_1 = cos(theta)*cos(theta)/(tau+sigma_part)+sin(theta)*sin(theta)/(tau-sigma_part);
				q_2 = sin(theta)*sin(theta)/(tau+sigma_part)+cos(theta)*cos(theta)/(tau-sigma_part);
				q_3 = -0.5*sin(2*theta)/(tau+sigma_part)+0.5*sin(2*theta)/(tau-sigma_part);
                
				if(m_20 <= m_02)
				{
					swap(q_1,q_2);
					q_3 = -q_3;
				}
			}
			else
			{
				q_1 = 0.5/m_20;
				q_2 = 0.5/m_02;
				q_3 = 0;
				if(m_20 <= m_02)
				{
					swap(q_1,q_2);
				}
			}
		}
		else
		{ // Default guess for fluorescence images in case estimation failes (sigma = 1.25 px)
			q_1 = 0.5/(1.25*1.25);
			q_2 = 0.5/(1.25*1.25);
			q_3 = 0.1;
		}


	}
	else if(  param_optimMask[5] || sigma_y >=0  )
	{
		q_3 = 0;
		if (sigma_x < 0 || sigma_y < 0)
		{
			if (tau > sigma_part) // is positive definite?
			{
				// check largest EV
				q_1 = 0.5/m_20;
				q_2 = 0.5/m_02;
				if(m_20 <= m_02)			
				{
					swap(q_1,q_2);
				}
			}
			else
			{
                // Default guess for fluorescence images in case estimation failes (sigma = 1.25 px)
				q_1 = 0.5/(1.25*1.25);
				q_2 = 0.5/(1.25*1.25);
			}
		}
	}
	else if(sigma_x<0)
	{
        if(tau>0)
            q_1 = 1/tau;
        else // Default guess for fluorescence images in case estimation failes (sigma = 1.25 px)
            q_1 = 0.5/(1.25*1.25);
		q_2 = -1;
		q_3 = 0;
	} 
		   
	bool computed_q_vals = (q_1==q_1) && (q_2==q_2) && (q_3==q_3); // check for NaN
	if( computed_q_vals )
	{
        convert_Qs_To_SxSyAngle(q_1, q_2, q_3, sigma_x_guess, sigma_y_guess, angle_guess);	
	} else
    {
        q_1 = -1;
        q_2 = -1;
        q_3 = 0;
        
        sigma_x_guess = -1;
        sigma_y_guess = -1;
        angle_guess = 0;
    }
	
	}
    
	if(param_optimMask[6] || angle != 0)
	{
		  angle = fmod(angle,M_PI);
		 //if(angle>= M_PI/4 && angle && angle< M_PI/4) // valid domain of definition
		  if(angle>= 0.25*M_PI && angle < 0.75*M_PI)
		  {  
			angle -= 0.5*M_PI;
			swap(sigma_x,sigma_y);
		  } else if( fabs(angle) > 0.75*M_PI  || angle == 0.75*M_PI)
		  {
			angle = (angle<0) ? angle+M_PI : angle-M_PI;
		  } else if ( angle >= -0.75*M_PI && angle < -0.25*M_PI)
		  {
			  angle += 0.5*M_PI;
			  swap(sigma_x,sigma_y);
		  }
	}
		
	if(sigma_x < 0 )
		sigma_x = sigma_x_guess;
	
	if(sigma_y < 0 )
		sigma_y = sigma_y_guess;
	
	if(param_optimMask[6] && angle == 0)
    {
		angle = angle_guess;
    }
	

			
	if(angle == 0)
	{
		q_1 = 1/(2*sigma_x*sigma_x);
		if(sigma_y>0)
			q_2 = 1/(2*sigma_y*sigma_y);
		else
			q_2 = -1;
		q_3 = 0;
	} else
	{
        
		q_1 = cos(angle)*cos(angle)/(2*sigma_x*sigma_x)+sin(angle)*sin(angle)/(2*sigma_y*sigma_y);
        q_2 = sin(angle)*sin(angle)/(2*sigma_x*sigma_x)+cos(angle)*cos(angle)/(2*sigma_y*sigma_y);
        q_3 = -sin(2*angle)/(4*sigma_x*sigma_x)+sin(2*angle)/(4*sigma_y*sigma_y);
	}
	
	
   
    // If xpos, ypos or amplitude should be guessed, find the image maximum value and its position. For non-isotropic Gaussians, calculate centroid and eccentricity
	double xpos_tmp = 0, ypos_tmp = 0, img_max = 0;
    if(xpos < 0  || ypos < 0 || A < 0) 
    {
		for(unsigned int iCol = 0; iCol<img.nCols; ++iCol)
		{
			for(unsigned int iRow = 0; iRow<img.nRows; ++iRow)
			{
				if(img(iRow,iCol)>img_max )
				{
					img_max = img(iRow,iCol);
					xpos_tmp = iCol;
					ypos_tmp = iRow;
				}
			}
		}
    }
    
    // Set x position guess
    if(xpos<0)
        xpos = xCoords[int(xpos_tmp)];
    
    // Set y position guess
    if(ypos<0)
        ypos = yCoords[int(ypos_tmp)];
        
    // Amplitude guess
    if (A<0)
        A = img_max-BG;        
}

void setParameterBounds(Problem& problem, Array2D& img, std::vector<int>& xCoords, std::vector<int>& yCoords, double& xpos,double& ypos,double& A,double& BG,double& q_1,double& q_2,double& q_3)
{
    problem.SetParameterLowerBound(&xpos,  0, xCoords[0]-1);
    problem.SetParameterUpperBound(&xpos,  0, xCoords[xCoords.size()-1]+1);
    problem.SetParameterLowerBound(&ypos,  0, yCoords[0]-1);
    problem.SetParameterUpperBound(&ypos,  0, yCoords[yCoords.size()-1]+1);
    problem.SetParameterLowerBound(&A,     0, 0);
    problem.SetParameterUpperBound(&A,     0, 2*(A+BG));
    problem.SetParameterLowerBound(&BG,    0, 0);
    problem.SetParameterUpperBound(&BG,    0, 2*(A+BG));
    problem.SetParameterLowerBound(&q_1, 0, 0.5/std::pow(max(img.nRows,img.nCols),2));
    //problem.SetParameterUpperBound(&q_1, 0, INF);
	if(problem.HasParameterBlock(&q_2))
	{
		problem.SetParameterLowerBound(&q_2, 0, 0.5/std::pow(max(img.nRows,img.nCols),2));
		//problem.SetParameterUpperBound(&q_2, 0, 0.INF);
	}
	if(problem.HasParameterBlock(&q_3))
	{
		//problem.SetParameterLowerBound(&q_3, 0, -INF);
		//problem.SetParameterUpperBound(&q_3, 0, INF);
	}
}

int getTerminationType(Solver::Summary& summary)
{
    int exitflag;
    
    switch(summary.termination_type)
    {
        // Minimizer terminated because one of the convergence criterion set by the user was satisfied.
        // 1.  (new_cost - old_cost) < function_tolerance * old_cost;
        // 2.  max_i |gradient_i| < gradient_tolerance
        // 3.  |step|_2 <= parameter_tolerance * ( |x|_2 +  parameter_tolerance)
        // The user's parameter blocks will be updated with the solution.
        case(ceres::CONVERGENCE): exitflag = 1; break;
        // Using an IterationCallback object, user code can control the
        // minimizer. The following enums indicate that the user code was
        // responsible for termination.
        // Minimizer terminated successfully because a user
        // IterationCallback returned SOLVER_TERMINATE_SUCCESSFULLY.
        // The user's parameter blocks will be updated with the solution.
        case(ceres::USER_SUCCESS): exitflag = 2; break;
        // The solver ran for maximum number of iterations or maximum amount
        // of time specified by the user, but none of the convergence
        // criterion specified by the user were met. The user's parameter
        // blocks will be updated with the solution found so far.
        case(ceres::NO_CONVERGENCE): exitflag = -1; break;
        // The minimizer terminated because of an error.  The user's parameter blocks will not be updated.
        case(ceres::FAILURE): exitflag = -2; break;
        // Minimizer terminated because because a user IterationCallback returned SOLVER_ABORT.
        // The user's parameter blocks will not be updated.
        case(ceres::USER_FAILURE): exitflag = -3; break;
        default:
        {
            char warnMsg[256];
            sprintf(warnMsg, "Solver returned with unkown termination type: %i!\n", summary.termination_type);
            mexWarnMsgTxt(warnMsg);
        }
    }
    
    return exitflag;
}

int getTerminationType(GradientProblemSolver::Summary& summary)
{
    int exitflag;
    
    switch(summary.termination_type)
    {
        // Minimizer terminated because one of the convergence criterion set by the user was satisfied.
        // 1.  (new_cost - old_cost) < function_tolerance * old_cost;
        // 2.  max_i |gradient_i| < gradient_tolerance
        // 3.  |step|_2 <= parameter_tolerance * ( |x|_2 +  parameter_tolerance)
        // The user's parameter blocks will be updated with the solution.
        case(ceres::CONVERGENCE): exitflag = 1; break;
        // Using an IterationCallback object, user code can control the
        // minimizer. The following enums indicate that the user code was
        // responsible for termination.
        // Minimizer terminated successfully because a user
        // IterationCallback returned SOLVER_TERMINATE_SUCCESSFULLY.
        // The user's parameter blocks will be updated with the solution.
        case(ceres::USER_SUCCESS): exitflag = 2; break;
        // The solver ran for maximum number of iterations or maximum amount
        // of time specified by the user, but none of the convergence
        // criterion specified by the user were met. The user's parameter
        // blocks will be updated with the solution found so far.
        case(ceres::NO_CONVERGENCE): exitflag = -1; break;
        // The minimizer terminated because of an error.  The user's parameter blocks will not be updated.
        case(ceres::FAILURE): exitflag = -2; break;
        // Minimizer terminated because because a user IterationCallback returned SOLVER_ABORT.
        // The user's parameter blocks will not be updated.
        case(ceres::USER_FAILURE): exitflag = -3; break;
        default:
        {
            char warnMsg[256];
            sprintf(warnMsg, "Solver returned with unkown termination type: %i!\n", summary.termination_type);
            mexWarnMsgTxt(warnMsg);
        }
    }
    
    return exitflag;
}














