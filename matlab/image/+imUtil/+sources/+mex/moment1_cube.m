% A fast mex for calculating the Gaussian weighted 1st central moments of images in a cube.
%   The code do at least two iteartions and upto MaxIter iterations.
%   In the first iteration, there may be a different weight used, and the max step size
%   is limited by MaxStepSize1.
%   The centeroid is converged after two sucessive iterations has a step
%   size smaller then the threshold (i.e., SigmaWidth/SN).
%   The moment is calculated in a circuler support with radius equal to the
%   half size of the stamps.
%   The first moment is calculated using "RESPONSIBILITY-WEIGHTED SCHEME"
%   (no PSF model, no iteration):"
%   For observed pixel value n and background B (same units):\n"
%     r = max(1 - B/n, 0) for n>0, else r=0\n"
%     s = n*r = max(n - B, 0)\n"
%   This is a fast soft-assignment of counts to 'source' vs 'background' that avoids\n"
%   negative weights / cancellation from direct background subtraction.\n"
%
% Input  : - A cube of images. The image index is in the 3rd dim.
%          - A vector of background (per image slice).
%          - (SN) A vector of S/N per image slice.
%            This will be used for the convergence. The calculation is
%            converged when the shift between two iterations is smaller than
%            SigmaWidth/SN.
%          - (MaxIter) Max. number of iterations. Default is 10.
%          - (SigmaWidth) sigma-width of the Gaussian weight function.
%            If two element vector, then the first is used only in the 1st
%            iteration, and the 2nd for all the other iterations.
%            Default is 1.5.
%          - (K) When calculate weights, truncate pixels
%            outside (+/- K*SigmaWidth). This may speed up the code.
%            Default is 3.
%          - (RelToCenter) If true, then the output X and Y are relative to
%            the image slice center. If false, then relative to corner.
%            Default is true.
%          - (MaxStepSize) Maximum step size in X and Y between uterations.
%            Default is 1/(sqrt(2)*MaxIter).
%          - (MaxStepSize1) The Max. step size allowed in the 1st
%            iteration. Default is MaxStepSize.
% Output : - A vector of 1st central moment in the X direction  (per image slice).
%          - A vector of 1st central moment in the Y direction (per image slice).
%          - A vector of iteartion number in which each slice converged.
%            NaN if not converged.
% Compilation:  mex -O CXXFLAGS="\$CXXFLAGS -O3 -std=c++17 -march=native -fopenmp" LDFLAGS="\$LDFLAGS -fopenmp" wcenteroid_cube.cpp
% Example: [X1,Y1,Con]=imUtil.sources.mex.moment1_cube(Cube,SN,10,1.5);