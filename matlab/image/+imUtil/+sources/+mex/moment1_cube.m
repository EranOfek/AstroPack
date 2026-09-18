% A fast mex for calculating the Gaussian weighted 1st central moments of images in a cube.
%   The stamps must be background-subtracted. Each pixel is weighted by
%   I*g/(Var + A*g), where g is a normalized Gaussian of width SigmaWidth
%   centered on the current position, Var is the per-slice noise variance
%   level and A the source amplitude estimated once from the stamp center;
%   i.e. signal x PSF weight / variance: Gaussian-windowed for faint sources,
%   flat for bright ones.
%   The code do at least two iteartions and upto MaxIter iterations.
%   In the first iteration, there may be a different weight used, and the max step size
%   is limited by MaxStepSize1.
%   The centeroid is converged after two sucessive iterations has a
%   proposed (unclamped) step size smaller then the threshold
%   min(SigmaWidth/SN, MaxStepSize).
%   The moment is calculated in a circuler support with radius
%   K*SigmaWidth(2) (at most the half size of the stamps), fixed on the
%   stamp center.
%
% Input  : - A cube of images. The image index is in the 3rd dim.
%          - (Var) A vector of noise variance level (per image slice),
%            e.g., sky + RN^2 or the annulus std^2. Enters only the pixel
%            weights; 0 degrades the estimator to a plain centroid inside
%            the support disc (issue #1275).
%          - (SN) A vector of S/N per image slice.
%            This will be used for the convergence. The calculation is
%            converged when the proposed shift between two iterations is
%            smaller than min(SigmaWidth/SN, MaxStepSize).
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
% Example: [X1,Y1,Con]=imUtil.sources.mex.moment1_cube(CubeBS,Var,SN,10,1.5);