% Fast mex for fit 2D Gaussian with a known center, using linear fit and moments fallback
% Description: Fit a 2D Gaussian profile to a background-subtracted image
%          stamp whose peak is assumed to lie at the stamp centre.
%          The model is:
%            I(x,y) = A * exp{ -[(x/SigmaX)^2 - 2*Rho*(x/SigmaX)*(y/SigmaY)
%                                + (y/SigmaY)^2] / [2*(1-Rho^2)] }
%          where x = col - (Ncols-1)/2,  y = row - (Nrows-1)/2.
%
%          PRIMARY algorithm: intensity-weighted log-linear regression.
%            Taking ln(I) linearises the problem into a 4-parameter
%            weighted least-squares system solved in a single pass via
%            4x4 normal equations (no iteration, no initial guess).
%            Pixels below 0.1% of the peak are excluded (cannot take log).
%            Weights w = I are optimal for Poisson/photon-noise.
%          FALLBACK: intensity-weighted second-image-moments, used when
%            fewer than 5 pixels exceed the log-threshold or when the
%            normal matrix is singular (very faint / mostly-negative stamp).
%
% Input  : - Stamp  : 2D single or double image stamp (background subtracted).
%                     The Gaussian peak must be centred on the stamp.
%                     Minimum size: 3x3 pixels.
% Output : - Status    : Logical scalar. true if the fit succeeded and
%                        all parameters are physically valid (sigma > 0.1 px,
%                        |Rho| < 0.99, finite positive amplitude); false
%                        if the stamp is blank, entirely non-positive, or
%                        too small to constrain the model.
%          - TotalFlux : Integrated flux of the fitted Gaussian [same units
%                        as the input stamp]:
%                          TotalFlux = A * 2*pi * SigmaX * SigmaY * sqrt(1-Rho^2)
%                        This equals the exact integral of the model over
%                        the full plane, independent of stamp size.
%          - SigmaX    : Gaussian sigma along the column (x) direction [pixels].
%          - SigmaY    : Gaussian sigma along the row    (y) direction [pixels].
%          - Rho       : Correlation coefficient of the 2D Gaussian, i.e., the
%                        off-diagonal element of the normalised covariance
%                        matrix.  Range: (-1, 1).  Zero for an axis-aligned PSF.
%                        The position angle of the major axis is
%                          PA = 0.5 * atan2(2*Rho*SigmaX*SigmaY, SigmaX^2-SigmaY^2)
%          - RMS       : RMS of per-pixel residuals over the full stamp:
%                          RMS = sqrt( sum((I - I_model).^2) / Npix )
%                        Provides a goodness-of-fit metric; for a well-fitted
%                        Gaussian with Poisson noise RMS ~ sqrt(A) [counts^0.5].
%
% Compile: mex -O CXXFLAGS='$CXXFLAGS -O3 -march=native -ffast-math' fitGauss2D.cpp
%
% Author : Claude + Eran Ofek (May 2026)
% Example: [X, Y]  = meshgrid(-10:10, -10:10);
%          Stamp   = 1000 .* exp(-0.5.*(X.^2./9 + Y.^2./4));  % SigmaX=3, SigmaY=2, Rho=0
%          [Status, TotalFlux, SigmaX, SigmaY, Rho, RMS] = fitGauss2D(Stamp)
%          % Expected: SigmaX~3, SigmaY~2, Rho~0, TotalFlux~37699, RMS~0