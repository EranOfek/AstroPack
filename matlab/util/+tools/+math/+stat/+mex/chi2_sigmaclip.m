% Fast mex for calculating Chi^2 with sigma clipping
%   Analog to:
%   Z=(A-B)./C;
%   Is = find(Z>-LowNsigma & Z<HighNsigma);
%   Chi2=sum(( (A(Is)-B(Is))./C).^2);
%   Nused=numel(Is);
% Input  : - A vector of data.
%          - A vector of model.
%          - A vector or scalar of sigma error.
%          - A two element matrix [abs(LowNsigma), HighNsigma].
%            E.g., [3 3].
%          - A logical indicating if to ignore NaNs.
%            Default is true.
% Output : - Chi2 of selected data points.
%          - Number of used data points
%          - Optional vector of logical indices indicating if each element in the
%            vector was used for the chi^2 calculation.
% Author : Eran Ofek (2026 Jan) 
% Compilation: mex -O CXXFLAGS="\$CXXFLAGS -O3 -std=c++17 -march=native" chi2_sigmaclip.cpp
% Example: A=randn(1e4,1); B=randn(1e4,1); C=1;
%          [Chi1,Nused]=tools.math.stat.mex.chi2_sigmaclip(A,B,C,[2 2]);