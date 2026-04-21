% Fast mex for the mode and robust variance of a sample based on its fitted histogram.
%       See also: imUtil.background.modeVar_SampleHist
%   This function estimates the mode and a variance-like width of a
%   sample using a histogram-based approximation. 
% 
%   This function should be executed using:
%       imUtil.background.modeVar_SampleHist
%   For more details see this function.
%
% Input  : - An array of numeric values (single or double).
%          - A structure with arguments specified in:
%            imUtil.background.modeVar_SampleHist
% Output : - Estimated mode of the sample.
%          - Estimated variance of the sample. For method 1, this is
%            derived from the curvature of the quadratic fit to
%            log(histogram counts). For method 2, this is the square of
%            a robust sigma estimate based on the normalized MAD.
%          - Method used:
%               1 - Histogram fitting to log(counts).
%               2 - Histogram fitting failed or was poorly constrained;
%                   estimate is based on the median and normalized MAD.
%
% Author : ChatGPT + Eran Ofek (Apr 2026)
% Complation: mex -O CXXFLAGS='$CXXFLAGS -O3 -fopenmp -DNDEBUG -march=native' LDFLAGS='$LDFLAGS -fopenmp' modeVar_SampleHist_mex.cpp
% Example: [m,v]=modeVar_SampleHist_mex(Im); 