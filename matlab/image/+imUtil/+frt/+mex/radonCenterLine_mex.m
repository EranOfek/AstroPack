% Calculate Radon transform through the image center (only) using mex
%   Return a vector of the sum along each line that passes through the
%   image center, with angle theta.
% Input  : - Image (single or double).
%          - Vector of rotation angles.
%          - If true then theta units are deg. Else, false.
%            Default is true.
%          - If true, use bilinear interpolation. Otherwise, nearest value.
%            Default is true.
%          - UseNormalAngle (optional).
%            If false (default): Theta is the line direction angle
%            If true: Theta is the normal angle (MATLAB radon convention)
%            measured from the +x axis.
% Compilation: mex -v -O CXXFLAGS="$CXXFLAGS -O3 -march=native -ffast-math -fopenmp -std=c++17" CXXOPTIMFLAGS="$CXXOPTIMFLAGS -O3 -march=native -ffast-math -fopenmp -std=c++17" LDFLAGS="$LDFLAGS -fopenmp" -largeArrayDims radonCenterLine_step1_mex.cpp
% Author : ChatGPT + Eran Ofek (Aug 2025)
% Example: P = imUtil.frt.mex.radonCenterLine_mex(Image);