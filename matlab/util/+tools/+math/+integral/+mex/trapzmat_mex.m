% Trapezoidal numerical integration on columns or rows of matrices (mex).
%   Trapezoidal numerical integration on columns or rows of matrices.
%   Contrary to trapz.m, the X input for this function can be a matrix.
% Input  : - X matrix or vector.
%          - Y matrix.
%          - Dimension along to preform the integration, default is 1.
% Output : - Vector of integrals.
% Compilation: mex -O CXXFLAGS="\$CXXFLAGS -O3 -march=native -ffast-math -DNDEBUG -fopenmp" LDFLAGS="\$LDFLAGS -fopenmp" trapzmat_mex.cpp
% Author : Eran Ofek (Jun 2009)
% Example: 1=tools.math.integral.mex.trapzmat_mex(W,Spec)