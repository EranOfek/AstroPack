% Fast mex for the operation: squeeze(sum(WInt.*MatXcen,[1 2],'omitnan')).*Norm
% Input  : - WInt. An NxNxM cube (single or double).
%          - MatXcen. An NxN matrix (single or double).
%          - Norm. A Mx1 colum vector (single or double).
% Output : - The result of the operator.
%            A Mx1 column vector. Class like the input arguments class.
% Author : Eran Ofek (Aug 2024)
% Compilation: mex CXXFLAGS="\$CXXFLAGS -O3 -mavx" squeezeSumCubeMatNorm.cpp
% Example: MatXcen=rand(15,15);       
%          Norm=ones(1000,1);
%          WInt=imUtil.kernel2.gauss(ones(1000,1)+randn(1000,1));
%          tic;for I=1:10000, a=tools.array.mex.squeezeSumCubeMatNorm(WInt, MatXcen, Norm);end, toc
%          % Elapsed time is 0.689787 seconds.
%          tic;for I=1:10000, b=squeeze(sum(WInt.*MatXcen,[1 2],'omitnan')).*Norm; end, toc        
%          % Elapsed time is 1.020883 seconds.
%          max(abs(a2-b))