% A fast mex for calculating: squeez(sum(A.*B,[1 2])).*Norm.
% Input  : - (A) A 3D array (single or double).
%          - (B) A 3D array (with size equal to that of the 1st input).
%          - (Norm) A vector wich length is identical to the 3rd dimension
%            of the 3D array.
% Output : - A column vector.
% Compilation: mex CXXFLAGS="\$CXXFLAGS -fopenmp -std=c++11 -mavx2" LDFLAGS="\$LDFLAGS -fopenmp" squeezeSumAmultB_Dim12.cpp
% Author : Eran Ofek (Aug 2024)
% Example: A=rand(25,25,1000);
%          B=rand(25,25,1000);
%          Norm=rand(1000,1); 
%          tic;for I=1:1000, C=tools.array.mex.squeezeSumAmultB_Dim12(A,B,Norm);end, toc
%          tic;for I=1:1000, C1=squeeze(sum(A.*B,[1 2])).*Norm; end, toc                
%          max(abs(C-C1))
