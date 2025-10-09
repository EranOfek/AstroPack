% (MEX) Find distance and angle between pairs of sources
%   Equivalent to: [CatDist, CatTan]=distAngPairs_mex(CatX,CatY,MaxDist, FlagClean, FlipX, FlipY)
%       CatX = CatX.*FlipX; CatY = CatY.*FlipY;
%       CatDiffX = CatX - CatX.'; CatDiffY = CatY - CatY.';
%       Fc = abs(CatDiffX(:))<MaxDist & abs(CatDiffY(:))<MaxDist;
%       CatDiffX = CatDiffX(Fc); CatDiffY = CatDiffY(Fc);
%       CatDist  = sqrt(CatDiffX.^2 + CatDiffY.^2);
%       CatTan   = atan(CatDiffY./CatDiffX);
%       Where FlagClean true return only the upper triangle distances
%       (unique pairs).
% Input  : - A column vector of X position.
%          - A column vector of Y position.
%          - Maximum distance to include in the output.
%          - A logical flag indicating if to remove duplicates
%            (i.e., use only upper triangle pairs).
%            Default is true.
%          - Flip X coordinate. Default is 1.
%          - Flip Y coordinate. Default is 1.
% Output : - A vector of the distances between all selected pairs.
%          - A vector of angle between all selected pairs
% Author : Eran Ofek (2025 Oct) 
% Compilation:mex -O CXXFLAGS="\$CXXFLAGS -fopenmp -march=native" LDFLAGS="\$LDFLAGS -fopenmp" distAngPairs_mex.cpp
% Example: [CatDist, CatTan]=distAngPairs_mex(CatX,CatY,MaxDist, FlagClean, FlipX, FlipY)