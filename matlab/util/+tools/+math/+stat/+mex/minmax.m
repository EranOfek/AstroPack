% Calculate te min and max of an array of any dimension in one pass (fast mex)
% Input  : - An array (single or double)
% Output : - Min.
%          - Max.
%          - Min index.
%          - Max index.
% Author : Eran Ofek (Aug 2024)
% Compilation: mex CFLAGS="\$CFLAGS -fopenmp" LDFLAGS="\$LDFLAGS -fopenmp" minmax.c 
% Example: R=randn(1700,1700);                                                 
%          tic; for I=1:1:1000, [Min,MinInd]=min(R,[],'all','linear'); [Max, MaxInd]=max(R,[],'all','linear'); end, toc
%          tic; for I=1:1:1000, [Min1,Max1,MinInd1,MaxInd1]=tools.math.stat.mex.minmax(R);  end, toc
%          [Min-Min1, Max-Max1, MinInd-MinInd1, MaxInd-MaxInd1]
