% hist2d of the results of X-X.' and Y-Y.'
%   A utility fast mex function for find_shift_pairs:
%   [H2,VecX,VecY] = hist2d_VVtrans_fix(Xcat,Ycat,Xref,Yref,FlipX,FlipY,RangeX,StepX,RangeY,StepY);
%   This function is equivalent to:
%       Dx=Xcat-FlipX.*Xref.';
%       Dy=Ycat-FlipY.*Yref.';
%       EdgesX = (RangeX(1):StepX:RangeX(2));
%       EdgesY = (RangeY(1):StepY:RangeY(2));
%       [H2] = histcounts2(Dx(:),Dy(:), EdgesX, EdgesY);
%       VecX = (EdgesX(1:end-1) + EdgesX(2:end)).*0.5;
%       VecY = (EdgesY(1:end-1) + EdgesY(2:end)).*0.5;
% Input  : - Xcat
%          - Ycat
%          - Xref
%          - Yref
%          - FlipX
%          - FlipY
%          - RangeX  [Xmin Xmax]
%          - StepX
%          - RangeY  [Ymin Ymax]
%          - StepY
% Output : - 2D histogram
%          - VecX of bins centers.
%          - VecY of bins centers.
% Author : Eran Ofek (2025 Oct) 
% Compilation: mex -O CXXFLAGS="-std=c++17 -O3 -march=native -fopenmp" LDFLAGS="$LDFLAGS -fopenmp" hist2d_VVtrans.cpp
% Example: Xcat=rand(1e3,1).*1024; Ycat=rand(1e3,1).*1024; Xref=[Xcat+2;1]; Yref=[Ycat+1;2];
%          FlipX=1; FlipY=1;
%          RangeX=[-2000 2000]; 
%          RangeY=[-1000 1000]; 
%          StepX=400;
%          StepY=400;
%          [H2,VecX,VecY] = hist2d_VVtrans_fix(Xcat,Ycat,Xref,Yref,FlipX,FlipY,RangeX,StepX,RangeY,StepY)