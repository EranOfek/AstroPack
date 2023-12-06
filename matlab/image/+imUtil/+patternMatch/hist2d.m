function [Mat,VecX,VecY,BinX,BinY]=hist2d(Xv,Yv,RangeX,RangeY,StepX,StepY)
% calculate the 2-D histogram of 2-D data set.
%       OBSOLETE: A faster version is in: tools.array.hist2d_fast
% Package: imUtil.patternMatch
% Description: calculate the 2-D histogram of 2-D data set.
% Input  : - Vector of X coordinates.
%          - Vector of Y coordinates.
%          - If 4 input arguments are provided, then this is the vector of
%            X edges in which the histogram will be calculated with.
%            If 6 input arguments are provided, then this is the X-axis
%            range of the histogram [min max].
%          - If 4 input arguments are provided, then this is the vector of
%            Y edges in which the histogram will be calculated with.
%            If 6 input arguments are provided, then this is the Y-axis
%            range of the histogram [min max].             
%          - Step size of X histogram.
%          - Step size of Y histogram.
% Output : - 2-D histogram
%          - Vector of X coordinate of center of X bins.
%          - Vector of Y coordinate of center of Y bins.
%          - The index array BinX (see histcounts2).
%          - The index array BinY (see histcounts2).
% Tested : Matlab 2011b
%     By : Eran O. Ofek                    Feb 2013
%    URL : http://weizmann.ac.il/home/eofek/matlab/
% Example: Xv=rand(100000,1).*2; Yv=rand(100000,1).*3+100; Mat=imUtil.patternMatch.hist2d(Xv,Yv,[0 2],[100 103],0.1,0.1);
% Reliable: 2
%--------------------------------------------------------------------------

%reject points out of range
% I    = find(Xv>=RangeX(1) & Xv<=RangeX(2) & Yv>=RangeY(1) & Yv<=RangeY(2));
% Xv   = Xv(I);
% Yv   = Yv(I);
% 
% NXv  = (Xv - RangeX(1))./StepX;
% NYv  = (Yv - RangeY(1))./StepY;
% VecX = (RangeX(1):StepX:(RangeX(2)-StepX)).' + StepX.*0.5;
% VecY = (RangeY(1):StepY:(RangeY(2)-StepY)).' + StepY.*0.5;
% 
% XY   = NYv + floor(NXv).*numel(VecY); 
% N    = histc(XY,(0:1:numel(VecX).*numel(VecY)).');
% N    = N(1:end-1);
% 
% Mat  = reshape(N,length(VecY),length(VecX));

% using histcounts2 spped it up


if nargin<4
    % assume RangeX/Y contains vector of edges
    VecX = RangeX;
    VecY = RangeY;
else
    % assume range and step are give
    
    VecX = (RangeX(1):StepX:RangeX(2)).';  %-StepX)).' + StepX.*0.5;
    VecY = (RangeY(1):StepY:RangeY(2)).';  %-StepY)).' + StepY.*0.5;
end

Flag = Xv>RangeX(1) & Xv<RangeX(2) & Yv>RangeY(1) & Yv<RangeY(2);

if nargout>3
    [Mat,~,~,BinX,BinY] = histcounts2(Xv(Flag),Yv(Flag),VecX,VecY);
    Mat = Mat.';
else
    Mat                 = histcounts2(Xv(Flag),Yv(Flag),VecX,VecY).';
end

VecX = (VecX(1:end-1) + VecX(2:end)).*0.5;
VecY = (VecY(1:end-1) + VecY(2:end)).*0.5;


% using accumarray - faster
% [~,binx] = histcountsmex(Xv,VecX);
% [~,biny] = histcountsmex(Yv,VecY);
% 
% countslenx = length(VecX)-1;
% countsleny = length(VecY)-1;
% % Filter out NaNs and out-of-range data
% subs = [binx(:) biny(:)];
% subs(any(subs==0,2),:) = [];
% n = accumarray(subs,ones(size(subs,1),1),[countslenx countsleny]);


