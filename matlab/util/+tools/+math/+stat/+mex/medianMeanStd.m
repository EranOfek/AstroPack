% Fast (mex) calculation of median, mean, and std.
%   Faster than matlab for large atrices.
% Input  : - An array.
%          - Dimension along to calculate the median, mean, std
%          - 'omitnan','includenan'. Default 'includenan'.
% Output : - Median.
%          - Mean.
%          - Std.
% Author : Eran Ofek (2025 Sep) 
% Example: [M1,M2,M3]=tools.math.stat.mex.medianMeanStd(A,1);
%
% A=rand(1726,1726,20);
% tic;for i=1:1e1, [M1,M2,M3]=tools.math.stat.mex.medianMeanStd(A,3); end,toc
% %Elapsed time is 3.32 seconds.
% tic;for i=1:1e1, M1a=median(A,3); M2a=mean(A,3); M3a=std(A,[],3); end,toc  
% %Elapsed time is 10.398966 seconds.