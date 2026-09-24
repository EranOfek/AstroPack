% Std using the mean absolute deviation (mad) function (MEX)
%   This is equivalent to: 1.253.*mean(abs(X - mean(X,Dim, 'omitnan')),Dim, 'omitnan');
%   For large arrays and Dim=3 this is faster than matlab, however, for
%   other options it may be slower.
%   See also: tools.math.stat.std_mad
% Input  : - An array (any dim), single or double.
%          - Dimensionality over to calculate the MAD.
%          - 0: include NaN; 1 - omit nan. Default is 1.
% Output : - MAD of array.
%          - Mean of array.
% Compilation: mex std_madmean_mex.cpp CXXFLAGS="\$CXXFLAGS -O3 -march=native -fopenmp" LDFLAGS="\$LDFLAGS -fopenmp"
% Author : Eran Ofek (2025 Jan) 
% Example: Data = single(randn(1700,1700,20));
%          tic;for i=1:10, [a,b]=tools.math.stat.mex.std_madmean_mex(Data,3,1);end, toc
%          tic;for i=1:10, a1=tools.math.stat.std_mad(Data,0,3);b1=mean(Data,3);end,toc
%          max(abs(a1-a),[],'all')
%          max(abs(b1-b),[],'all')