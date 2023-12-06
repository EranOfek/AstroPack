function Variance=rvar(Image,varargin) 
% robust variance (default is for a matrix) calculated using scaled iqr
% Package: @imUtil.background
% Description: robust variance of an array. Note that using the 
%              imUtil.background.modeVar_LogHist function is much faster.
% Input  : - An array.
%          - Vector of dimensions over which to calculate the robust
%            variance. Default is [1 2].
% Output : - The robust median calculated using the scaled iqr
%      By: Eran O. Ofek                       Apr 2020             
% Example: imUtil.background.rvar(randn(1000,1000))


if isempty(varargin)
    varargin = {[1 2]};
end

% iqr is ~3 times slower compared to tools.math.stat.iqrFast
FunIQR = @tools.math.stat.iqrFast; % @iqr; 
%FunIQR = @iqr;  

Factor = 0.7413;  %  = 1./norminv(0.75,0,1)

Variance = (FunIQR(Image,varargin{:}).*Factor).^2;
