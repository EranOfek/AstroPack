function Std=rstd(Image,varargin) 
% robust std (default is for a matrix) calculated using scaled iqr
% Package: @imUtil.background
% Description: robust std of an array. Note that using the 
%              imUtil.background.mode function is much faster.
% Input  : - An array.
%          - Vector of dimensions over which to calculate the robust
%            std. Default is [1 2].
% Output : - The robust median calculated using the scaled iqr
%      By: Eran O. Ofek                       Apr 2020             
% Example: imUtil.background.rstd(randn(1000,1000))


if isempty(varargin)
    varargin = {[1 2]};
end

Factor = 0.7413;  %  = 1./norminv(0.75,0,1)

Std = (iqr(Image,varargin{:}).*Factor);
