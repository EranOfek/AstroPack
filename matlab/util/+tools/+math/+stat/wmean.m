function [M,E,S]=wmean(Vec,Err,Dim,IgnoreNaN)
%--------------------------------------------------------------------------
% wmean function                                                 AstroStat
% Description: Calculated the weighted mean of a sample.
% Input  : - Either a two column matrix [Val, Err] or a matrix of values,
%            while the errors are in the second argument.
%          - Optional matrix of errors. If given, then the first input
%            argument is treated as values.
%          - If the first two input arguments are provided then this is the
%            dimension along to calculate the weighted mean.
%            Default is 1.
%          - Ignore nans. Default is true.
% Output : - Weighted mean.
%          - Weighted error on weighted mean.
%          - Weighted standard deviation.
% Tested : Matlab 7.0
%     By : Eran O. Ofek                    Jun 1998
%    URL : http://weizmann.ac.il/home/eofek/matlab/
% Example: [M,E]=tools.math.stat.wmean([1;2;3;4],[1;1;1;50]);
% Reliable: 2
%--------------------------------------------------------------------------
ColVal  = 1;
ColErr  = 2;

if (nargin<4)
    IgnoreNaN = true;
end

if (nargin>1)
    % Err is given in second argument
    if (nargin==2)
        % automatic Dim for row or column vectors
        if any(size(Vec)==1)
            Dim = find(size(Vec)>1);
        end
    end
else
    % Vec is a two coumn vector
    Err = Vec(:,ColErr);
    Vec = Vec(:,ColVal);
    Dim = 1;
end

% Ignore NaNs
if (IgnoreNaN)
    Flag = ~(any(isnan(Vec),Dim) & any(isnan(Err),Dim));
    if Dim==2
        Vec  = Vec(Flag,:);
        Err  = Err(Flag,:);
    else
        Vec  = Vec(:,Flag);
        Err  = Err(:,Flag);
    end
end

W = 1./double(Err.^2);  % weight (double to prevent Inf if small singles)
WS= sum(W,Dim);
E = sqrt(1./WS);
M = sum(Vec.*W,Dim)./WS;
S = sqrt((sum(W.*Vec.^2,Dim).*WS - sum(Vec.*W,Dim).^2)./ ...
              (WS.^2 - sum(W.^2,Dim)));

