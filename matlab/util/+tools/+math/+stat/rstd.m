function Rstd=rstd(Mat,Dim,Algo)
% Robust std calculated from the 50% inner percentile of the data.
% Package: Util.stat
% Description: Robust std calculated from the 50% inner percentile
%              of the data.
% Input  : - Matrix.
%          - Dimension along to calculate the std. Default is 1.
%          - Algorithm:
%            3 - use prctile.
%            1 - use direct prctile after sorting and taking the mean
%               up/down.
%            2 - use direct prctile after sorting
%            Default is 3. Best and fastest is 1.
% Output : - Robust std.
% License: GNU general public license version 3
% Tested : Matlab R2015b
%     By : Eran O. Ofek                    Mar 2016
%    URL : http://weizmann.ac.il/home/eofek/matlab/
% Example: Rstd=tools.math.stat.rstd(randn(1000,3))
% Reliable: 2
%--------------------------------------------------------------------------

if (nargin==1)
    Dim  = 1;
    Algo = 3;
end

Factor = 1.4826;  % = 1./norminv(0.75,0,1)

if Algo==1
    Mat     = sort(Mat, Dim);
    SizeMat = size(Mat);
    N       = SizeMat(Dim);
    Ilow    = floor(N.*0.25);
    Ihigh   = floor(N.*0.75);
    if Dim==1
        ValLow  = mean(Mat(Ilow:Ilow+1,:),1);
        ValHigh = mean(Mat(Ihigh:Ihigh+1,:),1);
    else
        ValLow  = mean(Mat(:,Ilow:Ilow+1),Dim);
        ValHigh = mean(Mat(:,Ihigh:Ihigh+1),Dim);
    end
elseif Algo==2
    Mat     = sort(Mat, Dim);
    SizeMat = size(Mat);
    N       = SizeMat(Dim);
    if Dim==1
        ValLow  = Mat(floor(N.*0.25),:);
        ValHigh = Mat(floor(N.*0.75),:);
    else
        ValLow  = Mat(:,floor(N.*0.25));
        ValHigh = Mat(:,floor(N.*0.75));
    end
    
elseif Algo==3
    ValLow  = prctile(Mat,25,Dim);
    ValHigh = prctile(Mat,75,Dim);
end

Rstd    = (ValHigh - ValLow).*0.5.*Factor;