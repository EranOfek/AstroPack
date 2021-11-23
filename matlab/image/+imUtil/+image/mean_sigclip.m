function [Mean,Var,FlagGood,GoodCounter]=mean_sigclip(Data,Dim,Args)
% Calculate the sigma-clipped mean of a dataset
% Package: imUtil.image
% Description: Calculate the sigma-clipped mean of a dataset with
%              iterations.
% Input  : - The dataset of any dimensionality.
%          - Dimension over which to calculate the mean.
%            For example, if you have a cube of images in which the
%            3rd dimension is the image index, and you want to calculate
%            the mean image. Set Dim=3.
%            Deafult is ndims(Data).
%          * Arbitrary number of ...,key,val,... arguments.
%            The following keywords are available:
%            'MeanFun' - Funtion handle for calculating the mean.
%                   Default is @nanamean.
%            'StdFun'  - A string indicating the method by which to
%                   calculate the data StD.
%                   Options are: 'std' - fo normal StD, or 'rstd' for
%                   robust StD calculated using imUtil.background.rvar.
%                   Default is 'rstd'.
%            'Nsigma' - [Lower, Upper] number of sigmas below/above to
%                   sigma clip the data.
%                   Default is [5 5].
%            'MaxIter' - Maximum number of iterations.
%                   Use 0 in order to calculate the mean without sigma
%                   clipping. Will stop before maximum number of iterations
%                   reached if no new points were clipped.
%                   Default is 3.
%            'EpsilonStd' - A small nuymber that will be added to the StD,
%                   in order to avoid division by zero. Default is 1e-12.
% Output : - Sigma clipped mean of data.
%          - Sigma clipped variance of the data.
%          - A logical array of the same size of the input Data which
%            indicate the good data points that were used for the mean
%            calculation.
%          - An image of number of images used in each pixel.
% License: GNU general public license version 3
% Tested : Matlab R2015b
%     By : Eran O. Ofek                    Sep 2016
%    URL : http://weizmann.ac.il/home/eofek/matlab/
% Example: Data = randn(10,10,100);
%          Data(4,4,18)=100; Data(1,1,1)=-14;  % some outliers
%          [Mean,Var,FlagGood,NC]=imUtil.image.mean_sigclip(Data,3);
%          [i,j,k]=ind2sub(size(Data),find(~FlagGood))
% Reliable: 2
%--------------------------------------------------------------------------

arguments
    Data
    Dim                              = [];
    Args.MeanFun                     = @mean;
    Args.StdFun                      = 'rstd';
    Args.Nsigma(1,2)                 = [5 5];
    Args.MaxIter(1,1)                = 3;
    Args.EpsilonStd                  = 1e-12;
end

if isempty(Dim)
    Dim = ndims(Data);
end


% if Niter=0, will calculate mean without sigma clipping
Iter = 0;
FlagGood = true(size(Data));
Ngood    = numel(Data);
NrejectNew = Inf;
Nreject    = Inf;
while Iter<=Args.MaxIter && NrejectNew~=0
    if Iter==0
        FlagGood = true(size(Data));
        DataF = Data;
    else
        Zstat = (DataF - Mean)./(Std+Args.EpsilonStd);
        FlagGood = Zstat>(-abs(Args.Nsigma(1))) & Zstat<Args.Nsigma(2);
        DataF(~FlagGood) = NaN;
    end
    % total number of rejected data points
    NrejectPrev = Nreject;
    Nreject     = Ngood-sum(FlagGood,'all');
    NrejectNew  = Nreject-NrejectPrev;
    
    Iter = Iter + 1;
    
    Mean = Args.MeanFun(DataF,Dim,'omitnan');
    switch lower(Args.StdFun)
        case 'std'
            Std  = nanstd(DataF,[],Dim);
        case 'rstd'
            Std  = imUtil.background.rstd(DataF,Dim);
        otherwise
            error('Unknown StdFun option');
    end

end

Var = Std.^2;

if nargout>3
    GoodCounter = sum(~isnan(DataF),Dim);
end










