function [Mean,Var,FlagGood,GoodCounter]=wmean_sigclip(Data,Var,Dim,varargin)
% Calculate the sigma-clipped weighted mean of a dataset
% Package: imUtil.image
% Description: Calculate the sigma-clipped weighted mean of a dataset with
%              iterations.
%              This is done by first selecting the datapoints to be used
%              useing imUtil.image.mean_sigclip and applying the weighted
%              mean to the result.
%              If the provided variances are all zeros than will set it to
%              1.
% Input  : - The dataset of any dimensionality.
%          - The variance of the dataset.
%          - Dimension over which to calculate the mean.
%            For example, if you have a cube of images in which the
%            3rd dimension is the image index, and you want to calculate
%            the mean image. Set Dim=3.
%            Deafult is ndims(Data).
%          * Arbitrary number of ...,key,val,... arguments to pass to
%            imUtil.image.mean_sigclip:
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
% Output : - Sigma clipped weighted mean of data.
%          - Sigma clipped weighted variance of adat.
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
%          [Mean,VarFlagGood,NC]=imUtil.image.mean_sigclip(Data,3);
%          [i,j,k]=ind2sub(size(Data),find(~FlagGood))
% Reliable: 2
%--------------------------------------------------------------------------

if nargin<3
    Dim = ndims(Data);
end

InPar = inputParser;

addOptional(InPar,'MeanFun',@tools.math.stat.nanmean);  
addOptional(InPar,'StdFun','rstd');   % std | rstd  
addOptional(InPar,'Nsigma',[5 5]);
addOptional(InPar,'MaxIter',3);


parse(InPar,varargin{:});
InPar = InPar.Results;

[~,~,FlagGood]=imUtil.image.mean_sigclip(Data,Dim);
Data(~FlagGood) = NaN;

% if all variances are zero, than set weight to 1.
if all(Var==0)
    Var = 1;
end

InvVar = 1./Var;

% weighted mean
Mean = nansum(Data.*InvVar,Dim)./nansum(InvVar,Dim);
if nargout>1
    % weighted variance
    Var  = 1./nansum(InvVar,Dim);
    if nargout>3
        GoodCounter = sum(~isnan(Data.*InvVar),Dim);
    end
end



