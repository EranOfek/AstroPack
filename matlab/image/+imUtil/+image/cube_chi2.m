function [Chi2,Z,Mean,Std,NaboveThreshold]=cube_chi2(Data,Dim,Args)
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
%            'ThresholdSigma' - Will count the number of times that the
%                   value is above this threshold (in units of sigma) for
%                   each pixel. Default is 5.
%            'Abs' - Use abs value when counting the number of events above
%                   sigma (i.e., true will count above and below the
%                   threshold).
%                   Default is true.
% Output : - A matrix of chi^2 per pixel calculated over the index
%            dimension.
%          - A cube of the Z statistics (distance of each pixel from the
%            mean in units of the std).
%          - A matrix of the mean.
%          - A matrix of the Std.
%          - A matrix of the number of times the pixel is above/below the
%            threshold.
% License: GNU general public license version 3
% Tested : Matlab R2015b
%     By : Eran O. Ofek                    Sep 2016
%    URL : http://weizmann.ac.il/home/eofek/matlab/
% Example:
% [Chi2,Z,Mean,Std,NaboveThreshold]=imUtil.image.cube_chi2(rand(10,10,5),3);
% Reliable: 2
%--------------------------------------------------------------------------

arguments
    Data
    Dim                          = [];
    Args.MeanFun                 = @tools.math.stat.nanmean;
    Args.StdFun                  = 'rstd';
    Args.ThresholdSigma          = 5;
    Args.Abs(1,1) logical        = true;
    
end

if isempty(Dim)
    Dim = ndims(Data);
end
% 
% if nargin<3
%     Dim = ndims(Data);
% end
% 
% InPar = inputParser;
% addOptional(InPar,'MeanFun',@nanmean);  
% addOptional(InPar,'StdFun','rstd');   % std | rstd  
% addOptional(InPar,'ThresholdSigma',5);
% addOptional(InPar,'Abs',true);
% parse(InPar,varargin{:});
% InPar = InPar.Results;


Mean = Args.MeanFun(Data,Dim);
switch lower(Args.StdFun)
    case 'std'
        %Std  = nanstd(Data,[],Dim);
        Std  = std(Data,[],Dim, 'omitnan');
    case 'rstd'
        Std  = sqrt(imUtil.background.rvar(Data,Dim));
    otherwise
        error('Unknown StdFun option');
end

Z = (Data - Mean)./Std;
Chi2 = sum(Z.^2,Dim);

if nargout>4
    NaboveThreshold = sum(abs(Z)>Args.ThresholdSigma,Dim);
end

